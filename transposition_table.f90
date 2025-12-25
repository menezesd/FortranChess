! ============================================
! Module: Transposition_Table
! Purpose: Implements a transposition table with Zobrist hashing
! ============================================
MODULE Transposition_Table
    USE Chess_Types
    IMPLICIT NONE
    PRIVATE

    PUBLIC :: init_zobrist_keys, compute_zobrist_hash, store_tt_entry, probe_tt, &
              TT_Entry_Type, HASH_FLAG_EXACT, HASH_FLAG_ALPHA, HASH_FLAG_BETA, &
              ZOBRIST_PIECES, ZOBRIST_BLACK_TO_MOVE, ZOBRIST_CASTLING, ZOBRIST_EP_FILE, tt, &
              new_search_generation

    ! --- Transposition Table Constants ---
    INTEGER, PARAMETER :: HASH_FLAG_EXACT = 0
    INTEGER, PARAMETER :: HASH_FLAG_ALPHA = 1 ! Lower bound
    INTEGER, PARAMETER :: HASH_FLAG_BETA  = 2 ! Upper bound
    INTEGER, PARAMETER :: TT_SIZE = 2**20 ! ~1 million entries

    ! --- Zobrist Keys (64-bit random numbers) ---
    INTEGER(KIND=8), DIMENSION(6, 2, 8, 8) :: ZOBRIST_PIECES
    INTEGER(KIND=8) :: ZOBRIST_BLACK_TO_MOVE
    INTEGER(KIND=8), DIMENSION(4) :: ZOBRIST_CASTLING
    INTEGER(KIND=8), DIMENSION(8) :: ZOBRIST_EP_FILE

    ! --- Transposition Table Entry ---
    TYPE :: TT_Entry_Type
        INTEGER(KIND=8) :: key = 0      ! Stored Zobrist key
        INTEGER         :: depth = 0    ! Search depth for this entry
        INTEGER         :: score = 0    ! Score from evaluation
        INTEGER         :: flag = 0     ! EXACT, ALPHA, or BETA
        INTEGER         :: age = 0      ! Search generation when stored
        TYPE(Move_Type) :: best_move    ! Best move found
    END TYPE TT_Entry_Type

    ! --- The Transposition Table ---
    TYPE(TT_Entry_Type), DIMENSION(TT_SIZE) :: tt

    ! --- Search Generation Counter ---
    ! Incremented each time a new search starts, used for aging entries
    INTEGER :: tt_generation = 0

CONTAINS

    ! --- Helper to generate a 64-bit random number ---
    FUNCTION random_u64() RESULT(rand_val)
        INTEGER(KIND=8) :: rand_val
        INTEGER :: i
        REAL :: r
        
        rand_val = 0
        DO i = 0, 63
            CALL RANDOM_NUMBER(r)
            IF (r > 0.5) THEN
                rand_val = IBSET(rand_val, i)
            END IF
        END DO
    END FUNCTION random_u64

    ! --- Initialize Zobrist Keys ---
    SUBROUTINE init_zobrist_keys()
        INTEGER :: i, j, k, l
        ! Seed the random number generator
        CALL RANDOM_SEED()

        ! Generate random keys for each piece on each square
        DO i = 1, 6 ! Piece types
            DO j = 1, 2 ! Colors
                DO k = 1, 8 ! Ranks
                    DO l = 1, 8 ! Files
                        ZOBRIST_PIECES(i, j, k, l) = random_u64()
                    END DO
                END DO
            END DO
        END DO

        ! Key for black to move
        ZOBRIST_BLACK_TO_MOVE = random_u64()

        ! Keys for castling rights (WK, WQ, BK, BQ)
        DO i = 1, 4
            ZOBRIST_CASTLING(i) = random_u64()
        END DO

        ! Keys for en passant file
        DO i = 1, 8
            ZOBRIST_EP_FILE(i) = random_u64()
        END DO
    END SUBROUTINE init_zobrist_keys

    ! --- Compute Zobrist Hash from Scratch ---
    FUNCTION compute_zobrist_hash(board) RESULT(hash)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER(KIND=8) :: hash
        INTEGER :: r, f, piece, color

        hash = 0

        ! Hash pieces
        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                piece = board%squares_piece(r, f)
                color = board%squares_color(r, f)
                IF (piece /= NO_PIECE) THEN
                    hash = IEOR(hash, ZOBRIST_PIECES(piece, color, r, f))
                END IF
            END DO
        END DO

        ! Hash turn
        IF (board%current_player == BLACK) THEN
            hash = IEOR(hash, ZOBRIST_BLACK_TO_MOVE)
        END IF

        ! Hash castling rights
        IF (board%wc_k) hash = IEOR(hash, ZOBRIST_CASTLING(1))
        IF (board%wc_q) hash = IEOR(hash, ZOBRIST_CASTLING(2))
        IF (board%bc_k) hash = IEOR(hash, ZOBRIST_CASTLING(3))
        IF (board%bc_q) hash = IEOR(hash, ZOBRIST_CASTLING(4))

        ! Hash en passant target
        IF (board%ep_target_present) THEN
            hash = IEOR(hash, ZOBRIST_EP_FILE(board%ep_target_sq%file))
        END IF

    END FUNCTION compute_zobrist_hash

    ! --- Increment Search Generation ---
    ! Called at the start of each new search to age TT entries
    SUBROUTINE new_search_generation()
        tt_generation = tt_generation + 1
    END SUBROUTINE new_search_generation

    ! --- Store an entry in the TT ---
    ! Uses a depth-preferred replacement strategy with age consideration
    SUBROUTINE store_tt_entry(hash_key, depth, score, flag, best_move)
        INTEGER(KIND=8), INTENT(IN) :: hash_key
        INTEGER, INTENT(IN) :: depth, score, flag
        TYPE(Move_Type), INTENT(IN) :: best_move
        INTEGER(KIND=8) :: index
        LOGICAL :: should_replace

        index = IAND(hash_key, INT(TT_SIZE - 1, KIND=8)) + 1

        ! Determine if we should replace the existing entry
        should_replace = .FALSE.

        IF (tt(index)%key == 0) THEN
            ! Empty slot - always replace
            should_replace = .TRUE.
        ELSE IF (tt(index)%key == hash_key) THEN
            ! Same position - replace if new entry is deeper or same depth
            should_replace = (depth >= tt(index)%depth)
        ELSE IF (tt(index)%age /= tt_generation) THEN
            ! Entry is from a previous search - replace
            should_replace = .TRUE.
        ELSE IF (depth >= tt(index)%depth) THEN
            ! Different position, same generation, but new entry is deeper
            should_replace = .TRUE.
        END IF

        IF (should_replace) THEN
            tt(index)%key = hash_key
            tt(index)%depth = depth
            tt(index)%score = score
            tt(index)%flag = flag
            tt(index)%age = tt_generation
            tt(index)%best_move = best_move
        END IF
    END SUBROUTINE store_tt_entry

    ! --- Probe the TT for an entry ---
    ! Returns .TRUE. if a usable entry is found
    FUNCTION probe_tt(hash_key, depth, alpha, beta, entry) RESULT(found)
        INTEGER(KIND=8), INTENT(IN) :: hash_key
        INTEGER, INTENT(IN) :: depth
        INTEGER, INTENT(INOUT) :: alpha, beta
        TYPE(TT_Entry_Type), INTENT(OUT) :: entry
        LOGICAL :: found
        
        INTEGER(KIND=8) :: index
        
        index = IAND(hash_key, INT(TT_SIZE - 1, KIND=8)) + 1
        entry = tt(index)
        found = .FALSE.

        IF (entry%key == hash_key) THEN
            ! Entry must be from a search at least as deep as the current search
            IF (entry%depth >= depth) THEN
                IF (entry%flag == HASH_FLAG_EXACT) THEN
                    found = .TRUE.
                ELSE IF (entry%flag == HASH_FLAG_ALPHA .AND. entry%score > alpha) THEN
                    alpha = entry%score
                ELSE IF (entry%flag == HASH_FLAG_BETA .AND. entry%score < beta) THEN
                    beta = entry%score
                END IF
                
                IF (alpha >= beta) THEN
                    found = .TRUE.
                END IF
            END IF
        END IF
    END FUNCTION probe_tt

END MODULE Transposition_Table
