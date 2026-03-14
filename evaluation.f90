! ============================================
! Module: Evaluation
! Purpose: Static board evaluation using material and piece-square tables
! ============================================
MODULE Evaluation
    USE Chess_Types, ONLY: PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, NO_PIECE, &
                           BOARD_SIZE, Board_Type, Square_Type, WHITE, BLACK, &
                           KNIGHT_DELTAS, BISHOP_DIRS
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: evaluate_board

    ! --- Game Phase Constants ---
    INTEGER, PARAMETER :: KNIGHT_PHASE = 1
    INTEGER, PARAMETER :: BISHOP_PHASE = 1
    INTEGER, PARAMETER :: ROOK_PHASE = 2
    INTEGER, PARAMETER :: QUEEN_PHASE = 4
    INTEGER, PARAMETER :: TOTAL_PHASE = (KNIGHT_PHASE * 2 + BISHOP_PHASE * 2 + ROOK_PHASE * 2 + QUEEN_PHASE) * 2 ! = 24
    ! Material values and PSTs borrowed from chess.cpp (mg/eg tapered eval)
    INTEGER, PARAMETER :: MATERIAL_MG(6) = (/ 82, 337, 365, 477, 1025, 20000 /)
    INTEGER, PARAMETER :: MATERIAL_EG(6) = (/ 94, 281, 297, 512, 936, 20000 /)

    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PSTM = RESHAPE( (/ &
        0, 0, 0, 0, 0, 0, 0, 0, &
        -35, -1, -20, -23, -15, 24, 38, -22, &
        -26, -4, -4, -10, 3, 3, 33, -12, &
        -27, -2, -5, 12, 17, 6, 10, -25, &
        -14, 13, 6, 21, 23, 12, 17, -23, &
        -6, 7, 26, 31, 65, 56, 25, -20, &
        98, 134, 61, 95, 68, 126, 34, -11, &
        0, 0, 0, 0, 0, 0, 0, 0 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PSTM = RESHAPE( (/ &
        -105, -21, -58, -33, -17, -28, -19, -23, &
        -29, -53, -12, -3, -1, 18, -14, -19, &
        -23, -9, 12, 10, 19, 17, 25, -16, &
        -13, 4, 16, 13, 28, 19, 21, -8, &
        -9, 17, 19, 53, 37, 69, 18, 22, &
        -47, 60, 37, 65, 84, 129, 73, 44, &
        -73, -41, 72, 36, 23, 62, 7, -17, &
        -167, -89, -34, -49, 61, -97, -15, -107 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PSTM = RESHAPE( (/ &
        -33, -3, -14, -21, -13, -12, -39, -21, &
        4, 15, 16, 0, 7, 21, 33, 1, &
        0, 15, 15, 15, 14, 27, 18, 10, &
        -6, 13, 13, 26, 34, 12, 10, 4, &
        -4, 5, 19, 50, 37, 37, 7, -2, &
        -16, 37, 43, 40, 35, 50, 37, -2, &
        -26, 16, -18, -13, 30, 59, 18, -47, &
        -29, 4, -82, -37, -25, -42, 7, -8 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PSTM = RESHAPE( (/ &
        -19, -13, 1, 17, 16, 7, -37, -26, &
        -44, -16, -20, -9, -1, 11, -6, -71, &
        -45, -25, -16, -17, 3, 0, -5, -33, &
        -36, -26, -12, -1, 9, -7, 6, -23, &
        -24, -11, 7, 26, 24, 35, -8, -20, &
        -5, 19, 26, 36, 17, 45, 61, 16, &
        27, 32, 58, 62, 80, 67, 26, 44, &
        32, 42, 32, 51, 63, 9, 31, 43 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PSTM = RESHAPE( (/ &
        -1, -18, -9, 10, -15, -25, -31, -50, &
        -35, -8, 11, 2, 8, 15, -3, 1, &
        -14, 2, -11, -2, -5, 2, 14, 5, &
        -9, -26, -9, -10, -2, -4, 3, -3, &
        -27, -27, -16, -16, -1, 17, -2, 1, &
        -13, -17, 7, 8, 29, 56, 47, 57, &
        -24, -39, -5, 1, -16, 57, 28, 54, &
        -28, 0, 29, 12, 59, 44, 43, 45 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PSTM = RESHAPE( (/ &
        -15, 36, 12, -54, 8, -28, 34, 14, &
        1, 7, -8, -64, -43, -16, 9, 8, &
        -14, -14, -22, -46, -44, -30, -15, -27, &
        -49, -1, -27, -39, -46, -44, -33, -51, &
        -17, -20, -12, -27, -30, -25, -14, -36, &
        -9, 24, 2, -16, -20, 6, 22, -22, &
        29, -1, -20, -7, -8, -4, -38, -29, &
        -65, 23, 16, -15, -56, -34, 2, 13 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PSTE = RESHAPE( (/ &
        0, 0, 0, 0, 0, 0, 0, 0, &
        13, 8, 8, 10, 13, 0, 2, -7, &
        4, 7, -6, 1, 0, -5, -1, -8, &
        13, 9, -3, -7, -7, -8, 3, -1, &
        32, 24, 13, 5, -2, 4, 17, 17, &
        94, 100, 85, 67, 56, 53, 82, 84, &
        178, 173, 158, 134, 147, 132, 165, 187, &
        0, 0, 0, 0, 0, 0, 0, 0 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PSTE = RESHAPE( (/ &
        -29, -51, -23, -15, -22, -18, -50, -64, &
        -42, -20, -10, -5, -2, -20, -23, -44, &
        -23, -3, -1, 15, 10, -3, -20, -22, &
        -18, -6, 16, 25, 16, 17, 4, -18, &
        -17, 3, 22, 22, 22, 11, 8, -18, &
        -24, -20, 10, 9, -1, -9, -19, -41, &
        -25, -8, -25, -2, -9, -25, -24, -52, &
        -58, -38, -13, -28, -31, -27, -63, -99 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PSTE = RESHAPE( (/ &
        -23, -9, -23, -5, -9, -16, -5, -17, &
        -14, -18, -7, -1, 4, -9, -15, -27, &
        -12, -3, 8, 10, 13, 3, -7, -15, &
        -6, 3, 13, 19, 7, 10, -3, -9, &
        -3, 9, 12, 9, 14, 10, 3, 2, &
        2, -8, 0, -1, -2, 6, 0, 4, &
        -8, -4, 7, -12, -3, -13, -4, -14, &
        -14, -21, -11, -8, -7, -9, -17, -24 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PSTE = RESHAPE( (/ &
        -9, 2, 3, -1, -5, -13, 4, -20, &
        -6, -6, 0, 2, -9, -9, -11, -3, &
        -4, 0, -5, -1, -7, -12, -8, -16, &
        3, 5, 8, 4, -5, -6, -8, -11, &
        4, 3, 13, 1, 2, 1, -1, 2, &
        7, 7, 7, 5, 4, -3, -5, -3, &
        11, 13, 13, 11, -3, 3, 8, 3, &
        13, 10, 18, 15, 12, 12, 8, 5 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PSTE = RESHAPE( (/ &
        -33, -28, -22, -43, -5, -32, -20, -41, &
        -22, -23, -30, -16, -16, -23, -36, -32, &
        -16, -27, 15, 6, 9, 17, 10, 5, &
        -18, 28, 19, 47, 31, 34, 39, 23, &
        3, 22, 24, 45, 57, 40, 57, 36, &
        -20, 6, 9, 49, 47, 35, 19, 9, &
        -17, 20, 32, 41, 58, 25, 30, 0, &
        -9, 22, 22, 27, 27, 19, 10, 20 /), (/8,8/), ORDER=(/2,1/))

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PSTE = RESHAPE( (/ &
        -53, -34, -21, -11, -28, -14, -24, -43, &
        -27, -11, 4, 13, 14, 4, -5, -17, &
        -19, -3, 11, 21, 23, 16, 7, -9, &
        -18, -4, 21, 24, 27, 23, 9, -11, &
        -8, 22, 24, 27, 26, 33, 26, 3, &
        10, 17, 23, 15, 20, 45, 44, 13, &
        -12, 17, 14, 17, 17, 38, 23, 11, &
        -74, -35, -18, -18, -11, 15, 4, -17 /), (/8,8/), ORDER=(/2,1/))


CONTAINS

    ! --- Get Piece Phase Value ---
    ! Returns the phase contribution for a piece type (used for tapered eval).
    PURE INTEGER FUNCTION get_piece_phase(piece)
        INTEGER, INTENT(IN) :: piece
        SELECT CASE(piece)
        CASE(KNIGHT); get_piece_phase = KNIGHT_PHASE
        CASE(BISHOP); get_piece_phase = BISHOP_PHASE
        CASE(ROOK);   get_piece_phase = ROOK_PHASE
        CASE(QUEEN);  get_piece_phase = QUEEN_PHASE
        CASE DEFAULT; get_piece_phase = 0
        END SELECT
    END FUNCTION get_piece_phase

    ! --- Calculate Tapered PST Value ---
    ! Blends middlegame and endgame PST values based on the game phase.
    !
    ! Parameters:
    !   mg_val (IN): Middlegame PST value
    !   eg_val (IN): Endgame PST value
    !   phase (IN): Current game phase (0 = endgame, TOTAL_PHASE = middlegame)
    !   total_phase (IN): Maximum possible game phase value
    !
    ! Returns:
    !   Blended PST value
    PURE INTEGER FUNCTION tapered_pst_value(mg_val, eg_val, phase, total_phase) RESULT(blended_value)
        INTEGER, INTENT(IN) :: mg_val, eg_val, phase, total_phase
        blended_value = ((mg_val * phase) + (eg_val * (total_phase - phase))) / total_phase
    END FUNCTION tapered_pst_value

    ! --- Evaluate Board ---
    ! Performs static evaluation of the current board position.
    !
    ! Evaluation combines material balance with positional factors:
    ! - Material: Piece values (pawn=100, knight=320, etc.)
    ! - Position: Piece-square table bonuses/penalties
    !
    ! The evaluation is calculated as: white_score - black_score
    ! Positive scores favor white, negative favor black.
    !
    ! Parameters:
    !   board (IN): Current board state to evaluate
    !
    ! Returns:
    !   Evaluation score in centipawns (positive = white advantage)
    !
    ! Notes:
    !   - Uses piece lists for efficient iteration
    !   - PSTs are flipped for black pieces (rank 8 becomes rank 1)
    !   - King value is high to ensure mate detection
    INTEGER FUNCTION evaluate_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: i, r, f, piece, eval_rank
        INTEGER :: phase
        TYPE(Square_Type) :: sq
        INTEGER :: mg_score, eg_score
        INTEGER :: mg_pst, eg_pst
        INTEGER :: white_bishops, black_bishops
        INTEGER :: white_pawn_count(BOARD_SIZE), black_pawn_count(BOARD_SIZE)
        LOGICAL :: white_pawn_on_file(BOARD_SIZE), black_pawn_on_file(BOARD_SIZE)
        INTEGER :: pr, blocked
        INTEGER :: white_king_r, white_king_f, black_king_r, black_king_f
        INTEGER :: shield_r, shield_f, mobility, nr, nf, dr, df, dir_idx
        LOGICAL :: has_adj_pawn

        ! --- Bonus/penalty constants ---
        INTEGER, PARAMETER :: BISHOP_PAIR_MG = 30, BISHOP_PAIR_EG = 50
        INTEGER, PARAMETER :: ROOK_OPEN_FILE_MG = 20, ROOK_OPEN_FILE_EG = 10
        INTEGER, PARAMETER :: ROOK_SEMI_OPEN_MG = 10, ROOK_SEMI_OPEN_EG = 5
        INTEGER, PARAMETER :: PASSED_PAWN_BONUS_MG(8) = (/ 0, 5, 10, 20, 35, 60, 100, 0 /)
        INTEGER, PARAMETER :: PASSED_PAWN_BONUS_EG(8) = (/ 0, 10, 20, 40, 70, 120, 200, 0 /)
        INTEGER, PARAMETER :: DOUBLED_PAWN_MG = -10, DOUBLED_PAWN_EG = -20
        INTEGER, PARAMETER :: ISOLATED_PAWN_MG = -15, ISOLATED_PAWN_EG = -20
        INTEGER, PARAMETER :: CONNECTED_PAWN_MG = 7, CONNECTED_PAWN_EG = 5
        INTEGER, PARAMETER :: PAWN_SHIELD_BONUS = 10
        INTEGER, PARAMETER :: MOBILITY_BONUS_MG = 4, MOBILITY_BONUS_EG = 3

        ! --- Calculate Game Phase ---
        phase = 0
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            phase = phase + get_piece_phase(piece)
        END DO
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            phase = phase + get_piece_phase(piece)
        END DO
        phase = MIN(phase, TOTAL_PHASE)

        ! --- Build pawn file maps, count bishops, find kings ---
        white_bishops = 0
        black_bishops = 0
        white_pawn_on_file = .FALSE.
        black_pawn_on_file = .FALSE.
        white_pawn_count = 0
        black_pawn_count = 0
        white_king_r = 1; white_king_f = 5
        black_king_r = 8; black_king_f = 5

        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            IF (piece == BISHOP) white_bishops = white_bishops + 1
            IF (piece == PAWN) THEN
                white_pawn_on_file(sq%file) = .TRUE.
                white_pawn_count(sq%file) = white_pawn_count(sq%file) + 1
            END IF
            IF (piece == KING) THEN
                white_king_r = sq%rank; white_king_f = sq%file
            END IF
        END DO
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            IF (piece == BISHOP) black_bishops = black_bishops + 1
            IF (piece == PAWN) THEN
                black_pawn_on_file(sq%file) = .TRUE.
                black_pawn_count(sq%file) = black_pawn_count(sq%file) + 1
            END IF
            IF (piece == KING) THEN
                black_king_r = sq%rank; black_king_f = sq%file
            END IF
        END DO

        ! --- Evaluate Pieces (mg/eg components) ---
        mg_score = 0
        eg_score = 0

        ! Evaluate white pieces
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = r  ! White: rank 1-8 as-is

            CALL get_piece_pst(piece, eval_rank, f, mg_pst, eg_pst)
            mg_score = mg_score + MATERIAL_MG(piece) + mg_pst
            eg_score = eg_score + MATERIAL_EG(piece) + eg_pst

            ! Rook on open/semi-open file
            IF (piece == ROOK) THEN
                IF (.NOT. white_pawn_on_file(f) .AND. .NOT. black_pawn_on_file(f)) THEN
                    mg_score = mg_score + ROOK_OPEN_FILE_MG
                    eg_score = eg_score + ROOK_OPEN_FILE_EG
                ELSE IF (.NOT. white_pawn_on_file(f)) THEN
                    mg_score = mg_score + ROOK_SEMI_OPEN_MG
                    eg_score = eg_score + ROOK_SEMI_OPEN_EG
                END IF
            END IF

            ! Passed pawn detection (no enemy pawns blocking or guarding)
            IF (piece == PAWN) THEN
                blocked = 0
                DO pr = r + 1, BOARD_SIZE
                    IF (board%squares_piece(pr, f) == PAWN .AND. &
                        board%squares_color(pr, f) == BLACK) blocked = 1
                    IF (f > 1) THEN
                        IF (board%squares_piece(pr, f-1) == PAWN .AND. &
                            board%squares_color(pr, f-1) == BLACK) blocked = 1
                    END IF
                    IF (f < BOARD_SIZE) THEN
                        IF (board%squares_piece(pr, f+1) == PAWN .AND. &
                            board%squares_color(pr, f+1) == BLACK) blocked = 1
                    END IF
                END DO
                IF (blocked == 0) THEN
                    mg_score = mg_score + PASSED_PAWN_BONUS_MG(r)
                    eg_score = eg_score + PASSED_PAWN_BONUS_EG(r)
                END IF
                ! Connected pawns: defended by another pawn diagonally behind
                IF (r > 1) THEN
                    IF ((f > 1 .AND. board%squares_piece(r-1, f-1) == PAWN .AND. &
                         board%squares_color(r-1, f-1) == WHITE) .OR. &
                        (f < BOARD_SIZE .AND. board%squares_piece(r-1, f+1) == PAWN .AND. &
                         board%squares_color(r-1, f+1) == WHITE)) THEN
                        mg_score = mg_score + CONNECTED_PAWN_MG
                        eg_score = eg_score + CONNECTED_PAWN_EG
                    END IF
                END IF
            END IF

            ! Knight/Bishop mobility: count available squares
            IF (piece == KNIGHT) THEN
                mobility = 0
                DO dir_idx = 1, 8
                    nr = r + KNIGHT_DELTAS(dir_idx, 1)
                    nf = f + KNIGHT_DELTAS(dir_idx, 2)
                    IF (nr >= 1 .AND. nr <= BOARD_SIZE .AND. nf >= 1 .AND. nf <= BOARD_SIZE) THEN
                        IF (board%squares_color(nr, nf) /= WHITE) mobility = mobility + 1
                    END IF
                END DO
                mg_score = mg_score + mobility * MOBILITY_BONUS_MG
                eg_score = eg_score + mobility * MOBILITY_BONUS_EG
            ELSE IF (piece == BISHOP) THEN
                mobility = 0
                DO dir_idx = 1, 4
                    dr = BISHOP_DIRS(dir_idx, 1)
                    df = BISHOP_DIRS(dir_idx, 2)
                    nr = r + dr; nf = f + df
                    DO WHILE (nr >= 1 .AND. nr <= BOARD_SIZE .AND. nf >= 1 .AND. nf <= BOARD_SIZE)
                        IF (board%squares_color(nr, nf) == WHITE) EXIT
                        mobility = mobility + 1
                        IF (board%squares_piece(nr, nf) /= NO_PIECE) EXIT
                        nr = nr + dr; nf = nf + df
                    END DO
                END DO
                mg_score = mg_score + mobility * MOBILITY_BONUS_MG
                eg_score = eg_score + mobility * MOBILITY_BONUS_EG
            END IF
        END DO

        ! Evaluate black pieces
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = BOARD_SIZE - r + 1  ! Black: flip ranks (8->1, 1->8)

            CALL get_piece_pst(piece, eval_rank, f, mg_pst, eg_pst)
            mg_score = mg_score - (MATERIAL_MG(piece) + mg_pst)
            eg_score = eg_score - (MATERIAL_EG(piece) + eg_pst)

            ! Rook on open/semi-open file
            IF (piece == ROOK) THEN
                IF (.NOT. white_pawn_on_file(f) .AND. .NOT. black_pawn_on_file(f)) THEN
                    mg_score = mg_score - ROOK_OPEN_FILE_MG
                    eg_score = eg_score - ROOK_OPEN_FILE_EG
                ELSE IF (.NOT. black_pawn_on_file(f)) THEN
                    mg_score = mg_score - ROOK_SEMI_OPEN_MG
                    eg_score = eg_score - ROOK_SEMI_OPEN_EG
                END IF
            END IF

            ! Passed pawn detection (no enemy pawns blocking or guarding)
            IF (piece == PAWN) THEN
                blocked = 0
                DO pr = r - 1, 1, -1
                    IF (board%squares_piece(pr, f) == PAWN .AND. &
                        board%squares_color(pr, f) == WHITE) blocked = 1
                    IF (f > 1) THEN
                        IF (board%squares_piece(pr, f-1) == PAWN .AND. &
                            board%squares_color(pr, f-1) == WHITE) blocked = 1
                    END IF
                    IF (f < BOARD_SIZE) THEN
                        IF (board%squares_piece(pr, f+1) == PAWN .AND. &
                            board%squares_color(pr, f+1) == WHITE) blocked = 1
                    END IF
                END DO
                IF (blocked == 0) THEN
                    ! Use flipped rank for black (rank 7 = 2nd rank = index 2)
                    mg_score = mg_score - PASSED_PAWN_BONUS_MG(BOARD_SIZE - r + 1)
                    eg_score = eg_score - PASSED_PAWN_BONUS_EG(BOARD_SIZE - r + 1)
                END IF
                ! Connected pawns: defended by another pawn diagonally behind
                IF (r < BOARD_SIZE) THEN
                    IF ((f > 1 .AND. board%squares_piece(r+1, f-1) == PAWN .AND. &
                         board%squares_color(r+1, f-1) == BLACK) .OR. &
                        (f < BOARD_SIZE .AND. board%squares_piece(r+1, f+1) == PAWN .AND. &
                         board%squares_color(r+1, f+1) == BLACK)) THEN
                        mg_score = mg_score - CONNECTED_PAWN_MG
                        eg_score = eg_score - CONNECTED_PAWN_EG
                    END IF
                END IF
            END IF

            ! Knight/Bishop mobility
            IF (piece == KNIGHT) THEN
                mobility = 0
                DO dir_idx = 1, 8
                    nr = r + KNIGHT_DELTAS(dir_idx, 1)
                    nf = f + KNIGHT_DELTAS(dir_idx, 2)
                    IF (nr >= 1 .AND. nr <= BOARD_SIZE .AND. nf >= 1 .AND. nf <= BOARD_SIZE) THEN
                        IF (board%squares_color(nr, nf) /= BLACK) mobility = mobility + 1
                    END IF
                END DO
                mg_score = mg_score - mobility * MOBILITY_BONUS_MG
                eg_score = eg_score - mobility * MOBILITY_BONUS_EG
            ELSE IF (piece == BISHOP) THEN
                mobility = 0
                DO dir_idx = 1, 4
                    dr = BISHOP_DIRS(dir_idx, 1)
                    df = BISHOP_DIRS(dir_idx, 2)
                    nr = r + dr; nf = f + df
                    DO WHILE (nr >= 1 .AND. nr <= BOARD_SIZE .AND. nf >= 1 .AND. nf <= BOARD_SIZE)
                        IF (board%squares_color(nr, nf) == BLACK) EXIT
                        mobility = mobility + 1
                        IF (board%squares_piece(nr, nf) /= NO_PIECE) EXIT
                        nr = nr + dr; nf = nf + df
                    END DO
                END DO
                mg_score = mg_score - mobility * MOBILITY_BONUS_MG
                eg_score = eg_score - mobility * MOBILITY_BONUS_EG
            END IF
        END DO

        ! Bishop pair bonus
        IF (white_bishops >= 2) THEN
            mg_score = mg_score + BISHOP_PAIR_MG
            eg_score = eg_score + BISHOP_PAIR_EG
        END IF
        IF (black_bishops >= 2) THEN
            mg_score = mg_score - BISHOP_PAIR_MG
            eg_score = eg_score - BISHOP_PAIR_EG
        END IF

        ! Doubled and isolated pawn penalties
        DO f = 1, BOARD_SIZE
            ! Doubled pawns: more than one pawn on same file
            IF (white_pawn_count(f) > 1) THEN
                mg_score = mg_score + DOUBLED_PAWN_MG * (white_pawn_count(f) - 1)
                eg_score = eg_score + DOUBLED_PAWN_EG * (white_pawn_count(f) - 1)
            END IF
            IF (black_pawn_count(f) > 1) THEN
                mg_score = mg_score - DOUBLED_PAWN_MG * (black_pawn_count(f) - 1)
                eg_score = eg_score - DOUBLED_PAWN_EG * (black_pawn_count(f) - 1)
            END IF
            ! Isolated pawns: no friendly pawns on adjacent files
            IF (white_pawn_on_file(f)) THEN
                has_adj_pawn = .FALSE.
                IF (f > 1) has_adj_pawn = has_adj_pawn .OR. white_pawn_on_file(f - 1)
                IF (f < BOARD_SIZE) has_adj_pawn = has_adj_pawn .OR. white_pawn_on_file(f + 1)
                IF (.NOT. has_adj_pawn) THEN
                    mg_score = mg_score + ISOLATED_PAWN_MG
                    eg_score = eg_score + ISOLATED_PAWN_EG
                END IF
            END IF
            IF (black_pawn_on_file(f)) THEN
                has_adj_pawn = .FALSE.
                IF (f > 1) has_adj_pawn = has_adj_pawn .OR. black_pawn_on_file(f - 1)
                IF (f < BOARD_SIZE) has_adj_pawn = has_adj_pawn .OR. black_pawn_on_file(f + 1)
                IF (.NOT. has_adj_pawn) THEN
                    mg_score = mg_score - ISOLATED_PAWN_MG
                    eg_score = eg_score - ISOLATED_PAWN_EG
                END IF
            END IF
        END DO

        ! King safety: pawn shield bonus (middlegame only)
        ! Check pawns in front of white king
        DO df = -1, 1
            shield_f = white_king_f + df
            IF (shield_f >= 1 .AND. shield_f <= BOARD_SIZE) THEN
                shield_r = white_king_r + 1
                IF (shield_r >= 1 .AND. shield_r <= BOARD_SIZE) THEN
                    IF (board%squares_piece(shield_r, shield_f) == PAWN .AND. &
                        board%squares_color(shield_r, shield_f) == WHITE) THEN
                        mg_score = mg_score + PAWN_SHIELD_BONUS
                    END IF
                END IF
            END IF
        END DO
        ! Check pawns in front of black king
        DO df = -1, 1
            shield_f = black_king_f + df
            IF (shield_f >= 1 .AND. shield_f <= BOARD_SIZE) THEN
                shield_r = black_king_r - 1
                IF (shield_r >= 1 .AND. shield_r <= BOARD_SIZE) THEN
                    IF (board%squares_piece(shield_r, shield_f) == PAWN .AND. &
                        board%squares_color(shield_r, shield_f) == BLACK) THEN
                        mg_score = mg_score - PAWN_SHIELD_BONUS
                    END IF
                END IF
            END IF
        END DO

        evaluate_board = tapered_pst_value(mg_score, eg_score, phase, TOTAL_PHASE)
        IF (board%current_player == BLACK) evaluate_board = -evaluate_board

    END FUNCTION evaluate_board

    SUBROUTINE get_piece_pst(piece, eval_rank, file, mg_pst, eg_pst)
        INTEGER, INTENT(IN) :: piece, eval_rank, file
        INTEGER, INTENT(OUT) :: mg_pst, eg_pst

        SELECT CASE(piece)
        CASE(PAWN)
            mg_pst = PAWN_PSTM(eval_rank, file)
            eg_pst = PAWN_PSTE(eval_rank, file)
        CASE(KNIGHT)
            mg_pst = KNIGHT_PSTM(eval_rank, file)
            eg_pst = KNIGHT_PSTE(eval_rank, file)
        CASE(BISHOP)
            mg_pst = BISHOP_PSTM(eval_rank, file)
            eg_pst = BISHOP_PSTE(eval_rank, file)
        CASE(ROOK)
            mg_pst = ROOK_PSTM(eval_rank, file)
            eg_pst = ROOK_PSTE(eval_rank, file)
        CASE(QUEEN)
            mg_pst = QUEEN_PSTM(eval_rank, file)
            eg_pst = QUEEN_PSTE(eval_rank, file)
        CASE(KING)
            mg_pst = KING_PSTM(eval_rank, file)
            eg_pst = KING_PSTE(eval_rank, file)
        CASE DEFAULT
            mg_pst = 0
            eg_pst = 0
        END SELECT
    END SUBROUTINE get_piece_pst

END MODULE Evaluation
