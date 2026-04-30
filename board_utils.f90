! ============================================
! Module: Board_Utils
! Purpose: Helper functions for board ops
! ============================================
MODULE Board_Utils
    USE Chess_Types
    USE Transposition_Table, ONLY: ZOBRIST_PIECES, ZOBRIST_BLACK_TO_MOVE, ZOBRIST_CASTLING, ZOBRIST_EP_FILE, compute_zobrist_hash
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: init_board, print_board, get_opponent_color, &
              sq_is_valid, char_to_file, char_to_rank, &
              file_rank_to_sq, is_square_attacked, &
              find_king, is_in_check, update_piece_lists, &
              update_piece_lists_unmake, remove_from_piece_list, &
              update_piece_position, add_to_piece_list, &
              is_fifty_move_draw, is_threefold_repetition, see_capture, compute_pawn_hash

CONTAINS

    ! --- Function to convert file char ('a'-'h') to index (1-8) ---
    ! Converts algebraic chess notation file character to array index.
    !
    ! Parameters:
    !   file_char (IN): Character 'a' through 'h' (case insensitive)
    !
    ! Returns:
    !   Integer 1-8 corresponding to file (a=1, b=2, ..., h=8)
    !   Returns -1 if the input is invalid
    INTEGER FUNCTION char_to_file(file_char)
        CHARACTER(LEN=1), INTENT(IN) :: file_char
        CHARACTER(LEN=1) :: lower_char

        ! Convert to lowercase for case-insensitive comparison
        IF (file_char >= 'A' .AND. file_char <= 'H') THEN
            lower_char = ACHAR(IACHAR(file_char) + 32)
        ELSE
            lower_char = file_char
        END IF

        ! Validate input
        IF (lower_char >= 'a' .AND. lower_char <= 'h') THEN
            char_to_file = ICHAR(lower_char) - ICHAR('a') + 1
        ELSE
            char_to_file = -1  ! Invalid input
        END IF
    END FUNCTION char_to_file

    ! --- Function to convert rank char ('1'-'8') to index (1-8) ---
    ! Converts algebraic chess notation rank character to array index.
    !
    ! Parameters:
    !   rank_char (IN): Character '1' through '8'
    !
    ! Returns:
    !   Integer 1-8 corresponding to rank (1=1, 2=2, ..., 8=8)
    !   Returns -1 if the input is invalid
    INTEGER FUNCTION char_to_rank(rank_char)
        CHARACTER(LEN=1), INTENT(IN) :: rank_char
        INTEGER :: ierr, result

        ! Validate input is a digit
        IF (rank_char >= '1' .AND. rank_char <= '8') THEN
            READ (rank_char, '(I1)', IOSTAT=ierr) result
            IF (ierr == 0) THEN
                char_to_rank = result
            ELSE
                char_to_rank = -1  ! Read error
            END IF
        ELSE
            char_to_rank = -1  ! Invalid input
        END IF
    END FUNCTION char_to_rank

    ! --- Subroutine to create a Square_Type ---
    ! Creates a Square_Type structure from file and rank coordinates.
    !
    ! Parameters:
    !   file (IN): File index (1-8)
    !   rank (IN): Rank index (1-8)
    !
    ! Returns:
    !   Square_Type structure with specified coordinates
    FUNCTION file_rank_to_sq(file, rank) RESULT(sq)
        INTEGER, INTENT(IN) :: file, rank
        TYPE(Square_Type)   :: sq
        sq%file = file
        sq%rank = rank
    END FUNCTION file_rank_to_sq

    ! --- Function to check if square indices are valid (1-8) ---
    ! Validates that file and rank coordinates are within chess board bounds.
    !
    ! Parameters:
    !   rank (IN): Rank coordinate to check
    !   file (IN): File coordinate to check
    !
    ! Returns:
    !   True if coordinates are valid (1-8), false otherwise
    LOGICAL FUNCTION sq_is_valid(rank, file)
        INTEGER, INTENT(IN) :: rank, file
        sq_is_valid = (rank >= 1 .AND. rank <= BOARD_SIZE .AND. &
                       file >= 1 .AND. file <= BOARD_SIZE)
    END FUNCTION sq_is_valid

    ! --- Get opponent color ---
    ! Returns the opposite color of the given player color.
    !
    ! Parameters:
    !   player_color (IN): WHITE or BLACK
    !
    ! Returns:
    !   BLACK if input is WHITE, WHITE if input is BLACK, NO_COLOR for invalid input
    INTEGER FUNCTION get_opponent_color(player_color)
        INTEGER, INTENT(IN) :: player_color
        IF (player_color == WHITE) THEN
            get_opponent_color = BLACK
        ELSE IF (player_color == BLACK) THEN
            get_opponent_color = WHITE
        ELSE
            get_opponent_color = NO_COLOR ! Should not happen
        END IF
    END FUNCTION get_opponent_color

    ! --- Initialize Board to Starting Position ---
    SUBROUTINE init_board(board)
        TYPE(Board_Type), INTENT(OUT) :: board

        INTEGER :: f
        INTEGER, DIMENSION(BOARD_SIZE) :: back_rank_pieces = &
                (/ ROOK, KNIGHT, BISHOP, QUEEN, KING, BISHOP, KNIGHT, ROOK /)

        ! Clear board
        board%squares_piece = NO_PIECE
        board%squares_color = NO_COLOR

        ! Initialize piece lists
        board%num_white_pieces = 0
        board%num_black_pieces = 0

        ! Place pieces
        DO f = 1, BOARD_SIZE
            ! White pieces
            board%squares_piece(1, f) = back_rank_pieces(f)
            board%squares_color(1, f) = WHITE
            board%num_white_pieces = board%num_white_pieces + 1
            board%white_pieces(board%num_white_pieces) = file_rank_to_sq(f, 1)
            
            board%squares_piece(2, f) = PAWN
            board%squares_color(2, f) = WHITE
            board%num_white_pieces = board%num_white_pieces + 1
            board%white_pieces(board%num_white_pieces) = file_rank_to_sq(f, 2)
            
            ! Black pieces
            board%squares_piece(8, f) = back_rank_pieces(f)
            board%squares_color(8, f) = BLACK
            board%num_black_pieces = board%num_black_pieces + 1
            board%black_pieces(board%num_black_pieces) = file_rank_to_sq(f, 8)
            
            board%squares_piece(7, f) = PAWN
            board%squares_color(7, f) = BLACK
            board%num_black_pieces = board%num_black_pieces + 1
            board%black_pieces(board%num_black_pieces) = file_rank_to_sq(f, 7)
        END DO

        ! Set initial state
        board%current_player = WHITE
        board%ep_target_present = .FALSE.
        board%ep_target_sq%rank = 0
        board%ep_target_sq%file = 0
        board%wc_k = .TRUE.
        board%wc_q = .TRUE.
        board%bc_k = .TRUE.
        board%bc_q = .TRUE.
        board%halfmove_clock = 0
        board%fullmove_number = 1

        ! Compute initial Zobrist key
        board%zobrist_key = compute_zobrist_hash(board)
        board%pawn_hash_key = compute_pawn_hash(board)
        board%repetition_history = 0
        board%repetition_count = 1
        board%repetition_history(1) = board%zobrist_key

    END SUBROUTINE init_board

    FUNCTION compute_pawn_hash(board) RESULT(hash)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER(KIND=8) :: hash
        INTEGER :: i
        TYPE(Square_Type) :: sq

        hash = 0_8

        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            IF (board%squares_piece(sq%rank, sq%file) == PAWN) THEN
                hash = IEOR(hash, ZOBRIST_PIECES(PAWN, WHITE, sq%rank, sq%file))
            END IF
        END DO

        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            IF (board%squares_piece(sq%rank, sq%file) == PAWN) THEN
                hash = IEOR(hash, ZOBRIST_PIECES(PAWN, BLACK, sq%rank, sq%file))
            END IF
        END DO
    END FUNCTION compute_pawn_hash

    ! --- Print Board to Console ---
    ! Displays the current board state in a human-readable format.
    !
    ! Shows the chess board with algebraic notation coordinates, piece symbols,
    ! and current player turn. White pieces are uppercase, black pieces lowercase.
    !
    ! Parameters:
    !   board (IN): Board state to display
    !
    ! Side effects:
    !   Prints board representation to console
    !   Shows current player turn
    SUBROUTINE print_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: r, f
        CHARACTER(LEN=1) :: piece_char

        PRINT *, " +---+---+---+---+---+---+---+---+"
        DO r = BOARD_SIZE, 1, -1
            WRITE(*, '(I1,A)', ADVANCE='NO') r, " |"
            DO f = 1, BOARD_SIZE
                SELECT CASE (board%squares_piece(r,f))
                CASE (PAWN)
                    piece_char = 'P'
                CASE (KNIGHT)
                    piece_char = 'N'
                CASE (BISHOP)
                    piece_char = 'B'
                CASE (ROOK)
                    piece_char = 'R'
                CASE (QUEEN)
                    piece_char = 'Q'
                CASE (KING)
                    piece_char = 'K'
                CASE DEFAULT
                    piece_char = ' '
                END SELECT

                IF (board%squares_color(r,f) == BLACK .AND. piece_char /= '.') THEN
                    ! Crude lowercase for black
                    piece_char = ACHAR(IACHAR(piece_char) + 32)
                END IF
                 WRITE(*, '(A,A)', ADVANCE='NO') " "//piece_char//" |"
            END DO
            PRINT *
            PRINT *, " +---+---+---+---+---+---+---+---+"
        END DO
        PRINT *, "    a   b   c   d   e   f   g   h"
        IF (board%current_player == WHITE) THEN
            PRINT *, "Turn: White"
        ELSE
            PRINT *, "Turn: Black"
        END IF
        ! Add EP target, Castling rights printout if desired
    END SUBROUTINE print_board


    ! --- Find King of a given color ---
    ! Locates the king of the specified color on the board.
    !
    ! Searches the entire board to find the king piece of the given color.
    ! Used for check detection and castling validation.
    !
    ! Parameters:
    !   board (IN): Current board state
    !   king_color (IN): Color of king to find (WHITE or BLACK)
    !
    ! Returns:
    !   Square_Type with king's position, or (0,0) if not found
    !
    ! Note: In a valid chess position, exactly one king of each color should exist
    FUNCTION find_king(board, king_color) RESULT(king_sq)

        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN)          :: king_color
        TYPE(Square_Type)            :: king_sq
        INTEGER :: i, r, f

        king_sq%rank = 0 ! Indicate not found initially
        king_sq%file = 0

        IF (king_color == WHITE) THEN
            DO i = 1, board%num_white_pieces
                r = board%white_pieces(i)%rank
                f = board%white_pieces(i)%file
                IF (board%squares_piece(r, f) == KING) THEN
                    king_sq = board%white_pieces(i)
                    RETURN
                END IF
            END DO
        ELSE
            DO i = 1, board%num_black_pieces
                r = board%black_pieces(i)%rank
                f = board%black_pieces(i)%file
                IF (board%squares_piece(r, f) == KING) THEN
                    king_sq = board%black_pieces(i)
                    RETURN
                END IF
            END DO
        END IF

    END FUNCTION find_king

    LOGICAL FUNCTION is_square_attacked(board, target_sq, attacker_color) RESULT(is_attacked)
        TYPE(Board_Type), INTENT(IN)  :: board
        TYPE(Square_Type), INTENT(IN) :: target_sq
        INTEGER, INTENT(IN)           :: attacker_color

        INTEGER :: i, tr, tf, r, f, piece, color, dir, df, dr
        LOGICAL :: is_diagonal

        is_attacked = .FALSE. ! Assume not attacked initially
        tr = target_sq%rank
        tf = target_sq%file

        ! 1. Check Pawn attacks
        IF (attacker_color == WHITE) THEN
            dir = -1 ! White pawns attack southwards (from rank+1)
        ELSE
            dir = 1  ! Black pawns attack northwards (from rank-1)
        END IF
        r = tr + dir
        DO df = -1, 1, 2 ! Check files tf-1 and tf+1
            f = tf + df
            IF (sq_is_valid(r, f)) THEN
                IF (board%squares_piece(r, f) == PAWN .AND. &
                    board%squares_color(r, f) == attacker_color) THEN
                    is_attacked = .TRUE.
                    RETURN
                END IF
            END IF
        END DO

        ! 2. Check Knight attacks
        DO i = 1, 8
             dr = KNIGHT_DELTAS(i, 1)
             df = KNIGHT_DELTAS(i, 2)
             r = tr + dr
             f = tf + df
             IF (sq_is_valid(r,f)) THEN
                  IF (board%squares_piece(r,f) == KNIGHT .AND. &
                      board%squares_color(r,f) == attacker_color) THEN
                      is_attacked = .TRUE.
                      RETURN
                  END IF
             END IF
        END DO

        ! 3. Check King attacks
        DO i = 1, 8
             dr = KING_DELTAS(i, 1)
             df = KING_DELTAS(i, 2)
             r = tr + dr
             f = tf + df
             IF (sq_is_valid(r,f)) THEN
                  IF (board%squares_piece(r,f) == KING .AND. &
                      board%squares_color(r,f) == attacker_color) THEN
                      is_attacked = .TRUE.
                      RETURN
                  END IF
             END IF
        END DO

        ! 4. Check Sliding attacks (Rook, Bishop, Queen)
        DO i = 1, 8
            dr = QUEEN_DIRS(i, 1)
            df = QUEEN_DIRS(i, 2)
            is_diagonal = (dr /= 0 .AND. df /= 0)

            r = tr + dr
            f = tf + df
            DO WHILE (sq_is_valid(r, f))
                piece = board%squares_piece(r,f)
                color = board%squares_color(r,f)
                IF (piece /= NO_PIECE) THEN
                    IF (color == attacker_color) THEN
                        ! Check for Queen, or Rook on straight, or Bishop on diagonal
                        IF (piece == QUEEN .OR. &
                           (piece == ROOK .AND. .NOT. is_diagonal) .OR. &
                           (piece == BISHOP .AND. is_diagonal)) THEN
                            is_attacked = .TRUE.
                            RETURN
                        END IF
                    END IF
                    ! An intervening piece blocks further attacks along this line
                    EXIT
                END IF

                r = r + dr
                f = f + df
            END DO
        END DO

    END FUNCTION is_square_attacked

    ! --- Remove a piece from a piece list ---
    ! Removes a piece at the given square from the appropriate piece list.
    !
    ! Parameters:
    !   board (INOUT): Board state with piece lists to modify
    !   sq (IN): Square of the piece to remove
    !   color (IN): Color of the piece to remove (WHITE or BLACK)
    !
    ! Returns:
    !   True if the piece was found and removed, false otherwise
    LOGICAL FUNCTION remove_from_piece_list(board, sq, color)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, INTENT(IN) :: color
        INTEGER :: i

        remove_from_piece_list = .FALSE.
        IF (color == WHITE) THEN
            DO i = 1, board%num_white_pieces
                IF (board%white_pieces(i)%rank == sq%rank .AND. &
                    board%white_pieces(i)%file == sq%file) THEN
                    board%white_pieces(i) = board%white_pieces(board%num_white_pieces)
                    board%num_white_pieces = board%num_white_pieces - 1
                    remove_from_piece_list = .TRUE.
                    RETURN
                END IF
            END DO
        ELSE
            DO i = 1, board%num_black_pieces
                IF (board%black_pieces(i)%rank == sq%rank .AND. &
                    board%black_pieces(i)%file == sq%file) THEN
                    board%black_pieces(i) = board%black_pieces(board%num_black_pieces)
                    board%num_black_pieces = board%num_black_pieces - 1
                    remove_from_piece_list = .TRUE.
                    RETURN
                END IF
            END DO
        END IF
    END FUNCTION remove_from_piece_list

    ! --- Update a piece position in a piece list ---
    ! Moves a piece from one square to another in the appropriate piece list.
    !
    ! Parameters:
    !   board (INOUT): Board state with piece lists to modify
    !   from_sq (IN): Original square of the piece
    !   to_sq (IN): New square for the piece
    !   color (IN): Color of the piece to update (WHITE or BLACK)
    SUBROUTINE update_piece_position(board, from_sq, to_sq, color)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq, to_sq
        INTEGER, INTENT(IN) :: color
        INTEGER :: i

        IF (color == WHITE) THEN
            DO i = 1, board%num_white_pieces
                IF (board%white_pieces(i)%rank == from_sq%rank .AND. &
                    board%white_pieces(i)%file == from_sq%file) THEN
                    board%white_pieces(i) = to_sq
                    RETURN
                END IF
            END DO
        ELSE
            DO i = 1, board%num_black_pieces
                IF (board%black_pieces(i)%rank == from_sq%rank .AND. &
                    board%black_pieces(i)%file == from_sq%file) THEN
                    board%black_pieces(i) = to_sq
                    RETURN
                END IF
            END DO
        END IF
    END SUBROUTINE update_piece_position

    ! --- Add a piece to a piece list ---
    ! Adds a piece at the given square to the appropriate piece list.
    !
    ! Parameters:
    !   board (INOUT): Board state with piece lists to modify
    !   sq (IN): Square of the piece to add
    !   color (IN): Color of the piece to add (WHITE or BLACK)
    SUBROUTINE add_to_piece_list(board, sq, color)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, INTENT(IN) :: color

        IF (color == WHITE) THEN
            board%num_white_pieces = board%num_white_pieces + 1
            board%white_pieces(board%num_white_pieces) = sq
        ELSE
            board%num_black_pieces = board%num_black_pieces + 1
            board%black_pieces(board%num_black_pieces) = sq
        END IF
    END SUBROUTINE add_to_piece_list

    ! --- Check if the king of 'player_color' is in check ---
    LOGICAL FUNCTION is_in_check(board, player_color)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN)          :: player_color
        TYPE(Square_Type) :: king_sq
        INTEGER           :: attacker_color

        king_sq = find_king(board, player_color)
        IF (king_sq%rank == 0) THEN ! King not found (error state)
             is_in_check = .FALSE. ! Or handle error
             RETURN
        END IF

        attacker_color = get_opponent_color(player_color)
        is_in_check = is_square_attacked(board, king_sq, attacker_color)

    END FUNCTION is_in_check

    LOGICAL FUNCTION is_fifty_move_draw(board)
        TYPE(Board_Type), INTENT(IN) :: board
        is_fifty_move_draw = (board%halfmove_clock >= 100)
    END FUNCTION is_fifty_move_draw

    LOGICAL FUNCTION is_threefold_repetition(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: i, matches

        matches = 0
        DO i = 1, board%repetition_count
            IF (board%repetition_history(i) == board%zobrist_key) THEN
                matches = matches + 1
            END IF
        END DO
        is_threefold_repetition = (matches >= 3)
    END FUNCTION is_threefold_repetition


    ! --- Static Exchange Evaluation (SEE) ---
    ! Estimates the material outcome of a capture sequence on a square.
    ! Returns the estimated material gain (positive = good for attacker).
    ! Uses a simplified approach: finds the least valuable attacker for each side.
    INTEGER FUNCTION see_capture(board, move) RESULT(see_val)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), INTENT(IN) :: move

        INTEGER, PARAMETER :: SEE_PIECE_VAL(6) = (/ 100, 320, 330, 500, 900, 20000 /)
        INTEGER :: gain(32), d, piece, color, target_sq_r, target_sq_f
        INTEGER :: attacker_piece, attacker_color
        INTEGER :: r, f
        LOGICAL :: found
        INTEGER :: occ_piece(8, 8), occ_color(8, 8)

        ! Copy board occupancy (we'll "remove" pieces as they capture)
        occ_piece = board%squares_piece
        occ_color = board%squares_color

        target_sq_r = move%to_sq%rank
        target_sq_f = move%to_sq%file

        ! Initial capture
        IF (move%captured_piece == NO_PIECE) THEN
            see_val = 0
            RETURN
        END IF

        attacker_piece = board%squares_piece(move%from_sq%rank, move%from_sq%file)
        attacker_color = board%squares_color(move%from_sq%rank, move%from_sq%file)

        d = 1
        gain(1) = SEE_PIECE_VAL(move%captured_piece)
        IF (move%promotion_piece /= NO_PIECE) THEN
            gain(1) = gain(1) + SEE_PIECE_VAL(move%promotion_piece) - SEE_PIECE_VAL(PAWN)
        END IF

        ! Remove initial attacker from occupancy
        occ_piece(move%from_sq%rank, move%from_sq%file) = NO_PIECE
        occ_color(move%from_sq%rank, move%from_sq%file) = NO_COLOR

        piece = attacker_piece
        color = get_opponent_color(attacker_color)

        ! Iterate captures on the target square
        DO WHILE (d < 30)
            d = d + 1
            gain(d) = SEE_PIECE_VAL(piece) - gain(d - 1) ! Negamax-style

            ! Find least valuable attacker of 'color' on target square
            CALL find_least_valuable_attacker(occ_piece, occ_color, &
                target_sq_r, target_sq_f, color, attacker_piece, r, f, found)

            IF (.NOT. found) EXIT

            ! Remove this attacker
            occ_piece(r, f) = NO_PIECE
            occ_color(r, f) = NO_COLOR

            piece = attacker_piece
            color = get_opponent_color(color)
        END DO

        ! Minimax the gain array
        d = d - 1
        DO WHILE (d > 1)
            gain(d - 1) = -MAX(-gain(d - 1), gain(d))
            d = d - 1
        END DO

        see_val = gain(1)
    END FUNCTION see_capture

    ! Find the least valuable piece of 'color' attacking (tr, tf)
    SUBROUTINE find_least_valuable_attacker(occ_piece, occ_color, tr, tf, color, &
                                             found_piece, found_r, found_f, found)
        INTEGER, INTENT(IN) :: occ_piece(8, 8), occ_color(8, 8)
        INTEGER, INTENT(IN) :: tr, tf, color
        INTEGER, INTENT(OUT) :: found_piece, found_r, found_f
        LOGICAL, INTENT(OUT) :: found

        INTEGER :: r, f, dr, df, nr, nf, i, pawn_dir

        found = .FALSE.

        ! Check pawns
        IF (color == WHITE) THEN
            pawn_dir = -1
        ELSE
            pawn_dir = 1
        END IF
        r = tr + pawn_dir
        IF (r >= 1 .AND. r <= 8) THEN
            DO df = -1, 1, 2
                f = tf + df
                IF (f >= 1 .AND. f <= 8) THEN
                    IF (occ_piece(r, f) == PAWN .AND. occ_color(r, f) == color) THEN
                        found_piece = PAWN; found_r = r; found_f = f; found = .TRUE.
                        RETURN
                    END IF
                END IF
            END DO
        END IF

        ! Check knights
        DO i = 1, 8
            nr = tr + KNIGHT_DELTAS(i, 1)
            nf = tf + KNIGHT_DELTAS(i, 2)
            IF (nr >= 1 .AND. nr <= 8 .AND. nf >= 1 .AND. nf <= 8) THEN
                IF (occ_piece(nr, nf) == KNIGHT .AND. occ_color(nr, nf) == color) THEN
                    found_piece = KNIGHT; found_r = nr; found_f = nf; found = .TRUE.
                    RETURN
                END IF
            END IF
        END DO

        ! Check bishops and queens (diagonal)
        DO i = 1, 4
            dr = BISHOP_DIRS(i, 1)
            df = BISHOP_DIRS(i, 2)
            nr = tr + dr; nf = tf + df
            DO WHILE (nr >= 1 .AND. nr <= 8 .AND. nf >= 1 .AND. nf <= 8)
                IF (occ_piece(nr, nf) /= NO_PIECE) THEN
                    IF (occ_color(nr, nf) == color) THEN
                        IF (occ_piece(nr, nf) == BISHOP) THEN
                            found_piece = BISHOP; found_r = nr; found_f = nf; found = .TRUE.
                            RETURN
                        ELSE IF (occ_piece(nr, nf) == QUEEN) THEN
                            ! Keep looking for a bishop first (less valuable)
                            IF (.NOT. found) THEN
                                found_piece = QUEEN; found_r = nr; found_f = nf; found = .TRUE.
                            END IF
                        END IF
                    END IF
                    EXIT ! Blocked by a piece
                END IF
                nr = nr + dr; nf = nf + df
            END DO
        END DO
        IF (found .AND. found_piece /= QUEEN) RETURN

        ! Check rooks and queens (straight)
        DO i = 1, 4
            dr = ROOK_DIRS(i, 1)
            df = ROOK_DIRS(i, 2)
            nr = tr + dr; nf = tf + df
            DO WHILE (nr >= 1 .AND. nr <= 8 .AND. nf >= 1 .AND. nf <= 8)
                IF (occ_piece(nr, nf) /= NO_PIECE) THEN
                    IF (occ_color(nr, nf) == color) THEN
                        IF (occ_piece(nr, nf) == ROOK) THEN
                            IF (.NOT. found .OR. found_piece == QUEEN) THEN
                                found_piece = ROOK; found_r = nr; found_f = nf; found = .TRUE.
                            END IF
                            EXIT
                        ELSE IF (occ_piece(nr, nf) == QUEEN .AND. .NOT. found) THEN
                            found_piece = QUEEN; found_r = nr; found_f = nf; found = .TRUE.
                        END IF
                    END IF
                    EXIT
                END IF
                nr = nr + dr; nf = nf + df
            END DO
        END DO
        IF (found) RETURN

        ! Check king
        DO dr = -1, 1
            DO df = -1, 1
                IF (dr == 0 .AND. df == 0) CYCLE
                nr = tr + dr; nf = tf + df
                IF (nr >= 1 .AND. nr <= 8 .AND. nf >= 1 .AND. nf <= 8) THEN
                    IF (occ_piece(nr, nf) == KING .AND. occ_color(nr, nf) == color) THEN
                        found_piece = KING; found_r = nr; found_f = nf; found = .TRUE.
                        RETURN
                    END IF
                END IF
            END DO
        END DO
    END SUBROUTINE find_least_valuable_attacker

    ! --- Update piece lists after a move ---
    ! Maintains piece position lists after a move is applied.
    !
    ! This function updates the white_pieces and black_pieces arrays to reflect
    ! the new positions after a move. It handles:
    ! - Removing captured pieces from opponent's list
    ! - Updating the moving piece's position
    ! - Handling promotions (piece type changes but position stays the same)
    !
    ! Parameters:
    !   board (INOUT): Board state with piece lists to update
    !   from_sq (IN): Square the piece moved from
    !   to_sq (IN): Square the piece moved to
    !   captured_sq (IN): Square where capture occurred (if any)
    !   captured_piece (IN): Type of piece captured (NO_PIECE if none)
    !   captured_color (IN): Color of piece captured

    !
    ! Side effects:
    !   Modifies piece list arrays and counts
    !   Assumes board squares have already been updated
    SUBROUTINE update_piece_lists(board, from_sq, to_sq, captured_sq, captured_piece, captured_color)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq, to_sq
        TYPE(Square_Type), INTENT(IN) :: captured_sq
        INTEGER, INTENT(IN) :: captured_piece, captured_color

        INTEGER :: color_moved
        INTEGER :: piece_moved
        LOGICAL :: dummy
        TYPE(Square_Type) :: rook_from_sq, rook_to_sq

        color_moved = get_opponent_color(board%current_player)
        piece_moved = board%squares_piece(to_sq%rank, to_sq%file)

        ! Remove captured piece from opponent's list if any
        IF (captured_piece /= NO_PIECE) THEN
            dummy = remove_from_piece_list(board, captured_sq, captured_color)
        END IF

        ! Update moving piece's position
        CALL update_piece_position(board, from_sq, to_sq, color_moved)

        IF (piece_moved == KING .AND. ABS(to_sq%file - from_sq%file) == 2) THEN
            IF (to_sq%file == 7) THEN
                rook_from_sq = file_rank_to_sq(8, from_sq%rank)
                rook_to_sq = file_rank_to_sq(6, from_sq%rank)
            ELSE
                rook_from_sq = file_rank_to_sq(1, from_sq%rank)
                rook_to_sq = file_rank_to_sq(4, from_sq%rank)
            END IF
            CALL update_piece_position(board, rook_from_sq, rook_to_sq, color_moved)
        END IF

    END SUBROUTINE update_piece_lists

    ! --- Update piece lists for unmake ---
    ! Restores piece position lists when a move is undone.
    !
    ! This function reverses the changes made by update_piece_lists, restoring
    ! the piece lists to their state before the move was applied. It handles:
    ! - Moving the piece back to its original position
    ! - Restoring captured pieces to their lists
    ! - Handling promotions (piece type changes back)
    !
    ! Parameters:
    !   board (INOUT): Board state with piece lists to restore
    !   from_sq (IN): Square the piece originally moved from
    !   to_sq (IN): Square the piece moved to (now current position)
    !   captured_sq (IN): Square where capture occurred (if any)
    !   captured_piece (IN): Type of piece that was captured
    !   captured_color (IN): Color of piece that was captured
    !
    ! Side effects:
    !   Modifies piece list arrays and counts to pre-move state
    !   Assumes board squares have already been restored
    SUBROUTINE update_piece_lists_unmake(board, from_sq, to_sq, captured_sq, captured_piece, captured_color)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq, to_sq, captured_sq
        INTEGER, INTENT(IN) :: captured_piece, captured_color
        INTEGER :: piece_moved
        TYPE(Square_Type) :: rook_from_sq, rook_to_sq

        ! Move piece back to from_sq
        piece_moved = board%squares_piece(from_sq%rank, from_sq%file)
        CALL update_piece_position(board, to_sq, from_sq, board%current_player)

        IF (piece_moved == KING .AND. ABS(to_sq%file - from_sq%file) == 2) THEN
            IF (to_sq%file == 7) THEN
                rook_from_sq = file_rank_to_sq(6, from_sq%rank)
                rook_to_sq = file_rank_to_sq(8, from_sq%rank)
            ELSE
                rook_from_sq = file_rank_to_sq(4, from_sq%rank)
                rook_to_sq = file_rank_to_sq(1, from_sq%rank)
            END IF
            CALL update_piece_position(board, rook_from_sq, rook_to_sq, board%current_player)
        END IF

        ! Restore captured piece if any
        IF (captured_piece /= NO_PIECE) THEN
            CALL add_to_piece_list(board, captured_sq, captured_color)
        END IF

    END SUBROUTINE update_piece_lists_unmake

END MODULE Board_Utils
