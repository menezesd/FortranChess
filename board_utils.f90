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
              update_piece_position, add_to_piece_list

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

        ! Compute initial Zobrist key
        board%zobrist_key = compute_zobrist_hash(board)

    END SUBROUTINE init_board

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
        LOGICAL :: dummy

        color_moved = get_opponent_color(board%current_player)

        ! Remove captured piece from opponent's list if any
        IF (captured_piece /= NO_PIECE) THEN
            dummy = remove_from_piece_list(board, captured_sq, captured_color)
        END IF

        ! Update moving piece's position
        CALL update_piece_position(board, from_sq, to_sq, color_moved)

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

        ! Move piece back to from_sq
        CALL update_piece_position(board, to_sq, from_sq, board%current_player)

        ! Restore captured piece if any
        IF (captured_piece /= NO_PIECE) THEN
            CALL add_to_piece_list(board, captured_sq, captured_color)
        END IF

    END SUBROUTINE update_piece_lists_unmake

END MODULE Board_Utils
