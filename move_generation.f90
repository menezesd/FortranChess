! ============================================
! Module: Move_Generation
! Purpose: Generate pseudo-legal and legal moves
! ============================================
MODULE Move_Generation
    USE Chess_Types
    USE Board_Utils
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: generate_moves, generate_pseudo_moves, generate_captures, order_moves ! Main function exposed

CONTAINS

    ! --- Helper to add a move to the list if array not full ---
    ! Adds a move to the move list array, checking for overflow.
    !
    ! Parameters:
    !   move_list (INOUT): Array to store moves
    !   num_moves (INOUT): Current count of moves (incremented if added)
    !   new_move (IN): The move to add
    !
    ! Side effects:
    !   Prints warning if move list is full (should not happen in normal play)
    SUBROUTINE add_move(move_list, num_moves, new_move)
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves
        TYPE(Move_Type), INTENT(IN) :: new_move
        IF (num_moves < MAX_MOVES) THEN
            num_moves = num_moves + 1
            move_list(num_moves) = new_move
        ELSE
            PRINT *, "Warning: Move list full!"
            ! Handle error - maybe stop program or ignore move
        END IF
    END SUBROUTINE add_move

    ! --- Helper to initialize a move ---
    ! Creates a properly initialized Move_Type structure.
    !
    ! Parameters:
    !   new_move (OUT): The move structure to initialize
    !   from_sq (IN): Starting square
    !   to_sq (IN): Ending square
    !   promotion (IN): Promotion piece type (NO_PIECE if none)
    !   captured (IN): Captured piece type (NO_PIECE if none)
    !   castling (IN): True if this is a castling move
    !   en_passant (IN): True if this is an en passant capture
    SUBROUTINE init_move(new_move, from_sq, to_sq, promotion, captured, castling, en_passant)
        TYPE(Move_Type), INTENT(OUT) :: new_move
        TYPE(Square_Type), INTENT(IN) :: from_sq, to_sq
        INTEGER, INTENT(IN) :: promotion, captured
        LOGICAL, INTENT(IN) :: castling, en_passant

        new_move%from_sq = from_sq
        new_move%to_sq = to_sq
        new_move%promotion_piece = promotion
        new_move%captured_piece = captured
        new_move%is_castling = castling
        new_move%is_en_passant = en_passant
    END SUBROUTINE init_move

    SUBROUTINE generate_captures(board, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(OUT) :: num_moves
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: pseudo_moves
        INTEGER :: num_pseudo_moves, i

        num_moves = 0
        CALL generate_pseudo_moves(board, pseudo_moves, num_pseudo_moves)

        DO i = 1, num_pseudo_moves
            ! Include actual captures AND promotions (which are tactical)
            IF (pseudo_moves(i)%captured_piece /= NO_PIECE .OR. &
                pseudo_moves(i)%promotion_piece /= NO_PIECE) THEN
                CALL add_move(move_list, num_moves, pseudo_moves(i))
            END IF
        END DO
    END SUBROUTINE generate_captures

    ! --- Generate Pawn Moves ---
    SUBROUTINE generate_pawn_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list ! Array to store moves
        INTEGER, INTENT(INOUT) :: num_moves           ! Current count of moves in list

        INTEGER :: r, f, dir, start_rank, promotion_rank, next_r, dbl_r, target_f
        INTEGER :: player_color, opponent_color, target_color, target_piece
        TYPE(Square_Type) :: to_sq
        TYPE(Move_Type) :: new_move
        LOGICAL :: can_promote
        INTEGER, DIMENSION(4) :: promotion_options = (/ QUEEN, ROOK, BISHOP, KNIGHT /)
        INTEGER :: i

        r = from_sq%rank
        f = from_sq%file
        player_color = board%current_player
        opponent_color = get_opponent_color(player_color)

        IF (player_color == WHITE) THEN
            dir = 1
            start_rank = 2
            promotion_rank = 8
        ELSE
            dir = -1
            start_rank = 7
            promotion_rank = 1
        END IF

        ! 1. Single Push
        next_r = r + dir
        IF (sq_is_valid(next_r, f)) THEN
            IF (board%squares_piece(next_r, f) == NO_PIECE) THEN
                can_promote = (next_r == promotion_rank)
                to_sq = file_rank_to_sq(f, next_r)
                IF (can_promote) THEN
                    DO i = 1, 4
                         CALL init_move(new_move, from_sq, to_sq, promotion_options(i), NO_PIECE, .FALSE., .FALSE.)
                         CALL add_move(move_list, num_moves, new_move)
                    END DO
                ELSE
                     CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .FALSE., .FALSE.)
                     CALL add_move(move_list, num_moves, new_move)

                     ! 2. Double Push (only if single push was possible)
                     IF (r == start_rank) THEN
                          dbl_r = r + 2*dir
                          IF (sq_is_valid(dbl_r, f)) THEN
                               IF (board%squares_piece(dbl_r, f) == NO_PIECE) THEN
                                    to_sq = file_rank_to_sq(f, dbl_r)
                                    CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .FALSE., .FALSE.)
                                    CALL add_move(move_list, num_moves, new_move)
                               END IF
                          END IF
                     END IF
                END IF
            END IF
        END IF

        ! 3. Captures (Diagonal)
        IF (sq_is_valid(next_r, 1)) THEN ! Only need to check rank validity once
             DO target_f = f-1, f+1, 2 ! Check f-1 and f+1
                  IF (sq_is_valid(next_r, target_f)) THEN
                       target_piece = board%squares_piece(next_r, target_f)
                       target_color = board%squares_color(next_r, target_f)
                       to_sq = file_rank_to_sq(target_f, next_r)

                       ! Regular Capture
                       IF (target_piece /= NO_PIECE .AND. target_color == opponent_color) THEN
                            can_promote = (next_r == promotion_rank)
                            IF (can_promote) THEN
                                 DO i = 1, 4
                                     CALL init_move(new_move, from_sq, to_sq, promotion_options(i), target_piece, .FALSE., .FALSE.)
                                     CALL add_move(move_list, num_moves, new_move)
                                 END DO
                            ELSE
                                 CALL init_move(new_move, from_sq, to_sq, NO_PIECE, target_piece, .FALSE., .FALSE.)
                                 CALL add_move(move_list, num_moves, new_move)
                            END IF
                       ! En Passant Capture
                       ELSE IF (board%ep_target_present .AND. &
                                next_r == board%ep_target_sq%rank .AND. &
                                target_f == board%ep_target_sq%file) THEN
                            CALL init_move(new_move, from_sq, board%ep_target_sq, NO_PIECE, PAWN, .FALSE., .TRUE.)
                            CALL add_move(move_list, num_moves, new_move)
                       END IF
                  END IF
             END DO
        END IF

    END SUBROUTINE generate_pawn_moves

    ! --- Generate Knight Moves ---
    SUBROUTINE generate_knight_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves

        INTEGER :: r, f, nr, nf, target_piece, target_color
        TYPE(Square_Type) :: to_sq
        TYPE(Move_Type) :: new_move
        INTEGER :: i

        r = from_sq%rank
        f = from_sq%file
        

        DO i = 1, 8
            nr = r + KNIGHT_DELTAS(i, 1)
            nf = f + KNIGHT_DELTAS(i, 2)
            IF (sq_is_valid(nr, nf)) THEN
                target_piece = board%squares_piece(nr, nf)
                target_color = board%squares_color(nr, nf)
                to_sq = file_rank_to_sq(nf, nr)

                IF (target_piece == NO_PIECE) THEN ! Move to empty square
                    CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .FALSE., .FALSE.)
                    CALL add_move(move_list, num_moves, new_move)
                ELSE IF (target_color /= board%current_player) THEN ! Capture
                    CALL init_move(new_move, from_sq, to_sq, NO_PIECE, target_piece, .FALSE., .FALSE.)
                    CALL add_move(move_list, num_moves, new_move)
                END IF
            END IF
        END DO
    END SUBROUTINE generate_knight_moves


    ! --- Generate Sliding Moves (Rook, Bishop, Queen) ---
     SUBROUTINE generate_sliding_moves(board, from_sq, directions, num_dirs, move_list, num_moves)
         TYPE(Board_Type), INTENT(IN) :: board
         TYPE(Square_Type), INTENT(IN) :: from_sq
         INTEGER, DIMENSION(num_dirs, 2), INTENT(IN) :: directions
         INTEGER, INTENT(IN) :: num_dirs
         TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
         INTEGER, INTENT(INOUT) :: num_moves

         INTEGER :: r, f, nr, nf, target_piece, target_color, dr, df
         TYPE(Square_Type) :: to_sq
         TYPE(Move_Type) :: new_move
         INTEGER :: i

         r = from_sq%rank
         f = from_sq%file

         DO i = 1, num_dirs
             dr = directions(i, 1)
             df = directions(i, 2)
             nr = r + dr
             nf = f + df
             DO WHILE (sq_is_valid(nr, nf))
                 target_piece = board%squares_piece(nr, nf)
                 target_color = board%squares_color(nr, nf)
                 to_sq = file_rank_to_sq(nf, nr)

                 IF (target_piece == NO_PIECE) THEN ! Move to empty square
                     CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .FALSE., .FALSE.)
                     CALL add_move(move_list, num_moves, new_move)
                 ELSE ! Hit a piece
                     IF (target_color /= board%current_player) THEN ! Capture
                         CALL init_move(new_move, from_sq, to_sq, NO_PIECE, target_piece, .FALSE., .FALSE.)
                         CALL add_move(move_list, num_moves, new_move)
                     END IF
                     EXIT ! Stop searching this direction (path blocked)
                 END IF

                 nr = nr + dr ! Continue sliding
                 nf = nf + df
             END DO
         END DO
     END SUBROUTINE generate_sliding_moves

     ! --- Generate King Moves (Including Castling placeholders) ---
     SUBROUTINE generate_king_moves(board, from_sq, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Square_Type), INTENT(IN) :: from_sq
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(INOUT) :: num_moves

        INTEGER :: r, f, nr, nf, target_piece, target_color, back_rank
        TYPE(Square_Type) :: to_sq
        TYPE(Move_Type) :: new_move
        INTEGER :: i
        LOGICAL :: can_castle_k, can_castle_q

        r = from_sq%rank
        f = from_sq%file

        ! 1. Normal Moves
        

        DO i = 1, 8
            nr = r + KING_DELTAS(i, 1)
            nf = f + KING_DELTAS(i, 2)
            IF (sq_is_valid(nr, nf)) THEN
                target_piece = board%squares_piece(nr, nf)
                target_color = board%squares_color(nr, nf)
                to_sq = file_rank_to_sq(nf, nr)

                IF (target_piece == NO_PIECE) THEN ! Move to empty square
                    CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .FALSE., .FALSE.)
                    CALL add_move(move_list, num_moves, new_move)
                ELSE IF (target_color /= board%current_player) THEN ! Capture
                    CALL init_move(new_move, from_sq, to_sq, NO_PIECE, target_piece, .FALSE., .FALSE.)
                    CALL add_move(move_list, num_moves, new_move)
                END IF
            END IF
        END DO

        ! 2. Castling (Pseudo-legal check: rights and empty squares)
        !    Legality check (squares not attacked) happens in generate_moves
        IF (board%current_player == WHITE) THEN
             back_rank = 1
             can_castle_k = board%wc_k
             can_castle_q = board%wc_q
        ELSE
             back_rank = 8
             can_castle_k = board%bc_k
             can_castle_q = board%bc_q
        END IF

        IF (r == back_rank .AND. f == 5) THEN ! King on e1/e8
             ! Kingside
             IF (can_castle_k .AND. &
                 board%squares_piece(back_rank, 6) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 7) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 8) == ROOK .AND. & ! Check rook presence
                 board%squares_color(back_rank, 8) == board%current_player) THEN

                 to_sq = file_rank_to_sq(7, back_rank) ! King to g1/g8
                 CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .TRUE., .FALSE.)
                 CALL add_move(move_list, num_moves, new_move)
             END IF
             ! Queenside
             IF (can_castle_q .AND. &
                 board%squares_piece(back_rank, 4) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 3) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 2) == NO_PIECE .AND. &
                 board%squares_piece(back_rank, 1) == ROOK .AND. & ! Check rook presence
                 board%squares_color(back_rank, 1) == board%current_player) THEN

                 to_sq = file_rank_to_sq(3, back_rank) ! King to c1/c8
                 CALL init_move(new_move, from_sq, to_sq, NO_PIECE, NO_PIECE, .TRUE., .FALSE.)
                 CALL add_move(move_list, num_moves, new_move)
             END IF
        END IF

     END SUBROUTINE generate_king_moves


    ! --- Generate All Pseudo-Legal Moves ---
    SUBROUTINE generate_pseudo_moves(board, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list ! Array to store moves
        INTEGER, INTENT(OUT) :: num_moves           ! Count of moves generated

        INTEGER :: i, piece
        TYPE(Square_Type) :: from_sq

        num_moves = 0 ! Reset count

        ! Use piece lists for efficiency
        IF (board%current_player == WHITE) THEN
            DO i = 1, board%num_white_pieces
                from_sq = board%white_pieces(i)
                piece = board%squares_piece(from_sq%rank, from_sq%file)
                SELECT CASE (piece)
                CASE (PAWN)
                    CALL generate_pawn_moves(board, from_sq, move_list, num_moves)
                CASE (KNIGHT)
                    CALL generate_knight_moves(board, from_sq, move_list, num_moves)
                CASE (BISHOP)
                    CALL generate_sliding_moves(board, from_sq, BISHOP_DIRS, 4, move_list, num_moves)
                CASE (ROOK)
                    CALL generate_sliding_moves(board, from_sq, ROOK_DIRS, 4, move_list, num_moves)
                CASE (QUEEN)
                    CALL generate_sliding_moves(board, from_sq, QUEEN_DIRS, 8, move_list, num_moves)
                CASE (KING)
                    CALL generate_king_moves(board, from_sq, move_list, num_moves)
                END SELECT
            END DO
        ELSE
            DO i = 1, board%num_black_pieces
                from_sq = board%black_pieces(i)
                piece = board%squares_piece(from_sq%rank, from_sq%file)
                SELECT CASE (piece)
                CASE (PAWN)
                    CALL generate_pawn_moves(board, from_sq, move_list, num_moves)
                CASE (KNIGHT)
                    CALL generate_knight_moves(board, from_sq, move_list, num_moves)
                CASE (BISHOP)
                    CALL generate_sliding_moves(board, from_sq, BISHOP_DIRS, 4, move_list, num_moves)
                CASE (ROOK)
                    CALL generate_sliding_moves(board, from_sq, ROOK_DIRS, 4, move_list, num_moves)
                CASE (QUEEN)
                    CALL generate_sliding_moves(board, from_sq, QUEEN_DIRS, 8, move_list, num_moves)
                CASE (KING)
                    CALL generate_king_moves(board, from_sq, move_list, num_moves)
                END SELECT
            END DO
        END IF
    END SUBROUTINE generate_pseudo_moves


    ! --- Generate Legal Moves (Filters Pseudo-Legal) ---
    ! Generates all legal moves from the current position.
    !
    ! This function first generates all pseudo-legal moves (moves that appear legal
    ! without considering check), then filters them by making each move and checking
    ! if the king would be in check afterward.
    !
    ! Parameters:
    !   board (INOUT): Current board state (modified during legality checking)
    !   legal_move_list (OUT): Array to store legal moves
    !   num_legal_moves (OUT): Number of legal moves found
    !
    ! Side effects:
    !   Temporarily modifies board state during move validation
    !   Board is restored to original state after checking each move
    !
    ! Algorithm:
    !   1. Generate all pseudo-legal moves
    !   2. For each pseudo-legal move:
    !      a. Make the move on the board
    !      b. Check if current player's king is in check
    !      c. If not in check, add to legal move list
    !      d. Unmake the move to restore board state
    !   3. Special handling for castling (check squares king passes through)
    SUBROUTINE generate_moves(board, legal_move_list, num_legal_moves)
        USE make_unmake
        TYPE(Board_Type), INTENT(INOUT) :: board ! Needs INOUT for make/unmake
        TYPE(Move_Type), DIMENSION(:), INTENT(OUT) :: legal_move_list
        INTEGER, INTENT(OUT) :: num_legal_moves

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: pseudo_moves
        INTEGER :: num_pseudo_moves
        INTEGER :: i
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: is_legal
        INTEGER :: player_color, opponent_color
        TYPE(Square_Type) :: king_sq, mid_sq

        num_legal_moves = 0
        player_color = board%current_player
        opponent_color = get_opponent_color(player_color)

        ! 1. Generate all pseudo-legal moves
        CALL generate_pseudo_moves(board, pseudo_moves, num_pseudo_moves)

        ! 2. Filter for legality
        DO i = 1, num_pseudo_moves
            current_move = pseudo_moves(i)
            is_legal = .TRUE. ! Assume legal initially

            ! Special check for castling through check
            IF (current_move%is_castling) THEN
                 king_sq = current_move%from_sq
                 IF (current_move%to_sq%file == 7) THEN ! Kingside
                     mid_sq = file_rank_to_sq(6, king_sq%rank)
                 ELSE ! Queenside (to_sq%file == 3)
                     mid_sq = file_rank_to_sq(4, king_sq%rank)
                 END IF
                 ! King cannot castle if it passes through or ends on attacked square
                 IF (is_square_attacked(board, king_sq, opponent_color) .OR. &
                     is_square_attacked(board, mid_sq, opponent_color) .OR. &
                     is_square_attacked(board, current_move%to_sq, opponent_color)) THEN
                     is_legal = .FALSE.
                 END IF
            END IF

            IF (is_legal) THEN
                 ! 3. Make the move, check if king is safe, unmake the move
                 CALL make_move(board, current_move, unmake_info)
                 IF (.NOT. is_in_check(board, player_color)) THEN
                     ! Add to legal move list
                     CALL add_move(legal_move_list, num_legal_moves, current_move)
                 END IF
                 CALL unmake_move(board, current_move, unmake_info)
            END IF
        END DO

    END SUBROUTINE generate_moves


    SUBROUTINE order_moves(board, move_list, num_moves)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(IN) :: num_moves
        INTEGER :: i, j
        TYPE(Move_Type) :: temp_move
        INTEGER, DIMENSION(num_moves) :: scores
        INTEGER :: piece_val, captured_val, temp_score

        ! Calculate MVV-LVA scores for captures
        DO i = 1, num_moves
            scores(i) = 0
            IF (move_list(i)%captured_piece /= NO_PIECE) THEN
                piece_val = get_piece_order(board%squares_piece( &
                    move_list(i)%from_sq%rank, move_list(i)%from_sq%file))
                captured_val = get_piece_order(move_list(i)%captured_piece) * 10
                scores(i) = captured_val - piece_val
            END IF
        END DO

        ! Sort moves by score (descending) using selection sort
        DO i = 1, num_moves - 1
            DO j = i + 1, num_moves
                IF (scores(i) < scores(j)) THEN
                    temp_move = move_list(i)
                    move_list(i) = move_list(j)
                    move_list(j) = temp_move
                    temp_score = scores(i)
                    scores(i) = scores(j)
                    scores(j) = temp_score
                END IF
            END DO
        END DO
    END SUBROUTINE order_moves
  END MODULE Move_Generation
