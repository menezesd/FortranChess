MODULE Move_Ordering_Heuristics
    USE Chess_Types, ONLY: get_piece_order, Move_Type, Board_Type, NO_PIECE, MAX_MOVES
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: clear_killers, store_killer, is_killer, moves_equal, order_moves_with_killers

    ! Constants for killer move storage
    INTEGER, PARAMETER :: NUM_KILLERS = 2 ! Number of killer moves per ply
    INTEGER, PARAMETER :: MAX_SEARCH_DEPTH = 64 ! Maximum search depth for killer move array sizing

    ! Killer move storage: stores quiet moves that caused beta cutoffs
    TYPE(Move_Type), DIMENSION(NUM_KILLERS, MAX_SEARCH_DEPTH) :: killer_moves

CONTAINS

    ! --- Returns a "null" move for initialization ---
    PURE FUNCTION null_move() RESULT(m)
        TYPE(Move_Type) :: m
        m%from_sq%rank = 0
        m%from_sq%file = 0
        m%to_sq%rank = 0
        m%to_sq%file = 0
        m%is_castling = .FALSE.
        m%is_en_passant = .FALSE.
        m%promotion_piece = NO_PIECE
        m%captured_piece = NO_PIECE
    END FUNCTION null_move

    ! --- Clear Killer Moves ---
    ! Resets all killer moves before a new search
    SUBROUTINE clear_killers()
        INTEGER :: d
        DO d = 1, MAX_SEARCH_DEPTH
            killer_moves(1, d) = null_move()
            killer_moves(2, d) = null_move()
        END DO
    END SUBROUTINE clear_killers

    ! --- Store Killer Move ---
    ! Stores a quiet move that caused a beta cutoff
    SUBROUTINE store_killer(move, ply)
        TYPE(Move_Type), INTENT(IN) :: move
        INTEGER, INTENT(IN) :: ply

        IF (ply < 1 .OR. ply > MAX_SEARCH_DEPTH) RETURN
        IF (move%captured_piece /= NO_PIECE) RETURN ! Only store quiet moves

        ! Don't store if it's already the first killer
        IF (moves_equal(killer_moves(1, ply), move)) RETURN

        ! Shift killers down and store new one at slot 1
        killer_moves(2, ply) = killer_moves(1, ply)
        killer_moves(1, ply) = move
    END SUBROUTINE store_killer

    ! --- Check if Move is a Killer ---
    LOGICAL FUNCTION is_killer(move, ply)
        TYPE(Move_Type), INTENT(IN) :: move
        INTEGER, INTENT(IN) :: ply
        INTEGER :: i

        is_killer = .FALSE.
        IF (ply < 1 .OR. ply > MAX_SEARCH_DEPTH) RETURN

        DO i = 1, NUM_KILLERS
            IF (moves_equal(killer_moves(i, ply), move)) THEN
                is_killer = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION is_killer

    ! --- Check if Two Moves are Equal ---
    PURE LOGICAL FUNCTION moves_equal(m1, m2)
        TYPE(Move_Type), INTENT(IN) :: m1, m2

        moves_equal = (m1%from_sq%rank == m2%from_sq%rank .AND. &
                       m1%from_sq%file == m2%from_sq%file .AND. &
                       m1%to_sq%rank == m2%to_sq%rank .AND. &
                       m1%to_sq%file == m2%to_sq%file .AND. &
                       m1%promotion_piece == m2%promotion_piece)
    END FUNCTION moves_equal

    ! --- Order Moves with Killer Heuristic ---
    ! Enhances move ordering by prioritizing killer moves after captures
    SUBROUTINE order_moves_with_killers(board, move_list, num_moves, ply)
        TYPE(Board_Type), INTENT(IN) :: board
        TYPE(Move_Type), DIMENSION(:), INTENT(INOUT) :: move_list
        INTEGER, INTENT(IN) :: num_moves, ply
        INTEGER :: i, j
        TYPE(Move_Type) :: temp_move
        INTEGER, DIMENSION(num_moves) :: scores
        INTEGER :: piece_val, captured_val, temp_score

        ! Calculate scores: captures first (MVV-LVA), then killers, then rest
        DO i = 1, num_moves
            scores(i) = 0
            IF (move_list(i)%captured_piece /= NO_PIECE) THEN
                ! Captures get high scores (10000+)
                piece_val = get_piece_order(board%squares_piece( &
                    move_list(i)%from_sq%rank, move_list(i)%from_sq%file))
                captured_val = get_piece_order(move_list(i)%captured_piece) * 10
                scores(i) = 10000 + captured_val - piece_val
            ELSE IF (is_killer(move_list(i), ply)) THEN
                ! Killer moves get medium scores (5000-5001)
                DO j = 1, NUM_KILLERS
                    IF (moves_equal(killer_moves(j, ply), move_list(i))) THEN
                        scores(i) = 5000 + (NUM_KILLERS - j)
                        EXIT
                    END IF
                END DO
            END IF
        END DO

        ! Sort moves by score (descending)
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
    END SUBROUTINE order_moves_with_killers

END MODULE Move_Ordering_Heuristics
