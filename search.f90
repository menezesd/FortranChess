! ============================================
! Module: Search
! Purpose: AI search algorithm implementation
! ============================================
MODULE Search
    USE Chess_Types
    USE Board_Utils
    USE Move_Generation
    USE Make_Unmake
    USE Evaluation
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: find_best_move

    ! Constants for search algorithm
    INTEGER, PARAMETER :: MATE_SCORE = 100000 ! Score indicating checkmate
    INTEGER, PARAMETER :: INF = MATE_SCORE + 1000 ! Represents infinity for alpha-beta bounds

CONTAINS

    ! --- Negamax Search (Recursive Helper) ---
    ! Implements the negamax algorithm with alpha-beta pruning for chess move evaluation.
    !
    ! Negamax is a variant of minimax that exploits the zero-sum property of chess
    ! (one player's gain is the other's loss). It searches the game tree to a given depth,
    ! evaluating positions and returning the best score for the current player.
    !
    ! Alpha-beta pruning optimizes the search by maintaining bounds [alpha, beta]:
    ! - Alpha: best score the maximizing player can guarantee
    ! - Beta: best score the minimizing player can guarantee
    ! When alpha >= beta, the remaining subtree can be pruned.
    !
    ! Parameters:
    !   board (INOUT): Current board state (modified during search but restored)
    !   depth (IN): Remaining search depth (0 = leaf node)
    !   alpha (IN): Alpha bound for pruning
    !   beta (IN): Beta bound for pruning
    !
    ! Returns:
    !   Best score for the current player from this position
    !
    ! Side effects:
    !   Temporarily modifies the board during recursive search (restored via make/unmake)
    RECURSIVE INTEGER FUNCTION negamax(board, depth, alpha, beta) RESULT(best_score)
        TYPE(Board_Type), INTENT(INOUT) :: board ! Needs INOUT for make/unmake
        INTEGER, INTENT(IN) :: depth, alpha, beta
        INTEGER :: score, current_alpha
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check

        current_alpha = alpha ! Local copy to modify

        ! Base case: evaluate position when search depth is reached
        IF (depth <= 0) THEN
            best_score = evaluate_board(board)
            RETURN
        END IF

        ! Generate all legal moves for current position
        CALL generate_moves(board, moves, num_moves)

        ! Check for terminal positions (checkmate/stalemate)
        IF (num_moves == 0) THEN
             in_check = is_in_check(board, board%current_player)
             IF (in_check) THEN
                 ! Checkmate: current player loses, score decreases with depth
                 ! (prefer faster mates)
                 best_score = -MATE_SCORE + (10 - depth)
             ELSE
                 ! Stalemate: draw
                 best_score = 0
             END IF
             RETURN
        END IF

        ! Initialize best score to worst possible outcome
        best_score = -INF

        ! Evaluate each possible move
        DO i = 1, num_moves
            current_move = moves(i)

            ! Make the move on the board
            CALL make_move(board, current_move, unmake_info)

            ! Recursively search from opponent's perspective
            ! Negate score because we're switching perspectives
            score = -negamax(board, depth - 1, -beta, -current_alpha)

            ! Undo the move to restore board state
            CALL unmake_move(board, current_move, unmake_info)

            ! Update best score found so far
            IF (score > best_score) THEN
                 best_score = score
            END IF

            ! Update alpha (best score current player can guarantee)
            IF (best_score > current_alpha) THEN
                 current_alpha = best_score
            END IF

            ! Alpha-beta pruning: if current best score is better than
            ! what opponent can force, stop searching this branch
            IF (current_alpha >= beta) THEN
                 EXIT ! Prune remaining moves
            END IF
        END DO

    END FUNCTION negamax


    ! --- Find Best Move (Top Level Search Call) ---
    ! Searches for the best move from the current position using negamax algorithm.
    !
    ! This is the main entry point for AI move selection. It performs a depth-limited
    ! search and returns the move that leads to the highest evaluated position.
    !
    ! Parameters:
    !   board (INOUT): Current board state
    !   depth (IN): Search depth (higher = stronger but slower)
    !   best_move_found (OUT): True if a legal move was found
    !   best_move (OUT): The best move found by the search
    !
    ! Side effects:
    !   Temporarily modifies the board during search (restored via make/unmake)
    !   May take significant time for deep searches
    SUBROUTINE find_best_move(board, depth, best_move_found, best_move)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: depth
        LOGICAL, INTENT(OUT) :: best_move_found
        TYPE(Move_Type), INTENT(OUT) :: best_move

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i
        INTEGER :: score, best_score_so_far, alpha, beta
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info

        best_move_found = .FALSE.
        best_score_so_far = -INF
        alpha = -INF  ! No lower bound initially
        beta = INF    ! No upper bound initially

        ! Generate all legal moves
        CALL generate_moves(board, moves, num_moves)

        ! No legal moves available
        IF (num_moves == 0) THEN
             RETURN
        END IF

        ! Default to first move if no better move found
        best_move = moves(1)
        best_move_found = .TRUE.

        ! Evaluate each possible move
        DO i = 1, num_moves
            current_move = moves(i)

            ! Make the move
            CALL make_move(board, current_move, unmake_info)

            ! Search from opponent's perspective
            score = -negamax(board, depth - 1, -beta, -alpha)

            ! Undo the move
            CALL unmake_move(board, current_move, unmake_info)

            ! Check if this move is better than previous best
            IF (score > best_score_so_far) THEN
                 best_score_so_far = score
                 best_move = current_move
            END IF

            ! Update alpha for root node (tracks best score found)
            IF (best_score_so_far > alpha) THEN
                 alpha = best_score_so_far
            END IF
            ! Note: No beta cutoff at root level - we want the actual best move
        END DO

    END SUBROUTINE find_best_move

END MODULE Search
