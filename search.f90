! ============================================
! Module: Search
! Purpose: AI search algorithm implementation
! ============================================
MODULE Search
    USE Chess_Types
    USE Board_Utils
    USE Move_Generation, ONLY: generate_moves, generate_captures, order_moves
    USE Make_Unmake
    USE Evaluation
    USE Transposition_Table, ONLY: probe_tt, store_tt_entry, TT_Entry_Type, &
        HASH_FLAG_EXACT, HASH_FLAG_ALPHA, HASH_FLAG_BETA, new_search_generation, &
        ZOBRIST_BLACK_TO_MOVE, ZOBRIST_EP_FILE
    USE Move_Ordering_Heuristics
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: find_best_move

    ! Constants for search algorithm
    INTEGER, PARAMETER :: MATE_SCORE = 100000 ! Score indicating checkmate
    INTEGER, PARAMETER :: INF = MATE_SCORE + 1000 ! Represents infinity for alpha-beta bounds
    INTEGER, PARAMETER :: MAX_DEPTH = 64 ! Maximum search depth
    ! Removed NUM_KILLERS and killer_moves array as they are now in Move_Ordering_Heuristics

CONTAINS

    ! --- Quiescence Search ---
    RECURSIVE INTEGER FUNCTION quiescence(board, alpha, beta) RESULT(score)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: alpha, beta

        INTEGER :: stand_pat, current_alpha, i
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: captures
        INTEGER :: num_captures
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info

        current_alpha = alpha

        stand_pat = evaluate_board(board)

        IF (stand_pat >= beta) THEN
            score = beta
            RETURN
        END IF

        IF (current_alpha < stand_pat) THEN
            current_alpha = stand_pat
        END IF

        CALL generate_captures(board, captures, num_captures)
        CALL order_moves(board, captures, num_captures) ! NEW: Order captures in quiescence search

        DO i = 1, num_captures
            current_move = captures(i)
            CALL make_move(board, current_move, unmake_info)
            score = -quiescence(board, -beta, -current_alpha)
            CALL unmake_move(board, current_move, unmake_info)

            IF (score >= beta) THEN
                score = beta
                RETURN
            END IF

            IF (score > current_alpha) THEN
                current_alpha = score
            END IF
        END DO

        score = current_alpha
    END FUNCTION quiescence

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
    !   ply (IN): Current ply from root (for killer move indexing)
    !   alpha (IN): Alpha bound for pruning
    !   beta (IN): Beta bound for pruning
    !
    ! Returns:
    !   Best score for the current player from this position
    !
    ! Side effects:
    !   Temporarily modifies the board during recursive search (restored via make/unmake)
    RECURSIVE INTEGER FUNCTION negamax(board, depth_param, ply, alpha, beta) RESULT(best_score)
        TYPE(Board_Type), INTENT(INOUT) :: board ! Needs INOUT for make/unmake
        INTEGER, INTENT(IN) :: depth_param ! Original depth (input only)
        INTEGER, INTENT(IN) :: ply
        INTEGER, INTENT(INOUT) :: alpha, beta ! alpha and beta are modified by TT lookups
        INTEGER :: score, current_alpha = -INF, next_alpha, next_beta
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i
        TYPE(Move_Type) :: current_move, best_move_here
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check
        TYPE(TT_Entry_Type) :: tt_entry
        LOGICAL :: tt_hit
        ! NMP constants and state
        INTEGER, PARAMETER :: NMP_R = 3 ! Depth reduction factor
        LOGICAL :: prev_ep_present
        TYPE(Square_Type) :: prev_ep_sq
        INTEGER(KIND=8) :: prev_key
        INTEGER :: current_depth ! Local variable for depth, can be modified

        current_depth = depth_param

        ! --- Transposition Table Lookup ---
        tt_hit = probe_tt(board%zobrist_key, depth_param, alpha, beta, tt_entry)
        IF (tt_hit) THEN
            best_score = tt_entry%score
            RETURN
        END IF

        ! Base case: evaluate position when search depth is reached
        IF (current_depth <= 0) THEN
            best_score = quiescence(board, alpha, beta)
            RETURN
        END IF

        in_check = is_in_check(board, board%current_player)
        IF (in_check) THEN
            current_depth = current_depth + 1
        END IF

        ! --- Null Move Pruning (NMP) ---
        ! If not in check, and we have enough material, and depth is sufficient,
        ! try giving a free move to the opponent. If the score is still high
        ! enough to cause a beta cutoff, we can prune this whole branch.
        IF (.NOT. in_check .AND. current_depth >= NMP_R + 1 .AND. board%num_white_pieces + board%num_black_pieces > 6) THEN
            ! Make a null move
            prev_key = board%zobrist_key
            prev_ep_present = board%ep_target_present
            prev_ep_sq = board%ep_target_sq

            board%current_player = get_opponent_color(board%current_player)
            board%zobrist_key = IEOR(board%zobrist_key, ZOBRIST_BLACK_TO_MOVE)
            IF (board%ep_target_present) THEN
                board%zobrist_key = IEOR(board%zobrist_key, ZOBRIST_EP_FILE(board%ep_target_sq%file))
                board%ep_target_present = .FALSE.
            END IF

            ! Search with reduced depth and a null window
            next_beta = -beta
            next_alpha = -beta + 1
            score = -negamax(board, current_depth - 1 - NMP_R, ply + 1, next_beta, next_alpha)

            ! Unmake the null move
            board%current_player = get_opponent_color(board%current_player)
            board%ep_target_present = prev_ep_present
            board%ep_target_sq = prev_ep_sq
            board%zobrist_key = prev_key

            ! If the null move search causes a beta cutoff, we can prune this node
            IF (score >= beta) THEN
                best_score = beta
                RETURN
            END IF
        END IF

        ! Generate all legal moves for current position
        CALL generate_moves(board, moves, num_moves)

        ! Check for terminal positions (checkmate/stalemate) after move generation
        IF (num_moves == 0) THEN
             IF (in_check) THEN
                 ! Checkmate: current player loses, score decreases with depth
                 ! (prefer faster mates)
                 best_score = -MATE_SCORE + (10 - current_depth)
             ELSE
                 ! Stalemate: draw
                 best_score = 0
             END IF
             RETURN
        END IF

        ! --- TT Best Move Ordering ---
        IF (tt_hit .AND. tt_entry%best_move%from_sq%rank /= 0) THEN ! Check if TT entry has a valid best move
            DO i = 1, num_moves
                IF (moves_equal(moves(i), tt_entry%best_move)) THEN
                    current_move = moves(1)
                    moves(1) = moves(i)
                    moves(i) = current_move
                    EXIT
                END IF
            END DO
        END IF

        ! Order moves with killer heuristic (now using external module)
        CALL order_moves_with_killers(board, moves, num_moves, ply)

        ! Initialize best score to worst possible outcome
        best_score = -INF
        best_move_here%from_sq%rank = 0 ! Null move

        ! Evaluate each possible move
        DO i = 1, num_moves
            current_move = moves(i)

            ! Make the move on the board
            CALL make_move(board, current_move, unmake_info)

            ! Recursively search from opponent's perspective
            ! Negate score because we're switching perspectives
            next_beta = -beta
            next_alpha = -current_alpha
            score = -negamax(board, current_depth - 1, ply + 1, next_beta, next_alpha)

            ! Undo the move to restore board state
            CALL unmake_move(board, current_move, unmake_info)

            ! Update best score found so far
            IF (score > best_score) THEN
                 best_score = score
                 best_move_here = current_move
            END IF

            ! Update alpha (best score current player can guarantee)
            IF (best_score > current_alpha) THEN
                 current_alpha = best_score
            END IF

            ! Alpha-beta pruning: if current best score is better than
            ! what opponent can force, stop searching this branch
            IF (current_alpha >= beta) THEN
                 CALL store_tt_entry(board%zobrist_key, depth_param, beta, HASH_FLAG_BETA, moves(i))
                 ! Store killer move if it's a quiet move (now using external module)
                 CALL store_killer(current_move, ply)
                 EXIT ! Prune remaining moves
            END IF
        END DO

        ! --- Store result in Transposition Table ---
        IF (best_move_here%from_sq%rank /= 0) THEN
            IF (best_score <= alpha) THEN ! Upper bound
                CALL store_tt_entry(board%zobrist_key, depth_param, best_score, HASH_FLAG_ALPHA, best_move_here)
            ELSE ! Exact score
                CALL store_tt_entry(board%zobrist_key, depth_param, best_score, HASH_FLAG_EXACT, best_move_here)
            END IF
        END IF


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
    SUBROUTINE find_best_move(board, max_depth, best_move_found, best_move)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: max_depth
        LOGICAL, INTENT(OUT) :: best_move_found
        TYPE(Move_Type), INTENT(OUT) :: best_move

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i, d
        INTEGER :: score, best_score_so_far, alpha, beta, next_alpha, next_beta
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: aspiration_delta ! NEW
        LOGICAL :: research_needed ! NEW
        INTEGER :: last_iteration_score ! NEW

        best_move_found = .FALSE.
        best_score_so_far = -INF
        last_iteration_score = 0 ! Initialize

        ! Aspiration window delta
        aspiration_delta = 50 ! Centipawns

        ! Clear killer moves and increment TT generation before new search (now using external module)
        CALL clear_killers()
        CALL new_search_generation()

        ! Generate legal moves at the root
        CALL generate_moves(board, moves, num_moves)

        ! No legal moves available
        IF (num_moves == 0) THEN
             RETURN
        END IF

        ! Iterative Deepening Loop
        DO d = 1, max_depth
            research_needed = .TRUE.
            DO WHILE (research_needed)
                research_needed = .FALSE. ! Assume no re-search needed unless bounds fail

                IF (d == 1) THEN ! First iteration, full window
                    alpha = -INF
                    beta = INF
                ELSE ! Subsequent iterations, aspiration window
                    alpha = last_iteration_score - aspiration_delta
                    beta = last_iteration_score + aspiration_delta
                END IF

                ! Ensure alpha and beta are within sensible bounds, especially after aspiration
                IF (alpha < -INF) alpha = -INF
                IF (beta > INF) beta = INF
                
                ! Re-initialize best score for the current search (may be part of a re-search)
                best_score_so_far = -INF
                
                ! Evaluate each possible move at the current depth
                DO i = 1, num_moves
                    current_move = moves(i)

                    ! Make the move
                    CALL make_move(board, current_move, unmake_info)

                    ! Search from opponent's perspective (ply=1 since we're at root+1)
                    next_beta = -beta
                    next_alpha = -alpha
                    score = -negamax(board, d - 1, 1, next_beta, next_alpha)

                    ! Undo the move
                    CALL unmake_move(board, current_move, unmake_info)

                    ! Check if this move is better than previous best for this iteration
                    IF (score > best_score_so_far) THEN
                         best_score_so_far = score
                         best_move = current_move
                         best_move_found = .TRUE.
                    END IF

                    ! Update alpha for root node
                    IF (best_score_so_far > alpha) THEN
                         alpha = best_score_so_far
                    END IF
                END DO

                ! --- Check Aspiration Window Failure ---
                IF (d /= 1) THEN ! Only check for aspiration failures after first iteration
                    IF (best_score_so_far <= last_iteration_score - aspiration_delta) THEN ! Fail low
                        alpha = -INF
                        beta = last_iteration_score + aspiration_delta
                        research_needed = .TRUE.
                        aspiration_delta = aspiration_delta + 50 ! Widen window for next try
                    ELSE IF (best_score_so_far >= last_iteration_score + aspiration_delta) THEN ! Fail high
                        alpha = last_iteration_score - aspiration_delta
                        beta = INF
                        research_needed = .TRUE.
                        aspiration_delta = aspiration_delta + 50 ! Widen window for next try
                    END IF
                END IF

            END DO ! End DO WHILE (research_needed)

            ! Store score for next iteration's aspiration window
            last_iteration_score = best_score_so_far

            ! Move ordering for the next iteration: move the best move from this iteration to the front
            DO i = 1, num_moves
                IF (moves_equal(moves(i), best_move)) THEN ! Now using moves_equal from external module
                    current_move = moves(1)
                    moves(1) = moves(i)
                    moves(i) = current_move
                    EXIT
                END IF
            END DO

        END DO

    END SUBROUTINE find_best_move

END MODULE Search
