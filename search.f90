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
    USE Search_Control, ONLY: poll_search_stop, search_stop_requested
    USE Transposition_Table, ONLY: probe_tt, store_tt_entry, TT_Entry_Type, &
        HASH_FLAG_EXACT, HASH_FLAG_ALPHA, HASH_FLAG_BETA, new_search_generation, &
        ZOBRIST_BLACK_TO_MOVE, ZOBRIST_EP_FILE
    USE Move_Ordering_Heuristics, ONLY: clear_killers, &
        store_killer, update_history_score, clear_history, moves_equal, order_moves_with_killers
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
    RECURSIVE INTEGER FUNCTION quiescence(board, ply, alpha, beta) RESULT(score)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: ply
        INTEGER, INTENT(IN) :: alpha, beta

        INTEGER :: stand_pat = 0, current_alpha, i, delta
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check
        ! Delta pruning: piece values for estimating max capture gain
        INTEGER, PARAMETER :: DELTA_PIECE_VAL(6) = (/ 100, 320, 330, 500, 900, 20000 /)
        INTEGER, PARAMETER :: DELTA_MARGIN = 200 ! Safety margin for positional factors

        IF (poll_search_stop()) THEN
            score = 0
            RETURN
        END IF

        IF (is_fifty_move_draw(board) .OR. is_threefold_repetition(board)) THEN
            score = 0
            RETURN
        END IF

        current_alpha = alpha
        in_check = is_in_check(board, board%current_player)

        IF (.NOT. in_check) THEN
            stand_pat = evaluate_board(board)

            IF (stand_pat >= beta) THEN
                score = beta
                RETURN
            END IF

            IF (current_alpha < stand_pat) THEN
                current_alpha = stand_pat
            END IF

            CALL generate_captures(board, moves, num_moves)
        ELSE
            CALL generate_moves(board, moves, num_moves)
            IF (num_moves == 0) THEN
                score = -MATE_SCORE + ply
                RETURN
            END IF
        END IF

        CALL order_moves(board, moves, num_moves)

        DO i = 1, num_moves
            current_move = moves(i)

            ! Delta pruning: skip captures that can't possibly raise alpha
            IF (.NOT. in_check .AND. current_move%captured_piece /= NO_PIECE) THEN
                delta = stand_pat + DELTA_PIECE_VAL(current_move%captured_piece) + DELTA_MARGIN
                IF (current_move%promotion_piece /= NO_PIECE) delta = delta + 800
                IF (delta < current_alpha) CYCLE
                ! SEE pruning: skip losing captures
                IF (see_capture(board, current_move) < 0) CYCLE
            END IF

            CALL make_move(board, current_move, unmake_info)
            score = -quiescence(board, ply + 1, -beta, -current_alpha)
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
        INTEGER :: score, current_alpha, next_alpha, next_beta, original_alpha
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i, reduction
        TYPE(Move_Type) :: current_move, best_move_here
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check, gives_check, is_quiet
        TYPE(TT_Entry_Type) :: tt_entry
        LOGICAL :: tt_hit
        ! NMP constants and state
        INTEGER, PARAMETER :: NMP_R = 3 ! Depth reduction factor
        LOGICAL :: prev_ep_present
        TYPE(Square_Type) :: prev_ep_sq
        INTEGER(KIND=8) :: prev_key
        INTEGER :: prev_halfmove_clock, prev_fullmove_number, prev_repetition_count
        INTEGER :: current_depth ! Local variable for depth, can be modified
        INTEGER :: static_eval
        LOGICAL :: can_futility_prune
        ! Pruning margins
        INTEGER, PARAMETER :: FUTILITY_MARGIN_D1 = 200
        INTEGER, PARAMETER :: FUTILITY_MARGIN_D2 = 500
        INTEGER, PARAMETER :: REVERSE_FUTILITY_MARGIN = 200
        INTEGER, PARAMETER :: RAZOR_MARGIN = 400

        current_depth = depth_param

        IF (poll_search_stop()) THEN
            best_score = 0
            RETURN
        END IF

        IF (is_fifty_move_draw(board) .OR. is_threefold_repetition(board)) THEN
            best_score = 0
            RETURN
        END IF

        original_alpha = alpha
        current_alpha = alpha

        ! --- Transposition Table Lookup ---
        tt_hit = probe_tt(board%zobrist_key, depth_param, ply, alpha, beta, tt_entry)
        current_alpha = alpha
        IF (tt_hit) THEN
            best_score = tt_entry%score
            RETURN
        END IF

        in_check = is_in_check(board, board%current_player)
        IF (in_check) THEN
            current_depth = current_depth + 1
        END IF

        ! Base case: evaluate position when search depth is reached
        IF (current_depth <= 0) THEN
            best_score = quiescence(board, ply, alpha, beta)
            RETURN
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
            prev_halfmove_clock = board%halfmove_clock
            prev_fullmove_number = board%fullmove_number
            prev_repetition_count = board%repetition_count

            board%halfmove_clock = board%halfmove_clock + 1
            IF (board%current_player == BLACK) THEN
                board%fullmove_number = board%fullmove_number + 1
            END IF
            board%current_player = get_opponent_color(board%current_player)
            board%zobrist_key = IEOR(board%zobrist_key, ZOBRIST_BLACK_TO_MOVE)
            IF (board%ep_target_present) THEN
                board%zobrist_key = IEOR(board%zobrist_key, ZOBRIST_EP_FILE(board%ep_target_sq%file))
                board%ep_target_present = .FALSE.
            END IF
            IF (board%repetition_count < MAX_GAME_HISTORY) THEN
                board%repetition_count = board%repetition_count + 1
            END IF
            board%repetition_history(board%repetition_count) = board%zobrist_key

            ! Search with reduced depth and a null window
            next_beta = -beta
            next_alpha = -beta + 1
            score = -negamax(board, current_depth - 1 - NMP_R, ply + 1, next_beta, next_alpha)

            ! Unmake the null move
            board%current_player = get_opponent_color(board%current_player)
            board%ep_target_present = prev_ep_present
            board%ep_target_sq = prev_ep_sq
            board%zobrist_key = prev_key
            board%halfmove_clock = prev_halfmove_clock
            board%fullmove_number = prev_fullmove_number
            board%repetition_count = prev_repetition_count

            ! If the null move search causes a beta cutoff, we can prune this node
            IF (score >= beta) THEN
                best_score = beta
                RETURN
            END IF
        END IF

        ! --- Static eval for pruning decisions ---
        static_eval = evaluate_board(board)

        ! --- Reverse Futility Pruning (Static Null Move Pruning) ---
        ! If static eval is well above beta at shallow depth, the position is so good
        ! that a full search is unlikely to change the result.
        IF (.NOT. in_check .AND. current_depth <= 3 .AND. &
            static_eval - REVERSE_FUTILITY_MARGIN * current_depth >= beta) THEN
            best_score = static_eval - REVERSE_FUTILITY_MARGIN * current_depth
            RETURN
        END IF

        ! --- Razoring ---
        ! If static eval is far below alpha at low depth, drop into qsearch
        IF (.NOT. in_check .AND. current_depth <= 2 .AND. &
            static_eval + RAZOR_MARGIN * current_depth <= alpha) THEN
            score = quiescence(board, ply, alpha, beta)
            IF (score <= alpha) THEN
                best_score = score
                RETURN
            END IF
        END IF

        ! Generate all legal moves for current position
        CALL generate_moves(board, moves, num_moves)

        ! Check for terminal positions (checkmate/stalemate) after move generation
        IF (num_moves == 0) THEN
             IF (in_check) THEN
                 ! Checkmate: current player loses; use ply so mate distance is stable.
                 best_score = -MATE_SCORE + ply
             ELSE
                 ! Stalemate: draw
                 best_score = 0
             END IF
             RETURN
        END IF

        ! --- Internal Iterative Deepening (IID) ---
        ! When no TT move is available at sufficient depth, do a shallow search
        ! to find a good move for ordering.
        IF (tt_entry%best_move%from_sq%rank == 0 .AND. current_depth >= 4 .AND. .NOT. in_check) THEN
            next_alpha = -beta
            next_beta = -alpha
            score = -negamax(board, current_depth - 2, ply, next_alpha, next_beta)
            ! Re-probe TT after IID search to get the best move it found
            tt_hit = probe_tt(board%zobrist_key, 0, ply, alpha, beta, tt_entry)
        END IF

        ! --- Futility pruning flag ---
        can_futility_prune = .FALSE.
        IF (.NOT. in_check .AND. current_depth <= 2) THEN
            IF (current_depth == 1 .AND. static_eval + FUTILITY_MARGIN_D1 <= alpha) THEN
                can_futility_prune = .TRUE.
            ELSE IF (current_depth == 2 .AND. static_eval + FUTILITY_MARGIN_D2 <= alpha) THEN
                can_futility_prune = .TRUE.
            END IF
        END IF

        ! Order moves with killers and history heuristic
        CALL order_moves_with_killers(board, moves, num_moves, ply)

        ! --- TT Best Move Ordering (after sort so it stays at position 1) ---
        IF (tt_entry%best_move%from_sq%rank /= 0) THEN
            DO i = 1, num_moves
                IF (moves_equal(moves(i), tt_entry%best_move)) THEN
                    current_move = moves(1)
                    moves(1) = moves(i)
                    moves(i) = current_move
                    EXIT
                END IF
            END DO
        END IF

        ! Initialize best score to worst possible outcome
        best_score = -INF
        best_move_here%from_sq%rank = 0 ! Null move

        ! Evaluate each possible move
        DO i = 1, num_moves
            IF (poll_search_stop()) EXIT
            current_move = moves(i)
            is_quiet = (current_move%captured_piece == NO_PIECE .AND. &
                        current_move%promotion_piece == NO_PIECE)

            ! Make the move on the board
            CALL make_move(board, current_move, unmake_info)
            gives_check = is_in_check(board, board%current_player)

            ! --- Futility pruning: skip quiet moves that can't raise alpha ---
            ! Must be after make_move so we can check if the move gives check
            IF (can_futility_prune .AND. is_quiet .AND. i > 1 .AND. &
                .NOT. current_move%is_castling .AND. .NOT. gives_check) THEN
                CALL unmake_move(board, current_move, unmake_info)
                CYCLE
            END IF

            ! --- Late Move Reductions (LMR) ---
            ! Reduce depth for late quiet moves that don't give check
            reduction = 0
            IF (i > 3 .AND. current_depth >= 3 .AND. is_quiet .AND. &
                .NOT. in_check .AND. .NOT. gives_check) THEN
                IF (i > 6) THEN
                    reduction = 2
                ELSE
                    reduction = 1
                END IF
            END IF

            ! --- Principal Variation Search (PVS) ---
            IF (i == 1) THEN
                ! First move: search with full window
                next_beta = -beta
                next_alpha = -current_alpha
                score = -negamax(board, current_depth - 1, ply + 1, next_beta, next_alpha)
            ELSE
                ! Later moves: search with null window (scout)
                next_beta = -current_alpha
                next_alpha = -current_alpha - 1
                score = -negamax(board, current_depth - 1 - reduction, ply + 1, next_beta, next_alpha)
                ! Re-search with full window if it beats alpha
                IF (score > current_alpha .AND. score < beta) THEN
                    next_beta = -beta
                    next_alpha = -current_alpha
                    score = -negamax(board, current_depth - 1, ply + 1, next_beta, next_alpha)
                END IF
            END IF

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
                 CALL store_tt_entry(board%zobrist_key, depth_param, beta, HASH_FLAG_ALPHA, moves(i), ply)
                 ! Store killer move if it's a quiet move (now using external module)
                 CALL store_killer(current_move, ply)
                 IF (is_quiet .AND. .NOT. current_move%is_castling) THEN
                     CALL update_history_score(board, current_move, current_depth)
                 END IF
                 EXIT ! Prune remaining moves
            END IF
        END DO

        IF (search_stop_requested()) THEN
            RETURN
        END IF

        ! --- Store result in Transposition Table ---
        IF (best_move_here%from_sq%rank /= 0) THEN
            IF (best_score <= original_alpha) THEN ! Upper bound
                CALL store_tt_entry(board%zobrist_key, depth_param, best_score, HASH_FLAG_BETA, best_move_here, ply)
            ELSE ! Exact score
                CALL store_tt_entry(board%zobrist_key, depth_param, best_score, HASH_FLAG_EXACT, best_move_here, ply)
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
    SUBROUTINE find_best_move(board, max_depth, best_move_found, best_move, best_score_out, completed_depth_out, root_moves_in, root_num_moves_in)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: max_depth
        LOGICAL, INTENT(OUT) :: best_move_found
        TYPE(Move_Type), INTENT(OUT) :: best_move
        INTEGER, INTENT(OUT), OPTIONAL :: best_score_out
        INTEGER, INTENT(OUT), OPTIONAL :: completed_depth_out
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN), OPTIONAL :: root_moves_in
        INTEGER, INTENT(IN), OPTIONAL :: root_num_moves_in

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i, d, target_depth
        INTEGER :: score, best_score_so_far, alpha, beta, next_alpha, next_beta
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: aspiration_delta
        LOGICAL :: research_needed
        INTEGER :: last_iteration_score
        INTEGER :: start_count, count_rate
        INTEGER :: completed_depth
        TYPE(Move_Type) :: iteration_best_move
        LOGICAL :: iteration_found, iteration_completed

        best_move_found = .FALSE.
        best_score_so_far = -INF
        last_iteration_score = 0 ! Initialize
        completed_depth = 0
        CALL SYSTEM_CLOCK(start_count, count_rate)

        ! Aspiration window delta
        aspiration_delta = 50 ! Centipawns

        ! Clear killer moves and increment TT generation before new search (now using external module)
        CALL clear_killers()
        CALL clear_history()
        CALL new_search_generation()

        IF (is_fifty_move_draw(board) .OR. is_threefold_repetition(board)) THEN
            best_move_found = .FALSE.
            IF (PRESENT(best_score_out)) best_score_out = 0
            IF (PRESENT(completed_depth_out)) completed_depth_out = 0
            RETURN
        END IF

        ! Generate legal moves at the root, or use caller-provided root restriction.
        IF (PRESENT(root_moves_in) .AND. PRESENT(root_num_moves_in)) THEN
            num_moves = root_num_moves_in
            IF (num_moves > 0) moves(1:num_moves) = root_moves_in(1:num_moves)
        ELSE
            CALL generate_moves(board, moves, num_moves)
        END IF

        ! No legal moves available
        IF (num_moves == 0) THEN
             IF (PRESENT(completed_depth_out)) completed_depth_out = 0
             RETURN
        END IF

        best_move = moves(1)
        best_move_found = .TRUE.

        IF (max_depth > 0) THEN
            target_depth = max_depth
        ELSE
            target_depth = MAX_DEPTH
        END IF

        ! Iterative Deepening Loop
        d = 1
        DO WHILE (d <= target_depth)
            research_needed = .TRUE.
            iteration_completed = .FALSE.
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
                iteration_found = .FALSE.
                
                ! Evaluate each possible move at the current depth
                DO i = 1, num_moves
                    IF (poll_search_stop()) EXIT
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
                         iteration_best_move = current_move
                         iteration_found = .TRUE.
                    END IF

                    ! Update alpha for root node
                    IF (best_score_so_far > alpha) THEN
                         alpha = best_score_so_far
                    END IF
                END DO

                IF (search_stop_requested()) EXIT

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

                IF (.NOT. research_needed) THEN
                    iteration_completed = .TRUE.
                END IF

            END DO ! End DO WHILE (research_needed)

            IF (search_stop_requested()) EXIT

            IF (iteration_completed .AND. iteration_found) THEN
                best_move = iteration_best_move
                best_move_found = .TRUE.
                completed_depth = d
                last_iteration_score = best_score_so_far
            END IF

            ! (Info printing handled by caller; stats available via optional outputs)

            ! Move ordering for the next iteration: move the best move from this iteration to the front
            DO i = 1, num_moves
                IF (moves_equal(moves(i), best_move)) THEN ! Now using moves_equal from external module
                    current_move = moves(1)
                    moves(1) = moves(i)
                    moves(i) = current_move
                    EXIT
                END IF
            END DO

            IF (max_depth <= 0 .AND. d == target_depth) THEN
                target_depth = target_depth + MAX_DEPTH
            END IF
            d = d + 1
        END DO

        IF (PRESENT(best_score_out)) best_score_out = best_score_so_far
        IF (PRESENT(completed_depth_out)) completed_depth_out = completed_depth

    END SUBROUTINE find_best_move

END MODULE Search
