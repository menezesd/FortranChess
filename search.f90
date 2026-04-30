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
    PUBLIC :: find_best_move, search_node_count

    ! Constants for search algorithm
    INTEGER, PARAMETER :: MATE_SCORE = 100000 ! Score indicating checkmate
    INTEGER, PARAMETER :: INF = MATE_SCORE + 1000 ! Represents infinity for alpha-beta bounds
    INTEGER, PARAMETER :: SEARCH_DEPTH_LIMIT = 64 ! Maximum search depth
    INTEGER, PARAMETER :: MAX_QSEARCH_DEPTH = 12 ! Prevent quiescence explosion
    INTEGER, PARAMETER :: LMP_BASE = 8 ! Late move pruning base move count

    ! Pruning margins
    INTEGER, PARAMETER :: RFP_MARGIN = 80 ! Reverse futility pruning margin per depth
    INTEGER, PARAMETER :: FUTILITY_MARGIN = 120 ! Futility pruning margin per depth

    ! Precomputed LMR table: reduction = floor(0.77 + ln(depth) * ln(move_idx) / 2.36)
    INTEGER, PARAMETER :: LMR_MAX_DEPTH = 32
    INTEGER, PARAMETER :: LMR_MAX_MOVES = 64
    INTEGER :: lmr_table(LMR_MAX_DEPTH, LMR_MAX_MOVES)
    LOGICAL :: lmr_initialized = .FALSE.

    ! Static eval at each ply for improving detection
    INTEGER :: ply_static_eval(SEARCH_DEPTH_LIMIT)

    ! Node counter for benchmarking
    INTEGER(KIND=8) :: search_node_count = 0

CONTAINS

    ! --- Initialize LMR Table ---
    SUBROUTINE init_lmr_table()
        INTEGER :: d, m
        REAL(KIND=8) :: val
        IF (lmr_initialized) RETURN
        lmr_table = 0
        DO d = 2, LMR_MAX_DEPTH
            DO m = 2, LMR_MAX_MOVES
                val = 0.77D0 + LOG(REAL(d, KIND=8)) * LOG(REAL(m, KIND=8)) / 2.36D0
                lmr_table(d, m) = MAX(0, INT(val))
            END DO
        END DO
        lmr_initialized = .TRUE.
    END SUBROUTINE init_lmr_table

    ! --- Quiescence Search ---
    RECURSIVE INTEGER FUNCTION quiescence(board, ply, qdepth, alpha, beta) RESULT(score)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: ply
        INTEGER, INTENT(IN) :: qdepth
        INTEGER, INTENT(IN) :: alpha, beta

        INTEGER :: stand_pat, current_alpha, i, delta
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves
        TYPE(Move_Type) :: current_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check
        ! Delta pruning: piece values for estimating max capture gain
        INTEGER, PARAMETER :: DELTA_PIECE_VAL(6) = (/ 100, 320, 330, 500, 900, 20000 /)
        INTEGER, PARAMETER :: DELTA_MARGIN = 200 ! Safety margin for positional factors

        search_node_count = search_node_count + 1

        IF (poll_search_stop()) THEN
            score = 0
            RETURN
        END IF

        IF (is_fifty_move_draw(board) .OR. is_threefold_repetition(board)) THEN
            score = 0
            RETURN
        END IF

        stand_pat = 0
        current_alpha = alpha
        in_check = is_in_check(board, board%current_player)

        ! Qsearch depth limit: prevent explosion in positions with many checks
        IF (qdepth >= MAX_QSEARCH_DEPTH) THEN
            IF (in_check) THEN
                score = 0 ! Can't evaluate reliably when in check at depth limit
            ELSE
                score = evaluate_board(board)
            END IF
            RETURN
        END IF

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
            score = -quiescence(board, ply + 1, qdepth + 1, -beta, -current_alpha)
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

    ! --- Negamax Search ---
    RECURSIVE INTEGER FUNCTION negamax(board, depth_param, ply, alpha, beta) RESULT(best_score)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: depth_param
        INTEGER, INTENT(IN) :: ply
        INTEGER, INTENT(INOUT) :: alpha, beta
        INTEGER :: score, current_alpha, next_alpha, next_beta, original_alpha
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves, i, reduction, extension, new_depth
        TYPE(Move_Type) :: current_move, best_move_here
        TYPE(UnmakeInfo_Type) :: unmake_info
        LOGICAL :: in_check, gives_check, is_quiet
        TYPE(TT_Entry_Type) :: tt_entry
        LOGICAL :: tt_hit
        ! NMP state
        LOGICAL :: prev_ep_present
        TYPE(Square_Type) :: prev_ep_sq
        INTEGER(KIND=8) :: prev_key
        INTEGER :: prev_halfmove_clock, prev_fullmove_number, prev_repetition_count
        INTEGER :: current_depth
        INTEGER :: nmp_reduction
        INTEGER :: moved_piece_type
        INTEGER :: static_eval
        LOGICAL :: improving, can_futility_prune

        current_depth = depth_param
        search_node_count = search_node_count + 1

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

        ! --- Mate Distance Pruning ---
        ! If we already found a mate in N, prune branches that can't do better.
        IF (ply > 0) THEN
            IF (alpha < -MATE_SCORE + ply) alpha = -MATE_SCORE + ply
            IF (beta > MATE_SCORE - ply + 1) beta = MATE_SCORE - ply + 1
            IF (alpha >= beta) THEN
                best_score = alpha
                RETURN
            END IF
            current_alpha = alpha
        END IF

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
            best_score = quiescence(board, ply, 0, alpha, beta)
            RETURN
        END IF

        ! --- Static evaluation for pruning decisions ---
        IF (.NOT. in_check) THEN
            static_eval = evaluate_board(board)
        ELSE
            static_eval = -INF
        END IF

        ! Store for improving detection
        IF (ply >= 1 .AND. ply <= SEARCH_DEPTH_LIMIT) THEN
            ply_static_eval(ply) = static_eval
        END IF

        ! Improving: is our eval better than 2 plies ago?
        improving = .TRUE.
        IF (.NOT. in_check .AND. ply >= 3 .AND. ply <= SEARCH_DEPTH_LIMIT) THEN
            improving = (static_eval > ply_static_eval(ply - 2))
        END IF

        ! --- Reverse Futility Pruning (RFP) ---
        ! If static eval is way above beta at shallow depth, prune the node.
        IF (.NOT. in_check .AND. current_depth <= 7 .AND. &
            static_eval - RFP_MARGIN * current_depth >= beta) THEN
            best_score = static_eval - RFP_MARGIN * current_depth
            RETURN
        END IF

        ! --- Null Move Pruning (NMP) ---
        ! Depth-adaptive reduction: at least R=3, more at high depth
        IF (.NOT. in_check .AND. current_depth >= 4 .AND. &
            board%num_white_pieces + board%num_black_pieces > 6) THEN
            nmp_reduction = MAX(3, 1 + (current_depth + 1) / 3)
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
            score = -negamax(board, current_depth - 1 - nmp_reduction, ply + 1, next_beta, next_alpha)

            ! Unmake the null move
            board%current_player = get_opponent_color(board%current_player)
            board%ep_target_present = prev_ep_present
            board%ep_target_sq = prev_ep_sq
            board%zobrist_key = prev_key
            board%halfmove_clock = prev_halfmove_clock
            board%fullmove_number = prev_fullmove_number
            board%repetition_count = prev_repetition_count

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
                 best_score = -MATE_SCORE + ply
             ELSE
                 best_score = 0
             END IF
             RETURN
        END IF

        ! --- Internal Iterative Deepening (IID) ---
        ! When no TT move is available at sufficient depth, do a shallow search
        ! to populate the TT with a best move for ordering.
        IF (tt_entry%best_move%from_sq%rank == 0 .AND. current_depth >= 4 .AND. .NOT. in_check) THEN
            next_alpha = alpha
            next_beta = beta
            score = negamax(board, current_depth - 2, ply, next_alpha, next_beta)
            tt_hit = probe_tt(board%zobrist_key, 0, ply, alpha, beta, tt_entry)
            current_alpha = alpha
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

            ! --- Per-move extensions ---
            extension = 0

            ! Passed pawn extension: pawn reaching 7th/2nd rank (one step from promotion)
            IF (current_move%promotion_piece == NO_PIECE) THEN
                moved_piece_type = board%squares_piece(current_move%to_sq%rank, current_move%to_sq%file)
                IF (moved_piece_type == PAWN) THEN
                    IF (current_move%to_sq%rank == 7 .OR. current_move%to_sq%rank == 2) THEN
                        extension = 1
                    END IF
                END IF
            END IF

            new_depth = current_depth - 1 + extension

            ! --- Futility Pruning ---
            ! If static eval + margin can't reach alpha, skip quiet moves.
            can_futility_prune = (.NOT. in_check .AND. current_depth <= 6 .AND. &
                static_eval + FUTILITY_MARGIN * current_depth <= current_alpha)

            IF (can_futility_prune .AND. is_quiet .AND. i > 1 .AND. &
                .NOT. gives_check .AND. .NOT. current_move%is_castling .AND. &
                extension == 0) THEN
                CALL unmake_move(board, current_move, unmake_info)
                CYCLE
            END IF

            ! --- Late Move Pruning (LMP) ---
            ! Skip late quiet moves at depth 1 — they almost never improve alpha.
            IF (current_depth == 1 .AND. i > LMP_BASE .AND. &
                is_quiet .AND. .NOT. in_check .AND. .NOT. gives_check .AND. &
                .NOT. current_move%is_castling .AND. extension == 0) THEN
                CALL unmake_move(board, current_move, unmake_info)
                CYCLE
            END IF

            ! --- Late Move Reductions (LMR) ---
            ! Uses precomputed logarithmic table for smooth reductions
            reduction = 0
            IF (current_depth >= 3 .AND. i > 3 .AND. is_quiet .AND. &
                .NOT. gives_check .AND. .NOT. in_check .AND. extension == 0) THEN
                reduction = lmr_table(MIN(current_depth, LMR_MAX_DEPTH), MIN(i, LMR_MAX_MOVES))
                ! Reduce more when position is NOT improving
                IF (.NOT. improving .AND. reduction > 0) reduction = reduction + 1
                ! Clamp reduction so we don't reduce below depth 1
                IF (reduction > 0) reduction = MIN(reduction, current_depth - 2)
            END IF

            ! --- Principal Variation Search (PVS) ---
            IF (i == 1) THEN
                ! First move: search with full window
                next_beta = -beta
                next_alpha = -current_alpha
                score = -negamax(board, new_depth, ply + 1, next_beta, next_alpha)
            ELSE
                ! Later moves: search with null window (scout)
                next_beta = -current_alpha - 1
                next_alpha = -current_alpha
                score = -negamax(board, new_depth - reduction, ply + 1, next_beta, next_alpha)
                ! LMR verification: if reduced search beat alpha, verify at full depth
                IF (score > current_alpha .AND. reduction > 0) THEN
                    next_beta = -current_alpha - 1
                    next_alpha = -current_alpha
                    score = -negamax(board, new_depth, ply + 1, next_beta, next_alpha)
                END IF
                ! PVS re-search: if null-window search beat alpha, full-window re-search
                IF (score > current_alpha .AND. score < beta) THEN
                    next_beta = -beta
                    next_alpha = -current_alpha
                    score = -negamax(board, new_depth, ply + 1, next_beta, next_alpha)
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

            ! Alpha-beta pruning
            IF (current_alpha >= beta) THEN
                 CALL store_tt_entry(board%zobrist_key, depth_param, beta, HASH_FLAG_ALPHA, moves(i), ply)
                 CALL store_killer(current_move, ply)
                 IF (is_quiet .AND. .NOT. current_move%is_castling) THEN
                     CALL update_history_score(board, current_move, current_depth)
                 END IF
                 best_score = current_alpha
                 RETURN
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
        last_iteration_score = 0
        completed_depth = 0
        search_node_count = 0
        CALL SYSTEM_CLOCK(start_count, count_rate)
        CALL init_lmr_table()

        aspiration_delta = 50

        CALL clear_killers()
        CALL clear_history()
        CALL new_search_generation()

        IF (is_fifty_move_draw(board) .OR. is_threefold_repetition(board)) THEN
            best_move_found = .FALSE.
            IF (PRESENT(best_score_out)) best_score_out = 0
            IF (PRESENT(completed_depth_out)) completed_depth_out = 0
            RETURN
        END IF

        IF (PRESENT(root_moves_in) .AND. PRESENT(root_num_moves_in)) THEN
            num_moves = root_num_moves_in
            IF (num_moves > 0) moves(1:num_moves) = root_moves_in(1:num_moves)
        ELSE
            CALL generate_moves(board, moves, num_moves)
        END IF

        IF (num_moves == 0) THEN
             IF (PRESENT(completed_depth_out)) completed_depth_out = 0
             RETURN
        END IF

        best_move = moves(1)
        best_move_found = .TRUE.

        IF (max_depth > 0) THEN
            target_depth = max_depth
        ELSE
            target_depth = SEARCH_DEPTH_LIMIT
        END IF

        ! Iterative Deepening Loop
        d = 1
        DO WHILE (d <= target_depth)
            research_needed = .TRUE.
            iteration_completed = .FALSE.
            DO WHILE (research_needed)
                research_needed = .FALSE.

                IF (d == 1) THEN
                    alpha = -INF
                    beta = INF
                ELSE
                    alpha = last_iteration_score - aspiration_delta
                    beta = last_iteration_score + aspiration_delta
                END IF

                IF (alpha < -INF) alpha = -INF
                IF (beta > INF) beta = INF

                best_score_so_far = -INF
                iteration_found = .FALSE.

                DO i = 1, num_moves
                    IF (poll_search_stop()) EXIT
                    current_move = moves(i)

                    CALL make_move(board, current_move, unmake_info)

                    ! PVS at root: full window for first move, null window for rest
                    IF (i == 1) THEN
                        next_beta = -beta
                        next_alpha = -alpha
                        score = -negamax(board, d - 1, 1, next_beta, next_alpha)
                    ELSE
                        ! Null window scout
                        next_beta = -alpha - 1
                        next_alpha = -alpha
                        score = -negamax(board, d - 1, 1, next_beta, next_alpha)
                        ! Re-search with full window if it beats alpha
                        IF (score > alpha .AND. score < beta) THEN
                            next_beta = -beta
                            next_alpha = -alpha
                            score = -negamax(board, d - 1, 1, next_beta, next_alpha)
                        END IF
                    END IF

                    CALL unmake_move(board, current_move, unmake_info)

                    IF (score > best_score_so_far) THEN
                         best_score_so_far = score
                         iteration_best_move = current_move
                         iteration_found = .TRUE.
                    END IF

                    IF (best_score_so_far > alpha) THEN
                         alpha = best_score_so_far
                    END IF
                END DO

                IF (search_stop_requested()) EXIT

                IF (d /= 1) THEN
                    IF (best_score_so_far <= last_iteration_score - aspiration_delta) THEN
                        alpha = -INF
                        beta = last_iteration_score + aspiration_delta
                        research_needed = .TRUE.
                        aspiration_delta = aspiration_delta + 50
                    ELSE IF (best_score_so_far >= last_iteration_score + aspiration_delta) THEN
                        alpha = last_iteration_score - aspiration_delta
                        beta = INF
                        research_needed = .TRUE.
                        aspiration_delta = aspiration_delta + 50
                    END IF
                END IF

                IF (.NOT. research_needed) THEN
                    iteration_completed = .TRUE.
                END IF

            END DO

            IF (search_stop_requested()) EXIT

            IF (iteration_completed .AND. iteration_found) THEN
                best_move = iteration_best_move
                best_move_found = .TRUE.
                completed_depth = d
                last_iteration_score = best_score_so_far
            END IF

            DO i = 1, num_moves
                IF (moves_equal(moves(i), best_move)) THEN
                    current_move = moves(1)
                    moves(1) = moves(i)
                    moves(i) = current_move
                    EXIT
                END IF
            END DO

            IF (max_depth <= 0 .AND. d == target_depth) THEN
                target_depth = target_depth + SEARCH_DEPTH_LIMIT
            END IF
            d = d + 1
        END DO

        IF (PRESENT(best_score_out)) best_score_out = best_score_so_far
        IF (PRESENT(completed_depth_out)) completed_depth_out = completed_depth

    END SUBROUTINE find_best_move

END MODULE Search
