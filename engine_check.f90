PROGRAM Engine_Check
    USE Chess_Types
    USE Board_Utils, ONLY: init_board, char_to_file, char_to_rank, compute_pawn_hash, is_in_check
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move, unmake_move
    USE Search, ONLY: find_best_move
    USE Transposition_Table, ONLY: init_zobrist_keys, compute_zobrist_hash
    USE UCI_Driver, ONLY: set_board_from_fen
    IMPLICIT NONE

    TYPE(Board_Type) :: board
    LOGICAL :: ok

    CALL init_zobrist_keys()
    CALL init_board(board)

    ok = check_perft_suite()
    ok = check_mate_in_one_suite() .AND. ok
    ok = check_en_passant_sequence() .AND. ok
    ok = check_castling_sequence() .AND. ok
    ok = check_search_runs() .AND. ok
    ok = check_repeated_search_stability() .AND. ok

    IF (ok) THEN
        WRITE(*,'(A)') 'engine_check: PASS'
        STOP 0
    END IF

    WRITE(*,'(A)') 'engine_check: FAIL'
    STOP 1

CONTAINS

    LOGICAL FUNCTION check_perft_suite() RESULT(ok)
        ok = .TRUE.
        CALL check_perft_case('Initial Position', &
            'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1', &
            (/1, 2, 3, 4/), (/20_8, 400_8, 8902_8, 197281_8/), 4, ok)
        CALL check_perft_case('Kiwipete', &
            'r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1', &
            (/1, 2, 3/), (/48_8, 2039_8, 97862_8/), 3, ok)
        CALL check_perft_case('Position 3', &
            '8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1', &
            (/1, 2, 3/), (/14_8, 191_8, 2812_8/), 3, ok)
        CALL check_perft_case('Position 4', &
            'r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1', &
            (/1, 2, 3/), (/6_8, 264_8, 9467_8/), 3, ok)
        CALL check_perft_case('Position 5', &
            'rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8', &
            (/1, 2, 3/), (/44_8, 1486_8, 62379_8/), 3, ok)
        CALL check_perft_case('Position 6', &
            'r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10', &
            (/1, 2, 3/), (/46_8, 2079_8, 89890_8/), 3, ok)
        CALL check_perft_case('En Passant Capture', &
            'rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3', &
            (/1, 2, 3/), (/31_8, 707_8, 21637_8/), 3, ok)
        CALL check_perft_case('Promotion', &
            'n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1', &
            (/1, 2, 3/), (/24_8, 496_8, 9483_8/), 3, ok)
        CALL check_perft_case('Castling', &
            'r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1', &
            (/1, 2, 3/), (/26_8, 568_8, 13744_8/), 3, ok)
    END FUNCTION check_perft_suite

    SUBROUTINE check_perft_case(name, fen, depths, expected, num_depths, ok)
        CHARACTER(LEN=*), INTENT(IN) :: name, fen
        INTEGER, INTENT(IN) :: num_depths
        INTEGER, DIMENSION(num_depths), INTENT(IN) :: depths
        INTEGER(KIND=8), DIMENSION(num_depths), INTENT(IN) :: expected
        LOGICAL, INTENT(INOUT) :: ok
        TYPE(Board_Type) :: board
        INTEGER(KIND=8) :: nodes
        LOGICAL :: local_ok
        INTEGER :: i

        IF (.NOT. ok) RETURN

        local_ok = set_board_from_fen(board, fen)
        IF (.NOT. local_ok) THEN
            WRITE(*,'(A,A)') 'failed to parse perft FEN: ', TRIM(name)
            ok = .FALSE.
            RETURN
        END IF

        DO i = 1, num_depths
            nodes = perft(board, depths(i), local_ok)
            IF (.NOT. local_ok .OR. nodes /= expected(i)) THEN
                WRITE(*,'(A,A,A,I0,A,I0,A,I0)') 'perft mismatch: ', TRIM(name), &
                    ' depth ', depths(i), ' expected ', expected(i), ' got ', nodes
                ok = .FALSE.
                RETURN
            END IF
        END DO
    END SUBROUTINE check_perft_case

    LOGICAL FUNCTION check_mate_in_one_suite() RESULT(ok)
        ok = .TRUE.
        CALL check_mate_in_one_case('problem 1', &
            '3q1rk1/5pbp/5Qp1/8/8/2B5/5PPP/6K1 w - - 0 1', 'f6g7', ok)
        CALL check_mate_in_one_case('problem 2', &
            '2r2rk1/2q2p1p/6pQ/4P1N1/8/8/PPP5/2KR4 w - - 0 1', 'h6h7', ok)
        CALL check_mate_in_one_case('problem 5', &
            '1r4k1/1q3p2/5Bp1/8/8/8/PP6/1K5R w - - 0 1', 'h1h8', ok)
        CALL check_mate_in_one_case('problem 13', &
            'r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5Q2/PPPP1PPP/RNB1K1NR w KQkq - 0 1', 'f3f7', ok)
        CALL check_mate_in_one_case('problem 19 promotion knight', &
            '6r1/2Q2P2/5k2/5P2/5K2/8/8/8 w - - 0 1', 'f7g8n', ok)
        CALL check_mate_in_one_case('problem 20 promotion queen', &
            '8/3pkP2/4p3/8/8/3K4/8/5R2 w - - 0 1', 'f7f8q', ok)
        CALL check_mate_in_one_case('problem 40', &
            '5k2/4np2/5N2/2B5/8/8/8/6RK w - - 0 1', 'g1g8', ok)
        CALL check_mate_in_one_case('problem 75', &
            '7k/8/5N2/8/8/8/8/6RK w - - 0 1', 'g1g8', ok)
    END FUNCTION check_mate_in_one_suite

    SUBROUTINE check_mate_in_one_case(name, fen, move_str, ok)
        CHARACTER(LEN=*), INTENT(IN) :: name, fen, move_str
        LOGICAL, INTENT(INOUT) :: ok
        TYPE(Board_Type) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves

        IF (.NOT. ok) RETURN

        ok = set_board_from_fen(board, fen)
        IF (.NOT. ok) THEN
            WRITE(*,'(A,A)') 'failed to parse mate FEN: ', TRIM(name)
            RETURN
        END IF

        ok = play_uci_move(board, move_str)
        IF (.NOT. ok) THEN
            WRITE(*,'(A,A,A,A)') 'mate move not legal: ', TRIM(name), ' move ', TRIM(move_str)
            RETURN
        END IF

        CALL generate_moves(board, moves, num_moves)
        ok = is_in_check(board, board%current_player) .AND. num_moves == 0
        IF (.NOT. ok) THEN
            WRITE(*,'(A,A,A,A,A,I0)') 'move did not checkmate: ', TRIM(name), &
                ' move ', TRIM(move_str), ' replies ', num_moves
        END IF
    END SUBROUTINE check_mate_in_one_case

    LOGICAL FUNCTION check_en_passant_sequence() RESULT(ok)
        TYPE(Board_Type) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves

        CALL init_board(board)
        ok = play_uci_move(board, 'e2e4')
        ok = play_uci_move(board, 'a7a6') .AND. ok
        ok = play_uci_move(board, 'e4e5') .AND. ok
        ok = play_uci_move(board, 'd7d5') .AND. ok
        IF (.NOT. ok) THEN
            WRITE(*,'(A)') 'failed to build en passant test position'
            RETURN
        END IF

        CALL generate_moves(board, moves, num_moves)
        ok = move_exists(moves, num_moves, 'e5d6')
        IF (.NOT. ok) WRITE(*,'(A)') 'missing legal en passant move e5d6'
    END FUNCTION check_en_passant_sequence

    LOGICAL FUNCTION check_castling_sequence() RESULT(ok)
        TYPE(Board_Type) :: board
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        INTEGER :: num_moves

        CALL init_board(board)
        ok = play_uci_move(board, 'e2e4')
        ok = play_uci_move(board, 'e7e5') .AND. ok
        ok = play_uci_move(board, 'g1f3') .AND. ok
        ok = play_uci_move(board, 'b8c6') .AND. ok
        ok = play_uci_move(board, 'f1e2') .AND. ok
        ok = play_uci_move(board, 'g8f6') .AND. ok
        IF (.NOT. ok) THEN
            WRITE(*,'(A)') 'failed to build castling test position'
            RETURN
        END IF

        CALL generate_moves(board, moves, num_moves)
        ok = move_exists(moves, num_moves, 'e1g1')
        IF (.NOT. ok) WRITE(*,'(A)') 'missing legal castling move e1g1'
    END FUNCTION check_castling_sequence

    LOGICAL FUNCTION check_search_runs() RESULT(ok)
        TYPE(Board_Type) :: board
        TYPE(Move_Type) :: best_move
        LOGICAL :: found
        INTEGER :: score, completed_depth

        CALL init_board(board)
        CALL find_best_move(board, 1, found, best_move, best_score_out=score, completed_depth_out=completed_depth)
        ok = found .AND. completed_depth == 1
        IF (.NOT. ok) THEN
            WRITE(*,'(A,L1,A,I0)') 'search depth-1 failed: found=', found, ' completed_depth=', completed_depth
        END IF
    END FUNCTION check_search_runs

    LOGICAL FUNCTION check_repeated_search_stability() RESULT(ok)
        TYPE(Board_Type) :: board
        TYPE(Move_Type) :: best_move
        LOGICAL :: found
        INTEGER :: score, completed_depth, i

        ok = set_board_from_fen(board, '7k/8/5N2/8/8/8/8/6RK w - - 0 1')
        IF (.NOT. ok) THEN
            WRITE(*,'(A)') 'failed to parse repeated-search regression FEN'
            RETURN
        END IF

        DO i = 1, 3
            CALL find_best_move(board, 2, found, best_move, best_score_out=score, completed_depth_out=completed_depth)
            ok = found .AND. completed_depth == 2 .AND. move_to_string(best_move) == 'g1g8'
            IF (.NOT. ok) THEN
                WRITE(*,'(A,I0,A,A,A,L1,A,I0)') 'repeated search mismatch on iteration ', i, &
                    ': move=', move_to_string(best_move), ' found=', found, ' completed_depth=', completed_depth
                RETURN
            END IF
        END DO
    END FUNCTION check_repeated_search_stability

    RECURSIVE INTEGER(KIND=8) FUNCTION perft(board, depth, ok) RESULT(nodes)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(IN) :: depth
        LOGICAL, INTENT(INOUT) :: ok
        TYPE(Board_Type) :: before
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        TYPE(UnmakeInfo_Type) :: info
        INTEGER :: i, num_moves

        IF (.NOT. ok) THEN
            nodes = 0_8
            RETURN
        END IF

        CALL verify_board_state(board, ok)
        IF (.NOT. ok) THEN
            nodes = 0_8
            RETURN
        END IF

        IF (depth == 0) THEN
            nodes = 1_8
            RETURN
        END IF

        CALL generate_moves(board, moves, num_moves)
        IF (depth == 1) THEN
            nodes = INT(num_moves, KIND=8)
            RETURN
        END IF

        nodes = 0_8
        before = board
        DO i = 1, num_moves
            CALL make_move(board, moves(i), info)
            nodes = nodes + perft(board, depth - 1, ok)
            CALL unmake_move(board, moves(i), info)
            IF (.NOT. boards_equal(board, before)) THEN
                WRITE(*,'(A,I0,A,A)') 'board mismatch after unmake at depth ', depth, ' move ', move_to_string(moves(i))
                CALL print_board_diff(before, board)
                ok = .FALSE.
                RETURN
            END IF
            IF (.NOT. ok) RETURN
        END DO
    END FUNCTION perft

    SUBROUTINE verify_board_state(board, ok)
        TYPE(Board_Type), INTENT(IN) :: board
        LOGICAL, INTENT(INOUT) :: ok
        INTEGER(KIND=8) :: recomputed_key, recomputed_pawn_key

        IF (.NOT. ok) RETURN

        recomputed_key = compute_zobrist_hash(board)
        IF (board%zobrist_key /= recomputed_key) THEN
            WRITE(*,'(A)') 'zobrist mismatch detected'
            ok = .FALSE.
            RETURN
        END IF

        recomputed_pawn_key = compute_pawn_hash(board)
        IF (board%pawn_hash_key /= recomputed_pawn_key) THEN
            WRITE(*,'(A)') 'pawn hash mismatch detected'
            ok = .FALSE.
            RETURN
        END IF

        IF (.NOT. piece_lists_match_board(board)) THEN
            WRITE(*,'(A)') 'piece list mismatch detected'
            ok = .FALSE.
        END IF
    END SUBROUTINE verify_board_state

    LOGICAL FUNCTION piece_lists_match_board(board) RESULT(ok)
        TYPE(Board_Type), INTENT(IN) :: board
        LOGICAL :: seen_white(BOARD_SIZE, BOARD_SIZE), seen_black(BOARD_SIZE, BOARD_SIZE)
        INTEGER :: i, r, f, white_count, black_count

        ok = .TRUE.
        seen_white = .FALSE.
        seen_black = .FALSE.
        white_count = 0
        black_count = 0

        DO i = 1, board%num_white_pieces
            r = board%white_pieces(i)%rank
            f = board%white_pieces(i)%file
            IF (r < 1 .OR. r > BOARD_SIZE .OR. f < 1 .OR. f > BOARD_SIZE) THEN
                ok = .FALSE.
                RETURN
            END IF
            IF (board%squares_color(r, f) /= WHITE .OR. board%squares_piece(r, f) == NO_PIECE) THEN
                ok = .FALSE.
                RETURN
            END IF
            IF (seen_white(r, f)) THEN
                ok = .FALSE.
                RETURN
            END IF
            seen_white(r, f) = .TRUE.
            white_count = white_count + 1
        END DO

        DO i = 1, board%num_black_pieces
            r = board%black_pieces(i)%rank
            f = board%black_pieces(i)%file
            IF (r < 1 .OR. r > BOARD_SIZE .OR. f < 1 .OR. f > BOARD_SIZE) THEN
                ok = .FALSE.
                RETURN
            END IF
            IF (board%squares_color(r, f) /= BLACK .OR. board%squares_piece(r, f) == NO_PIECE) THEN
                ok = .FALSE.
                RETURN
            END IF
            IF (seen_black(r, f)) THEN
                ok = .FALSE.
                RETURN
            END IF
            seen_black(r, f) = .TRUE.
            black_count = black_count + 1
        END DO

        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                SELECT CASE (board%squares_color(r, f))
                CASE (WHITE)
                    IF (.NOT. seen_white(r, f)) THEN
                        ok = .FALSE.
                        RETURN
                    END IF
                CASE (BLACK)
                    IF (.NOT. seen_black(r, f)) THEN
                        ok = .FALSE.
                        RETURN
                    END IF
                CASE DEFAULT
                    IF (board%squares_piece(r, f) /= NO_PIECE) THEN
                        ok = .FALSE.
                        RETURN
                    END IF
                END SELECT
            END DO
        END DO

        ok = ok .AND. white_count == board%num_white_pieces .AND. black_count == board%num_black_pieces
    END FUNCTION piece_lists_match_board

    LOGICAL FUNCTION boards_equal(a, b) RESULT(equal)
        TYPE(Board_Type), INTENT(IN) :: a, b

        equal = ALL(a%squares_piece == b%squares_piece)
        equal = equal .AND. ALL(a%squares_color == b%squares_color)
        equal = equal .AND. (a%current_player == b%current_player)
        equal = equal .AND. (a%ep_target_present .EQV. b%ep_target_present)
        equal = equal .AND. (a%ep_target_sq%rank == b%ep_target_sq%rank)
        equal = equal .AND. (a%ep_target_sq%file == b%ep_target_sq%file)
        equal = equal .AND. (a%wc_k .EQV. b%wc_k)
        equal = equal .AND. (a%wc_q .EQV. b%wc_q)
        equal = equal .AND. (a%bc_k .EQV. b%bc_k)
        equal = equal .AND. (a%bc_q .EQV. b%bc_q)
        equal = equal .AND. (a%num_white_pieces == b%num_white_pieces)
        equal = equal .AND. (a%num_black_pieces == b%num_black_pieces)
        equal = equal .AND. (a%halfmove_clock == b%halfmove_clock)
        equal = equal .AND. (a%fullmove_number == b%fullmove_number)
        equal = equal .AND. (a%zobrist_key == b%zobrist_key)
        equal = equal .AND. (a%repetition_count == b%repetition_count)
        equal = equal .AND. repetition_prefix_equal(a, b)
        equal = equal .AND. piece_lists_match_board(a)
        equal = equal .AND. piece_lists_match_board(b)
    END FUNCTION boards_equal

    LOGICAL FUNCTION repetition_prefix_equal(a, b) RESULT(equal)
        TYPE(Board_Type), INTENT(IN) :: a, b
        INTEGER :: active

        equal = .FALSE.
        IF (a%repetition_count /= b%repetition_count) RETURN
        active = a%repetition_count
        IF (active <= 0) THEN
            equal = .TRUE.
        ELSE
            equal = ALL(a%repetition_history(1:active) == b%repetition_history(1:active))
        END IF
    END FUNCTION repetition_prefix_equal

    SUBROUTINE print_board_diff(expected, actual)
        TYPE(Board_Type), INTENT(IN) :: expected, actual
        INTEGER :: i

        IF (.NOT. ALL(expected%squares_piece == actual%squares_piece)) WRITE(*,'(A)') '  squares_piece differs'
        IF (.NOT. ALL(expected%squares_color == actual%squares_color)) WRITE(*,'(A)') '  squares_color differs'
        IF (expected%current_player /= actual%current_player) WRITE(*,'(A,2(I0,1X))') '  current_player: ', expected%current_player, actual%current_player
        IF (.NOT. (expected%ep_target_present .EQV. actual%ep_target_present)) WRITE(*,'(A,2(L1,1X))') '  ep_target_present: ', expected%ep_target_present, actual%ep_target_present
        IF (expected%ep_target_sq%rank /= actual%ep_target_sq%rank .OR. expected%ep_target_sq%file /= actual%ep_target_sq%file) THEN
            WRITE(*,'(A,4(I0,1X))') '  ep_target_sq: ', expected%ep_target_sq%rank, expected%ep_target_sq%file, actual%ep_target_sq%rank, actual%ep_target_sq%file
        END IF
        IF (.NOT. (expected%wc_k .EQV. actual%wc_k) .OR. .NOT. (expected%wc_q .EQV. actual%wc_q) .OR. &
            .NOT. (expected%bc_k .EQV. actual%bc_k) .OR. .NOT. (expected%bc_q .EQV. actual%bc_q)) THEN
            WRITE(*,'(A,8(L1,1X))') '  castling: ', expected%wc_k, expected%wc_q, expected%bc_k, expected%bc_q, &
                actual%wc_k, actual%wc_q, actual%bc_k, actual%bc_q
        END IF
        IF (expected%num_white_pieces /= actual%num_white_pieces .OR. expected%num_black_pieces /= actual%num_black_pieces) THEN
            WRITE(*,'(A,4(I0,1X))') '  piece counts: ', expected%num_white_pieces, expected%num_black_pieces, actual%num_white_pieces, actual%num_black_pieces
        END IF
        IF (.NOT. ALL(expected%white_pieces(:)%rank == actual%white_pieces(:)%rank) .OR. &
            .NOT. ALL(expected%white_pieces(:)%file == actual%white_pieces(:)%file)) WRITE(*,'(A)') '  white piece list differs'
        IF (.NOT. ALL(expected%black_pieces(:)%rank == actual%black_pieces(:)%rank) .OR. &
            .NOT. ALL(expected%black_pieces(:)%file == actual%black_pieces(:)%file)) WRITE(*,'(A)') '  black piece list differs'
        IF (expected%halfmove_clock /= actual%halfmove_clock .OR. expected%fullmove_number /= actual%fullmove_number) THEN
            WRITE(*,'(A,4(I0,1X))') '  clocks: ', expected%halfmove_clock, expected%fullmove_number, actual%halfmove_clock, actual%fullmove_number
        END IF
        IF (expected%zobrist_key /= actual%zobrist_key) WRITE(*,'(A,2(I0,1X))') '  zobrist: ', expected%zobrist_key, actual%zobrist_key
        WRITE(*,'(A,2(I0,1X))') '  repetition_count: ', expected%repetition_count, actual%repetition_count
        WRITE(*,'(A,2(L1,1X))') '  piece list valid: ', piece_lists_match_board(expected), piece_lists_match_board(actual)
        IF (.NOT. ALL(expected%repetition_history == actual%repetition_history)) THEN
            WRITE(*,'(A)') '  repetition_history differs'
            DO i = 1, MIN(4, SIZE(expected%repetition_history))
                IF (expected%repetition_history(i) /= actual%repetition_history(i)) THEN
                    WRITE(*,'(A,I0,A,2(I0,1X))') '    idx ', i, ': ', expected%repetition_history(i), actual%repetition_history(i)
                END IF
            END DO
        END IF
    END SUBROUTINE print_board_diff

    LOGICAL FUNCTION play_uci_move(board, move_str) RESULT(ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: move_str
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: moves
        TYPE(UnmakeInfo_Type) :: info
        INTEGER :: num_moves, i, ff, fr, tf, tr, promo_piece

        ok = .FALSE.
        CALL generate_moves(board, moves, num_moves)

        ff = char_to_file(move_str(1:1))
        fr = char_to_rank(move_str(2:2))
        tf = char_to_file(move_str(3:3))
        tr = char_to_rank(move_str(4:4))
        promo_piece = promotion_piece_from_string(move_str)
        IF (ff < 1 .OR. fr < 1 .OR. tf < 1 .OR. tr < 1) RETURN

        DO i = 1, num_moves
            IF (moves(i)%from_sq%file == ff .AND. moves(i)%from_sq%rank == fr .AND. &
                moves(i)%to_sq%file == tf .AND. moves(i)%to_sq%rank == tr .AND. &
                moves(i)%promotion_piece == promo_piece) THEN
                CALL make_move(board, moves(i), info)
                ok = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION play_uci_move

    LOGICAL FUNCTION move_exists(moves, num_moves, move_str) RESULT(found)
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: moves
        INTEGER, INTENT(IN) :: num_moves
        CHARACTER(LEN=*), INTENT(IN) :: move_str
        INTEGER :: ff, fr, tf, tr, promo_piece, i

        found = .FALSE.
        ff = char_to_file(move_str(1:1))
        fr = char_to_rank(move_str(2:2))
        tf = char_to_file(move_str(3:3))
        tr = char_to_rank(move_str(4:4))
        promo_piece = promotion_piece_from_string(move_str)

        DO i = 1, num_moves
            IF (moves(i)%from_sq%file == ff .AND. moves(i)%from_sq%rank == fr .AND. &
                moves(i)%to_sq%file == tf .AND. moves(i)%to_sq%rank == tr .AND. &
                moves(i)%promotion_piece == promo_piece) THEN
                found = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION move_exists

    INTEGER FUNCTION promotion_piece_from_string(move_str) RESULT(piece)
        CHARACTER(LEN=*), INTENT(IN) :: move_str

        piece = NO_PIECE
        IF (LEN_TRIM(move_str) < 5) RETURN

        SELECT CASE (move_str(5:5))
        CASE ('q', 'Q')
            piece = QUEEN
        CASE ('r', 'R')
            piece = ROOK
        CASE ('b', 'B')
            piece = BISHOP
        CASE ('n', 'N')
            piece = KNIGHT
        END SELECT
    END FUNCTION promotion_piece_from_string

    CHARACTER(LEN=5) FUNCTION move_to_string(move) RESULT(str)
        TYPE(Move_Type), INTENT(IN) :: move

        str = ''
        str(1:1) = CHAR(ICHAR('a') + move%from_sq%file - 1)
        str(2:2) = CHAR(ICHAR('0') + move%from_sq%rank)
        str(3:3) = CHAR(ICHAR('a') + move%to_sq%file - 1)
        str(4:4) = CHAR(ICHAR('0') + move%to_sq%rank)
        IF (move%promotion_piece /= NO_PIECE) THEN
            SELECT CASE (move%promotion_piece)
            CASE (QUEEN)
                str(5:5) = 'q'
            CASE (ROOK)
                str(5:5) = 'r'
            CASE (BISHOP)
                str(5:5) = 'b'
            CASE (KNIGHT)
                str(5:5) = 'n'
            END SELECT
        END IF
    END FUNCTION move_to_string

END PROGRAM Engine_Check
