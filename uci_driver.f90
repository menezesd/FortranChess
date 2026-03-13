MODULE UCI_Driver
    USE Chess_Types
    USE Board_Utils, ONLY: char_to_file, char_to_rank, file_rank_to_sq, init_board
    USE Move_Generation, ONLY: generate_moves
    USE Make_Unmake, ONLY: make_move
    USE Search, ONLY: find_best_move
    USE Search_Control, ONLY: begin_search_polling, end_search_polling, search_quit_requested, &
        has_buffered_command, pop_buffered_command, read_next_command
    USE Transposition_Table, ONLY: init_zobrist_keys, compute_zobrist_hash
    USE Evaluation, ONLY: evaluate_board
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: run_uci_mode

CONTAINS

    SUBROUTINE run_uci_mode()
        TYPE(Board_Type) :: board
        CHARACTER(LEN=256) :: line
        LOGICAL :: has_command

        CALL init_zobrist_keys()
        CALL init_board(board)

        DO
            IF (has_buffered_command()) THEN
                CALL pop_buffered_command(line, has_command)
                IF (.NOT. has_command) CYCLE
            ELSE
                CALL read_next_command(line, has_command)
                IF (.NOT. has_command) EXIT
            END IF
            CALL handle_command(board, TRIM(ADJUSTL(line)))
        END DO
    END SUBROUTINE run_uci_mode

    SUBROUTINE handle_command(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        CHARACTER(LEN=32) :: command
        INTEGER :: pos
        LOGICAL :: has_token

        IF (LEN_TRIM(line) == 0) RETURN

        pos = 1
        CALL next_token(line, pos, command, has_token)
        IF (.NOT. has_token) RETURN

        IF (command == 'ucinewgame') THEN
            CALL init_board(board)
        ELSE IF (command == 'uci') THEN
            CALL uci_init()
        ELSE IF (command == 'isready') THEN
            WRITE(*,'(A)') 'readyok'
        ELSE IF (command == 'debug') THEN
            CONTINUE
        ELSE IF (command == 'setoption') THEN
            CONTINUE
        ELSE IF (command == 'register') THEN
            CONTINUE
        ELSE IF (command == 'position') THEN
            CALL handle_position(board, line)
        ELSE IF (command == 'go') THEN
            CALL handle_go(board, line)
        ELSE IF (command == 'ponderhit') THEN
            CONTINUE
        ELSE IF (command == 'stop') THEN
            CONTINUE
        ELSE IF (command == 'quit') THEN
            STOP
        END IF
    END SUBROUTINE handle_command

    SUBROUTINE uci_init()
        WRITE(*,'(A)') 'id name FortranChess'
        WRITE(*,'(A)') 'id author Dean Menezes'
        WRITE(*,'(A)') 'uciok'
    END SUBROUTINE uci_init

    SUBROUTINE handle_position(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: pos_moves, lenline
        LOGICAL :: fen_ok, moves_ok, has_base_position

        lenline = LEN_TRIM(line)
        pos_moves = find_token(line, 'moves', 1)
        fen_ok = .TRUE.
        has_base_position = .FALSE.

        IF (find_token(line, 'startpos', 1) > 0) THEN
            CALL init_board(board)
            has_base_position = .TRUE.
        ELSE IF (find_token(line, 'fen', 1) > 0) THEN
            CALL handle_fen(board, line, fen_ok)
            has_base_position = fen_ok
        END IF

        IF (has_base_position .AND. fen_ok .AND. pos_moves > 0) THEN
            IF (pos_moves + 5 <= lenline) THEN
                CALL parse_moves(board, line(pos_moves+5:lenline), moves_ok)
            END IF
        END IF
    END SUBROUTINE handle_position

    SUBROUTINE parse_moves(board, moves_str, ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: moves_str
        LOGICAL, INTENT(OUT) :: ok
        TYPE(Board_Type) :: parsed_board
        INTEGER :: start, finish, l
        CHARACTER(LEN=8) :: mv
        LOGICAL :: move_ok

        ok = .FALSE.
        parsed_board = board
        l = LEN_TRIM(moves_str)
        start = 1
        DO WHILE (start <= l)
            DO WHILE (start <= l .AND. moves_str(start:start) == ' ')
                start = start + 1
            END DO
            IF (start > l) EXIT
            finish = start
            DO WHILE (finish <= l)
                IF (moves_str(finish:finish) == ' ') EXIT
                finish = finish + 1
            END DO
            mv = moves_str(start:MIN(finish-1, LEN(moves_str)))
            move_ok = apply_uci_move(parsed_board, TRIM(mv))
            IF (.NOT. move_ok) RETURN
            start = finish + 1
        END DO
        board = parsed_board
        ok = .TRUE.
    END SUBROUTINE parse_moves

    SUBROUTINE handle_fen(board, line, ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        LOGICAL, INTENT(OUT) :: ok
        INTEGER :: fen_pos, moves_pos, fen_end
        CHARACTER(LEN=256) :: fen

        fen_pos = find_token(line, 'fen', 1)
        IF (fen_pos == 0) THEN
            ok = .FALSE.
            RETURN
        END IF
        moves_pos = find_token(line, 'moves', fen_pos + 1)
        IF (moves_pos > 0) THEN
            fen_end = moves_pos - 1
        ELSE
            fen_end = LEN_TRIM(line)
        END IF
        fen = TRIM(ADJUSTL(line(fen_pos+3:fen_end)))
        ok = set_board_from_fen(board, fen)
    END SUBROUTINE handle_fen

    LOGICAL FUNCTION set_board_from_fen(board, fen) RESULT(ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: fen
        TYPE(Board_Type) :: parsed_board
        CHARACTER(LEN=128) :: placement_str
        CHARACTER(LEN=16) :: side_str, castle_str, ep_str
        INTEGER :: rank, file, idx
        INTEGER :: pos1, pos2, pos3, pos4
        INTEGER :: halfmove_clock, fullmove_number

        ok = .FALSE.
        parsed_board = board
        parsed_board%squares_piece = NO_PIECE
        parsed_board%squares_color = NO_COLOR
        parsed_board%num_white_pieces = 0
        parsed_board%num_black_pieces = 0
        parsed_board%wc_k = .FALSE.; parsed_board%wc_q = .FALSE.
        parsed_board%bc_k = .FALSE.; parsed_board%bc_q = .FALSE.
        parsed_board%ep_target_present = .FALSE.
        parsed_board%ep_target_sq%file = 0
        parsed_board%ep_target_sq%rank = 0
        parsed_board%repetition_history = 0
        parsed_board%repetition_count = 0

        pos1 = INDEX(fen, ' ')
        IF (pos1 == 0) RETURN
        pos2 = INDEX(fen(pos1+1:), ' ')
        IF (pos2 == 0) RETURN
        pos2 = pos1 + pos2
        pos3 = INDEX(fen(pos2+1:), ' ')
        IF (pos3 == 0) RETURN
        pos3 = pos2 + pos3
        pos4 = INDEX(fen(pos3+1:), ' ')
        IF (pos4 == 0) RETURN
        pos4 = pos3 + pos4

        placement_str = TRIM(fen(1:pos1-1))
        side_str = TRIM(ADJUSTL(fen(pos1+1:pos2-1)))
        castle_str = TRIM(ADJUSTL(fen(pos2+1:pos3-1)))
        ep_str = TRIM(ADJUSTL(fen(pos3+1:pos4-1)))

        READ(fen(pos4+1:), *, ERR=900, END=900) halfmove_clock, fullmove_number

        rank = 8
        file = 1
        DO idx = 1, LEN_TRIM(placement_str)
            CALL parse_fen_piece_char(placement_str(idx:idx), rank, file, parsed_board, ok)
            IF (.NOT. ok) RETURN
        END DO
        IF (rank /= 1 .OR. file /= 9) RETURN
        IF (.NOT. has_exactly_one_king(parsed_board, WHITE)) RETURN
        IF (.NOT. has_exactly_one_king(parsed_board, BLACK)) RETURN

        SELECT CASE (side_str)
        CASE ('w')
            parsed_board%current_player = WHITE
        CASE ('b')
            parsed_board%current_player = BLACK
        CASE DEFAULT
            RETURN
        END SELECT

        IF (castle_str /= '-') THEN
            IF (INDEX(castle_str, 'K') > 0) parsed_board%wc_k = .TRUE.
            IF (INDEX(castle_str, 'Q') > 0) parsed_board%wc_q = .TRUE.
            IF (INDEX(castle_str, 'k') > 0) parsed_board%bc_k = .TRUE.
            IF (INDEX(castle_str, 'q') > 0) parsed_board%bc_q = .TRUE.
        END IF

        IF (ep_str /= '-') THEN
            IF (LEN_TRIM(ep_str) /= 2) RETURN
            parsed_board%ep_target_sq%file = char_to_file(ep_str(1:1))
            parsed_board%ep_target_sq%rank = char_to_rank(ep_str(2:2))
            IF (parsed_board%ep_target_sq%file == -1 .OR. parsed_board%ep_target_sq%rank == -1) RETURN
            parsed_board%ep_target_present = .TRUE.
        END IF

        parsed_board%halfmove_clock = halfmove_clock
        parsed_board%fullmove_number = fullmove_number
        parsed_board%zobrist_key = compute_zobrist_hash(parsed_board)
        parsed_board%repetition_count = 1
        parsed_board%repetition_history(1) = parsed_board%zobrist_key
        board = parsed_board
        ok = .TRUE.
        RETURN

900     CONTINUE
    END FUNCTION set_board_from_fen

    SUBROUTINE parse_fen_piece_char(c, rank, file, board, ok)
        CHARACTER(LEN=1), INTENT(IN) :: c
        INTEGER, INTENT(INOUT) :: rank, file
        TYPE(Board_Type), INTENT(INOUT) :: board
        LOGICAL, INTENT(OUT) :: ok
        INTEGER :: piece, color, empty_count

        ok = .FALSE.

        IF (c == '/') THEN
            IF (file /= 9 .OR. rank <= 1) RETURN
            rank = rank - 1
            file = 1
            ok = .TRUE.
            RETURN
        END IF

        IF (c >= '1' .AND. c <= '8') THEN
            empty_count = IACHAR(c) - IACHAR('0')
            IF (file + empty_count - 1 > 8) RETURN
            file = file + empty_count
            ok = .TRUE.
            RETURN
        END IF

        CALL fen_char_to_piece(c, piece, color)
        IF (piece == NO_PIECE .OR. file > 8 .OR. rank < 1) RETURN

        board%squares_piece(rank, file) = piece
        board%squares_color(rank, file) = color
        CALL add_piece_list_entry(board, file_rank_to_sq(file, rank), color, ok)
        IF (.NOT. ok) RETURN
        file = file + 1
        ok = .TRUE.
    END SUBROUTINE parse_fen_piece_char

    SUBROUTINE fen_char_to_piece(c, piece, color)
        CHARACTER(LEN=1), INTENT(IN) :: c
        INTEGER, INTENT(OUT) :: piece, color
        SELECT CASE(c)
        CASE('P'); piece = PAWN; color = WHITE
        CASE('N'); piece = KNIGHT; color = WHITE
        CASE('B'); piece = BISHOP; color = WHITE
        CASE('R'); piece = ROOK; color = WHITE
        CASE('Q'); piece = QUEEN; color = WHITE
        CASE('K'); piece = KING; color = WHITE
        CASE('p'); piece = PAWN; color = BLACK
        CASE('n'); piece = KNIGHT; color = BLACK
        CASE('b'); piece = BISHOP; color = BLACK
        CASE('r'); piece = ROOK; color = BLACK
        CASE('q'); piece = QUEEN; color = BLACK
        CASE('k'); piece = KING; color = BLACK
        CASE DEFAULT
            piece = NO_PIECE; color = NO_COLOR
        END SELECT
    END SUBROUTINE fen_char_to_piece

    SUBROUTINE add_piece_list_entry(board, sq, color, ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Square_Type), INTENT(IN) :: sq
        INTEGER, INTENT(IN) :: color
        LOGICAL, INTENT(OUT) :: ok
        ok = .FALSE.
        IF (color == WHITE) THEN
            IF (board%num_white_pieces >= SIZE(board%white_pieces)) RETURN
            board%num_white_pieces = board%num_white_pieces + 1
            board%white_pieces(board%num_white_pieces) = sq
            ok = .TRUE.
        ELSE IF (color == BLACK) THEN
            IF (board%num_black_pieces >= SIZE(board%black_pieces)) RETURN
            board%num_black_pieces = board%num_black_pieces + 1
            board%black_pieces(board%num_black_pieces) = sq
            ok = .TRUE.
        END IF
    END SUBROUTINE add_piece_list_entry

    LOGICAL FUNCTION has_exactly_one_king(board, color) RESULT(ok)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER, INTENT(IN) :: color
        INTEGER :: r, f, king_count

        king_count = 0
        DO r = 1, BOARD_SIZE
            DO f = 1, BOARD_SIZE
                IF (board%squares_piece(r, f) == KING .AND. board%squares_color(r, f) == color) THEN
                    king_count = king_count + 1
                END IF
            END DO
        END DO
        ok = (king_count == 1)
    END FUNCTION has_exactly_one_king

    LOGICAL FUNCTION apply_uci_move(board, mv_str) RESULT(ok)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: mv_str
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
        INTEGER :: num_legal, promo_piece
        TYPE(Move_Type) :: chosen_move
        TYPE(UnmakeInfo_Type) :: unmake_info
        INTEGER :: mv_len

        ok = .FALSE.
        mv_len = LEN_TRIM(mv_str)
        IF (mv_len /= 4 .AND. mv_len /= 5) RETURN

        CALL generate_moves(board, legal_moves, num_legal)

        promo_piece = NO_PIECE
        IF (mv_len == 5) THEN
            SELECT CASE(mv_str(5:5))
            CASE('q','Q'); promo_piece = QUEEN
            CASE('r','R'); promo_piece = ROOK
            CASE('b','B'); promo_piece = BISHOP
            CASE('n','N'); promo_piece = KNIGHT
            CASE DEFAULT
                RETURN
            END SELECT
        END IF

        IF (find_move_in_list(mv_str, promo_piece, legal_moves, num_legal, chosen_move)) THEN
            CALL make_move(board, chosen_move, unmake_info)
            ok = .TRUE.
        END IF
    END FUNCTION apply_uci_move

    LOGICAL FUNCTION find_move_in_list(mv_str, promo_piece, move_list, num_moves, chosen) RESULT(found)
        CHARACTER(LEN=*), INTENT(IN) :: mv_str
        INTEGER, INTENT(IN) :: promo_piece, num_moves
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: move_list
        TYPE(Move_Type), INTENT(OUT) :: chosen
        INTEGER :: ff, fr, tf, tr, i

        found = .FALSE.
        ff = char_to_file(mv_str(1:1)); fr = char_to_rank(mv_str(2:2))
        tf = char_to_file(mv_str(3:3)); tr = char_to_rank(mv_str(4:4))
        IF (ff == -1 .OR. fr == -1 .OR. tf == -1 .OR. tr == -1) RETURN

        DO i = 1, num_moves
            IF (move_list(i)%from_sq%file == ff .AND. move_list(i)%from_sq%rank == fr .AND. &
                move_list(i)%to_sq%file == tf   .AND. move_list(i)%to_sq%rank == tr .AND. &
                move_list(i)%promotion_piece == promo_piece) THEN
                chosen = move_list(i)
                found = .TRUE.
                RETURN
            END IF
        END DO
    END FUNCTION find_move_in_list

    SUBROUTINE handle_go(board, line)
        TYPE(Board_Type), INTENT(INOUT) :: board
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER :: depth, movetime, wtime, btime, winc, binc, movestogo
        INTEGER :: chosen_depth, reported_depth, time_budget
        TYPE(Move_Type) :: best_move
        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves, root_moves
        LOGICAL :: best_found
        CHARACTER(LEN=8) :: best_str
        INTEGER :: start_clock, end_clock, rate
        INTEGER :: num_legal_moves, num_root_moves, num_searchmoves
        LOGICAL :: has_depth, has_movetime, has_infinite, use_deadline
        CHARACTER(LEN=8), DIMENSION(MAX_MOVES) :: searchmove_strs

        depth = -1; movetime = -1; wtime = -1; btime = -1; winc = 0; binc = 0; movestogo = -1
        time_budget = 0
        reported_depth = 0
        num_searchmoves = 0
        CALL parse_go_tokens(line, depth, movetime, wtime, btime, winc, binc, movestogo, &
            has_depth, has_movetime, has_infinite, searchmove_strs, num_searchmoves)

        use_deadline = has_movetime .OR. wtime >= 0 .OR. btime >= 0
        IF (has_depth) THEN
            chosen_depth = MAX(depth, 0)
            IF (use_deadline .AND. chosen_depth > 0) THEN
                time_budget = derive_time_ms(board%current_player, movetime, wtime, btime, winc, binc, movestogo)
            END IF
        ELSE IF (has_infinite) THEN
            chosen_depth = 0
        ELSE IF (use_deadline) THEN
            time_budget = derive_time_ms(board%current_player, movetime, wtime, btime, winc, binc, movestogo)
            chosen_depth = 0
        ELSE
            time_budget = derive_time_ms(board%current_player, movetime, wtime, btime, winc, binc, movestogo)
            chosen_depth = depth_from_time(time_budget)
        END IF

        CALL generate_moves(board, legal_moves, num_legal_moves)
        IF (num_searchmoves > 0) THEN
            CALL filter_root_moves(legal_moves, num_legal_moves, searchmove_strs, num_searchmoves, root_moves, num_root_moves)
        ELSE
            num_root_moves = num_legal_moves
            IF (num_root_moves > 0) root_moves(1:num_root_moves) = legal_moves(1:num_root_moves)
        END IF

        CALL SYSTEM_CLOCK(start_clock, rate)
        IF (use_deadline) THEN
            CALL begin_search_polling(time_budget)
        ELSE
            CALL begin_search_polling()
        END IF
        CALL find_best_move(board, chosen_depth, best_found, best_move, completed_depth_out=reported_depth, &
            root_moves_in=root_moves, root_num_moves_in=num_root_moves)
        CALL end_search_polling()
        IF (search_quit_requested()) STOP
        IF (.NOT. best_found) THEN
            IF (num_root_moves > 0) THEN
                best_move = root_moves(1)
                best_found = .TRUE.
            END IF
        END IF
        CALL SYSTEM_CLOCK(end_clock)
        CALL print_uci_info(reported_depth, start_clock, end_clock, rate, best_move, best_found)
        IF (best_found) THEN
            best_str = move_to_uci(best_move)
        ELSE
            best_str = '0000'
        END IF
        WRITE(*,'(A)') 'bestmove ' // TRIM(best_str)
    END SUBROUTINE handle_go

    SUBROUTINE print_uci_info(depth, start_clk, end_clk, rate, best_move, found)
        INTEGER, INTENT(IN) :: depth, start_clk, end_clk, rate
        TYPE(Move_Type), INTENT(IN) :: best_move
        LOGICAL, INTENT(IN) :: found
        REAL :: ms
        CHARACTER(LEN=8) :: mvstr
        CHARACTER(LEN=128) :: line

        ms = 1000.0 * REAL(end_clk - start_clk) / REAL(rate)
        IF (found) THEN
            mvstr = move_to_uci(best_move)
        ELSE
            mvstr = '0000'
        END IF
        WRITE(line,'(A,I0,A,F7.2,A,A)') 'info depth ', depth, ' time ', ms, ' pv ', TRIM(mvstr)
        WRITE(*,'(A)') TRIM(line)
    END SUBROUTINE print_uci_info

    SUBROUTINE parse_go_tokens(line, depth, movetime, wtime, btime, winc, binc, movestogo, &
                               has_depth, has_movetime, has_infinite, searchmoves, num_searchmoves)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER, INTENT(OUT) :: depth, movetime, wtime, btime, winc, binc, movestogo
        LOGICAL, INTENT(OUT) :: has_depth, has_movetime, has_infinite
        CHARACTER(LEN=8), DIMENSION(MAX_MOVES), INTENT(OUT) :: searchmoves
        INTEGER, INTENT(OUT) :: num_searchmoves
        INTEGER :: val, ios, pos
        CHARACTER(LEN=32) :: tok, tokval, pending_tok
        LOGICAL :: has, has_pending_tok

        depth = -1; movetime = -1; wtime = -1; btime = -1; winc = 0; binc = 0; movestogo = -1
        has_depth = .FALSE.
        has_movetime = .FALSE.
        has_infinite = .FALSE.
        searchmoves = ''
        num_searchmoves = 0
        pos = 1
        has_pending_tok = .FALSE.
        DO
            IF (has_pending_tok) THEN
                tok = pending_tok
                has = .TRUE.
                has_pending_tok = .FALSE.
            ELSE
                CALL next_token(line, pos, tok, has)
            END IF
            IF (.NOT. has) EXIT
            SELECT CASE (tok)
            CASE('infinite')
                has_infinite = .TRUE.
            CASE('depth')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) THEN
                        depth = val
                        has_depth = .TRUE.
                    END IF
                END IF
            CASE('movetime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) THEN
                        movetime = val
                        has_movetime = .TRUE.
                    END IF
                END IF
            CASE('wtime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) wtime = val
                END IF
            CASE('btime')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) btime = val
                END IF
            CASE('winc')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) winc = val
                END IF
            CASE('binc')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) binc = val
                END IF
            CASE('movestogo')
                CALL next_token(line, pos, tokval, has)
                IF (has) THEN
                    READ(tokval,*,IOSTAT=ios) val
                    IF (ios == 0) movestogo = val
                END IF
            CASE('searchmoves')
                DO
                    CALL next_token(line, pos, tokval, has)
                    IF (.NOT. has) EXIT
                    IF (is_go_keyword(tokval)) THEN
                        pending_tok = tokval
                        has_pending_tok = .TRUE.
                        EXIT
                    END IF
                    IF (num_searchmoves < MAX_MOVES) THEN
                        num_searchmoves = num_searchmoves + 1
                        searchmoves(num_searchmoves) = TRIM(tokval)
                    END IF
                END DO
            END SELECT
        END DO
    END SUBROUTINE parse_go_tokens

    SUBROUTINE next_token(line, pos, token, has)
        CHARACTER(LEN=*), INTENT(IN) :: line
        INTEGER, INTENT(INOUT) :: pos
        CHARACTER(LEN=*), INTENT(OUT) :: token
        LOGICAL, INTENT(OUT) :: has
        INTEGER :: l, start, finish

        l = LEN(line)
        DO WHILE (pos <= l)
            IF (line(pos:pos) /= ' ') EXIT
            pos = pos + 1
        END DO
        IF (pos > l) THEN
            has = .FALSE.
            token = ''
            RETURN
        END IF
        start = pos
        finish = start
        DO WHILE (finish <= l)
            IF (line(finish:finish) == ' ') EXIT
            finish = finish + 1
        END DO
        token = line(start:finish-1)
        pos = finish + 1
        has = .TRUE.
    END SUBROUTINE next_token

    INTEGER FUNCTION find_token(line, keyword, start_pos) RESULT(token_pos)
        CHARACTER(LEN=*), INTENT(IN) :: line, keyword
        INTEGER, INTENT(IN) :: start_pos
        INTEGER :: i, j, l, keyword_len

        token_pos = 0
        l = LEN_TRIM(line)
        keyword_len = LEN_TRIM(keyword)
        i = MAX(start_pos, 1)

        DO WHILE (i <= l)
            DO WHILE (i <= l .AND. line(i:i) == ' ')
                i = i + 1
            END DO
            IF (i > l) EXIT

            j = i
            DO WHILE (j <= l .AND. line(j:j) /= ' ')
                j = j + 1
            END DO

            IF (j - i == keyword_len) THEN
                IF (line(i:j-1) == keyword(1:keyword_len)) THEN
                    token_pos = i
                    RETURN
                END IF
            END IF
            i = j + 1
        END DO
    END FUNCTION find_token

    INTEGER FUNCTION derive_time_ms(side_to_move, movetime, wtime, btime, winc, binc, movestogo) RESULT(ms)
        INTEGER, INTENT(IN) :: side_to_move, movetime, wtime, btime, winc, binc, movestogo
        INTEGER :: pool, inc, moves_left

        IF (movetime >= 0) THEN
            ms = movetime
            RETURN
        END IF

        IF (side_to_move == WHITE) THEN
            pool = wtime
            inc = winc
        ELSE
            pool = btime
            inc = binc
        END IF

        IF (movestogo > 0) THEN
            moves_left = movestogo
        ELSE
            moves_left = 30
        END IF

        IF (pool <= 0) THEN
            ms = 2000 ! fallback
        ELSE
            ms = pool / moves_left + inc * 4 / 5
            IF (ms < 200) ms = 200
        END IF
    END FUNCTION derive_time_ms

    LOGICAL FUNCTION is_go_keyword(token) RESULT(is_keyword)
        CHARACTER(LEN=*), INTENT(IN) :: token

        SELECT CASE (TRIM(token))
        CASE ('searchmoves', 'ponder', 'wtime', 'btime', 'winc', 'binc', 'movestogo', &
              'depth', 'nodes', 'mate', 'movetime', 'infinite')
            is_keyword = .TRUE.
        CASE DEFAULT
            is_keyword = .FALSE.
        END SELECT
    END FUNCTION is_go_keyword

    SUBROUTINE filter_root_moves(legal_moves, num_legal_moves, searchmove_strs, num_searchmoves, root_moves, num_root_moves)
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves, num_searchmoves
        CHARACTER(LEN=8), DIMENSION(MAX_MOVES), INTENT(IN) :: searchmove_strs
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(OUT) :: root_moves
        INTEGER, INTENT(OUT) :: num_root_moves
        INTEGER :: i, j
        CHARACTER(LEN=8) :: move_str

        num_root_moves = 0
        DO i = 1, num_legal_moves
            move_str = move_to_uci(legal_moves(i))
            DO j = 1, num_searchmoves
                IF (TRIM(move_str) == TRIM(searchmove_strs(j))) THEN
                    num_root_moves = num_root_moves + 1
                    root_moves(num_root_moves) = legal_moves(i)
                    EXIT
                END IF
            END DO
        END DO
    END SUBROUTINE filter_root_moves

    INTEGER FUNCTION depth_from_time(ms) RESULT(depth)
        INTEGER, INTENT(IN) :: ms
        IF (ms <= 300) THEN
            depth = 2
        ELSE IF (ms <= 700) THEN
            depth = 3
        ELSE IF (ms <= 1500) THEN
            depth = 4
        ELSE IF (ms <= 3500) THEN
            depth = 5
        ELSE IF (ms <= 7000) THEN
            depth = 6
        ELSE
            depth = 7
        END IF
    END FUNCTION depth_from_time

    CHARACTER(LEN=8) FUNCTION move_to_uci(mv) RESULT(str)
        TYPE(Move_Type), INTENT(IN) :: mv
        CHARACTER(LEN=1) :: promo_char
        str = ''
        str(1:1) = CHAR(ICHAR('a') + mv%from_sq%file - 1)
        str(2:2) = CHAR(ICHAR('0') + mv%from_sq%rank)
        str(3:3) = CHAR(ICHAR('a') + mv%to_sq%file - 1)
        str(4:4) = CHAR(ICHAR('0') + mv%to_sq%rank)
        IF (mv%promotion_piece /= NO_PIECE) THEN
            SELECT CASE(mv%promotion_piece)
            CASE(QUEEN); promo_char = 'q'
            CASE(ROOK);  promo_char = 'r'
            CASE(BISHOP); promo_char = 'b'
            CASE(KNIGHT); promo_char = 'n'
            CASE DEFAULT; promo_char = 'q'
            END SELECT
            str(5:5) = promo_char
        END IF
    END FUNCTION move_to_uci

END MODULE UCI_Driver
