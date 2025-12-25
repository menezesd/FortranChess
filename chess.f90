! ============================================
! Main Program: Human vs Computer
! ============================================
PROGRAM Fortran_Chess
    USE Chess_Types
    USE Board_Utils
    USE Move_Generation ! Needs Make_Unmake implicitly
    USE Make_Unmake
    USE Search
    USE Transposition_Table, ONLY: init_zobrist_keys
    USE User_Input_Processor
    USE Game_State_Checker
    USE UCI_Driver, ONLY: run_uci_mode
    IMPLICIT NONE

    TYPE(Board_Type) :: game_board
    TYPE(Move_Type) :: chosen_move
    TYPE(UnmakeInfo_Type) :: move_info ! Needed for make_move call
    LOGICAL :: move_found, is_human_turn, game_over
    INTEGER :: human_player_color, ai_player_color
    INTEGER :: search_depth
    CHARACTER(LEN=10) :: user_input ! Keep for color selection
    TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
    INTEGER :: num_legal_moves
    INTEGER :: game_winner_color, current_game_status
    INTEGER :: argc
    CHARACTER(LEN=16) :: arg1
    LOGICAL :: uci_mode

    uci_mode = .FALSE.
    argc = COMMAND_ARGUMENT_COUNT()
    IF (argc >= 1) THEN
        CALL GET_COMMAND_ARGUMENT(1, arg1)
        IF (TRIM(arg1) == '--uci' .OR. TRIM(arg1) == '-uci') THEN
            uci_mode = .TRUE.
        END IF
    END IF

    IF (uci_mode) THEN
        CALL run_uci_mode()
        STOP
    END IF

    search_depth = 5 ! AI Difficulty
    ! --- Player Color Selection ---
    DO
        PRINT *, "Choose your color (White/Black): "
        READ *, user_input
        SELECT CASE (TRIM(ADJUSTL(user_input))) ! Basic input handling
        CASE ('White', 'white', 'W', 'w')
            human_player_color = WHITE
            ai_player_color = BLACK
            PRINT *, "You play as White."
            EXIT
        CASE ('Black', 'black', 'B', 'b')
            human_player_color = BLACK
            ai_player_color = WHITE
            PRINT *, "You play as Black."
            EXIT
        CASE DEFAULT
            PRINT *, "Invalid input. Please enter 'White' or 'Black'."
        END SELECT
    END DO

    ! --- Search Depth Selection ---
    DO
        PRINT *, "Enter AI search depth (e.g., 5): "
        READ *, search_depth
        IF (search_depth >= 1 .AND. search_depth <= 10) THEN ! Example valid range
            EXIT
        ELSE
            PRINT *, "Invalid search depth. Please enter a number between 1 and 10."
        END IF
    END DO

    ! --- Initialize Zobrist Keys and Board ---
    CALL init_zobrist_keys()
    CALL init_board(game_board)
    CALL print_board(game_board)

    ! --- Game Loop ---
    game_over = .FALSE.
    DO WHILE (.NOT. game_over)

        ! 1. Check Game Over
        game_over = is_game_over(game_board, game_winner_color, current_game_status)
        IF (game_over) THEN
            SELECT CASE (current_game_status)
                CASE (GAME_CHECKMATE)
                    IF (game_winner_color == WHITE) THEN
                        PRINT *, "=== CHECKMATE! White wins! ==="
                    ELSE
                        PRINT *, "=== CHECKMATE! Black wins! ==="
                    END IF
                CASE (GAME_STALEMATE)
                    PRINT *, "=== STALEMATE! Draw. ==="
                CASE DEFAULT
                    ! Should not happen
                    PRINT *, "Error: Unknown game over status."
            END SELECT
            EXIT ! Exit game loop
        END IF

        ! 2. Determine Turn
        is_human_turn = (game_board%current_player == human_player_color)

        IF (is_human_turn) THEN
            ! --- Human's Turn ---
            ! Generate the current legal moves before prompting the player
            CALL generate_moves(game_board, legal_moves, num_legal_moves)
            IF (num_legal_moves == 0) THEN
                PRINT *, "No legal moves available."
                game_over = .TRUE.
                CYCLE
            END IF

            move_found = get_human_move(legal_moves, num_legal_moves, chosen_move, game_over)

            IF (game_over) EXIT ! Exit game loop if user quit

            IF (move_found) THEN
                 ! Make human move
                 PRINT *, "You moved." ! Add more detail later
                 CALL make_move(game_board, chosen_move, move_info)
            END IF

        ELSE
            ! --- AI's Turn ---
                        PRINT *, "" ! Newline
            PRINT *, "Computer's turn. Thinking..."
            CALL find_best_move(game_board, search_depth, move_found, chosen_move)
            IF (move_found) THEN
                PRINT *, "Computer moved." ! Add move details later
                CALL make_move(game_board, chosen_move, move_info)
            ELSE
                ! Should be caught by game over check, but safety print
                PRINT *, "Error: AI found no move but game not over?"
                game_over = .TRUE.
            END IF
        END IF

        ! Print board after move (if game not over)
         IF (.NOT. game_over) THEN
             CALL print_board(game_board)
         END IF

    END DO ! End game loop

    PRINT *, "Game finished."

END PROGRAM Fortran_Chess
