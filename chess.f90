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
    USE User_Input_Processor ! NEW: For human move input processing
    IMPLICIT NONE

    TYPE(Board_Type) :: game_board
    TYPE(Move_Type) :: chosen_move
    TYPE(UnmakeInfo_Type) :: move_info ! Needed for make_move call
    LOGICAL :: move_found, is_human_turn, game_over
    INTEGER :: human_player_color, ai_player_color, winner
    INTEGER :: search_depth
    CHARACTER(LEN=10) :: user_input ! Keep for color selection
    TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves
    INTEGER :: num_legal_moves

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

    ! --- Initialize Zobrist Keys and Board ---
    CALL init_zobrist_keys()
    CALL init_board(game_board)
    CALL print_board(game_board)

    ! --- Game Loop ---
    game_over = .FALSE.
    DO WHILE (.NOT. game_over)

        ! 1. Check Game Over
        ! Need a way to check checkmate/stalemate without modifying board state here,
        ! or accept that generate_moves modifies it temporarily.
        CALL generate_moves(game_board, legal_moves, num_legal_moves)
        IF (num_legal_moves == 0) THEN
            IF (is_in_check(game_board, game_board%current_player)) THEN
                winner = get_opponent_color(game_board%current_player)
                IF (winner == WHITE) THEN
                    PRINT *, "=== CHECKMATE! White wins! ==="
                ELSE
                    PRINT *, "=== CHECKMATE! Black wins! ==="
                END IF
            ELSE
                 PRINT *, "=== STALEMATE! Draw. ==="
            END IF
            game_over = .TRUE.
            EXIT ! Exit game loop
        END IF

        ! 2. Determine Turn
        is_human_turn = (game_board%current_player == human_player_color)

        IF (is_human_turn) THEN
            ! --- Human's Turn ---
            move_found = get_human_move(legal_moves, num_legal_moves, chosen_move, game_over)

            IF (game_over) EXIT ! Exit game loop if user quit

            IF (move_found) THEN
                 ! Make human move
                 PRINT *, "You moved." ! Add more detail later
                 CALL make_move(game_board, chosen_move, move_info)
            END IF

        ELSE
            ! --- AI's Turn ---
            PRINT *, " " ! Newline
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
