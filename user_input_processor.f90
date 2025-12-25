MODULE User_Input_Processor
    USE Chess_Types
    USE Board_Utils, ONLY: char_to_file, char_to_rank
    USE Move_Generation ! Corrected: legal_moves is passed as argument
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: get_human_move

CONTAINS

    ! --- Get Human Move ---
    ! Prompts the human player for a move, parses it, validates it against legal moves,
    ! and returns the chosen move. Handles input validation and "quit" command.
    !
    ! Parameters:
    !   game_board (IN): Current board state
    !   legal_moves (IN): Array of currently legal moves
    !   num_legal_moves (IN): Number of legal moves in the array
    !   chosen_move (OUT): The validated move chosen by the player
    !   game_over_flag (INOUT): Set to .TRUE. if the player quits
    !
    ! Returns:
    !   LOGICAL: .TRUE. if a valid move was found, .FALSE. otherwise (e.g., quit, invalid input)
    LOGICAL FUNCTION get_human_move(legal_moves, num_legal_moves, chosen_move, game_over_flag)
        TYPE(Move_Type), DIMENSION(MAX_MOVES), INTENT(IN) :: legal_moves
        INTEGER, INTENT(IN) :: num_legal_moves
        TYPE(Move_Type), INTENT(OUT) :: chosen_move
        LOGICAL, INTENT(INOUT) :: game_over_flag

        CHARACTER(LEN=10) :: user_input
        CHARACTER(LEN=1) :: from_f_char, from_r_char, to_f_char, to_r_char, promo_char
        TYPE(Square_Type) :: parsed_from_sq, parsed_to_sq
        INTEGER :: parsed_promo_piece
        LOGICAL :: move_found_internal
        INTEGER :: i ! Declared loop variable

        get_human_move = .FALSE. ! Default to no valid move found
        move_found_internal = .FALSE.
        game_over_flag = .FALSE. ! Default to not game over

        PRINT *, " " ! Newline
        PRINT *, "Your turn. Enter move (e.g., e2e4, e7e8q): "

        DO WHILE (.NOT. move_found_internal .AND. .NOT. game_over_flag)
            READ *, user_input
            IF (TRIM(ADJUSTL(user_input)) == 'quit' .OR. TRIM(ADJUSTL(user_input)) == 'exit') THEN
                PRINT *, "Exiting game."
                game_over_flag = .TRUE.
                RETURN
            END IF

            ! Basic Parsing
            IF (LEN_TRIM(user_input) >= 4) THEN
                from_f_char = user_input(1:1); from_r_char = user_input(2:2)
                to_f_char = user_input(3:3);   to_r_char = user_input(4:4)
                
                parsed_from_sq%file = char_to_file(from_f_char)
                parsed_from_sq%rank = char_to_rank(from_r_char)
                parsed_to_sq%file = char_to_file(to_f_char)
                parsed_to_sq%rank = char_to_rank(to_r_char)

                ! Validate parsed squares
                IF (parsed_from_sq%file == -1 .OR. parsed_from_sq%rank == -1 .OR. &
                    parsed_to_sq%file == -1   .OR. parsed_to_sq%rank == -1) THEN
                    PRINT *, "Invalid square notation. Please use e.g., 'e2e4'."
                    CYCLE
                END IF

                parsed_promo_piece = NO_PIECE
                IF (LEN_TRIM(user_input) == 5) THEN
                     promo_char = user_input(5:5)
                     SELECT CASE(promo_char)
                     CASE('q'); parsed_promo_piece = QUEEN
                     CASE('r'); parsed_promo_piece = ROOK
                     CASE('b'); parsed_promo_piece = BISHOP
                     CASE('n'); parsed_promo_piece = KNIGHT
                     CASE DEFAULT
                        PRINT *, "Invalid promotion piece. Use 'q', 'r', 'b', or 'n'."
                        CYCLE
                     END SELECT
                END IF

                ! Find the move in the legal list
                DO i = 1, num_legal_moves
                     IF (legal_moves(i)%from_sq%rank == parsed_from_sq%rank .AND. &
                         legal_moves(i)%from_sq%file == parsed_from_sq%file .AND. &
                         legal_moves(i)%to_sq%rank == parsed_to_sq%rank .AND. &
                         legal_moves(i)%to_sq%file == parsed_to_sq%file .AND. &
                         legal_moves(i)%promotion_piece == parsed_promo_piece) THEN
                         chosen_move = legal_moves(i)
                         move_found_internal = .TRUE.
                         EXIT ! Exit move finding loop
                     END IF
                END DO
            END IF

            IF (.NOT. move_found_internal) THEN
                 PRINT *, "Invalid or illegal move. Try again."
            END IF
        END DO ! End move input loop

        IF (move_found_internal) THEN
            get_human_move = .TRUE.
        END IF

    END FUNCTION get_human_move

END MODULE User_Input_Processor
