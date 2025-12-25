MODULE Game_State_Checker
    USE Chess_Types
    USE Board_Utils, ONLY: is_in_check, get_opponent_color
    USE Move_Generation, ONLY: generate_moves
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: is_game_over

CONTAINS

    ! --- Check if game is over (checkmate, stalemate) ---
    ! Determines if the current board state represents a game over condition.
    !
    ! Parameters:
    !   board (INOUT): Current board state
    !   winner_color (OUT): Color of the winning player (WHITE, BLACK) or NO_COLOR for draw/ongoing
    !   game_status (OUT): Status of the game (GAME_CHECKMATE, GAME_STALEMATE, GAME_ONGOING)
    !
    ! Returns:
    !   .TRUE. if the game is over, .FALSE. otherwise.
    LOGICAL FUNCTION is_game_over(board, winner_color, game_status) RESULT(is_over)
        TYPE(Board_Type), INTENT(INOUT) :: board
        INTEGER, INTENT(OUT) :: winner_color
        INTEGER, INTENT(OUT) :: game_status

        TYPE(Move_Type), DIMENSION(MAX_MOVES) :: legal_moves_temp
        INTEGER :: num_legal_moves_temp

        ! Default values
        is_over = .FALSE.
        winner_color = NO_COLOR
        game_status = GAME_ONGOING

        ! Generate legal moves for the current player
        CALL generate_moves(board, legal_moves_temp, num_legal_moves_temp)

        IF (num_legal_moves_temp == 0) THEN
            ! No legal moves, so it's either checkmate or stalemate
            IF (is_in_check(board, board%current_player)) THEN
                game_status = GAME_CHECKMATE
                winner_color = get_opponent_color(board%current_player)
            ELSE
                game_status = GAME_STALEMATE
                winner_color = NO_COLOR ! Draw
            END IF
            is_over = .TRUE.
        END IF

    END FUNCTION is_game_over

END MODULE Game_State_Checker
