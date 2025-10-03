! ============================================
! Module: Evaluation
! Purpose: Static board evaluation using material and piece-square tables
! ============================================
MODULE Evaluation
    USE Chess_Types
    USE Board_Utils
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: evaluate_board

    ! Piece-Square Tables (PSTs) for positional evaluation
    ! These tables assign bonus/penalty values to pieces based on their position
    ! Higher values encourage centralization, development, and king safety


INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PST = RESHAPE( &
    [ 0, 50, 10, 5, 0, 5, 5, 0, &  ! Column 1
      0, 50, 10, 5, 0, -5, 10, 0, &  ! Column 2
      0, 50, 20, 10, 0, -10, 10, 0, &  ! Column 3
      0, 50, 30, 25, 20, 0, -20, 0, &  ! Column 4
      0, 50, 30, 25, 20, -10, -20, 0, &  ! Column 5
      0, 50, 20, 10, 0, -10, 10, 0, &  ! Column 6
      0, 50, 10, 5, 0, 5, 10, 0, &  ! Column 7
      0, 50, 0, 0, 0, 0, 0, 0 ], &  ! Column 8
    SHAPE(PAWN_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PST = RESHAPE( &
    [ -50, -40, -30, -30, -30, -30, -40, -50, &  ! Column 1
      -40, -20, 0, 0, 0, 0, -20, -40, &  ! Column 2
      -30, 0, 10, 15, 15, 10, 0, -30, &  ! Column 3
      -30, 5, 15, 20, 20, 15, 5, -30, &  ! Column 4
      -30, 0, 15, 20, 20, 15, 0, -30, &  ! Column 5
      -30, 5, 10, 15, 15, 10, 5, -30, &  ! Column 6
      -40, -20, 0, 5, 5, 0, -20, -40, &  ! Column 7
      -50, -40, -30, -30, -30, -30, -40, -50 ], &  ! Column 8
    SHAPE(KNIGHT_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PST = RESHAPE( &
    [ -20, -10, -10, -10, -10, -10, -10, -20, &  ! Column 1
      -10, 0, 0, 0, 0, 0, 0, -10, &  ! Column 2
      -10, 0, 10, 10, 10, 10, 0, -10, &  ! Column 3
      -10, 5, 5, 10, 10, 5, 5, -10, &  ! Column 4
      -10, 0, 5, 10, 10, 5, 0, -10, &  ! Column 5
      -10, 5, 5, 5, 5, 5, 5, -10, &  ! Column 6
      -10, 0, 5, 0, 0, 5, 0, -10, &  ! Column 7
      -20, -10, -10, -10, -10, -10, -10, -20 ], &  ! Column 8
    SHAPE(BISHOP_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PST = RESHAPE( &
    [ 0, 0, 0, 0, 0, 0, 0, 0, &  ! Column 1
      5, 10, 10, 10, 10, 10, 10, 5, &  ! Column 2
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 3
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 4
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 5
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 6
      -5, 0, 0, 0, 0, 0, 0, -5, &  ! Column 7
      0, 0, 0, 5, 5, 0, 0, 0 ], &  ! Column 8
    SHAPE(ROOK_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PST = RESHAPE( &
    [ -20, -10, -10, -5, -5, -10, -10, -20, &  ! Column 1
      -10, 0, 0, 0, 0, 0, 0, -10, &  ! Column 2
      -10, 0, 5, 5, 5, 5, 0, -10, &  ! Column 3
      -5, 0, 5, 5, 5, 5, 0, -5, &  ! Column 4
      0, 0, 5, 5, 5, 5, 0, -5, &  ! Column 5
      -10, 5, 5, 5, 5, 5, 0, -10, &  ! Column 6
      -10, 0, 5, 0, 0, 0, 0, -10, &  ! Column 7
      -20, -10, -10, -5, -5, -10, -10, -20 ], &  ! Column 8
    SHAPE(QUEEN_PST))

INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PST = RESHAPE( &
    [ -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 1
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 2
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 3
      -30, -40, -40, -50, -50, -40, -40, -30, &  ! Column 4
      -20, -30, -30, -40, -40, -30, -30, -20, &  ! Column 5
      -10, -20, -20, -20, -20, -20, -20, -10, &  ! Column 6
      20, 20, 0, 0, 0, 0, 20, 20, &  ! Column 7
      20, 30, 10, 0, 0, 10, 30, 20 ], &  ! Column 8
    SHAPE(KING_PST))

     INTEGER, PARAMETER :: PAWN_VAL = 100, KNIGHT_VAL = 320, BISHOP_VAL = 330, &
                           ROOK_VAL = 500, QUEEN_VAL = 900, KING_VAL = 20000

CONTAINS
    ! --- Evaluate Board ---
    ! Performs static evaluation of the current board position.
    !
    ! Evaluation combines material balance with positional factors:
    ! - Material: Piece values (pawn=100, knight=320, etc.)
    ! - Position: Piece-square table bonuses/penalties
    !
    ! The evaluation is calculated as: white_score - black_score
    ! Positive scores favor white, negative favor black.
    !
    ! Parameters:
    !   board (IN): Current board state to evaluate
    !
    ! Returns:
    !   Evaluation score in centipawns (positive = white advantage)
    !
    ! Notes:
    !   - Uses piece lists for efficient iteration
    !   - PSTs are flipped for black pieces (rank 8 becomes rank 1)
    !   - King value is high to ensure mate detection
    INTEGER FUNCTION evaluate_board(board)
        TYPE(Board_Type), INTENT(IN) :: board
        INTEGER :: i, r, f, piece, eval_rank, piece_value, pst_value
        TYPE(Square_Type) :: sq

        ! Initialize score
        evaluate_board = 0

        ! Evaluate white pieces using piece list
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = r  ! White: rank 1-8 as-is

            ! Get material value and positional bonus
            SELECT CASE(piece)
            CASE(PAWN)   ; piece_value = PAWN_VAL;   pst_value = PAWN_PST(eval_rank, f)
            CASE(KNIGHT) ; piece_value = KNIGHT_VAL; pst_value = KNIGHT_PST(eval_rank, f)
            CASE(BISHOP) ; piece_value = BISHOP_VAL; pst_value = BISHOP_PST(eval_rank, f)
            CASE(ROOK)   ; piece_value = ROOK_VAL;   pst_value = ROOK_PST(eval_rank, f)
            CASE(QUEEN)  ; piece_value = QUEEN_VAL;  pst_value = QUEEN_PST(eval_rank, f)
            CASE(KING)   ; piece_value = KING_VAL;   pst_value = KING_PST(eval_rank, f)
            CASE DEFAULT ; piece_value = 0; pst_value = 0
            END SELECT

            evaluate_board = evaluate_board + piece_value + pst_value
        END DO

        ! Evaluate black pieces using piece list
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            r = sq%rank
            f = sq%file
            piece = board%squares_piece(r, f)
            eval_rank = BOARD_SIZE - r + 1  ! Black: flip ranks (8->1, 1->8)

            ! Get material value and positional bonus
            SELECT CASE(piece)
            CASE(PAWN)   ; piece_value = PAWN_VAL;   pst_value = PAWN_PST(eval_rank, f)
            CASE(KNIGHT) ; piece_value = KNIGHT_VAL; pst_value = KNIGHT_PST(eval_rank, f)
            CASE(BISHOP) ; piece_value = BISHOP_VAL; pst_value = BISHOP_PST(eval_rank, f)
            CASE(ROOK)   ; piece_value = ROOK_VAL;   pst_value = ROOK_PST(eval_rank, f)
            CASE(QUEEN)  ; piece_value = QUEEN_VAL;  pst_value = QUEEN_PST(eval_rank, f)
            CASE(KING)   ; piece_value = KING_VAL;   pst_value = KING_PST(eval_rank, f)
            CASE DEFAULT ; piece_value = 0; pst_value = 0
            END SELECT

            evaluate_board = evaluate_board - (piece_value + pst_value)
        END DO

        ! Adjust score relative to current player
        ! (positive = current player advantage, negative = opponent advantage)
        IF (board%current_player == BLACK) THEN
            evaluate_board = -evaluate_board
        END IF

    END FUNCTION evaluate_board

END MODULE Evaluation
