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

    ! --- Game Phase Constants ---
    INTEGER, PARAMETER :: KNIGHT_PHASE = 1
    INTEGER, PARAMETER :: BISHOP_PHASE = 1
    INTEGER, PARAMETER :: ROOK_PHASE = 2
    INTEGER, PARAMETER :: QUEEN_PHASE = 4
    INTEGER, PARAMETER :: TOTAL_PHASE = (KNIGHT_PHASE * 2 + BISHOP_PHASE * 2 + ROOK_PHASE * 2 + QUEEN_PHASE) * 2 ! = 24

    ! --- Middlegame Piece-Square Tables (PSTs) ---
    ! These tables assign bonus/penalty values to pieces based on their position
    ! Higher values encourage centralization, development, and king safety
    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PST_MG = RESHAPE( &
        [ 0, 0, 0, 0, 0, 0, 0, 0, &
          50, 50, 50, 50, 50, 50, 50, 50, &
          10, 10, 20, 30, 30, 20, 10, 10, &
          5, 5, 10, 25, 25, 10, 5, 5, &
          0, 0, 0, 20, 20, 0, 0, 0, &
          5, -5, -10, 0, 0, -10, -5, 5, &
          5, 10, 10, -20, -20, 10, 10, 5, &
          0, 0, 0, 0, 0, 0, 0, 0 ], &
        SHAPE(PAWN_PST_MG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PST_MG = RESHAPE( &
        [ -50, -40, -30, -30, -30, -30, -40, -50, &
          -40, -20, 0, 0, 0, 0, -20, -40, &
          -30, 0, 10, 15, 15, 10, 0, -30, &
          -30, 5, 15, 20, 20, 15, 5, -30, &
          -30, 0, 15, 20, 20, 15, 0, -30, &
          -30, 5, 10, 15, 15, 10, 5, -30, &
          -40, -20, 0, 5, 5, 0, -20, -40, &
          -50, -40, -30, -30, -30, -30, -40, -50 ], &
        SHAPE(KNIGHT_PST_MG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PST_MG = RESHAPE( &
        [ -20, -10, -10, -10, -10, -10, -10, -20, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, 0, 5, 10, 10, 5, 0, -10, &
          -10, 5, 5, 10, 10, 5, 5, -10, &
          -10, 0, 10, 10, 10, 10, 0, -10, &
          -10, 10, 10, 10, 10, 10, 10, -10, &
          -10, 5, 0, 0, 0, 0, 5, -10, &
          -20, -10, -10, -10, -10, -10, -10, -20 ], &
        SHAPE(BISHOP_PST_MG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PST_MG = RESHAPE( &
        [ 0, 0, 0, 0, 0, 0, 0, 0, &
          5, 10, 10, 10, 10, 10, 10, 5, &
          -5, 0, 0, 0, 0, 0, 0, -5, &
          -5, 0, 0, 0, 0, 0, 0, -5, &
          -5, 0, 0, 0, 0, 0, 0, -5, &
          -5, 0, 0, 0, 0, 0, 0, -5, &
          -5, 0, 0, 0, 0, 0, 0, -5, &
          0, 0, 0, 5, 5, 0, 0, 0 ], &
        SHAPE(ROOK_PST_MG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PST_MG = RESHAPE( &
        [ -20, -10, -10, -5, -5, -10, -10, -20, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, 0, 5, 5, 5, 5, 0, -10, &
          -5, 0, 5, 5, 5, 5, 0, -5, &
          0, 0, 5, 5, 5, 5, 0, -5, &
          -10, 5, 5, 5, 5, 5, 0, -10, &
          -10, 0, 5, 0, 0, 0, 0, -10, &
          -20, -10, -10, -5, -5, -10, -10, -20 ], &
        SHAPE(QUEEN_PST_MG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PST_MG = RESHAPE( &
        [ -30, -40, -40, -50, -50, -40, -40, -30, &
          -30, -40, -40, -50, -50, -40, -40, -30, &
          -30, -40, -40, -50, -50, -40, -40, -30, &
          -30, -40, -40, -50, -50, -40, -40, -30, &
          -20, -30, -30, -40, -40, -30, -30, -20, &
          -10, -20, -20, -20, -20, -20, -20, -10, &
          20, 20, 0, 0, 0, 0, 20, 20, &
          20, 30, 10, 0, 0, 10, 30, 20 ], &
        SHAPE(KING_PST_MG), ORDER=[2,1])

    ! --- Endgame Piece-Square Tables (PSTs) ---
    INTEGER, PARAMETER, DIMENSION(8, 8) :: PAWN_PST_EG = RESHAPE( &
        [ 0, 80, 80, 80, 80, 80, 80, 0, &
          0, 60, 60, 60, 60, 60, 60, 0, &
          0, 40, 40, 40, 40, 40, 40, 0, &
          0, 20, 20, 20, 20, 20, 20, 0, &
          0, 10, 10, 10, 10, 10, 10, 0, &
          0, 5, 5, 5, 5, 5, 5, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0 ], &
        SHAPE(PAWN_PST_EG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KNIGHT_PST_EG = RESHAPE( &
        [ -50, -30, -20, -20, -20, -20, -30, -50, &
          -30, -10, 0, 0, 0, 0, -10, -30, &
          -20, 0, 10, 10, 10, 10, 0, -20, &
          -20, 0, 10, 15, 15, 10, 0, -20, &
          -20, 0, 10, 15, 15, 10, 0, -20, &
          -20, 0, 10, 10, 10, 10, 0, -20, &
          -30, -10, 0, 0, 0, 0, -10, -30, &
          -50, -30, -20, -20, -20, -20, -30, -50 ], &
        SHAPE(KNIGHT_PST_EG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: BISHOP_PST_EG = RESHAPE( &
        [ -10, -10, -10, -10, -10, -10, -10, -10, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, 0, 5, 5, 5, 5, 0, -10, &
          -10, 0, 5, 10, 10, 5, 0, -10, &
          -10, 0, 5, 10, 10, 5, 0, -10, &
          -10, 0, 5, 5, 5, 5, 0, -10, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, -10, -10, -10, -10, -10, -10, -10 ], &
        SHAPE(BISHOP_PST_EG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: ROOK_PST_EG = RESHAPE( &
        [ 0, 0, 0, 0, 0, 0, 0, 0, &
          5, 5, 5, 5, 5, 5, 5, 5, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0, &
          0, 0, 0, 0, 0, 0, 0, 0 ], &
        SHAPE(ROOK_PST_EG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: QUEEN_PST_EG = RESHAPE( &
        [ -10, -10, -10, -5, -5, -10, -10, -10, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, 0, 5, 5, 5, 5, 0, -10, &
          -5, 0, 5, 5, 5, 5, 0, -5, &
          -5, 0, 5, 5, 5, 5, 0, -5, &
          -10, 0, 5, 5, 5, 5, 0, -10, &
          -10, 0, 0, 0, 0, 0, 0, -10, &
          -10, -10, -10, -5, -5, -10, -10, -10 ], &
        SHAPE(QUEEN_PST_EG), ORDER=[2,1])

    INTEGER, PARAMETER, DIMENSION(8, 8) :: KING_PST_EG = RESHAPE( &
        [ -50, -30, -10, 0, 0, -10, -30, -50, &
          -30, -10, 20, 30, 30, 20, -10, -30, &
          -10, 20, 40, 50, 50, 40, 20, -10, &
          0, 30, 50, 60, 60, 50, 30, 0, &
          0, 30, 50, 60, 60, 50, 30, 0, &
          -10, 20, 40, 50, 50, 40, 20, -10, &
          -30, -10, 20, 30, 30, 20, -10, -30, &
          -50, -30, -10, 0, 0, -10, -30, -50 ], &
        SHAPE(KING_PST_EG), ORDER=[2,1])


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
        INTEGER :: mg_pst_val, eg_pst_val, phase
        TYPE(Square_Type) :: sq

        ! --- Calculate Game Phase ---
        ! Iterate over all pieces to determine the game phase.
        phase = 0
        DO i = 1, board%num_white_pieces
            sq = board%white_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            SELECT CASE(piece)
            CASE(KNIGHT); phase = phase + KNIGHT_PHASE
            CASE(BISHOP); phase = phase + BISHOP_PHASE
            CASE(ROOK);   phase = phase + ROOK_PHASE
            CASE(QUEEN);  phase = phase + QUEEN_PHASE
            END SELECT
        END DO
        DO i = 1, board%num_black_pieces
            sq = board%black_pieces(i)
            piece = board%squares_piece(sq%rank, sq%file)
            SELECT CASE(piece)
            CASE(KNIGHT); phase = phase + KNIGHT_PHASE
            CASE(BISHOP); phase = phase + BISHOP_PHASE
            CASE(ROOK);   phase = phase + ROOK_PHASE
            CASE(QUEEN);  phase = phase + QUEEN_PHASE
            END SELECT
        END DO
        ! Clamp phase to be within the valid range [0, TOTAL_PHASE]
        phase = MIN(phase, TOTAL_PHASE)

        ! --- Evaluate Pieces ---
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
            CASE(PAWN)
                piece_value = PAWN_VAL
                mg_pst_val = PAWN_PST_MG(eval_rank, f)
                eg_pst_val = PAWN_PST_EG(eval_rank, f)
            CASE(KNIGHT)
                piece_value = KNIGHT_VAL
                mg_pst_val = KNIGHT_PST_MG(eval_rank, f)
                eg_pst_val = KNIGHT_PST_EG(eval_rank, f)
            CASE(BISHOP)
                piece_value = BISHOP_VAL
                mg_pst_val = BISHOP_PST_MG(eval_rank, f)
                eg_pst_val = BISHOP_PST_EG(eval_rank, f)
            CASE(ROOK)
                piece_value = ROOK_VAL
                mg_pst_val = ROOK_PST_MG(eval_rank, f)
                eg_pst_val = ROOK_PST_EG(eval_rank, f)
            CASE(QUEEN)
                piece_value = QUEEN_VAL
                mg_pst_val = QUEEN_PST_MG(eval_rank, f)
                eg_pst_val = QUEEN_PST_EG(eval_rank, f)
            CASE(KING)
                piece_value = KING_VAL
                mg_pst_val = KING_PST_MG(eval_rank, f)
                eg_pst_val = KING_PST_EG(eval_rank, f)
            CASE DEFAULT
                piece_value = 0
                mg_pst_val = 0
                eg_pst_val = 0
            END SELECT

            ! Tapered PST value
            pst_value = ((mg_pst_val * phase) + (eg_pst_val * (TOTAL_PHASE - phase))) / TOTAL_PHASE

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
            CASE(PAWN)
                piece_value = PAWN_VAL
                mg_pst_val = PAWN_PST_MG(eval_rank, f)
                eg_pst_val = PAWN_PST_EG(eval_rank, f)
            CASE(KNIGHT)
                piece_value = KNIGHT_VAL
                mg_pst_val = KNIGHT_PST_MG(eval_rank, f)
                eg_pst_val = KNIGHT_PST_EG(eval_rank, f)
            CASE(BISHOP)
                piece_value = BISHOP_VAL
                mg_pst_val = BISHOP_PST_MG(eval_rank, f)
                eg_pst_val = BISHOP_PST_EG(eval_rank, f)
            CASE(ROOK)
                piece_value = ROOK_VAL
                mg_pst_val = ROOK_PST_MG(eval_rank, f)
                eg_pst_val = ROOK_PST_EG(eval_rank, f)
            CASE(QUEEN)
                piece_value = QUEEN_VAL
                mg_pst_val = QUEEN_PST_MG(eval_rank, f)
                eg_pst_val = QUEEN_PST_EG(eval_rank, f)
            CASE(KING)
                piece_value = KING_VAL
                mg_pst_val = KING_PST_MG(eval_rank, f)
                eg_pst_val = KING_PST_EG(eval_rank, f)
            CASE DEFAULT
                piece_value = 0
                mg_pst_val = 0
                eg_pst_val = 0
            END SELECT

            ! Tapered PST value
            pst_value = ((mg_pst_val * phase) + (eg_pst_val * (TOTAL_PHASE - phase))) / TOTAL_PHASE

            evaluate_board = evaluate_board - (piece_value + pst_value)
        END DO

        ! Adjust score relative to current player
        ! (positive = current player advantage, negative = opponent advantage)
        IF (board%current_player == BLACK) THEN
            evaluate_board = -evaluate_board
        END IF

    END FUNCTION evaluate_board

END MODULE Evaluation
