! ============================================
! Module: Chess_Types
! Purpose: Define constants and basic types
! ============================================
MODULE Chess_Types
    IMPLICIT NONE

    ! --- Constants for Pieces ---
    INTEGER, PARAMETER :: NO_PIECE = 0
    INTEGER, PARAMETER :: PAWN     = 1
    INTEGER, PARAMETER :: KNIGHT   = 2
    INTEGER, PARAMETER :: BISHOP   = 3
    INTEGER, PARAMETER :: ROOK     = 4
    INTEGER, PARAMETER :: QUEEN    = 5
    INTEGER, PARAMETER :: KING     = 6

    ! --- Constants for Colors ---
    INTEGER, PARAMETER :: NO_COLOR = 0
    INTEGER, PARAMETER :: WHITE    = 1
    INTEGER, PARAMETER :: BLACK    = 2

    ! --- Board Dimensions ---
    INTEGER, PARAMETER :: BOARD_SIZE = 8

    ! --- Maximum number of moves possible in a position ---
    INTEGER, PARAMETER :: MAX_MOVES = 256 ! Sufficient for chess

    ! --- Direction arrays for piece movement ---
    INTEGER, PARAMETER, DIMENSION(4, 2) :: BISHOP_DIRS = RESHAPE((/  1,  1,  -1, -1, -1,  1, 1, -1 /), (/4, 2/))
    INTEGER, PARAMETER, DIMENSION(4, 2) :: ROOK_DIRS = RESHAPE((/  1,  0, -1,  0,  0,  1,  0, -1 /), (/4, 2/))
    INTEGER, PARAMETER, DIMENSION(8, 2) :: QUEEN_DIRS = RESHAPE((/ 1, -1,  0,  0,  1,  1, -1, -1, &
                                                         0,  0,  1, -1, 1, -1,  1, -1 /), (/8, 2/))
    INTEGER, PARAMETER, DIMENSION(8, 2) :: KNIGHT_DELTAS = RESHAPE((/ 2, 1, -1, -2, -2, -1, 1, 2, &
                                                            1, 2,  2,  1, -1, -2, -2, -1 /), (/8, 2/))
    INTEGER, PARAMETER, DIMENSION(8, 2) :: KING_DELTAS = RESHAPE((/  1,  0, -1,  0,  1,  1, -1, -1, &
                                                          0,  1,  0, -1, 1, -1,  1, -1 /), (/8, 2/))

    ! --- Derived Type for Square (Using 1-based indexing) ---
    TYPE :: Square_Type
        INTEGER :: rank = 0
        INTEGER :: file = 0
    END TYPE Square_Type

    ! --- Derived Type for Move ---
    TYPE :: Move_Type
        TYPE(Square_Type) :: from_sq
        TYPE(Square_Type) :: to_sq
        LOGICAL           :: is_castling = .FALSE.
        LOGICAL           :: is_en_passant = .FALSE.
        INTEGER           :: promotion_piece = NO_PIECE ! e.g., QUEEN, KNIGHT...
        INTEGER           :: captured_piece = NO_PIECE  ! Type of piece captured
    END TYPE Move_Type

    ! --- Derived Type for Undoing Moves ---
    TYPE :: UnmakeInfo_Type
        INTEGER           :: captured_piece_type = NO_PIECE
        INTEGER           :: captured_piece_color = NO_COLOR
        TYPE(Square_Type) :: captured_sq          ! Needed for EP undo
        LOGICAL           :: prev_ep_target_present = .FALSE.
        TYPE(Square_Type) :: prev_ep_target_sq
        LOGICAL           :: prev_wc_k = .FALSE.
        LOGICAL           :: prev_wc_q = .FALSE.
        LOGICAL           :: prev_bc_k = .FALSE.
        LOGICAL           :: prev_bc_q = .FALSE.
    END TYPE UnmakeInfo_Type

    ! --- Derived Type for Board State ---
    TYPE :: Board_Type
        INTEGER, DIMENSION(BOARD_SIZE, BOARD_SIZE) :: squares_piece = NO_PIECE
        INTEGER, DIMENSION(BOARD_SIZE, BOARD_SIZE) :: squares_color = NO_COLOR
        INTEGER           :: current_player = WHITE
        LOGICAL           :: ep_target_present = .FALSE.
        TYPE(Square_Type) :: ep_target_sq
        LOGICAL           :: wc_k = .FALSE. ! White Kingside Castle Right
        LOGICAL           :: wc_q = .FALSE. ! White Queenside Castle Right
        LOGICAL           :: bc_k = .FALSE. ! Black Kingside Castle Right
        LOGICAL           :: bc_q = .FALSE. ! Black Queenside Castle Right
        ! Piece lists for optimization
        TYPE(Square_Type), DIMENSION(16) :: white_pieces ! Max 16 pieces
        TYPE(Square_Type), DIMENSION(16) :: black_pieces
        INTEGER :: num_white_pieces = 0
        INTEGER :: num_black_pieces = 0
    END TYPE Board_Type

END MODULE Chess_Types
