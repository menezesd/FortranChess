! ============================================
! Module: Make_Unmake
! Purpose: Apply and revert moves on the board
! ============================================
MODULE Make_Unmake
    USE Chess_Types
    USE Board_Utils, ONLY: get_opponent_color, update_piece_lists, update_piece_lists_unmake, file_rank_to_sq
    USE Transposition_Table, ONLY: ZOBRIST_PIECES, ZOBRIST_BLACK_TO_MOVE, ZOBRIST_CASTLING, ZOBRIST_EP_FILE
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: make_move, unmake_move

CONTAINS

    ! --- Make Move ---
    ! Applies a move to the board, updating all necessary state.
    !
    ! This function handles all aspects of move execution:
    ! - Piece movement and captures
    ! - Special moves (castling, en passant, promotion)
    ! - Castling rights updates
    ! - En passant target updates
    ! - Player turn switching
    ! - Piece list maintenance
    !
    ! Parameters:
    !   board (INOUT): Board state to modify
    !   move (IN): Move to apply
    !   unmake_info (OUT): Information needed to undo this move
    !
    ! Side effects:
    !   Completely modifies board state
    !   Updates piece lists for efficient iteration
    SUBROUTINE make_move(board, move, unmake_info)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Move_Type), INTENT(IN) :: move
        TYPE(UnmakeInfo_Type), INTENT(OUT) :: unmake_info

        INTEGER :: r_from, f_from, r_to, f_to, player_color, opponent_color
        INTEGER :: piece_moved, color_moved, r_capture
        INTEGER(KIND=8) :: new_key
        TYPE(Square_Type) :: from_sq, to_sq

        player_color = board%current_player
        opponent_color = get_opponent_color(player_color)
        from_sq = move%from_sq
        to_sq = move%to_sq
        r_from = from_sq%rank; f_from = from_sq%file
        r_to = to_sq%rank; f_to = to_sq%file

        ! 1. Store info for unmake
        unmake_info%prev_ep_target_present = board%ep_target_present
        unmake_info%prev_ep_target_sq = board%ep_target_sq
        unmake_info%prev_wc_k = board%wc_k
        unmake_info%prev_wc_q = board%wc_q
        unmake_info%prev_bc_k = board%bc_k
        unmake_info%prev_bc_q = board%bc_q
        unmake_info%prev_zobrist_key = board%zobrist_key

        piece_moved = board%squares_piece(r_from, f_from)
        color_moved = board%squares_color(r_from, f_from)

        IF (move%is_en_passant) THEN
            unmake_info%captured_piece_type = PAWN
            unmake_info%captured_piece_color = opponent_color
            IF (player_color == WHITE) THEN
                r_capture = r_to - 1
            ELSE
                r_capture = r_to + 1
            END IF
            unmake_info%captured_sq = file_rank_to_sq(f_to, r_capture)
            ! Remove the captured pawn
            board%squares_piece(r_capture, f_to) = NO_PIECE
            board%squares_color(r_capture, f_to) = NO_COLOR
        ELSE IF (move%is_castling) THEN
             unmake_info%captured_piece_type = NO_PIECE
             unmake_info%captured_piece_color = NO_COLOR
             unmake_info%captured_sq%rank = 0 ! Not used
        ELSE
            unmake_info%captured_piece_type = board%squares_piece(r_to, f_to)
            unmake_info%captured_piece_color = board%squares_color(r_to, f_to)
            unmake_info%captured_sq = to_sq ! Capture happens on 'to' square
        END IF

        ! 2. Update Squares
        ! Clear 'from' square
        board%squares_piece(r_from, f_from) = NO_PIECE
        board%squares_color(r_from, f_from) = NO_COLOR

        IF (move%is_castling) THEN
            board%squares_piece(r_to, f_to) = KING
            board%squares_color(r_to, f_to) = player_color
            ! Move the rook
            IF (f_to == 7) THEN ! Kingside (g file)
                board%squares_piece(r_from, 8) = NO_PIECE; board%squares_color(r_from, 8) = NO_COLOR
                board%squares_piece(r_from, 6) = ROOK;   board%squares_color(r_from, 6) = player_color
            ELSE ! Queenside (f_to == 3, c file)
                board%squares_piece(r_from, 1) = NO_PIECE; board%squares_color(r_from, 1) = NO_COLOR
                board%squares_piece(r_from, 4) = ROOK;   board%squares_color(r_from, 4) = player_color
            END IF
        ELSE
            ! Place moving piece (handle promotion)
            IF (move%promotion_piece /= NO_PIECE) THEN
                board%squares_piece(r_to, f_to) = move%promotion_piece
            ELSE
                board%squares_piece(r_to, f_to) = piece_moved
            END IF
            board%squares_color(r_to, f_to) = color_moved
        END IF

        ! 3. Update EP Target
        board%ep_target_present = .FALSE.
        IF (piece_moved == PAWN .AND. ABS(r_to - r_from) == 2) THEN
             board%ep_target_present = .TRUE.
             board%ep_target_sq%rank = (r_to + r_from) / 2
             board%ep_target_sq%file = f_from
        END IF

        ! 4. Update Castling Rights
        IF (piece_moved == KING) THEN
            IF (player_color == WHITE) THEN
                board%wc_k = .FALSE.; board%wc_q = .FALSE.
            ELSE
                board%bc_k = .FALSE.; board%bc_q = .FALSE.
            END IF
        END IF
        IF (piece_moved == ROOK) THEN
            IF (player_color == WHITE) THEN
                IF (r_from == 1 .AND. f_from == 1) board%wc_q = .FALSE.
                IF (r_from == 1 .AND. f_from == 8) board%wc_k = .FALSE.
            ELSE
                IF (r_from == 8 .AND. f_from == 1) board%bc_q = .FALSE.
                IF (r_from == 8 .AND. f_from == 8) board%bc_k = .FALSE.
            END IF
        END IF
        ! Rook captured on home square
        IF (unmake_info%captured_piece_type == ROOK) THEN
             IF (unmake_info%captured_piece_color == WHITE) THEN
                 IF (r_to == 1 .AND. f_to == 1) board%wc_q = .FALSE.
                 IF (r_to == 1 .AND. f_to == 8) board%wc_k = .FALSE.
             ELSE ! Black rook captured
                 IF (r_to == 8 .AND. f_to == 1) board%bc_q = .FALSE.
                 IF (r_to == 8 .AND. f_to == 8) board%bc_k = .FALSE.
             END IF
        END IF

        ! 5. Switch Player
        board%current_player = opponent_color

        ! --- Update Zobrist Key ---
        ! Start with current key
        new_key = board%zobrist_key

        ! XOR out the piece from its original square
        new_key = IEOR(new_key, ZOBRIST_PIECES(piece_moved, player_color, r_from, f_from))
        ! XOR in the piece at its new square
        new_key = IEOR(new_key, ZOBRIST_PIECES(piece_moved, player_color, r_to, f_to))

        ! Handle captures
        IF (unmake_info%captured_piece_type /= NO_PIECE) THEN
            new_key = IEOR(new_key, &
                ZOBRIST_PIECES(unmake_info%captured_piece_type, opponent_color, &
                               unmake_info%captured_sq%rank, unmake_info%captured_sq%file))
        END IF

        ! Handle castling rights changes
        IF (unmake_info%prev_wc_k .NEQV. board%wc_k) new_key = IEOR(new_key, ZOBRIST_CASTLING(1))
        IF (unmake_info%prev_wc_q .NEQV. board%wc_q) new_key = IEOR(new_key, ZOBRIST_CASTLING(2))
        IF (unmake_info%prev_bc_k .NEQV. board%bc_k) new_key = IEOR(new_key, ZOBRIST_CASTLING(3))
        IF (unmake_info%prev_bc_q .NEQV. board%bc_q) new_key = IEOR(new_key, ZOBRIST_CASTLING(4))

        ! Handle en passant changes
        IF (unmake_info%prev_ep_target_present) THEN
            new_key = IEOR(new_key, ZOBRIST_EP_FILE(unmake_info%prev_ep_target_sq%file))
        END IF
        IF (board%ep_target_present) THEN
            new_key = IEOR(new_key, ZOBRIST_EP_FILE(board%ep_target_sq%file))
        END IF

        ! Always flip the side to move key
        new_key = IEOR(new_key, ZOBRIST_BLACK_TO_MOVE)
        
        board%zobrist_key = new_key

        ! 6. Update piece lists
        CALL update_piece_lists(board, from_sq, to_sq, unmake_info%captured_sq, &
                               unmake_info%captured_piece_type, unmake_info%captured_piece_color, &
                               move%promotion_piece)

    END SUBROUTINE make_move

    ! --- Unmake Move ---
    ! Reverses a move that was previously applied with make_move.
    !
    ! This function restores the board to its exact state before the move was made,
    ! using the information stored in unmake_info. It handles:
    ! - Reversing piece movement and captures
    ! - Restoring special move states (castling, en passant, promotion)
    ! - Restoring castling rights
    ! - Restoring en passant target
    ! - Switching player turn back
    ! - Restoring piece lists
    !
    ! Parameters:
    !   board (INOUT): Board state to restore
    !   move (IN): The move that was previously applied
    !   unmake_info (IN): Information from make_move needed to undo
    !
    ! Side effects:
    !   Completely restores board state to pre-move condition
    !   Updates piece lists to match restored board state
    !   Must be called immediately after make_move for the same move
    SUBROUTINE unmake_move(board, move, unmake_info)
        TYPE(Board_Type), INTENT(INOUT) :: board
        TYPE(Move_Type), INTENT(IN) :: move
        TYPE(UnmakeInfo_Type), INTENT(IN) :: unmake_info

        INTEGER :: r_from, f_from, r_to, f_to, player_color
        INTEGER :: piece_to_restore, color_to_restore

        ! 1. Switch Player Back
        board%current_player = get_opponent_color(board%current_player)
        player_color = board%current_player ! Color of player who made the move being unmade

        ! 2. Restore Board State
        board%ep_target_present = unmake_info%prev_ep_target_present
        board%ep_target_sq = unmake_info%prev_ep_target_sq
        board%wc_k = unmake_info%prev_wc_k
        board%wc_q = unmake_info%prev_wc_q
        board%bc_k = unmake_info%prev_bc_k
        board%bc_q = unmake_info%prev_bc_q
        board%zobrist_key = unmake_info%prev_zobrist_key

        ! 3. Reverse Piece Movements
        r_from = move%from_sq%rank; f_from = move%from_sq%file
        r_to = move%to_sq%rank; f_to = move%to_sq%file

        ! Determine the piece type that moved (was it a pawn before promotion?)
        IF (move%promotion_piece /= NO_PIECE) THEN
             piece_to_restore = PAWN
        ELSE IF (move%is_castling) THEN
             piece_to_restore = KING ! King moved during castling
        ELSE
             ! Get the piece from the 'to' square (it must be there after make_move)
             piece_to_restore = board%squares_piece(r_to, f_to)
        END IF
        color_to_restore = player_color

        ! Put the piece back on 'from' square
        board%squares_piece(r_from, f_from) = piece_to_restore
        board%squares_color(r_from, f_from) = color_to_restore

        ! Restore 'to' square (and potentially captured piece / EP pawn)
        IF (move%is_castling) THEN
            board%squares_piece(r_to, f_to) = NO_PIECE  ! Clear king's landing sq
            board%squares_color(r_to, f_to) = NO_COLOR
            ! Put rook back
            IF (f_to == 7) THEN ! Kingside (g file)
                board%squares_piece(r_from, 6) = NO_PIECE; board%squares_color(r_from, 6) = NO_COLOR
                board%squares_piece(r_from, 8) = ROOK;   board%squares_color(r_from, 8) = player_color
            ELSE ! Queenside (f_to == 3, c file)
                board%squares_piece(r_from, 4) = NO_PIECE; board%squares_color(r_from, 4) = NO_COLOR
                board%squares_piece(r_from, 1) = ROOK;   board%squares_color(r_from, 1) = player_color
            END IF
        ELSE IF (move%is_en_passant) THEN
             board%squares_piece(r_to, f_to) = NO_PIECE  ! Clear EP landing sq
             board%squares_color(r_to, f_to) = NO_COLOR
             ! Put captured pawn back
             board%squares_piece(unmake_info%captured_sq%rank, unmake_info%captured_sq%file) = PAWN
             board%squares_color(unmake_info%captured_sq%rank, unmake_info%captured_sq%file) = unmake_info%captured_piece_color
        ELSE
            ! Restore whatever was on the 'to' square (captured piece or nothing)
             board%squares_piece(r_to, f_to) = unmake_info%captured_piece_type
             board%squares_color(r_to, f_to) = unmake_info%captured_piece_color
        END IF

        ! Update piece lists for unmake
        CALL update_piece_lists_unmake(board, move%from_sq, move%to_sq, unmake_info%captured_sq, &
                                       unmake_info%captured_piece_type, unmake_info%captured_piece_color)

    END SUBROUTINE unmake_move

END MODULE Make_Unmake
