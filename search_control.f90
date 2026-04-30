MODULE Search_Control
    USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_size_t, c_long, c_char, c_short
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: begin_search_polling, end_search_polling, poll_search_stop, &
              reset_search_stop, search_stop_requested, search_quit_requested, &
              has_buffered_command, pop_buffered_command, read_next_command

    INTEGER(c_int), PARAMETER :: STDIN_FD = 0_c_int
    INTEGER(c_int), PARAMETER :: O_NONBLOCK = int(Z'00000004', c_int)
    INTEGER(c_short), PARAMETER :: POLLIN = 1_c_short

    TYPE, BIND(C) :: pollfd_type
        INTEGER(c_int) :: fd
        INTEGER(c_short) :: events
        INTEGER(c_short) :: revents
    END TYPE pollfd_type

    LOGICAL :: stop_requested = .FALSE.
    LOGICAL :: quit_requested = .FALSE.
    LOGICAL :: polling_enabled = .FALSE.
    LOGICAL :: timed_search_enabled = .FALSE.
    INTEGER(c_int) :: saved_flags = -1_c_int
    INTEGER :: search_start_count = 0
    INTEGER :: search_count_rate = 0
    INTEGER :: search_time_limit_ms = -1
    CHARACTER(LEN=4096) :: pending_command = ''
    INTEGER :: pending_len = 0
    INTEGER, PARAMETER :: MAX_BUFFERED_COMMANDS = 16
    CHARACTER(LEN=4096), DIMENSION(MAX_BUFFERED_COMMANDS) :: buffered_commands = ''
    INTEGER :: buffered_count = 0

    INTERFACE
        FUNCTION c_fcntl_getfl(fd) BIND(C, NAME="c_fcntl_getfl") RESULT(res)
            IMPORT :: c_int
            INTEGER(c_int), VALUE :: fd
            INTEGER(c_int) :: res
        END FUNCTION c_fcntl_getfl

        FUNCTION c_fcntl_setfl(fd, flags) BIND(C, NAME="c_fcntl_setfl") RESULT(res)
            IMPORT :: c_int
            INTEGER(c_int), VALUE :: fd, flags
            INTEGER(c_int) :: res
        END FUNCTION c_fcntl_setfl

        FUNCTION c_read(fd, buf, count) BIND(C, NAME="read") RESULT(res)
            IMPORT :: c_int, c_size_t, c_long, c_char
            INTEGER(c_int), VALUE :: fd
            CHARACTER(KIND=c_char), DIMENSION(*), INTENT(OUT) :: buf
            INTEGER(c_size_t), VALUE :: count
            INTEGER(c_long) :: res
        END FUNCTION c_read

        FUNCTION c_poll(fds, nfds, timeout) BIND(C, NAME="poll") RESULT(res)
            IMPORT :: pollfd_type, c_int
            TYPE(pollfd_type), DIMENSION(*), INTENT(INOUT) :: fds
            INTEGER(c_int), VALUE :: nfds
            INTEGER(c_int), VALUE :: timeout
            INTEGER(c_int) :: res
        END FUNCTION c_poll
    END INTERFACE

CONTAINS

    SUBROUTINE reset_search_stop()
        stop_requested = .FALSE.
        quit_requested = .FALSE.
        timed_search_enabled = .FALSE.
        search_start_count = 0
        search_count_rate = 0
        search_time_limit_ms = -1
    END SUBROUTINE reset_search_stop

    SUBROUTINE begin_search_polling(time_limit_ms)
        INTEGER, INTENT(IN), OPTIONAL :: time_limit_ms
        INTEGER(c_int) :: flags

        CALL reset_search_stop()
        flags = c_fcntl_getfl(STDIN_FD)
        IF (flags < 0_c_int) RETURN

        saved_flags = flags
        IF (IAND(flags, O_NONBLOCK) == 0_c_int) THEN
            IF (c_fcntl_setfl(STDIN_FD, IOR(flags, O_NONBLOCK)) < 0_c_int) THEN
                saved_flags = -1_c_int
                RETURN
            END IF
        END IF

        polling_enabled = .TRUE.
        IF (PRESENT(time_limit_ms)) THEN
            search_time_limit_ms = MAX(time_limit_ms, 0)
            CALL SYSTEM_CLOCK(search_start_count, search_count_rate)
            IF (search_count_rate > 0) THEN
                timed_search_enabled = .TRUE.
            ELSE
                search_time_limit_ms = -1
            END IF
        END IF
        CALL consume_buffered_control_commands()
    END SUBROUTINE begin_search_polling

    SUBROUTINE end_search_polling()
        INTEGER(c_int) :: rc

        IF (polling_enabled .AND. saved_flags >= 0_c_int) THEN
            rc = c_fcntl_setfl(STDIN_FD, saved_flags)
        END IF
        polling_enabled = .FALSE.
        timed_search_enabled = .FALSE.
        search_start_count = 0
        search_count_rate = 0
        search_time_limit_ms = -1
        saved_flags = -1_c_int
    END SUBROUTINE end_search_polling

    LOGICAL FUNCTION poll_search_stop() RESULT(stop_now)
        CHARACTER(KIND=c_char), DIMENSION(256) :: buffer
        INTEGER(c_long) :: bytes_read
        INTEGER :: i

        stop_now = stop_requested
        IF (stop_now .OR. .NOT. polling_enabled) RETURN

        IF (timed_search_enabled .AND. deadline_reached()) THEN
            stop_requested = .TRUE.
            stop_now = .TRUE.
            RETURN
        END IF

        DO
            IF (.NOT. stdin_ready()) EXIT

            bytes_read = c_read(STDIN_FD, buffer, SIZE(buffer, KIND=c_size_t))
            IF (bytes_read == 0_c_long) THEN
                IF (pending_len > 0) THEN
                    CALL flush_pending_command_for_search()
                    IF (stop_requested) THEN
                        stop_now = .TRUE.
                        RETURN
                    END IF
                END IF
                EXIT
            ELSE IF (bytes_read < 0_c_long) THEN
                EXIT
            END IF

            DO i = 1, INT(bytes_read)
                CALL consume_char(TRANSFER(buffer(i), 'a'))
                IF (stop_requested) THEN
                    stop_now = .TRUE.
                    RETURN
                END IF
            END DO
        END DO

        IF (.NOT. stop_requested .AND. timed_search_enabled .AND. deadline_reached()) THEN
            stop_requested = .TRUE.
        END IF
        stop_now = stop_requested
    END FUNCTION poll_search_stop

    LOGICAL FUNCTION search_stop_requested() RESULT(is_requested)
        is_requested = stop_requested
    END FUNCTION search_stop_requested

    LOGICAL FUNCTION search_quit_requested() RESULT(is_requested)
        is_requested = quit_requested
    END FUNCTION search_quit_requested

    LOGICAL FUNCTION has_buffered_command() RESULT(has_command)
        has_command = (buffered_count > 0)
    END FUNCTION has_buffered_command

    SUBROUTINE read_next_command(command, has_command)
        CHARACTER(LEN=*), INTENT(OUT) :: command
        LOGICAL, INTENT(OUT) :: has_command
        CHARACTER(KIND=c_char), DIMENSION(256) :: buffer
        INTEGER(c_long) :: bytes_read
        INTEGER :: i

        IF (buffered_count > 0) THEN
            CALL pop_buffered_command(command, has_command)
            RETURN
        END IF

        command = ''
        has_command = .FALSE.
        DO
            bytes_read = c_read(STDIN_FD, buffer, SIZE(buffer, KIND=c_size_t))
            IF (bytes_read <= 0_c_long) THEN
                IF (pending_len > 0) THEN
                    CALL flush_pending_command_to_buffer()
                    IF (buffered_count > 0) THEN
                        CALL pop_buffered_command(command, has_command)
                    END IF
                END IF
                EXIT
            END IF

            DO i = 1, INT(bytes_read)
                CALL consume_buffered_char(TRANSFER(buffer(i), 'a'))
            END DO

            IF (buffered_count > 0) THEN
                CALL pop_buffered_command(command, has_command)
                RETURN
            END IF
        END DO
    END SUBROUTINE read_next_command

    SUBROUTINE pop_buffered_command(command, has_command)
        CHARACTER(LEN=*), INTENT(OUT) :: command
        LOGICAL, INTENT(OUT) :: has_command
        INTEGER :: i

        IF (buffered_count <= 0) THEN
            command = ''
            has_command = .FALSE.
            RETURN
        END IF

        command = TRIM(buffered_commands(1))
        DO i = 2, buffered_count
            buffered_commands(i - 1) = buffered_commands(i)
        END DO
        buffered_commands(buffered_count) = ''
        buffered_count = buffered_count - 1
        has_command = .TRUE.
    END SUBROUTINE pop_buffered_command

    SUBROUTINE consume_char(ch)
        CHARACTER(LEN=1), INTENT(IN) :: ch
        CHARACTER(LEN=4096) :: trimmed

        SELECT CASE (ch)
        CASE (NEW_LINE('a'), CHAR(13))
            IF (pending_len > 0) THEN
                trimmed = TRIM(ADJUSTL(pending_command(:pending_len)))
                SELECT CASE (trimmed)
                CASE ('stop')
                    stop_requested = .TRUE.
                CASE ('quit')
                    stop_requested = .TRUE.
                    quit_requested = .TRUE.
                CASE ('isready')
                    WRITE(*,'(A)') 'readyok'
                CASE DEFAULT
                    CALL buffer_command(trimmed)
                END SELECT
            END IF
            pending_command = ''
            pending_len = 0
        CASE DEFAULT
            IF (pending_len < LEN(pending_command)) THEN
                pending_len = pending_len + 1
                pending_command(pending_len:pending_len) = ch
            ELSE
                pending_command = ''
                pending_len = 0
            END IF
        END SELECT
    END SUBROUTINE consume_char

    SUBROUTINE buffer_command(command)
        CHARACTER(LEN=*), INTENT(IN) :: command

        IF (buffered_count >= MAX_BUFFERED_COMMANDS) THEN
            buffered_commands(1:MAX_BUFFERED_COMMANDS-1) = buffered_commands(2:MAX_BUFFERED_COMMANDS)
            buffered_count = MAX_BUFFERED_COMMANDS - 1
        END IF
        buffered_count = buffered_count + 1
        buffered_commands(buffered_count) = TRIM(command)
    END SUBROUTINE buffer_command

    SUBROUTINE consume_buffered_control_commands()
        INTEGER :: i, keep_count
        CHARACTER(LEN=4096) :: command

        keep_count = 0
        DO i = 1, buffered_count
            command = TRIM(buffered_commands(i))
            SELECT CASE (command)
            CASE ('stop')
                stop_requested = .TRUE.
            CASE ('quit')
                stop_requested = .TRUE.
                quit_requested = .TRUE.
            CASE ('isready')
                WRITE(*,'(A)') 'readyok'
            CASE DEFAULT
                keep_count = keep_count + 1
                buffered_commands(keep_count) = command
            END SELECT
        END DO

        DO i = keep_count + 1, buffered_count
            buffered_commands(i) = ''
        END DO
        buffered_count = keep_count
    END SUBROUTINE consume_buffered_control_commands

    SUBROUTINE flush_pending_command_to_buffer()
        CHARACTER(LEN=4096) :: trimmed

        IF (pending_len <= 0) RETURN
        trimmed = TRIM(ADJUSTL(pending_command(:pending_len)))
        IF (LEN_TRIM(trimmed) > 0) THEN
            CALL buffer_command(trimmed)
        END IF
        pending_command = ''
        pending_len = 0
    END SUBROUTINE flush_pending_command_to_buffer

    SUBROUTINE flush_pending_command_for_search()
        CHARACTER(LEN=4096) :: trimmed

        IF (pending_len <= 0) RETURN
        trimmed = TRIM(ADJUSTL(pending_command(:pending_len)))
        SELECT CASE (trimmed)
        CASE ('stop')
            stop_requested = .TRUE.
        CASE ('quit')
            stop_requested = .TRUE.
            quit_requested = .TRUE.
        CASE ('isready')
            WRITE(*,'(A)') 'readyok'
        CASE DEFAULT
            IF (LEN_TRIM(trimmed) > 0) THEN
                CALL buffer_command(trimmed)
            END IF
        END SELECT
        pending_command = ''
        pending_len = 0
    END SUBROUTINE flush_pending_command_for_search

    SUBROUTINE consume_buffered_char(ch)
        CHARACTER(LEN=1), INTENT(IN) :: ch
        CHARACTER(LEN=4096) :: trimmed

        SELECT CASE (ch)
        CASE (NEW_LINE('a'), CHAR(13))
            IF (pending_len > 0) THEN
                trimmed = TRIM(ADJUSTL(pending_command(:pending_len)))
                CALL buffer_command(trimmed)
            END IF
            pending_command = ''
            pending_len = 0
        CASE DEFAULT
            IF (pending_len < LEN(pending_command)) THEN
                pending_len = pending_len + 1
                pending_command(pending_len:pending_len) = ch
            ELSE
                pending_command = ''
                pending_len = 0
            END IF
        END SELECT
    END SUBROUTINE consume_buffered_char

    LOGICAL FUNCTION deadline_reached() RESULT(reached)
        INTEGER :: current_count
        REAL :: elapsed_ms

        reached = .FALSE.
        IF (.NOT. timed_search_enabled) RETURN
        IF (search_count_rate <= 0) RETURN

        CALL SYSTEM_CLOCK(current_count)
        elapsed_ms = 1000.0 * REAL(current_count - search_start_count) / REAL(search_count_rate)
        reached = (elapsed_ms >= REAL(search_time_limit_ms))
    END FUNCTION deadline_reached

    LOGICAL FUNCTION stdin_ready() RESULT(is_ready)
        TYPE(pollfd_type) :: descriptor(1)
        INTEGER(c_int) :: poll_result

        descriptor(1)%fd = STDIN_FD
        descriptor(1)%events = POLLIN
        descriptor(1)%revents = 0_c_short

        poll_result = c_poll(descriptor, 1_c_int, 0_c_int)
        is_ready = (poll_result > 0_c_int .AND. &
            IAND(INT(descriptor(1)%revents, KIND=c_int), INT(POLLIN, KIND=c_int)) /= 0_c_int)
    END FUNCTION stdin_ready

END MODULE Search_Control
