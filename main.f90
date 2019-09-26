PROGRAM main
USE m_control, ONLY : init_menu, init_controls, init_screen, update_controls
USE m_globals, ONLY : is_started, is_looping, myscreen, flowa_allocated
USE m_progressbar, ONLY : set_progressbar
USE m_flowa
USE appgraphics

IMPLICIT NONE

    INTEGER :: progress = 1
        
    ! INITIALISATION
    CALL init_screen()
    CALL init_menu()
    CALL init_controls()
    CALL init_flowa()
    
    is_started = .FALSE.
    is_looping = .TRUE.

    ! MAIN LOOP
    DO WHILE(is_looping)
        IF(.NOT. is_started) THEN
            IF(flowa_allocated) CALL stop_flowa()
            CALL loop()
        ELSE
            !CALL startidle(5)
            progress = progress + 2
            IF(progress > 100) progress = 1            
            CALL set_progressbar(progress)
            CALL update_flowa()
            CALL update_controls()
        END IF
    END DO
    
    ! EXIT
    CALL close_flowa
    CALL closewindow(myscreen)

END PROGRAM main
