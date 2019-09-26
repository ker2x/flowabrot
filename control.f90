MODULE m_control
    USE m_progressbar, ONLY : init_progressbar, draw_progressbar
    USE m_globals,     ONLY : is_looping, is_started, myscreen
    USE appgraphics
    
    IMPLICIT NONE
    
    INTEGER :: root_menu, file_menu, help_menu, item_menu
    INTEGER :: start_button, stop_button, sort_button, filter_button

    CONTAINS
    
    ! INIT SUBROUTINES
    SUBROUTINE init_screen()
        IMPLICIT NONE
        myscreen = initwindow(200, 200, title="flowabrot", closeflag=.TRUE.)
        CALL setbkcolor(systemcolor(COLOR_WINDOW_BKGD))
        CALL settextstyle(WINDOWS_FONT, HORIZ_DIR, 16)
        CALL setmatchthemetextstyle()
        CALL clearviewport()
    END SUBROUTINE init_screen
    
    SUBROUTINE init_menu()
        IMPLICIT NONE
        root_menu = addmenu("", MENU_FOR_WINDOW)
        file_menu = addmenu("File", root_menu)
        help_menu = addmenu("Help", root_menu)
        item_menu = addmenuitem("About", help_menu, about)
        item_menu = addmenuitem("Quit", file_menu, quit)
    END SUBROUTINE
    
    SUBROUTINE init_controls()
        USE m_progressbar
        IMPLICIT NONE

        start_button  = createbutton(10,10,50,30,"Start", cb_start)
        stop_button   = createbutton(75, 10, 50, 30, "Stop", cb_stop) 
        sort_button   = createbutton(10,140,50,30, "Sort", cb_sort)
        filter_button = createbutton(70, 140, 50, 30, "Filter", cb_filter)
        
        is_started = .FALSE.
        
        CALL enablebutton(sort_button, .FALSE.)        
        CALL enablebutton(filter_button, .FALSE.)        
        CALL enablebutton(stop_button, .FALSE.)
        CALL setcolor(DARKGRAY)
        CALL line(10, 50, 190, 50)
        CALL init_progressbar(10,60,180,20)
        CALL set_progressbar(1)
        CALL draw_progressbar()
        CALL line(10, 130, 190, 130)
    END SUBROUTINE
    
    ! redrawing each frame when needed
    SUBROUTINE update_controls()
        USE appgraphics,   ONLY : clearviewport, line
        USE m_progressbar, ONLY : draw_progressbar
        USE m_globals,     ONLY : pts_found

        IMPLICIT NONE
      
        CHARACTER (LEN=20) :: text_found, text_size
        INTEGER :: est_size = 0
      
        CALL clearviewport()
        CALL line(10, 50, 190, 50)
        CALL draw_progressbar()

        WRITE(text_found,*) pts_found / 1000
        CALL outtextxy(10, 90,  "points found (k) : " // ADJUSTL(text_found) )

        est_size = (pts_found*44) / 1024 / 1024     !44 byte per point with x,y,z,r,g,b in formatted ascii output
        WRITE(text_size, '(I6)') est_size
        CALL outtextxy(10, 110, "est. size (MB) : " // ADJUSTL(text_size) )

        CALL setcolor(DARKGRAY)
        CALL line(10, 130, 190, 130)        
    END SUBROUTINE

    SUBROUTINE quit()
        USE m_globals, ONLY : is_looping, is_started
        IMPLICIT NONE
        
        IF(is_started) THEN
            CALL cb_stop()
        END IF
        is_looping = .FALSE.
        is_started = .FALSE.
        CALL stopidle()
    END SUBROUTINE

    SUBROUTINE about()
        IMPLICIT NONE
        CALL dlgmessage(DIALOG_INFO, "flowabrot by ker2x, with love from fractalforum")
    END SUBROUTINE

    ! ---- CALLBACKS ----

    ! start button
    SUBROUTINE cb_start()
        USE m_globals, ONLY : is_started
        USE m_flowa
        USE appgraphics
        IMPLICIT NONE
        
        IF(is_started .eqv. .FALSE.) THEN
            is_started = .TRUE.
            CALL enablebutton(start_button, .FALSE.)
            CALL enablebutton(stop_button, .TRUE.)
            CALL enablebutton(sort_button, .FALSE.)
            CALL start_flowa()
            CALL stopidle()
        END IF
    END SUBROUTINE

    ! stop button
    SUBROUTINE cb_stop()
        USE m_globals, ONLY : is_started
        USE m_flowa
        IMPLICIT NONE
        
        IF(is_started .eqv. .TRUE.) THEN
            is_started = .FALSE.
            CALL enablebutton(start_button, .TRUE.)
            CALL enablebutton(stop_button, .FALSE.)
            CALL enablebutton(sort_button, .TRUE.)
        END IF
    END SUBROUTINE

    ! sort button
    SUBROUTINE cb_sort()
        USE m_flowa
        IMPLICIT NONE
        
        CALL enablebutton(sort_button, .FALSE.)
        CALL enablebutton(start_button, .FALSE.)
        CALL enablebutton(stop_button, .FALSE.)
        CALL sort_flowa()
        !CALL enablebutton(sort_button, .TRUE.)        
        CALL enablebutton(filter_button, .TRUE.)        
    END SUBROUTINE

    SUBROUTINE cb_filter()
        USE m_flowa
        IMPLICIT NONE
        
        CALL enablebutton(sort_button, .FALSE.)
        CALL enablebutton(filter_button, .FALSE.)
        CALL filter_flowa()
        CALL enablebutton(filter_button, .TRUE.)
    END SUBROUTINE
END MODULE m_control