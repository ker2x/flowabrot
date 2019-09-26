module m_progressbar
    use appgraphics
    implicit none
    
    type progressbar
        integer :: x,y,width,height, oldcolor, progress
        type(fillsettingstype) :: oldfill
    end type progressbar
    type(progressbar) :: pb
    
contains

    subroutine init_progressbar(x,y,width,height, progress)
        use appgraphics, only : getcolor
        implicit none        
        integer, intent(in) :: x,y,width,height
        integer, optional :: progress
    
        pb%x = x
        pb%y = y
        pb%width = width
        pb%height = height        
        pb%oldcolor = getcolor()
        
        pb%progress = 0                                 ! default value
        if(present(progress)) pb%progress = progress    ! overiden if specified in argument
    
    end subroutine

    subroutine set_progressbar(progress)
        implicit none
        integer, intent(in) :: progress
        pb%progress = progress
        if(pb%progress < 1) pb%progress = 1
        if(pb%progress > 100) pb%progress = 100
    end subroutine
    
    function get_progressbar() result(progress)
        implicit none
        integer :: progress
        progress = pb%progress
    end function
    
    subroutine draw_progressbar()
        use appgraphics
        implicit none
        integer :: bar_size

        bar_size = ((pb%progress / 100.0) * pb%width) + 1        
        pb%oldcolor = getcolor()
        call getfillsettings(pb%oldfill)

        call setcolor(BLACK)
        call rectangle(pb%x, pb%y, pb%x + pb%width, pb%y + pb%height)
        call setcolor(pb%oldcolor)
        call setfillstyle(SOLID_FILL, LIGHTGREEN)
        call bar(pb%x + 2, pb%y +1, (pb%x + bar_size) - 1, (pb%y + pb%height))
        call setcolor(pb%oldcolor)
        call setfillstyle(pb%oldfill%pattern, pb%oldfill%color )
        
    end subroutine
    
end module