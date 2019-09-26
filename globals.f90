MODULE m_globals
    IMPLICIT NONE
    
    LOGICAL, VOLATILE :: is_started  ! if start button was pressed    
    LOGICAL, VOLATILE :: is_looping  ! same kind of control for the main loop
    LOGICAL, VOLATILE :: flowa_allocated = .FALSE. ! pain-in-the-*ss memory alloc/dealloc logic helper
    
    INTEGER :: myscreen
    INTEGER :: pts_found

    TYPE data_t
        REAL (KIND=4) :: x,y,z
        INTEGER (KIND=2) :: r,g,b
    END TYPE
    
END MODULE m_globals