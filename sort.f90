MODULE m_sort
    USE m_globals

CONTAINS

! regular unmodified quicksort on real*4
RECURSIVE SUBROUTINE quicksort(a, first, last)
    IMPLICIT NONE

    REAL (KIND=4) :: a(*), x, t
    INTEGER :: first, last
    INTEGER :: i, j

    x = a( (first+last) / 2 )
    i = first
    j = last
    
    DO
        DO WHILE (a(i) < x)
            i=i+1
        END DO
     
        DO WHILE (x < a(j))
            j=j-1
        END DO
     
        IF(i >= j) EXIT    ! sort completed, exit
     
        t = a(i);  a(i) = a(j);  a(j) = t
        i=i+1
        j=j-1
    END DO
  
    IF (first < i-1) CALL quicksort(a, first, i-1)
    IF (j+1 < last)  CALL quicksort(a, j+1, last)
END SUBROUTINE

! modified quickstort, order by a%x
RECURSIVE SUBROUTINE quicksort_x(a, first, last)
    IMPLICIT NONE

    TYPE(data_t) :: a(*), x, t
    INTEGER :: first, last
    INTEGER :: i, j

    x = a( (first+last) / 2 )
    i = first
    j = last
    DO
        DO WHILE(a(i)%x < x%x)
            i=i+1
        END DO
        
        DO WHILE(x%x < a(j)%x)
            j=j-1
        END DO
        
        IF(i >= j) EXIT

        t = a(i)
        a(i) = a(j)
        a(j) = t
        i=i+1
        j=j-1

    END DO
    
    IF(first < i-1) CALL quicksort_x(a, first, i-1)
    IF(j+1 < last)  CALL quicksort_x(a, j+1, last)

END SUBROUTINE

END MODULE