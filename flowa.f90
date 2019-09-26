MODULE m_flowa

    USE m_globals
    IMPLICIT NONE
    
    ! filenames (hardcoded, for now)
    CHARACTER ( LEN = 255 ), PARAMETER :: filename_xyz = 'flowa.xyz'
    CHARACTER ( LEN = 255 ), PARAMETER :: filename_dat = 'flowa.dat'
    CHARACTER ( LEN = 255 ), PARAMETER :: filename_sorted = 'flowa_sorted.xyz'
    CHARACTER ( LEN = 255 ), PARAMETER :: filename_filtered = 'flowa_filtered.xyz'
    
    INTEGER, PARAMETER :: file_xyz_unit = 10
    INTEGER, PARAMETER :: file_dat_unit = 11
    INTEGER, PARAMETER :: file_sorted_unit = 12
    INTEGER, PARAMETER :: file_filtered_unit = 13
    
    INTEGER, PARAMETER :: miniter = 1000
    INTEGER, PARAMETER :: maxiter = 4000
    INTEGER, PARAMETER :: batchsize = 1000


    TYPE orbite_t
        REAL (KIND=4), DIMENSION(:), ALLOCATABLE :: pts_x
        REAL (KIND=4), DIMENSION(:), ALLOCATABLE :: pts_y
        REAL (KIND=4), DIMENSION(:), ALLOCATABLE :: pts_z
        INTEGER (KIND=2) :: col_r
        INTEGER (KIND=2) :: col_g
        INTEGER (KIND=2) :: col_b
        INTEGER :: iter
        REAL :: r
    END TYPE orbite_t
    
    TYPE(orbite_t), DIMENSION(:) :: orbite(batchsize)

CONTAINS

    SUBROUTINE init_flowa
        IMPLICIT NONE

        pts_found = 0
        
        OPEN( unit = file_xyz_unit, file = filename_xyz, status = 'replace', &
            form = 'formatted',   access = 'sequential')
        OPEN( unit = file_dat_unit, file = filename_dat, status = 'replace', &
            form = 'unformatted', access = 'stream')
    END SUBROUTINE init_flowa


SUBROUTINE start_flowa()
    USE m_globals
    IMPLICIT NONE
    
    INTEGER :: i

    DO i=1, batchsize
        ALLOCATE(orbite(i)%pts_x(maxiter - miniter))
        ALLOCATE(orbite(i)%pts_y(maxiter - miniter))
        ALLOCATE(orbite(i)%pts_z(maxiter - miniter))
    END DO
    flowa_allocated = .TRUE.
END SUBROUTINE

SUBROUTINE stop_flowa()
    IMPLICIT NONE
    INTEGER :: i
    
    DO i=1, batchsize
        DEALLOCATE(orbite(i)%pts_x)
        DEALLOCATE(orbite(i)%pts_y)
        DEALLOCATE(orbite(i)%pts_z)
    END DO
    flowa_allocated = .FALSE.
END SUBROUTINE
    
SUBROUTINE update_flowa()
        IMPLICIT NONE

        REAL :: x=0 ,y=0 ,z=0 ,nx=0 ,ny=0 ,nz=0, r2p=0 ,th=0 ,ph=0 , p=2
        REAL :: xmin = -1.0, xmax = 2.0, ymin = -1.3, ymax =1.3
        INTEGER (KIND=8) :: i = 1, j=1


        !$OMP PARALLEL DEFAULT(PRIVATE) SHARED(orbite)        
        x=0; y=0; z=0
        nx=0; ny=0; nz=0
        r2p=0; th=0; ph=0; p=2
        xmin = -1.0; xmax = 2.0
        ymin = -1.3; ymax =1.3

        !$OMP DO 
        DO i=1, batchsize

            CALL RANDOM_NUMBER(x)
            CALL RANDOM_NUMBER(y)
            CALL RANDOM_NUMBER(z)
            orbite(i)%pts_x = 0
            orbite(i)%pts_y = 0
            orbite(i)%pts_z = 0
            orbite(i)%iter = 0
            orbite(i)%r = 0
            
            x = (x * 2.6) - 1.3
            y = (y * 2.6) - 1.3
            z = (z * 2.6) - 1.3
            nx = x
            ny = y
            nz = z
            orbite(i)%r=sqrt(x*x+y*y+z*z)
            th=atan(y/x)*p
            ph=asin(z/orbite(i)%r)*p
        
            DO WHILE((orbite(i)%iter < MAXITER) .AND. (orbite(i)%r < 2.0) &
                .AND. (nx /= 0.0) .AND. (orbite(i)%r /= 0.0) .AND. (nx /= -0.0) &
                    .AND. (orbite(i)%r /= -0.0))
            
               r2p = (orbite(i)%r*orbite(i)%r)
               th=atan(ny/nx)*p
               ph=asin(nz/orbite(i)%r)*p
               nx=r2p*cos(ph)*cos(th)+x
               ny=r2p*cos(ph)*sin(th)+y
               nz=r2p*sin(ph)+z
               orbite(i)%r=sqrt(nx*nx+ny*ny+nz*nz)
               
               IF(orbite(i)%iter > miniter) THEN
                    orbite(i)%pts_x(orbite(i)%iter - miniter) = nx
                    orbite(i)%pts_y(orbite(i)%iter - miniter) = ny
                    orbite(i)%pts_z(orbite(i)%iter - miniter) = nz
               END IF
               
               orbite(i)%iter = orbite(i)%iter +1
  
            END DO
        END DO
        !$OMP END DO
        !$OMP END PARALLEL

        DO i=1, batchsize
        
            IF(((orbite(i)%iter) > miniter) .AND. (orbite(i)%iter) < maxiter) THEN
            
                DO j=1, (orbite(i)%iter) - miniter , 1
                
                    !only write if r < 2 
                    IF(sqrt(orbite(i)%pts_x(j) * orbite(i)%pts_x(j) + &
                        orbite(i)%pts_y(j) * orbite(i)%pts_y(j) + &
                        orbite(i)%pts_z(j) * orbite(i)%pts_z(j)) < 2.0) THEN
                        pts_found = pts_found + 1
                        IF(j == 1) THEN
                            orbite(i)%col_r = max(6,min(254,INT(abs(orbite(i)%pts_x(j) * 255))))
                            orbite(i)%col_g = max(6,min(254,INT(abs(orbite(i)%pts_y(j) * 255))))
                            orbite(i)%col_b = max(6,min(254,INT(abs(orbite(i)%pts_z(j) * 255))))
                        END IF
                        
                        !!$OMP CRITICAL
                        WRITE( file_xyz_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)' ) &
                            orbite(i)%pts_x(j), orbite(i)%pts_y(j), orbite(i)%pts_z(j), &
                                orbite(i)%col_r, orbite(i)%col_g, orbite(i)%col_b
                        WRITE( file_dat_unit ) orbite(i)%pts_x(j), orbite(i)%pts_y(j), orbite(i)%pts_z(j), &
                            orbite(i)%col_r, orbite(i)%col_g, orbite(i)%col_b
                        !!$OMP END CRITICAL
                    END IF
                END DO
            END IF
        END DO


END SUBROUTINE

SUBROUTINE sort_flowa()
    USE appgraphics
    USE m_sort
    
    IMPLICIT NONE
    
    INTEGER :: i
    INTEGER :: reason = 0
    INTEGER :: lines = 0
    LOGICAL :: xyzo

    TYPE(data_t), ALLOCATABLE :: dat(:)

    ! close and reopen xyz
    INQUIRE(file_xyz_unit,opened=xyzo)          ! check if opened
    IF(xyzo) close ( unit = file_xyz_unit )     ! close if already opened
    open( unit = file_xyz_unit, file = filename_xyz, status = 'old', &
        form = 'formatted',   access = 'sequential')

    ! count lines
    lines = 0
    reason = 0
    DO WHILE(reason == 0)
        read(file_xyz_unit, *, iostat=reason)
        IF(reason == 0) lines = lines + 1
    END DO

    ! allocate dat with line numbers    
    ALLOCATE(dat(lines))
    reason = 0
    lines = 0

    ! close and reopen xyz
    close(file_xyz_unit)
    open ( unit = file_xyz_unit, file = filename_xyz, status = 'old', &
        form = 'formatted',   access = 'sequential')

    ! read xyz, dump to memory (dat), close
    DO WHILE(reason == 0)
        lines = lines + 1
        read( file_xyz_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)', iostat=reason) &
            dat(lines)%x,dat(lines)%y,dat(lines)%z,dat(lines)%r,dat(lines)%g,dat(lines)%b
        !if(reason == 0) write( file_sorted_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)' ) x,y,z,r,g,b
    END do
    close(file_xyz_unit)
    
    ! call sort
    call quicksort_x(dat, 1, lines-1)
    
    ! open sorted, write, close
    open ( unit = file_sorted_unit, file = filename_sorted, status = 'replace', &
        form = 'formatted',   access = 'sequential')
    DO i=1, lines
        write( file_sorted_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)' ) &
            dat(i)%x, dat(i)%y, dat(i)%z, dat(i)%r, dat(i)%g, dat(i)%b
    END do
    close(file_sorted_unit)
    
    !cleanup
    DEALLOCATE(dat)
    
END SUBROUTINE

SUBROUTINE filter_flowa()
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: reason = 0
    INTEGER :: lines = 0
    LOGICAL :: sortedo

    TYPE(data_t), ALLOCATABLE :: dat(:)

    ! close and reopen xyz
    INQUIRE(file_sorted_unit,opened=sortedo)          ! check if opened
    IF(sortedo) close ( unit = file_sorted_unit )     ! close if already opened
    open( unit = file_sorted_unit, file = filename_sorted, status = 'old', &
        form = 'formatted',   access = 'sequential')

    ! count lines
    lines = 0
    reason = 0
    DO WHILE(reason == 0)
        read(file_sorted_unit, *, iostat=reason)
        IF(reason == 0) lines = lines + 1
    END DO

    ! allocate dat with line numbers    
    ALLOCATE(dat(lines))
    reason = 0
    lines = 0

    DO WHILE(reason == 0)
        lines = lines + 1
        read( file_sorted_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)', iostat=reason) &
            dat(lines)%x,dat(lines)%y,dat(lines)%z,dat(lines)%r,dat(lines)%g,dat(lines)%b
        !if(reason == 0) write( file_sorted_unit, '(f8.4,f8.4,f8.4,i4,i4,i4)' ) x,y,z,r,g,b
    END do
    close(file_sorted_unit)

    !do the filtering
    
    !write to sorted file

    !cleanup
    DEALLOCATE(dat)

END SUBROUTINE

SUBROUTINE close_flowa()
    IMPLICIT NONE
    LOGICAL :: xyzo, dato, sorto
    
    inquire(file_dat_unit,opened=dato)
    IF(dato) close ( unit = file_dat_unit )

    inquire(file_xyz_unit,opened=xyzo)
    IF(xyzo) close ( unit = file_xyz_unit )

    inquire(file_sorted_unit,opened=sorto)
    IF(sorto) close ( unit = file_sorted_unit )

END SUBROUTINE
    
END MODULE m_flowa