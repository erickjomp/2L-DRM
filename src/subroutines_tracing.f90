MODULE subroutines_tracing

    implicit none

contains
! subroutine getvalues_from_ij(i,j, islab, iday, out_i, )
        
    ! end subroutine
    PURE subroutine get_xy_from_ij(i, j, out_x, out_y)
        use global_data, only : dx, dy
        integer, intent(in)    :: i, j
        real(4), intent(out)   :: out_x, out_y

        out_x = dx * i - dx / 2
        out_y = dy * j - dy / 2
    end subroutine

    PURE subroutine get_ij_from_xy(x, y, out_i, out_j)
        use global_data, only : dx, dy
        real(4), intent(in)    :: x, y
        integer, intent(out)   :: out_i, out_j

        out_i = ceiling(x / dx)
        out_j = ceiling(y / dy)

    end subroutine


    PURE subroutine get_i_from_x(x,out_i)
        use global_data, only: dx
        real(4), intent(in)    :: x
        integer, intent(out)   :: out_i

        out_i = ceiling(x / dx)
    end subroutine
    
    PURE subroutine get_j_from_y(y, out_j)
        use global_data, only : dy
        real(4), intent(in)    :: y
        integer, intent(out)   :: out_j

        out_j = ceiling(y / dy)
    end subroutine

    PURE subroutine get_x_from_i(i, out_x)
        use global_data, only: dx
        integer, intent(in)    :: i
        real(4), intent(out)   :: out_x
    
        out_x = dx * i - dx / 2
    end subroutine

    PURE subroutine get_y_from_j(j, out_y)
        use global_data, only : dy
        integer, intent(in)    :: j
        real(4), intent(out)   :: out_y

        out_y = dy * j - dy / 2
    end subroutine



    ! option_next_point   :   1 for iterative technique, 2 for direct technique
    subroutine tracing_from_ij(i1, j1, islab, iday, option_next_point, path_xy, path_ij, length_path, path_uv, verbose)

        use global_data, only :  U3,V3,max_tracing,velocity_dt, &
                                    tol_error_iteration, max_iteration, NAN_value, tracing_dt
        integer, intent(in)                                      :: i1,j1,islab,iday
        integer, intent(in)                                      :: option_next_point
        integer                                                  :: time,i_velstep, next_i, next_j, i_velstep_next
        real(4), dimension(2,max_tracing), intent(out)           :: path_xy
        real(4), dimension(2,max_tracing), intent(out), optional :: path_uv
        integer, dimension(2,max_tracing), intent(out)           :: path_ij
        integer, intent(out)                                     :: length_path
        integer                                                  :: i,j
        integer                                                  :: option, counter_tracing_dts
        real(4)                                                  :: x,y,next_x,next_y, u, v, next_u, next_v
        logical                                                  :: is_end
        ! integer(4)                                             :: next_i, next_j
        integer                                                  :: n_tracingdt_in_veldt
        ! REMOVE
        logical, intent(in), optional                            :: verbose
        logical                                                  :: verbose2
        ! end REMOVE

        i = i1
        j = j1
        call get_xy_from_ij(i,j,x,y)

         !!!! UPDATE !!!!
        n_tracingdt_in_veldt = (velocity_dt * 60 * 60)/ tracing_dt  ! probably should go in GLOBAL DATA

        ! tracing_dt = real(velocity_dt * ) /   
        
        i_velstep = iday * (24 / velocity_dt)
        counter_tracing_dts  = 0          !  when counter_tracing_dt == valve1, then i_velstep--

        ! print *, "i  ", i
        ! print *, "j", j
        ! print *, "i_slab", islab
        ! print *, "i_velstep", i_velstep
        u = U3(i,j,islab,i_velstep)
        v = V3(i,j,islab,i_velstep)

        if (isnan(u) .or. isnan(v) ) then
            ! REMOVE
            if (present(verbose)) then
                if (verbose) then
                    print *, "HERE isnan(u)",  ", i = ", i, ", j = ",j
                end if
            end if
            ! END REMOVE

            ! path_ij(:,1:max_tracing) = 0
            ! path_xy(:,1:max_tracing) = NAN_value   
            path_ij(:,1) = 0      
            path_xy(:,1) = NAN_value    
            length_path = 0
            path_uv(:,1) = NAN_value
            return
        end if

        is_end = .false.

        
        tracing_loop: do time = 1,max_tracing 
            ! print * , " "
            ! print *, "day = ", iday, "   i_0 = ", i, "   j_0 = ", j, "   time", time
            path_xy(1,time) = x 
            path_xy(2,time) = y
            path_ij(1,time) = i
            path_ij(2,time) = j
            path_uv(1,time) = u
            path_uv(2,time) = v

            if (is_end) then
                length_path = time - 1
                exit tracing_loop
            end if

            if (time == max_tracing)  then 
                length_path = max_tracing
                exit tracing_loop
            end if
            ! if you want to store last out of grid/mask  point, activate next line
            ! and add is_end = .false. before loop and  deactivate the   if (is_end) 

            counter_tracing_dts = counter_tracing_dts + 1
            if (counter_tracing_dts == n_tracingdt_in_veldt) then
                counter_tracing_dts = 0
                i_velstep_next = i_velstep - 1
                if (i_velstep_next <= 0)  then
                    stop "Not possible to complete backtracing due to insufficient data back in time from day: "
                end if
            else 
                i_velstep_next = i_velstep
            end if 

            ! REMOVE
            u = U3(i,j,islab,i_velstep)
            v = V3(i,j,islab,i_velstep)

            ! end REMOVE

            ! REMOVE
            if (present(verbose)) then
                if (verbose .and. time < 4) then 
                    print *, " Looking inside tracing: time = ", time
                    print *, "i=",i,"j=",j,"x=",x,"y=",y,"u=",u,"v=",v,&
                            "i_velstep=",i_velstep
                end if
                if (verbose .and. (time >= 455) .and. (time <= 460)) then
                    verbose2 = .true.
                    print *, "time = ", time
                else 
                    verbose2 = .false.
                end if
                
            end if
            ! end REMOVE


            if (isnan(u) .or. isnan(v)) then
                length_path = time - 1
                exit tracing_loop
            end if
            
            if (option_next_point == 1) then
                ! actually only i j or x y is required in this subroutube,
                ! but then we would have and if with each branch 
                ! calling the subroutine, so better calculate x y outside.
                ! well it would be better to just use x y for all cases
                call next_point__iterative(i = i, j = j, &
                                            x = x, y = y, &
                                            u = u, v = v, &
                                            dt = real(tracing_dt), &
                                            Ugrid_precedent = U3(:,:,islab,i_velstep), &
                                            Vgrid_precedent = V3(:,:,islab,i_velstep), &
                                            max_iteration = max_iteration, &
                                            tol =  tol_error_iteration, &
                                            next_x = next_x, & ! output
                                            next_y = next_y, & ! output
                                            next_u = next_u, & ! output
                                            next_v = next_v, & ! output
                                            next_i = next_i, & ! output
                                            next_j = next_j, & ! output
                                            is_end = is_end, & ! output
                                            verbose = verbose2 &   ! REMOVE
                                            )  
            else if (option_next_point == 2) then
                call next_point__direct(i = i, j = j, &
                                        x = x, y = y, &
                                        u = u, v = v, dt = real(tracing_dt), &
                                        Ugrid_precedent = U3(:,:,islab,i_velstep_next), &
                                        Vgrid_precedent = V3(:,:,islab,i_velstep_next), &
                                        next_x = next_x, &  ! output
                                        next_y = next_y, &  ! output
                                        next_u = next_u, &  ! output 
                                        next_v = next_v, &  ! output
                                        next_i = next_i, &  ! output
                                        next_j = next_j, &  ! output
                                        is_end = is_end  &  ! output 
                                        )
            end if

            x = next_x
            y = next_y
            u = next_u
            v = next_v
            i = next_i
            j = next_j
            i_velstep = i_velstep_next

        end do tracing_loop


        if (time < max_tracing) then
            ! path_ij(:,(time+1):max_tracing) = 0
            ! path_xy(:,(time+1):max_tracing) = NAN_value
            path_ij(:,(time+1)) = 0
            path_xy(:,(time+1)) = NAN_value
        end if

    end subroutine

    PURE function is_out_grid(x,y) result(is_out)
        use global_data, only:  dx,dy, bigx, bigy
        real(4), intent(in) :: x,y
        logical             :: is_out

        ! is_out = (x < 0) .or. (x > (bigx * dx)) .or. (y < 0) .or. (y > (bigy * dy))
        is_out = (x <= 0) .or. (x > (bigx * dx)) .or. (y <= 0) .or. (y > (bigy * dy)) 
        ! we use  <= 0 pr  since in case of x = 0, get_ij_from_xy would generate next_i = 0 which does not make sense
    end function

    ! PURE subroutine  next_point_from_xy__iterative    : make use of next_point_fromij__iterative


    ! only ij ir xy is required, but if both are entered then both will be used (the subroutine assumes they  natch each others)
    PURE subroutine  next_point__iterative(&
                                        x, y, i, j, u, v, dt, Ugrid_precedent, Vgrid_precedent, &
                                        max_iteration, tol, &
                                        next_x, next_y, next_u, next_v, next_i, next_j, is_end, &
                                        verbose) ! REMOVE
        ! use global_data, only:  U3, V3
        use global_data, only :  NAN_value
        real(4), intent(in), dimension(:,:)   :: Ugrid_precedent, Vgrid_precedent                              
        real(4), intent(in)                   :: dt,u,v, tol
        integer, intent(inout), optional      :: i,j 
        real(4), intent(inout), optional      :: x,y
        integer, intent(in)                   :: max_iteration
        integer, intent(out)                  :: next_i, next_j
        real(4), intent(out)                  :: next_x, next_y, next_u, next_v
        logical, intent(out)                  :: is_end
        integer                               :: iiter
        real(4)                               :: avg_u, avg_v, diffx, diffy,next_x_old, next_y_old
        ! REMOVE 
        logical, intent(in), optional         :: verbose
        logical                               :: ver
        ! end REMOVE

        is_end = .false.

        if (.not.present(i))    call get_i_from_x(x, i)
        if (.not.present(j))    call get_j_from_y(y, j)
        if (.not.present(x))    call get_x_from_i(i, x)
        if (.not.present(y))    call get_y_from_j(j, y)

        next_x_old = x   ! temporal
        next_y_old = y   ! temporal

        avg_u = u   ! first value for first iteration
        avg_v = v   ! first value for first iteration

        iteration: do iiter = 1, max_iteration

            next_x = x - avg_u * dt
            next_y = y - avg_v * dt

            ! REMOVE
            ! ver = .false.
            ! if (present(verbose)) then
            !     ver = verbose
            ! end if
            
            ! if (ver) then
            !     print *, "iiter=",iiter, ", next_x=",next_x,", next_y=", next_y
            ! end if
            ! end REMOVE

            if (is_out_grid(next_x,next_y)) then
                !print *, "HERE 1"
                next_i = 0
                next_j = 0
                next_u = NAN_value
                next_v = NAN_value
                is_end = .true.
                exit iteration
            end if 

            call get_ij_from_xy(next_x, next_y, next_i, next_j)

            ! print *,"next_xy = ",next_x, next_y
            ! print *,"next_ij = ",next_i, next_j
            next_u = Ugrid_precedent(next_i, next_j)
            next_v = Vgrid_precedent(next_i, next_j)

            if (isnan(next_u) .or. isnan(next_v)) then
                ! print *, "HERE 2"
                is_end = .true.
                exit iteration
            end if
            ! storing after confirming next_x_temp and next_y_temp are ok

            if (iiter == max_iteration)                  exit iteration    ! NO  MORE CALCULATIONS FOR NEXT ITER 

            diffx = abs(next_x - next_x_old)
            diffy = abs(next_y - next_y_old)

            if ((diffx < tol)  .and. (diffy < tol))     exit iteration

            next_x_old = next_x
            next_y_old = next_y

            ! calculating next temporal next_x and next_y
            avg_u = (u + next_u) / 2
            avg_v = (v + next_v) / 2

        end do iteration

        ! REMOVE
        ! if (iiter > 1) then
        !     print *, "i = ", i, "j = ", j, "   iiter = ", iiter
        !     print *, "next_x = ", next_x, "   next_x_temp = ", next_x_temp, "  u = ", u, "  next_u = ", next_u
        ! end if
        ! END REMOVE
    end subroutine

    ! PURE subroutine  next_point_from_xy__iterative    : make use of next_point_fromij__direct
 
    PURE subroutine  next_point__direct(x, y, i, j, u, v, dt, Ugrid_precedent, Vgrid_precedent, &
                                        next_x, next_y, next_u, next_v, next_i,  next_j, is_end)
        ! use global_data, only:    U3, V3
        use global_data, only: NAN_value
        real(4), intent(in), dimension(:,:)       :: Ugrid_precedent, Vgrid_precedent    
        integer, intent(inout), optional          :: i,j
        real(4), intent(inout), optional          :: x,y
        real(4), intent(in)                       :: u,v,dt
        integer, intent(out)                      :: next_i,next_j
        real(4), intent(out)                      :: next_x, next_y, next_u, next_v
        logical, intent(out)                      :: is_end

        is_end = .false.

        if (.not.present(i))    call get_i_from_x(x, i)
        if (.not.present(j))    call get_j_from_y(y, j)
        if (.not.present(x))    call get_x_from_i(i, x)
        if (.not.present(y))    call get_y_from_j(j, y)

        next_x = x - u * dt 
        next_y = y - v * dt

        if (is_out_grid(next_x, next_y)) then
            next_i = 0
            next_j = 0
            next_u = NAN_value
            next_v = NAN_value
            is_end = .true.
            return
        end if
        ! print *, "i = ", i, "j = ", j
        ! print *, "x = ", x, "y = ", y
        ! print *, "u = ", u, "v = ", v
        ! print *, "next_x = ", next_x, "next_y = ", next_y
        call get_ij_from_xy(next_x,next_y, next_i, next_j)
        ! print *, "next_i = ", next_i, "next_j = ", next_j

        next_u = Ugrid_precedent(next_i,next_j)
        next_v = Vgrid_precedent(next_i,next_j)

        if (isnan(next_u) .or. isnan(next_v)) then
            ! print *, "HERE"
            is_end = .true.
            return
        end if

    end subroutine

END MODULE subroutines_tracing