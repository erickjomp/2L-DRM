program recycling
    USE derived_types
    USE global_data
    USE subroutines_IO
    USE subroutines_tracing
    USE subroutines_process

    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_NAN

    implicit none

    integer                                           :: i,j,k, time, t_data
    real(4), dimension(:,:), allocatable              :: path_xy, path_uv
    integer, dimension(:,:), allocatable              :: path_ij
    integer                                           :: length_path, n_sections
    type(start_end),dimension(:), allocatable         :: path_sections
    real(4), dimension(:,:,:), allocatable            :: rho  ! stores rho2 for each cell of domain
    real(4), dimension(:,:), allocatable              :: rhocolumn 
    ! real(4), dimension(nregions, nslabs)            :: rho_domain  ! stores rho 
    real(4), dimension(:,:,:), allocatable            :: rho_domain  ! stores rho 
    real(4), dimension(:,:), allocatable              :: rhocolumn_domain ! stores rho 
    real(4), dimension(:), allocatable                :: rho_timeavg
    real(4), dimension(:), allocatable                :: PP_domain
    real(4), dimension(:,:), allocatable              :: PW_domain
    real(4), dimension(:), allocatable                :: PP_areal_days
    integer                                           :: n_datadt_analysis
    integer                                           :: islab
    real(4)                                           :: startTime, stopTime
    integer                                           :: i_file, max_tdata_array
    integer                                           :: i_UVdt
    ! type(timedelta)                                   :: timedelta_c
    ! type(datetime)                                    :: datetime0
    type(datetime)                                    :: datetime_c
    ! REMOVE
    integer       :: ii,jj,i_celldomain,i_region, i_section, i_point
    logical       :: found, verbose2
    integer       :: i_path
    ! END REMOVE

    call read_global_data()

    ! allocating local variables
    allocate(path_xy(2, max_tracing))
    allocate(path_uv(2, max_tracing))
    allocate(path_ij(2, max_tracing))
    allocate(path_sections(max_tracing))
    allocate(rho(nregions, domsize, nslabs))
    allocate(rhocolumn(nregions, domsize))
    allocate(rho_domain(nregions, nslabs, t_data_end-t_data_start+1))
    allocate(rhocolumn_domain(nregions,  t_data_end-t_data_start+1))
    allocate(rho_timeavg(nregions))
    allocate(PP_domain(domsize))
    allocate(PW_domain(domsize, nslabs))
    allocate(PP_areal_days(t_data_end-t_data_start+1))


    ! Loading data
    call read_masks()
    call read_targetregion()
    ! call openfile()  
    ! call daily_FLUX()
    call cpu_time(startTime)

    max_tdata_array = 0
    i_file = 0
    n_datadt_analysis= t_data_end-t_data_start+1


    print * ,"Verbose mode is actived:", verbose
    ! start the daily loop during the chosen duration from day1 to day2
    print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print *,'!!!  Daily Loop Starts  !!!'
    print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    daily : do t_data = t_data_start, t_data_end
        
        
        call check_and_update_globalarrays(t_data, max_tdata_array, i_file)
        i_UVdt = t_data * (data_dt/UV_dt)

        ! if (datetime0 == "0") then
        !     print *,'time step:', t_data
        ! else
        !     print *, strptime()
        ! end if
        datetime_c = datetime0 + timedelta(hours = data_dt * (t_data - 1))
        print *, datetime_c%isoformat(' '), "    | time step = ",t_data
        print *, "----"

        call get_PP_domain(t_data, PP_domain)
        call get_PW_domain(t_data, PW_domain)

        ! if (day == 16) then
        !     print *, sum(PP_domain)
        !     print *, sum(PW_FLUX(:,:,:,day),MASK = .not. isnan(PW_FLUX(:,:,:,day)))
        ! end if

        do islab = 1,2
            ! start the domain loop


            !$OMP PARALLEL DO PRIVATE(i,j,path_xy,path_ij,length_path,path_uv, &
            !$OMP                     path_sections, n_sections)
            domain : do k=1,domsize 
                i=domain_ij(k,1)
                j=domain_ij(k,2)

                call tracing_from_ij(i,j, islab, i_UVdt, option_next_point, &
                                    path_xy, path_ij, length_path, path_uv) ! outputs
                ! length_path does not include last point in case it runs out of grid or mask
                ! if (day == 16 .and. k == 100) then
                !     do i_path = 1,length_path 
                !         print *, i_path, path_xy(1,i_path), path_xy(2,i_path), path_uv(1, i_path), path_uv(2, i_path), &
                !                "i_UVdt=",i_UVdt
                !     end do
                ! end if

                if (write_paths)  call write_path(path_xy, length_path, islab, t_data, k, step_write_paths, datetime_c)
                
                call identify_path_sections(path_ij,length_path, &
                                            path_sections, n_sections)  ! outputs
                
                if (islab == 1) then
                    ! REMOVE
                    if (t_data == 62 .and. k == 1817) then 
                        verbose2 = .false.
                        call calculate_rhos_slabs__diffin(path_ij, &
                                                    path_sections, n_sections, i_UVdt, &
                                                    1, rho(:,k,islab),verbose_in = verbose2)   ! outputs
                        print *,"slab 1 rho diffin = ",rho(:,k,islab)
                    end if

                    verbose2 = .false.
                    if (t_data == 384 .and. k == 2565) then 
                        verbose2 = .false.
                    end if

                    ! END REMOVE
                    if (solver == 1) then
                        call calculate_rho1_slab1__2LDRM(path_ij, &
                                                        path_sections, n_sections, i_UVdt, &
                                                        rho(:,k,islab))   ! outputs
                    else if (solver >= 2) then
                        call calculate_rhos_slabs__diffin(path_ij, &
                                                        path_sections, n_sections, i_UVdt, &
                                                        1, rho(:,k,islab))   ! outputs
                    end if 
                    
                    ! if (t_data == 32 .and. ieee_is_nan(rho(1,k,islab))) then
                    !     print *, "t_data = ", t_data ,", k = ", k ,", rho = ", rho(:,k,islab)
                    ! end if
                    ! if (k==100 .and. t_data == 32)  print *, "rho2_confirm:",rho(:,k,islab)

                    ! REMOVE
                    if (t_data == 62 .and. k == 1817) then 
                        print *,"slab 1 rho = ",rho(:,k,islab)
                    end if
                    ! end REMOVE
                else if (islab == 2) then
                    verbose2 = .false.
                    ! REMOVE
                    if (t_data == 62 .and. k == 1817) then 
                        verbose2 = .false.
                        call calculate_rhos_slabs__diffin(path_ij, &
                                                    path_sections, n_sections, i_UVdt, &
                                                    2, rho(:,k,islab),verbose_in = verbose2)   ! outputs
                        print *,"slab 2 rho diffin = ",rho(:,k,islab)
                        ! do i_point = 1,path_sections(n_sections)%end_l
                        !     print *, "i_point:",i_point,  ", path_ij: ",path_ij(1,i_point), path_ij(2,i_point)
                        ! end do
                        ! do i_section = 1,n_sections
                        !     print *, "id_reg= ", path_sections(i_section)%id_region, &
                        !             ",  start=", path_sections(i_section)%start_l, &
                        !             ",  end=", path_sections(i_section)%end_l
                        !     ! id_region, start_l, end_l
                        ! end do
                    end if

                    verbose2 = .false.
                    if (t_data == 310 .and. k == 3024) then  !k=2565
                        verbose2 = .false.
                    end if

                    ! end REMOVE
                    if (solver == 1) then
                        call calculate_rho2_slab2__2LDRM(path_ij, &
                                                    path_sections, n_sections, i_UVdt, &
                                                    rho(:,k,islab),verbose_in = verbose2)   ! outputs
                    else if (solver >= 2) then
                        call calculate_rhos_slabs__diffin(path_ij, &
                                                    path_sections, n_sections, i_UVdt, &
                                                    2, rho(:,k,islab),verbose_in = verbose2)   ! outputs
                    end if
                    
                    if (verbose2)    print *, "rho2_confirm:",rho(:,k,islab)
                    ! REMOVE
                    if (t_data == 62 .and. k == 1817) then 
                        print *,"slab 2 rho = ",rho(:,k,islab)
                    end if

                    ! if (t_data == 32 .and. ieee_is_nan(rho(1,k,islab))) then
                    !     print *, "slab2:  t_data = ", t_data ,", k = ", k ,", rho = ", rho(:,k,islab)
                    ! end if
                    ! if (t_data == 384 .and. .not.(ieee_is_finite(rho(1,k,islab)))) then
                    
                    if (t_data == 310 .and. (rho(1,k,islab)<0)) then
                        print *, "slab2:  t_data = ", t_data ,", k = ", k ,", rho = ", rho(:,k,islab)
                    end if

                    ! end REMOVE
                end if
            
            end do domain
            !$OMP END PARALLEL DO

            ! call calculate_areal_rho(rho(:,:,islab), PW_domain(:,islab), domsize, nregions,&
            !                             rho_domain(:,islab,t_data-t_data_start+1))  ! outputs
            call calculate_areal_rho(rho(:,:,islab), PP_domain, domsize, nregions,&
                                        rho_domain(:,islab,t_data-t_data_start+1))  ! outputs
            if (verbose)  print * ,"slab ",islab, " : ", rho_domain(:,islab, t_data-t_data_start+1)

            ! found =.false.
            ! if (islab == 2) then
            !     print *, sum(PW_domain(:,islab))
            ! end if
            ! do i_celldomain = 1,domsize
            !     do i_region = 1,nregions
            !         if ((.not. IEEE_IS_FINITE( rho(i_region,i_celldomain,islab))) .and. (islab == 2)) then
            !             ! if (.not. found)  
            !             print * , i_region,i_celldomain,"slab", islab, " is Infinite : ",  rho(i_region,i_celldomain,islab)
            !             ! found = .true.
            !         end if
            !     end do
            ! end do
            ! do i_celldomain = 1,domsize
            !     if (.not. IEEE_IS_FINITE( PW_domain(i_celldomain, islab))) then
            !         if (.not. found) then
            !             print * , i_celldomain,"slab", islab, " is nan"
            !         end if
            !         found = .true.
            !     end if
            ! end do
            
        end do

        if (t_data == 62) then
        call calculate_column_rho(rho, PW_domain, domsize, nregions, nslabs, &
                                rhocolumn, .true.)
        else
        call calculate_column_rho(rho, PW_domain, domsize, nregions, nslabs, &
                                rhocolumn, .false.)
        end if

        if (write_rhocolumn_grid) then
            call write_rhocolumn_grid_sr(rhocolumn)
            call write_rhoslabs_grid_sr(rho)
        end if

        ! if (t_data >= 60 .and. t_data <= 63) then 
        !     print * ,"t_data = ", t_data, "rho = ", rho(1,51,48)
        !     ! do k = 1,100
        !     !     print *,"k=",k, "slab1", rho(:,k,1), "slab2", rho(:,k,2)
        !     !     print *, "column  ", "  : ", rhocolumn(:,k)
        !     ! end do
        ! end if

        call calculate_areal_rho(rhocolumn, &
                                 PP_domain,&
                                 domsize, nregions,&
                                 rhocolumn_domain(:,t_data-t_data_start+1))
                                 
        ! call calculate_areal_rho(rhocolumn, &
        !                          sum(PW_domain, dim = 2, mask = .not. isnan(PW_domain)),&
        !                          domsize, nregions,&
        !                          rhocolumn_domain(:,t_data-t_data_start+1))  
        if (verbose)  print *, "column    ", "   "," : ", rhocolumn_domain(:, t_data-t_data_start+1)

        PP_areal_days(t_data-t_data_start+1) = sum(PP_domain) /size(PP_domain)    
                  
    end do daily

    call cpu_time(stopTime)
    write(*, '(A, F8.4)') 'Elapsed time, min : ',  (stopTime - startTime)/60.0
    
    call write_rhodomain_daily(rho_domain)
    call write_rhocolumn_domain_daily(rhocolumn_domain)

    call average_in_time(rhocolumn_domain, PP_areal_days, n_datadt_analysis, nregions, rho_timeavg)

    print *,"rho - time averaged :"
    if (verbose)   print *, rho_timeavg
    call write_rhodomain_timeavg(rho_timeavg)

    call write_precipareal_daily(PP_areal_days)

    ! EXTRA
    ! call average_in_time(rho_domain(:,1,:), PP_areal_days, n_datadt_analysis, nregions, rho_timeavg)
    ! call write_rhodomain_timeavg2(rho_timeavg, &
    ! trim(path_output) // 'rho_slab1_domain_timeavg.csv')

    ! call average_in_time(rho_domain(:,2,:), PP_areal_days, n_datadt_analysis, nregions, rho_timeavg)
    ! call write_rhodomain_timeavg2(rho_timeavg, &
    ! trim(path_output) // 'rho_slab2_domain_timeavg.csv')




end program recycling
