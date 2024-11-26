program recycling
    USE derived_types
    USE global_data
    USE subroutines_IO
    USE subroutines_tracing
    USE subroutines_process

    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE

    implicit none

    integer                                           :: i,j,k, time, day
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
    integer                                           :: ndays_analysis
    integer                                           :: islab
    real(4)                                           :: startTime, stopTime
    integer                                           :: i_file, max_iday_array

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
    allocate(rho_domain(nregions, nslabs, day2-day1+1))
    allocate(rhocolumn_domain(nregions,  day2-day1+1))
    allocate(rho_timeavg(nregions))
    allocate(PP_domain(domsize))
    allocate(PW_domain(domsize, nslabs))
    allocate(PP_areal_days(day2-day1+1))


    ! Loading data
    call readmap()
    call monsoon()
    ! call openfile()  
    ! call daily_FLUX()
    call cpu_time(startTime)

    max_iday_array = 0
    i_file = 0
    ndays_analysis= day2-day1+1


    print * ,"Verbose mode is actived:", verbose
    ! start the daily loop during the chosen duration from day1 to day2
    print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print *,'!!!  Daily Loop Starts  !!!'
    print *,'!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    daily : do day = day1, day2
    
        
        call check_and_update_globalarrays(day, max_iday_array, i_file)

        print *,'day:', day
        print *, "----"
        call get_PP_domain(day, PP_domain)
        call get_PW_domain(day, PW_domain)

        ! if (day == 16) then
        !     print *, sum(PP_domain)
        !     print *, sum(PW_FLUX(:,:,:,day),MASK = .not. isnan(PW_FLUX(:,:,:,day)))
        ! end if

        do islab = 1,2
            ! start the domain loop
            domain : do k=1,domsize 
                i=domain_ij(k,1)
                j=domain_ij(k,2)

                call tracing_from_ij(i,j, islab, day, option_next_point, &
                                    path_xy, path_ij, length_path, path_uv) ! outputs
                ! length_path does not include last point in case it runs out of grid or mask
                ! if (day == 16 .and. k == 100) then
                !     do i_path = 1,length_path 
                !         print *, i_path, path_xy(1,i_path), path_xy(2,i_path) 
                !     end do
                ! end if

                if (write_paths)  call write_path(path_xy, length_path, islab, day, k, step_write_paths)
                
                call identify_path_sections(path_ij,length_path, &
                                            path_sections, n_sections)  ! outputs
                
                if (islab == 1) then
                    call calculate_rho1_slab1__2LDRM(path_ij, &
                                                    path_sections, n_sections, day, &
                                                    rho(:,k,islab))   ! outputs
                else if (islab == 2) then
                    verbose2 = .false.
                    if (day == 16 .and. k == 100) then 
                        verbose2 = .false.
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

                    call calculate_rho2_slab2__2LDRM(path_ij, &
                                                    path_sections, n_sections, day, &
                                                    rho(:,k,islab),verbose_in = verbose2)   ! outputs
                    ! if (verbose2)    print *, "rho2_confirm:",rho(:,k,islab)
                end if
            
            end do domain

            call calculate_areal_rho(rho(:,:,islab), PW_domain(:,islab), domsize, nregions,&
                                        rho_domain(:,islab,day-day1+1))  ! outputs
            ! if (verbose)  print * , rho_domain(:,islab, day-day1+1)

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

        call calculate_column_rho(rho, PW_domain, domsize, nregions, nslabs, &
                                rhocolumn, .false.)
        ! do k = 1,100
        !     print *,"k=",k
        !     print *, rhocolumn(:,k)
        ! end do

        ! call calculate_areal_rho(rhocolumn, PP_domain, domsize, nregions,&
        !                         rhocolumn_domain(:,day-day1+1))        
        call calculate_areal_rho(rhocolumn, &
                                 sum(PW_domain, dim = 2, mask = .not. isnan(PW_domain)),&
                                 domsize, nregions,&
                                 rhocolumn_domain(:,day-day1+1))  
        if (verbose)  print * , rhocolumn_domain(:, day-day1+1)

        PP_areal_days(day-day1+1) = sum(PP_domain) /size(PP_domain)    
                  
    end do daily

    call cpu_time(stopTime)
    write(*, '(A, F8.4)') 'Elapsed time, min : ',  (stopTime - startTime)/60.0
    
    call write_rhodomain_daily(rho_domain)
    call write_rhocolumn_domain_daily(rhocolumn_domain)

    call average_in_time(rhocolumn_domain, PP_areal_days, ndays_analysis, nregions, rho_timeavg)

    print *,"rho - time averaged :"
    if (verbose)   print *, rho_timeavg
    call write_rhodomain_timeavg(rho_timeavg)

    call write_precipareal_daily(PP_areal_days)

    ! EXTRA
    ! call average_in_time(rho_domain(:,1,:), PP_areal_days, ndays_analysis, nregions, rho_timeavg)
    ! call write_rhodomain_timeavg2(rho_timeavg, &
    ! trim(path_output) // 'rho_slab1_domain_timeavg.csv')

    ! call average_in_time(rho_domain(:,2,:), PP_areal_days, ndays_analysis, nregions, rho_timeavg)
    ! call write_rhodomain_timeavg2(rho_timeavg, &
    ! trim(path_output) // 'rho_slab2_domain_timeavg.csv')




end program recycling
