program recycling
    USE derived_types
    USE global_data
    USE subroutines_IO
    USE subroutines_tracing
    USE subroutines_process

    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
    USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_NAN
    ! use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
    !                                       stdout=>output_unit, &
    !                                       stderr=>error_unit
    ! https://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90

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


    if ((nslabs .ne. 2) .and. (solver == 1)) then
        write(*,"('Can not use solver 1 (analytical 2LDRM) with more than 2 slab')")
        stop 0
    end if

    if (solver == 2) then 
        write(*,"('Solver 2 not implemented yet. Choose solver 1 or 3.')")
        stop 0
    end if


    write (*,*) "Verbose mode is actived:", verbose
    ! start the daily loop during the chosen duration from day1 to day2
    write (*,*)  '!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    write (*,*)  '!!!  Daily Loop Starts  !!!'
    write (*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    daily : do t_data = t_data_start, t_data_end
        
        
        call check_and_update_globalarrays(t_data, max_tdata_array, i_file)
        i_UVdt = t_data * (data_dt/UV_dt)

        datetime_c = datetime0 + timedelta(hours = data_dt * (t_data - 1))
        ! print stdout,"------ ", datetime_c%isoformat(' '), "    | time step = ",t_data, " ------"
        write (*,*) "------ ", datetime_c%isoformat(' '), "    | time step = ",t_data, " ------"
        ! print *, "----"

        call get_PP_domain(t_data, PP_domain)
        call get_PW_domain(t_data, PW_domain)



        do islab = 1,nslabs
            ! start the domain loop

            !$OMP PARALLEL DO PRIVATE(i,j,path_xy,path_ij,length_path,path_uv, &
            !$OMP                     path_sections, n_sections, verbose2)
            domain : do k=1,domsize 
                i=domain_ij(k,1)
                j=domain_ij(k,2)

                call tracing_from_ij(i,j, islab, i_UVdt, option_next_point, &
                                    path_xy, path_ij, length_path, path_uv) ! outputs

                if (write_paths)  call write_path(path_xy, length_path, islab, t_data, k, step_write_paths, datetime_c)
                
                call identify_path_sections(path_ij,length_path, &
                                            path_sections, n_sections)  ! outputs
                
                if (solver == 1) then
                    if (islab == 1) then
                        call calculate_rho1_slab1__2LDRM(path_ij, &
                                                        path_sections, n_sections, i_UVdt, &
                                                        rho(:,k,islab))   ! outputs

                    else if (islab == 2) then
                        verbose2 = .false.
                        call calculate_rho2_slab2__2LDRM(path_ij, &
                                                        path_sections, n_sections, i_UVdt, &
                                                        rho(:,k,islab),verbose_in = verbose2)   ! outputs
                        if (verbose2)    print *, "rho2_confirm:",rho(:,k,islab)

                    end if
                else   ! other solvers
                    verbose2 = .false.

                    call calculate_rhos_slabs__diffin(path_ij, &
                                                    path_sections, n_sections, i_UVdt, &
                                                    islab, rho(:,k,islab), verbose_in = verbose2)   ! outputs
                end if
            
            end do domain
            !$OMP END PARALLEL DO

            ! call calculate_areal_rho(rho(:,:,islab), PW_domain(:,islab), domsize, nregions,&
            !                             rho_domain(:,islab,t_data-t_data_start+1))  ! outputs
            call calculate_areal_rho(rho(:,:,islab), PP_domain, domsize, nregions,&
                                        rho_domain(:,islab,t_data-t_data_start+1))  ! outputs
            ! if (verbose)  print * ,"slab ",islab, " : ", rho_domain(:,islab, t_data-t_data_start+1)
            if (verbose)  write(*,'(A, I1, A, *(XX,F4.3))')  "slab ",islab, " : ", rho_domain(:,islab, t_data-t_data_start+1)

        end do

        call calculate_column_rho(rho, PW_domain, domsize, nregions, nslabs, &
                                rhocolumn, .false.)

        if (write_rho_grid) then
            call write_rho_grid_sr(rhocolumn, datetime_c)
            ! call write_rhoslabs_grid_sr(rho)
        end if


        call calculate_areal_rho(rhocolumn, &
                                 PP_domain,&
                                 domsize, nregions,&
                                 rhocolumn_domain(:,t_data-t_data_start+1))
                                 
        ! call calculate_areal_rho(rhocolumn, &
        !                          sum(PW_domain, dim = 2, mask = .not. isnan(PW_domain)),&
        !                          domsize, nregions,&
        !                          rhocolumn_domain(:,t_data-t_data_start+1))  
        ! if (verbose)  print *, "column    ", "   "," : ", rhocolumn_domain(:, t_data-t_data_start+1)
        
        ! if (verbose)  write(*,'(A, A, *(XX,"0",F4.3))')  "column ", " : ", rhocolumn_domain(:, t_data-t_data_start+1)
        if (verbose)  write(*,'(A, A, *(XX,F4.3))')  "column", " : ", rhocolumn_domain(:, t_data-t_data_start+1)

        PP_areal_days(t_data-t_data_start+1) = sum(PP_domain) /size(PP_domain)    
                  
    end do daily

    call cpu_time(stopTime)
    write(*, '(A, F8.4)') 'Elapsed time, min : ',  (stopTime - startTime)/60.0
    
    call write_rhodomain_daily(rho_domain)
    call write_rhocolumn_domain_daily(rhocolumn_domain)

    call average_in_time(rhocolumn_domain, PP_areal_days, n_datadt_analysis, nregions, rho_timeavg)

    ! print stdout,"rho - time averaged :"
    write (*,*)   "rho - time averaged :"
    if (verbose)   write(*,"(*(XX,F4.3))") rho_timeavg
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
