MODULE subroutines_IO

    implicit none

contains

    subroutine read_global_data()
        use global_data
        use FLAP

        type(command_line_interface)                 :: cli    ! Command Line Interface (CLI).
        integer                                      :: error_cli  

        ! bigx=1471 !253 ! 1471 !1471  !number of grids in x and y direction
        ! bigy=2027 !346 ! 2027 ! 2027
        ! ndays= 488 !196 ! 120+15 months-01to04 no leapyear ! 123+15  months-05to08  ! 122+15  months-09to12   ! CHANGE ! it seems it no longer affects
        ! dx=4000.00  !unit(m), the distance of each grid
        ! dy=4000.00
        ! !__________________________starting dates and ending dates,core region
        ! day1=16
        ! day2= 488 !196  !123+15  !day1=16, day2=   !date starts and ends simulation
        ! domsize=49509  !1386 nonCPM  !49509 CPM !49509  !number of grids within the monsoon region
        ! nregions = 8  ! 83148 cells landocean CPM  ! 31774 cells land CPM ! 8    !number of regions in map file    ! CHANGE 
        ! !______________________________________________________________________
        ! tracing_dt = 1800  !time interval of each-time back tracing
        ! max_tracing=600 !1500 ! maximum times of back trajactory(>48*31)
        ! option_next_point = 1 ! 1 for iterative method, and 2 for direct method
        ! max_iteration=101 ! maximum times of iteration
        ! tol_error_iteration = 0.0001 
        ! ! integer, save :: ndays_warmup = 15 !days that to be excluded at the beginning    ! CHANGE 
        ! velocity_dt = 3 
        ! verbose = .true.
        ! write_paths = .true.
        ! step_write_paths =  12
        ! delta_write_paths = 6
        ! ij0_write_paths = 3

        call cli%init(description = 'Program that runs the 2LDRM model. Instead of just providing one binary file per variable &
                        &(default), you can provide multiple binay files per variable by activating the option --listfiles. &
                        &All the binary files should contain data with 4-bytes real numbers (float32).')
        call cli%add(switch='--pathin', &
            switch_ab='-pi',    &
            help='Path inputs. Optional. Not necessary if you will specific the complete path of all the other input files.',   &
            required=.false.,   &
            act='store',       &
            def='',            &
            error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--listsfiles', &
                    switch_ab='-lf',    &
                    help='Instead of reading one file at a time, you can read a list of them. &
                           &Use  .true.   if fileET, filePP, filePW, fileU, fileV, filePWflux are text files containing a list of &
                           &binary files. &
                           &Use  .false.  if it is only 1 binary file.', &
                    required=.false.,   &
                    def='.false.',     &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--fileET', &
                    switch_ab='-ET',    &
                    ! help='Text file containing the list of Evapotranspiration (ET) binary files to read. &
                    help='Evapotranspiration (ET) binary file to read. The data has to have dimensions (nx, ny, nslabs, ndays)&
                            & with nslabs = 2.  &
                            &Data at daily time step. Units of mm.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop       

        call cli%add(switch='--filePP', &
                    switch_ab='-PP',    &
                    ! help='Text file containing the list of Precipitation (PP) binary files to read. The data has to have dimensions (nx, ny, 2, ndays). &
                    help='Precipitation (PP) binary files to read. The data has to have dimensions (nx, ny, nslabs, ndays) &
                            & with nslabs = 2. &
                            &Data at daily time step. Units of mm.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop  


        call cli%add(switch='--filePW', &
                    switch_ab='-PW',    &
                    ! help='Text file containing the list of Precipitable Water (PW) binary files. The data has to have dimensions (nx, ny, 2, ndays). &
                    help='Precipitable Water (PW) binary files. The data has to have dimensions (nx, ny, nslabs, ndays) &
                            &with nslabs = 2. &
                            &Data at daily time step. Units of mm',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop


        call cli%add(switch='--fileU', &
                    switch_ab='-U',    &
                    ! help='Text file containing the list of vapor weighted wind speed U (U) binary files to read. The data has to have dimensions (nx, ny, 2, ,n_timesteps). &
                    help='Wind speed U (U) binary files to read. The data has to have dimensions (nx, ny, nslabs, n_timesteps)&
                            & with nslabs = 2. &
                            &Data at velocity_dt time step. Units of m/s.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--fileV', &
                    switch_ab='-V',    &
                    ! help='Text file containing the list of vapor weighted wind speed V (V) binary files to read. The data has to have dimensions (nx, ny, 2, ,n_timesteps). &
                    help='Wind speed V (V) binary files to read. The data has to have dimensions (nx, ny, nslabs, n_timesteps)&
                            & with nslabs = 2. &
                            &Data at velocity_dt time step. Units of m/s.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop


        call cli%add(switch='--filePWflux', &
                    switch_ab='-PWf',    &
                    ! help='Text file containing the list of PW flux between slabs (PWflux) binary files to read. The data has to have dimensions (nx, ny, 2, ,ndays).&
                    help='PW flux between slabs (PWflux) binary files to read. The data has to have dimensions &
                            & (nx, ny, n_interslabs, ndays) with n_interslabs = 1.&
                            &Data at daily time step. Units of mm/day. ',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop


        call cli%add(switch='--regions', &
                    switch_ab='-reg',    &
                    help='Binary file containing the mask of the source regions to analyze. The dimennsions must be (nx, ny). &
                    &Each region must be represented by a different integer number starting by 1.0 . ',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--topomask', &
                    switch_ab='-mask',    &
                    help='Binary file containing the topography mask for interslabs. The dimennsions must be (nx, ny). &
                    &The value is 1.0 for grid cells where the surface is below the interslab pressure level and 0.0 otherwise. &
                    &This is useful for mountainous regions. If the topography is flat, all the values will be 1.0. ',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--itarget', &
                    switch_ab='-it',    &
                    help='Binary file of i grid values of target region. This a unidimensional array with &
                    &length equal to the number of grid cells in target region.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--jtarget', &
                    switch_ab='-jt',    &
                    help='Binary file of j grid values of target region. This a unidimensional array with &
                    &length equal to the number of grid cells in target region.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--pathout', &
                    switch_ab='-po',    &
                    help='Path outputs.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--prefixout', &
                    switch_ab='-pre',    &
                    help='Prefix of output files. Optional.',   &
                    required=.false.,   &
                    act='store',       &
                    def='',            &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--nx', &
                    switch_ab='-nx',    &
                    help='Number of grid cells in X direction',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--ny', &
                    switch_ab='-ny',    &
                    help='Number of grid cells in Y direction',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--ndays', &
                    switch_ab='-nd',    &
                    help='Number of days in data',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--dx', &
                    switch_ab='-dx',    &
                    help='Delta x of grid cells',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--dy', &
                    switch_ab='-dy',    &
                    help='Delta y of grid cells',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--day1', &
                    switch_ab='-d1',    &
                    help='Day 1 of analysis. Since there should be some days for backtracing, it cant be 1.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--day2', &
                    switch_ab='-d2',    &
                    help='Final day of analysis. Optional, if not provided, it will be the total number of days',   &
                    required=.false.,   &
                    act='store',       &
                    def='0',           &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--domsize', &
                    switch_ab='-ds',    &
                    help='Number of grid cells within target region',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--nregions', &
                    switch_ab='-nreg',    &
                    help='Number of source regions',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   


        call cli%add(switch='--tracing_dt', &
                    switch_ab='-tdt',    &
                    help='Time interval of each-time back tracing. In seconds. Should exactly divide velocity_dt.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--max_traxing', &
                    switch_ab='-mtr',    &
                    help='Maximum times of back trajactory. In time intervals of back tracing. ',   &
                    required=.true.,   &
                    act='store',       &
                    def='0',           &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--nextpoint', &
                    switch_ab='-next',    &
                    help='Alhotitm to calculate next point (actually previous) in backtracing. 1 for iterative method,&
                           & and 2 for direct method',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   


        call cli%add(switch='--maxiter', &
                    switch_ab='-mit',    &
                    help='Maximum number of iterations for finding next point with iterative method. Default 101',   &
                    required=.false.,   &
                    act='store',       &
                    def = '101',       &
                    error=error_cli)
        if (error_cli/=0) stop   


        call cli%add(switch='--maxtol', &
                    switch_ab='-tol',    &
                    help='Maximum tolerance error in iterative method. Default 0.001',   &
                    required=.false.,   &
                    act='store',       &
                    def='0.001',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--velocity_dt', &
                    switch_ab='-vdt',    &
                    help='Wind speed dt. In hours. Must be a divisor of the number of 24 (number of hours in day)',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--verbose', &
                    switch_ab='-ver',    &
                    help='Print values of recycling ratio per day while running program. &
                            &.false. for deactivating verbose, .true. for activating it. Default  .true.',   &
                    required=.false.,   &
                    act='store',       &
                    def='.true.',           &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--writepaths', &
                switch_ab='-wp',    &
                help='Write paths of backtracing. .false. for deactivating it, .true. for activating it. Default .false.',   &
                required=.false.,   &
                act='store',       &
                def='.false.',           &
                error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--writepaths_interval', &
                switch_ab='-wpi',    &
                help='Write paths interval. sometimes writing all the points of paths can generate big files. &
                        & Use this option to only write the backtracing points at this interval. &
                        & Default 1 (i.e. write all time steps of backtracing)',   &
                required=.false.,   &
                act='store',       &
                def='1',           &
                error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--writepaths_delta', &
                switch_ab='-wpd',    &
                help='Write paths interval. sometimes writing all the points of paths can generate big files. &
                        & Use this option to only write the backtracing from one point of a&
                        & squre of wpi x wpi of the target region. Default 1 (i.e. all points)',   &
                required=.false.,   &
                act='store',       &
                def='1',           &
                error=error_cli)
        if (error_cli/=0) stop   


        call cli%add(switch='--writepaths_ij0', &
                switch_ab='-wp0',    &
                help='Write paths interval. sometimes writing all the points of paths can generate big files. &
                        & Use this option to set the grid values i,j (wp0,wp0) of the first point to write. &
                        & Following points will be calculated at a number of cells wpd of this initial point.&
                        & Default 1 (i.e. first point of grid). ',   &
                required=.false.,   &
                act='store',       &
                def='1',           &
                error=error_cli)
        if (error_cli/=0) stop   

        call cli%get(switch='-pi', val=path_input, error=error_cli)
        if (error_cli/=0) stop

        call cli%get(switch='-lf', val=listsfiles, error=error_cli)
        if (error_cli/=0) stop

        call cli%get(switch='-ET', val=filename_ET, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) //  filename_ET, file_ET, 0) 
        else 
            allocate(file_ET(1))
            file_ET(1) = trim(path_input) // filename_ET
        end if
        
        call cli%get(switch='-PP', val=filename_PP, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) // filename_PP, file_PP, 0) 
        else 
            allocate(file_PP(1))
            file_PP(1) = trim(path_input) // filename_PP
        end if

        call cli%get(switch='-PW', val=filename_PW, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) // filename_PW, file_PW, 0) 
        else 
            allocate(file_PW(1))
            file_PW(1) = trim(path_input) // filename_PW
        end if

        call cli%get(switch='-U', val=filename_U3, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) // filename_U3, file_U3, 0) 
        else 
            allocate(file_U3(1))
            file_U3(1) = trim(path_input) // filename_U3
        end if

        call cli%get(switch='-V', val=filename_V3, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) // filename_V3, file_V3, 0) 
        else 
            allocate(file_V3(1))
            file_V3(1) = trim(path_input) // filename_V3
        end if

        call cli%get(switch='-PWf', val=filename_PWflux, error=error_cli)
        if (error_cli/=0) stop
        if (listsfiles) then
            call read_list_files(trim(path_input) // filename_PWflux, file_PWflux, 0) 
        else 
            allocate(file_PWflux(1))
            file_PWflux(1) = trim(path_input) // filename_PWflux
        end if

        call cli%get(switch='-reg', val=filename_regions, error=error_cli)
        if (error_cli/=0) stop
        file_regions = trim(path_input) // filename_regions
        
        call cli%get(switch='-mask', val=filename_topomask, error=error_cli)
        if (error_cli/=0) stop
        file_topomask = trim(path_input) // filename_topomask

        call cli%get(switch='-it', val=filename_domain_i, error=error_cli)
        if (error_cli/=0) stop
        file_domain_i = trim(path_input) // filename_domain_i

        call cli%get(switch='-jt', val=filename_domain_j, error=error_cli)
        if (error_cli/=0) stop
        file_domain_j = trim(path_input) // filename_domain_j
        

        call cli%get(switch='-po', val=path_output, error=error_cli)
        if (error_cli/=0) stop

        call cli%get(switch='-pre', val=prefix_outfiles, error=error_cli)
        if (error_cli/=0) stop

        

        call cli%get(switch='-nx', val=bigx, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-ny', val=bigy, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-nd', val=ndays, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-dx', val=dx, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-dy', val=dy, error=error_cli)
        if (error_cli/=0) stop

        call cli%get(switch='-d1', val=day1, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-d2', val=day2, error=error_cli)
        if (error_cli/=0) stop
        if (day2 == 0)  day2 = ndays
        call cli%get(switch='-ds', val=domsize, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-nreg', val=nregions, error=error_cli)
        if (error_cli/=0) stop


        call cli%get(switch='-tdt', val=tracing_dt, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-mtr', val=max_tracing, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-next', val=option_next_point, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-mit', val=max_iteration, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-tol', val=tol_error_iteration, error=error_cli)
        if (error_cli/=0) stop


        call cli%get(switch='-vdt', val=velocity_dt, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-ver', val=verbose, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-wp', val=write_paths, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-wpi', val=step_write_paths, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-wpd', val=delta_write_paths, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-wp0', val=ij0_write_paths, error=error_cli)
        if (error_cli/=0) stop





        ! derived variables
        ! calculate number of days
        n_days_tracing = ceiling(max_tracing * tracing_dt * 1.0 / (60*60*24))
        n_dt_tracing = ceiling(max_tracing * tracing_dt * 1.0 / (60*60*velocity_dt))
        ! calculte number of dt (dt of velocities)


        allocate(MASK_TOPO(bigx,bigy,nslabs-1))
        allocate(MAP_REGIONS(bigx, bigy))
        ! allocate(PP(bigx,bigy,ndays))
        ! allocate(ET(bigx,bigy,ndays))
        ! allocate(PW(bigx,bigy,nslabs,ndays))
        ! allocate(U3(bigx,bigy,nslabs, ndays*24/velocity_dt))
        ! allocate(V3(bigx,bigy,nslabs, ndays*24/velocity_dt))
        allocate(domain_ij(domsize,2))
        ! allocate(PW_FLUX(bigx,bigy,nslabs-1,ndays))
        ! allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,ndays))

        allocate(temp_PP(bigx,bigy,n_days_tracing))
        allocate(temp_ET(bigx,bigy,n_days_tracing))
        allocate(temp_PW(bigx,bigy,nslabs,n_days_tracing))
        allocate(temp_U3(bigx,bigy,nslabs,n_dt_tracing ))     
        allocate(temp_V3(bigx,bigy,nslabs,n_dt_tracing ))
        allocate(temp_PW_FLUX(bigx,bigy,nslabs-1,n_days_tracing))
        ! allocate(temp_MASK_PW_FLUX(bigx,bigy,nslabs-1,n_days_tracing))

        !!!! ##########  TEMPORAL ########## !!!!!
        ! file_PP(1)  = trim(path_input) // "2018_months05to08_PP24.dat"
        ! file_PP(2)  = trim(path_input) // "2018_months09to12_PP24.dat"
        ! file_PP(3)  = trim(path_input) // "2019_months01to04_PP24.dat"
        ! file_PP(4)  = trim(path_input) // "2019_months05to08_PP24.dat"
        ! file_ET(1)  = trim(path_input) // "2018_months05to08_ET24.dat"
        ! file_ET(2)  = trim(path_input) // "2018_months09to12_ET24.dat"
        ! file_ET(3)  = trim(path_input) // "2019_months01to04_ET24.dat"
        ! file_ET(4)  = trim(path_input) // "2019_months05to08_ET24.dat"
        ! file_PW(1)  = trim(path_input) // "2018_months05to08_PW24.dat"
        ! file_PW(2)  = trim(path_input) // "2018_months09to12_PW24.dat"
        ! file_PW(3)  = trim(path_input) // "2019_months01to04_PW24.dat"
        ! file_PW(4)  = trim(path_input) // "2019_months05to08_PW24.dat"
        ! file_U3(1)  = trim(path_input) // "2018_months05to08_U.dat"
        ! file_U3(2)  = trim(path_input) // "2018_months09to12_U.dat"
        ! file_U3(3)  = trim(path_input) // "2019_months01to04_U.dat"
        ! file_U3(4)  = trim(path_input) // "2019_months05to08_U.dat"
        ! file_V3(1)  = trim(path_input) // "2018_months05to08_V.dat"
        ! file_V3(2)  = trim(path_input) // "2018_months09to12_V.dat"
        ! file_V3(3)  = trim(path_input) // "2019_months01to04_V.dat"
        ! file_V3(4)  = trim(path_input) // "2019_months05to08_V.dat"
        ! file_PWflux(1)  = trim(path_input) // '2018_months05to08_PWflux24.dat'
        ! file_PWflux(2)  = trim(path_input) // '2018_months09to12_PWflux24.dat'
        ! file_PWflux(3)  = trim(path_input) // '2019_months01to04_PWflux24.dat'
        ! file_PWflux(4)  = trim(path_input) // '2019_months05to08_PWflux24.dat'
        !!!! ##########  end TEMPORAL ########## !!!!!


        ! file_regions = trim(path_input) // filename_regions !  , file_topomask ! &
        ! file_topomask = trim(path_input) // filename_topomask
        ! file_domain_i = trim(path_input) // filename_domain_i
        ! file_domain_j = trim(path_input) // filename_domain_j
        filename_rhodomain_daily = trim(prefix_outfiles) // filename_rhodomain_daily
        file_rhodomain_daily = trim(path_output) // filename_rhodomain_daily
        print *, file_rhodomain_daily 

        filename_rhocolumn_domain_daily = trim(prefix_outfiles) // filename_rhocolumn_domain_daily
        file_rhocolumn_domain_daily = trim(path_output) // filename_rhocolumn_domain_daily
        print *, file_rhocolumn_domain_daily

        filename_rhocolumn_domain_timeavg = trim(prefix_outfiles) // filename_rhocolumn_domain_timeavg
        file_rhocolumn_domain_timeavg = trim(path_output) // filename_rhocolumn_domain_timeavg
        print *, file_rhocolumn_domain_timeavg

        filename_precipareal_domain_daily = trim(prefix_outfiles) // filename_precipareal_domain_daily
        file_precipareal_domain_daily = trim(path_output) // filename_precipareal_domain_daily
        print *, file_precipareal_domain_daily

        filename_paths_xy = trim(prefix_outfiles) // filename_paths_xy
        file_paths_xy = trim(path_output) // filename_paths_xy
        print *, file_paths_xy

    end subroutine

    subroutine read_list_files(text_file, files, rem_file) 
        ! https://fortran-lang.discourse.group/t/allocation-of-array-for-reading-strings-from-a-text-file/5986/4
        ! type :: vlstring_t
        ! character(len=:), allocatable :: string
        ! end type
        integer :: iun, nrecs, istat
        character(len=*), intent(in)  :: text_file
        character(len=200), intent(out), allocatable :: files(:)
        integer, intent(in)               :: rem_file
        character(len=200) :: buffer
        character(len=1) :: dummy
        integer          :: i
        character(len=200), allocatable :: files_temp(:)

        ! print * , text_file
        open(newunit=iun, file=text_file, form="formatted", status = "old")
        ! count number of records (note you don't have to read the line of text just the first char)
        nrecs = 0
        count_loop: do
            read(iun, '(a1)', iostat=istat) dummy
            if (istat /= 0) exit count_loop
            nrecs = nrecs+1
        end do count_loop

        allocate(files(nrecs))
        rewind(iun)
        read_loop: do i=1,nrecs
            buffer=repeat(" ", 200)
            read(iun,'(a)', iostat=istat) buffer
            ! print *,buffer
            if (istat /= 0) then
                print *,' error while reading text file'
                exit read_loop
            end if
            files(i) = trim(buffer)
        end do read_loop
        close(iun)

        ! only in case removing file
        if (rem_file == 1 .or. rem_file == 2) then
            allocate(files_temp(nrecs - 1))
            if (rem_file == 1) then
                files_temp = files(2:)
            else if (rem_file == 2) then
                files_temp = files(:size(files)-1)
            end if
            deallocate(files)
            allocate(files(nrecs-1))
            files = files_temp
            deallocate(files_temp)
        end if
    end subroutine

    ! allocates arrays for the following files and also copies the last data of the last files (for tracing purposes)
    subroutine allocate_global_arrays(day, max_day_array, i_file)
        use global_data
        integer, intent(in)  :: day
        integer, intent(out) :: max_day_array
        integer, intent(in) :: i_file

        integer           :: n_days_file
        integer           :: min_day_array
        integer           :: min_dt_array, max_dt_array
        ! select 
        
        n_days_file = get_ndays_file_PP(i_file)
        print *,"n_days_file=",n_days_file
        ! end select
        


        if (i_file == 1) then
            allocate(PP(bigx,bigy,n_days_file))
            allocate(ET(bigx,bigy,n_days_file))
            allocate(PW(bigx,bigy,nslabs,n_days_file))
            allocate(U3(bigx,bigy,nslabs, n_days_file*24/velocity_dt))     
            allocate(V3(bigx,bigy,nslabs, n_days_file*24/velocity_dt))
            allocate(PW_FLUX(bigx,bigy,nslabs-1,n_days_file))
            allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,n_days_file))
            max_day_array = n_days_file
        else 
            max_dt_array = max_day_array *24 / velocity_dt

            temp_PP = PP(:,:,max_day_array-n_days_tracing+1:max_day_array)
            temp_ET = ET(:,:,max_day_array-n_days_tracing+1:max_day_array)
            temp_PW = PW(:,:,:,max_day_array-n_days_tracing+1:max_day_array)
            temp_U3 = U3(:,:,:,max_dt_array-n_dt_tracing+1:max_dt_array)
            temp_V3 = V3(:,:,:,max_dt_array-n_dt_tracing+1:max_dt_array)
            temp_PW_FLUX = PW_FLUX(:,:,:,max_day_array-n_days_tracing+1:max_day_array)
            ! temp_MASK_PW_FLUX = MASK_PW_FLUX(:,:,:,max_day_array-n_days_tracing+1:max_day_array)

            deallocate(PP)
            deallocate(ET)
            deallocate(PW)
            deallocate(U3)
            deallocate(V3)
            deallocate(PW_FLUX)
            deallocate(MASK_PW_FLUX)
            min_day_array = day - n_days_tracing
            max_day_array = day + n_days_file -1 
            ! min_dt_array = (min_day_array - 1) * 24/velocity_dt + 1
            min_dt_array = (day- 1) * 24/velocity_dt + 1 - n_dt_tracing
            max_dt_array = max_day_array * 24/velocity_dt
            allocate(PP(bigx,bigy,min_day_array:max_day_array ))
            allocate(ET(bigx,bigy,min_day_array:max_day_array ))
            allocate(PW(bigx,bigy,nslabs,min_day_array:max_day_array))
            allocate(U3(bigx,bigy,nslabs, min_dt_array:max_dt_array))     
            allocate(V3(bigx,bigy,nslabs, min_dt_array:max_dt_array))
            allocate(PW_FLUX(bigx,bigy,nslabs-1,min_day_array:max_day_array))
            allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,min_day_array:max_day_array))
        
        end if

    end subroutine

    function get_ndays_file_PP(i_file) result(n_days)
        use global_data, only : nbytes_real,file_PP, bigx, bigy
        integer, intent(in)  :: i_file
        integer              :: file_size
        integer              :: n_days
        
        INQUIRE(FILE=file_PP(i_file), SIZE=file_size)

        n_days = file_size / (nbytes_real * bigx * bigy) 
    end function

    subroutine check_and_update_globalarrays(day, max_iday_array, i_file)
        integer, intent(in)     :: day
        integer, intent(inout)     :: max_iday_array
        integer, intent(inout)     :: i_file

        if (day > max_iday_array) then
            i_file = i_file + 1
            print *,'Reading data from files', i_file
            call allocate_global_arrays(day, max_iday_array, i_file)
            print *,"max_day_array= ",max_iday_array
            call openfile(day, i_file)  
            call daily_FLUX()
        end if
    end subroutine

    ! reads  MASK_TOPO, MAP_REGIONS
    subroutine readmap()
        use global_data, only : MASK_TOPO, MAP_REGIONS, bigx, bigy, file_regions, file_topomask

        real,dimension(bigx,bigy) :: regions2b

        open(113, file = file_topomask,status = "old", form = "unformatted", access = "stream")
        read(113) MASK_TOPO
        close(113)
        write(*,*) 'Topography Mask has been loaded.'

        open(112,file=file_regions,status='old',form='unformatted',access='stream')
        read(112) regions2b
        close(112)
        write(*,*) 'Regions Map has been loaded.'

        MAP_REGIONS = int(regions2b)
          
    end subroutine


    ! reads PP, ET, PW, U3, V3, W3U, W3D, QLIMIT
    subroutine openfile(day, i_file)
        use global_data, only : PP, ET, PW, U3, V3, PW_FLUX, &
                                file_PP, file_ET, file_PW, file_U3, file_V3, file_PWflux,&
                                bigx, bigy, ndays, &
                                temp_PP, temp_ET,temp_PW,temp_U3, temp_V3, temp_PW_FLUX, n_days_tracing, &
                                velocity_dt
        integer, intent(in)  :: day, i_file
        integer :: i,j,k
        integer :: i_dt

        ! real(4) :: startTime, stopTime
        i_dt = (day  -1 )* 24 / velocity_dt + 1

        ! call cpu_time(startTime)
        open(10, file = file_PP(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) PP
            close(10)
        else
            PP(:,:,:day-1) = temp_PP
            read(10) PP(:,:,day:)
            close(10)
        end if

        
        open(10, file = file_ET(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) ET
            close(10)
        else
            ET(:,:,:day-1) = temp_ET
            read(10) ET(:,:,day:)
            close(10)
        end if

        write(*,'(A20)') 'ET/PP have been read'

        ! correction ET
        ET = max(ET, 0.0)
        ! do k = 1, ndays
        !     do j = 1, bigy
        !       do i = 1, bigx
        !         ET(i,j,k) = max(ET(i,j,k),0.0)
        !       end do
        !     end do
        ! end do
        

        write(*,*) 'ET/PP correction done'
        ! call cpu_time(stopTime)
        ! print *, "Total time new: ", stopTime - startTime

        open(10,  file = file_PW(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) PW
            close(10)
        else
            PW(:,:,:,:day-1) = temp_PW
            read(10) PW(:,:,:,day:)
            close(10)
        end if



        open(10,  file = file_U3(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) U3
            close(10)
        else
            U3(:,:,:,:i_dt-1) = temp_U3
            read(10) U3(:,:,:,i_dt:)
            close(10)
        end if


        open(10,  file = file_V3(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) V3
            close(10)
        else
            V3(:,:,:,:i_dt-1) = temp_V3
            read(10) V3(:,:,:,i_dt:)
            close(10)
        end if


        open(10,  file = file_PWflux(i_file), status='old',form='unformatted',access='stream')
        if (i_file == 1) then
            read(10) PW_FLUX
            close(10)
        else
            PW_FLUX(:,:,:,:day-1) = temp_PW_FLUX
            read(10) PW_FLUX(:,:,:,day:)
            close(10)
        end if


        ! print *, U3(30,100,25,1)

    end subroutine


    ! reads domain
    subroutine monsoon()
        use global_data, only: domain_ij, domsize, file_domain_i, file_domain_j
        real, dimension(domsize,2) :: domain2
        integer :: ii
        
        open(14,file=file_domain_i,status='old',form='unformatted',access='direct',recl=4)
        open(15,file=file_domain_j,status='old',form='unformatted',access='direct',recl=4)
        do ii = 1,domsize
            read(14,rec=ii) domain2(ii,1)
            read(15,rec=ii) domain2(ii,2)
        enddo
        domain_ij = int(domain2)
      
        write(*,*) 'Study region has been loaded.'
        close(14)
        close(15)
    end subroutine


    subroutine daily_FLUX()
        use global_data, only : PW_FLUX, MASK_PW_FLUX, velocity_dt, bigx, bigy, nslabs!, n_days
        ! use global_data, only : QLIMIT, W3U, W3D, PW_FLUX, MASK_PW_FLUX, &
        !                         ndays, velocity_dt, bigx, bigy, ndays, NAN_value, nslabs
        integer  :: i, j, istep_start, istep_end, istep, iday, i_interslab, flag1
        integer   :: day_min, day_max
        ! real(4)     :: sum_day  
        day_min = lbound(PW_FLUX,4)
        day_max = ubound(PW_FLUX,4)
        do iday = day_min,day_max 
            do i_interslab = 1,nslabs-1
                do j = 1,bigy
                    do i = 1,bigx
                        if (isnan(PW_FLUX(i,j,i_interslab,iday))) then
                            MASK_PW_FLUX(i,j,i_interslab,iday) = 0
                        else if (PW_FLUX(i,j,i_interslab,iday) >= 0 ) then
                            MASK_PW_FLUX(i,j,i_interslab,iday) = 1
                        else 
                            MASK_PW_FLUX(i,j,i_interslab,iday) = 2
                        end if
                    end do
                end do
            end do 
        end do

        PW_FLUX = PW_FLUX / (24/velocity_dt)   ! ' converted to daily averages with mm/3h unite.'
        


        ! do iday = 1, ndays
        !     istep_start = (iday-1)*24/velocity_dt+1
        !     istep_end = iday*24/velocity_dt

        !     do j = 1,bigy
        !         do i = 1,bigx
        !             do i_interslab = 1, nslabs-1
        !                 sum_day = 0.0 
        !                 flag1 = 0
        !                 do istep = istep_start, istep_end
        !                     if (isnan(QLIMIT(i,j,i_interslab,istep))) then
        !                         MASK_PW_FLUX(i,j,i_interslab,iday) = 0
        !                         PW_FLUX(i,j,i_interslab,iday) = NAN_value
        !                         flag1 = 1
        !                         exit
        !                     else
        !                         sum_day = sum_day + &
        !                             (QLIMIT(i,j,i_interslab,istep) * W3U(i,j,i_interslab,istep) + &
        !                             QLIMIT(i,j,i_interslab,istep) * W3D(i,j,i_interslab,istep) ) * &
        !                             3600 * velocity_dt           ! 3600 seg in 1 hour
        !                     end if
        !                 end do
        !                 if (flag1 == 0) then 
        !                     if (sum_day >=0) then
        !                         MASK_PW_FLUX(i,j,i_interslab, iday) = 1
        !                     else
        !                         MASK_PW_FLUX(i,j,i_interslab, iday) = 2
        !                     end if
        !                     PW_FLUX(i,j,i_interslab,iday) = sum_day/(24/velocity_dt)     ! 24 hours in 1 day
        !                 end if
        !             end do
        !         end do
        !     end do
        ! end do
        print *, "Precipitable water (moisture?) flux (PW_FLUX) calculated as daily averages with mm/3h units"
        ! print *, ' converted to daily averages with mm/3h unite.'
    end subroutine 


    subroutine write_rhodomain_daily(rho_domain)
        use global_data, only: day1,day2, nregions, nslabs, file_rhodomain_daily 
        real(4), dimension(:,:,:), intent(in)            :: rho_domain
        integer                                          :: day, iday, islab, i_region
        integer, dimension(nregions)                     :: id_regions
        ! character(10), dimension(nregions)               :: name_regions

        
        do i_region = 1, nregions
            id_regions(i_region) = i_region
        end do

        open(10, file = file_rhodomain_daily, status = "REPLACE")

        write(10,"(A3,A,A4,*(', ', I10))") "day", ",","slab", id_regions 
        do day = day1,day2
            iday = day - day1 +1
            do islab = 1,nslabs
                write(10,"(I3,A,I4,*(', ', F10.6))") day, ",", islab, rho_domain(:,islab, iday)
            end do
        end do
        close(10)
    end subroutine


    subroutine write_rhocolumn_domain_daily(rhocolumn_domain)
        use global_data, only: day1,day2, nregions, file_rhocolumn_domain_daily 
        real(4), dimension(:,:), intent(in)              :: rhocolumn_domain
        integer                                          :: day, iday, i_region
        integer, dimension(nregions)                     :: id_regions
        ! character(10), dimension(nregions)               :: name_regions

        do i_region = 1, nregions
            id_regions(i_region) = i_region
        end do

        open(10, file = file_rhocolumn_domain_daily, status = "REPLACE" )

        write(10,"(A3,*(', ', I10))") "day", id_regions 
        do day = day1,day2
            iday = day - day1 +1
            write(10,"(I3,*(', ', F10.6))") day, rhocolumn_domain(:, iday)
        end do
        close(10)
    end subroutine


    subroutine  write_rhodomain_timeavg(rhocolumn_domain_timeavg) 
        use global_data, only: nregions, file_rhocolumn_domain_timeavg
        real(4), dimension(:), intent(in)              :: rhocolumn_domain_timeavg
        integer                                          :: i_region
        integer, dimension(nregions)                     :: id_regions
        ! character(10), dimension(nregions)               :: name_regions

        open(10, file = file_rhocolumn_domain_timeavg, status = "REPLACE") 
        write(10, "(A10,', ', A10)") adjustl("id_region"), adjustl("rho")
        do i_region = 1, nregions
            write(10, "(I10,', ', F10.6)")   i_region, rhocolumn_domain_timeavg(i_region)
        end do
        close(10)
    end subroutine

    subroutine  write_rhodomain_timeavg2(rhocolumn_domain_timeavg,file_rho_domain_timeavg) 
        use global_data, only: nregions!, !file_rhocolumn_domain_timeavg
        real(4), dimension(:), intent(in)              :: rhocolumn_domain_timeavg
        integer                                          :: i_region
        integer, dimension(nregions)                     :: id_regions
        character(200), intent(in)                       :: file_rho_domain_timeavg
        ! character(10), dimension(nregions)               :: name_regions

        open(10, file = file_rho_domain_timeavg, status = "REPLACE") 
        write(10, "(A10,', ', A10)") adjustl("id_region"), adjustl("rho")
        do i_region = 1, nregions
            write(10, "(I10,', ', F10.6)")   i_region, rhocolumn_domain_timeavg(i_region)
        end do
        close(10)
    end subroutine


    subroutine  write_precipareal_daily(PP_areal_days)
        use global_data, only: day2, day1, file_precipareal_domain_daily
        real(4), dimension(day2-day1+1)    :: PP_areal_days
        integer                            :: iday

        open(10, file = file_precipareal_domain_daily, status = "REPLACE") 
        write(10, "(A10,', ', A10)") adjustl("day"), adjustl("pr_areal")
        do iday = day1, day2
            write(10, "(I10,', ', F12.6)")   iday, PP_areal_days(iday-day1 + 1)
        end do
        close(10)
    end subroutine

    ! step: every how many timesteps of the tracing_dt, it will be written
    subroutine write_path(path_xy, length_path, islab, iday, k, step)
        use global_data, only:   day1, file_paths_xy, domain_ij, delta_write_paths, ij0_write_paths
        real(4), dimension(:,:)  :: path_xy
        integer, intent(in)      :: islab, iday, length_path, k, step
        integer                  :: i_point
        logical, save            :: first_time = .true.

        if (delta_write_paths  /= 1 ) then
            if ((mod(domain_ij(k,1) - ij0_write_paths, delta_write_paths) /=0) .or. &
                (mod(domain_ij(k,2) - ij0_write_paths, delta_write_paths) /=0)) then
                return
            end if
            ! print *, "k = ", k, ",  i = ", domain_ij(k,1), ",  j = ", domain_ij(k,2)
        end if


        if (first_time) then
            open(10, file = file_paths_xy, status = "REPLACE") 
            write(10, "(A6,',', A4,', ', A5, ', ', A6,', ',A12, ', ', A12)") &
                    adjustl("day"), adjustl("slab"), adjustl("point"), adjustl("k"), adjustl("X"), adjustl("Y")
            first_time = .false.
        else
            open(10, file = file_paths_xy, status = "OLD", position = "APPEND")
        end if

        do i_point = 1, length_path
            if (mod(i_point-1, step) == 0) then
                write(10, "(I6,',',I4, ', ', I5, ', ', I6 ,', ', F12.2, ', ', F12.2)")  &
                                iday, islab, i_point, k, path_xy(1, i_point), path_xy(2, i_point)
            end if
        end do


        close(10)
        

    end subroutine

END MODULE subroutines_IO