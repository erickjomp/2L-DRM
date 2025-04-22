MODULE subroutines_IO

    implicit none

contains

    subroutine read_global_data()
        use global_data
        use FLAP
        use datetime_module

        type(command_line_interface)                 :: cli    ! Command Line Interface (CLI).
        integer                                      :: error_cli
        character(len=19)                            :: datetime0_str, datetime1_str, datetime2_str
        type(datetime)                               :: datetime2
        type(timedelta)                              :: td
        logical                                      :: is_passed
        ! bigx=1471 !253 ! 1471 !1471  !number of grids in x and y direction
        ! bigy=2027 !346 ! 2027 ! 2027
        ! n_datadt= 488 !196 ! 120+15 months-01to04 no leapyear ! 123+15  months-05to08  ! 122+15  months-09to12   ! CHANGE ! it seems it no longer affects
        ! dx=4000.00  !unit(m), the distance of each grid
        ! dy=4000.00
        ! !__________________________starting dates and ending dates,core region
        ! t_data_start=16
        ! t_data_end= 488 !196  !123+15  !t_data_start=16, t_data_end=   !date starts and ends simulation
        ! domsize=49509  !1386 nonCPM  !49509 CPM !49509  !number of grids within the monsoon region
        ! nregions = 8  ! 83148 cells landocean CPM  ! 31774 cells land CPM ! 8    !number of regions in map file    ! CHANGE 
        ! !______________________________________________________________________
        ! tracing_dt = 1800  !time interval of each-time back tracing
        ! max_tracing=600 !1500 ! maximum times of back trajactory(>48*31)
        ! option_next_point = 1 ! 1 for iterative method, and 2 for direct method
        ! max_iteration=101 ! maximum times of iteration
        ! tol_error_iteration = 0.0001 
        ! ! integer, save :: n_datadt_warmup = 15 !days that to be excluded at the beginning    ! CHANGE 
        ! UV_dt = 3 
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
                    help='Evapotranspiration (ET) binary file to read. The data has to have dimensions (nx, ny, nslabs, n_datadt)&
                            & with nslabs = 2.  &
                            &Data at daily time step. Units of mm.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop       

        call cli%add(switch='--filePP', &
                    switch_ab='-PP',    &
                    ! help='Text file containing the list of Precipitation (PP) binary files to read. The data has to have dimensions (nx, ny, 2, n_datadt). &
                    help='Precipitation (PP) binary files to read. The data has to have dimensions (nx, ny, nslabs, n_datadt) &
                            & with nslabs = 2. &
                            &Data at daily time step. Units of mm.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop  


        call cli%add(switch='--filePW', &
                    switch_ab='-PW',    &
                    ! help='Text file containing the list of Precipitable Water (PW) binary files. The data has to have dimensions (nx, ny, 2, n_datadt). &
                    help='Precipitable Water (PW) binary files. The data has to have dimensions (nx, ny, nslabs, n_datadt) &
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
                            &Data at UV_dt time step. Units of m/s.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop

        call cli%add(switch='--fileV', &
                    switch_ab='-V',    &
                    ! help='Text file containing the list of vapor weighted wind speed V (V) binary files to read. The data has to have dimensions (nx, ny, 2, ,n_timesteps). &
                    help='Wind speed V (V) binary files to read. The data has to have dimensions (nx, ny, nslabs, n_timesteps)&
                            & with nslabs = 2. &
                            &Data at UV_dt time step. Units of m/s.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop


        call cli%add(switch='--filePWflux', &
                    switch_ab='-PWf',    &
                    ! help='Text file containing the list of PW flux between slabs (PWflux) binary files to read. The data has to have dimensions (nx, ny, 2, ,n_datadt).&
                    help='PW flux between slabs (PWflux) binary files to read. The data has to have dimensions &
                            & (nx, ny, n_interslabs, n_datadt) with n_interslabs = 1.&
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

        call cli%add(switch='--nslabs', &
                    switch_ab='-ns',    &
                    help='Number of slabs. Does not work with solver 1 (analytical 2LDRM).',   &
                    required=.false.,   &
                    act='store',       &
                    def="2",           &
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

        ! call cli%add(switch='--n_datadt', &
        !             switch_ab='-nt',    &
        !             help='Number of time steps data_dt in data',   &
        !             required=.true.,   &
        !             act='store',       &
        !             error=error_cli)
        ! if (error_cli/=0) stop   

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

        ! call cli%add(switch='--t_data_start', &
        !             switch_ab='-t1',    &
        !             help='Day 1 of analysis. Since there should be some days for backtracing, it cant be 1.',   &
        !             required=.true.,   &
        !             act='store',       &
        !             error=error_cli)
        ! if (error_cli/=0) stop   
        call cli%add(switch='--datetime_start', &
                    switch_ab='-d1',    &
                    help='Initial datetime of analysis (i.e. First datetime for which backtracing trajectory will be calculated). &
                             &Since there should be some days for backtracing, it cant be the same same as datetime0.',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   
        

        ! call cli%add(switch='--t_data_end', &
        !             switch_ab='-t2',    &
        !             help='Final day of analysis. Optional, if not provided, it will be the total number of days',   &
        !             required=.false.,   &
        !             act='store',       &
        !             def='0',           &
        !             error=error_cli)
        ! if (error_cli/=0) stop   
        call cli%add(switch='--datetime_end', &
                    switch_ab='-d2',    &
                    ! help='Final datetime of analysis. Optional, if not provided, the final datetime of analysis will correspond&
                    ! & to the last datetime of the data.',   &
                    help='Final datetime of analysis. Does not have to match the maximum datetime present in the data &
                    &(length of data),&
                    & but it has to be equal or lower.',  &
                    required=.true.,   &   ! this should be optional. so if no present just count until the maximum
                    act='store',       &
                    ! def='0',           &
                    error=error_cli)
        if (error_cli/=0) stop  

        call cli%add(switch='--datetime0', &
                switch_ab='-d0',    &
                help='Initial datetime of data. Optional. Used for labelling outputs with datetimes. &
                      &Format example: 2018-06-14_00:00:00 ',   &
                required=.true.,   &
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
                    help='Time interval of each-time back tracing. In seconds. Should exactly divide UV_dt * 60  * 60.',   &
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

        call cli%add(switch='--solver', &
                switch_ab='-s',    &
                help='Enter 1 to solve differential equation analitically or 3 for solving with finite diferences without tracing &
                     &dt refining.', &
                required=.false.,   &
                act='store',       &
                def = '1',       &
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

        call cli%add(switch='--data_dt', &
                    switch_ab='-ddt',    &
                    help='PP, PW, ET, PW, PWflux dt. In hours',   &
                    required=.true.,   &
                    act='store',       &
                    error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--UV_dt', &
                    switch_ab='-udt',    &
                    help='Optional. Wind speed dt. In hours. Must be a divisor of data_dt (in hours). &
                         & If not provided, it takes the same value as --data_dt',   &
                    required=.false.,   &
                    act='store',       &
                    def='0',           &
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
                help='Write paths of backtracing. Enter .true. for activating it. The default is .false.  .&
                     & It may not work when parallelization is activated. ',   &
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
                        & squre of wpd x wpd of the target region. Default 1 (i.e. all points)',   &
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


        ! call cli%add(switch='--writepaths_datetimestart', &
        !         ! switch_ab='-wp0',    &
        !         help='Write paths interval. sometimes writing all the paths can generate big files. &
        !                 & Use this option to only write paths that are traced between &
        !                 & writepaths_datetimestart and writepaths_datetimeend',   &
        !         required=.false.,   &
        !         act='store',       &
        !         def='1',           &
        !         error=error_cli)
        ! if (error_cli/=0) stop   

        ! call cli%add(switch='--writepaths_datetimeend', &
        !         ! switch_ab='-wp0',    &
        !         help='Write paths interval. sometimes writing all the paths can generate big files. &
        !                 & Use this option to only write paths that are traced between &
        !                 & writepaths_datetimestart and writepaths_datetimeend',   &
        !         required=.false.,   &
        !         act='store',       &
        !         def='1',           &
        !         error=error_cli)
        ! if (error_cli/=0) stop   


        call cli%add(switch='--writerhogrid', &
                switch_ab='-wr',    &
                help='Write rho column grid, with dimensions (time, regions, ny, nx). &
                        & Not recommended when having many regions.', &
                required=.false.,   &
                act='store',       &
                def='.false.',           &
                error=error_cli)
        if (error_cli/=0) stop   

        call cli%add(switch='--writerhogrid_option', &
                switch_ab='-wro',    &
                help='Use option 2 to write rho column grid separated by months. &
                      &Option 1 (default) writes everything to 1 file.', &
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

        
        call cli%get(switch='-ns', val=nslabs, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-nx', val=bigx, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-ny', val=bigy, error=error_cli)
        if (error_cli/=0) stop
        ! call cli%get(switch='-nt', val=n_datadt, error=error_cli)
        ! if (error_cli/=0) stop
        call cli%get(switch='-dx', val=dx, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-dy', val=dy, error=error_cli)
        if (error_cli/=0) stop

        call cli%get(switch='-ddt', val=data_dt, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-udt', val=UV_dt, error=error_cli)
        if (error_cli/=0) stop
        if (UV_dt .eq. 0) then
            UV_dt = data_dt
        end if



        call cli%get(switch='-d0', val=datetime0_str, error=error_cli)
        if (error_cli/=0) stop
        datetime0 = strptime(datetime0_str,"%Y-%m-%d_%H:%M:%S")
        ! print *, datetime0 % isoformat()

        ! call cli%get(switch='-t1', val=t_data_start, error=error_cli)
        ! if (error_cli/=0) stop
        call cli%get(switch='-d1', val=datetime1_str, error=error_cli)
        if (error_cli/=0) stop
        datetime1 = strptime(datetime1_str,"%Y-%m-%d_%H:%M:%S")
        td = datetime1 - datetime0
        t_data_start = 1 + td%total_seconds() / (data_dt*60*60)
        ! print *, "#############    t1 = ",t_data_start, "##############"

        ! call cli%get(switch='-t2', val=t_data_end, error=error_cli)
        ! if (error_cli/=0) stop
        ! if (t_data_end == 0)  t_data_end = n_datadt
        call cli%get(switch='-d2', val=datetime2_str, error=error_cli)
        if (error_cli/=0) stop
        datetime2 = strptime(datetime2_str,"%Y-%m-%d_%H:%M:%S")
        td = datetime2 - datetime0
        t_data_end = 1 + td%total_seconds() / (data_dt*60*60)
        ! is_passed = cli%is_passed(switch='-d2')
        ! if (is_passed) then
        !     call cli%get(switch='-d2', val=datetime2_str, error=error_cli)
        !     if (error_cli/=0) stop
        !     datetime2 = strptime(datetime2_str,"%Y-%m-%d_%H:%M:%S")
        !     td = datetime2 - datetime0
        !     t_data_end = 1 + td%total_seconds() / (data_dt*60*60)
        !     ! if (t_data_end == 0)  t_data_end = n_datadt
        ! else
        !     t_data_end = n_datadt
        ! end if




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
        call cli%get(switch='-s', val=solver, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-tol', val=tol_error_iteration, error=error_cli)
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
        call cli%get(switch='-wr', val=write_rho_grid, error=error_cli)
        if (error_cli/=0) stop
        call cli%get(switch='-wro', val=write_rho_grid_option, error=error_cli)
        if (error_cli/=0) stop





        ! derived variables
        ! calculate number of days
        n_datadt_tracing = ceiling(max_tracing * tracing_dt * 1.0 / (60*60* data_dt))
        n_UVdt_tracing = ceiling(max_tracing * tracing_dt * 1.0 / (60*60*UV_dt))
        ! calculte number of dt (dt of velocities)


        allocate(MASK_TOPO(bigx,bigy,nslabs-1))
        allocate(MAP_REGIONS(bigx, bigy))
        ! allocate(PP(bigx,bigy,n_datadt))
        ! allocate(ET(bigx,bigy,n_datadt))
        ! allocate(PW(bigx,bigy,nslabs,n_datadt))
        ! allocate(U3(bigx,bigy,nslabs, n_datadt*24/UV_dt))
        ! allocate(V3(bigx,bigy,nslabs, n_datadt*24/UV_dt))
        allocate(domain_ij(domsize,2))
        ! allocate(PW_FLUX(bigx,bigy,nslabs-1,n_datadt))
        ! allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,n_datadt))

        allocate(temp_PP(bigx,bigy,n_datadt_tracing))
        allocate(temp_ET(bigx,bigy,n_datadt_tracing))
        allocate(temp_PW(bigx,bigy,nslabs,n_datadt_tracing))
        allocate(temp_U3(bigx,bigy,nslabs,n_UVdt_tracing ))     
        allocate(temp_V3(bigx,bigy,nslabs,n_UVdt_tracing ))
        allocate(temp_PW_FLUX(bigx,bigy,nslabs-1,n_datadt_tracing))
        ! allocate(temp_MASK_PW_FLUX(bigx,bigy,nslabs-1,n_datadt_tracing))

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
        file_rhodomain_daily = trim(path_output) // "/" // filename_rhodomain_daily
        ! print *, file_rhodomain_daily 

        filename_rhocolumn_domain_daily = trim(prefix_outfiles) // filename_rhocolumn_domain_daily
        file_rhocolumn_domain_daily = trim(path_output) //  "/" // filename_rhocolumn_domain_daily
        ! print *, file_rhocolumn_domain_daily

        filename_rhocolumn_domain_timeavg = trim(prefix_outfiles) // filename_rhocolumn_domain_timeavg
        file_rhocolumn_domain_timeavg = trim(path_output) // "/" // filename_rhocolumn_domain_timeavg
        ! print *, file_rhocolumn_domain_timeavg

        filename_precipareal_domain_daily = trim(prefix_outfiles) // filename_precipareal_domain_daily
        file_precipareal_domain_daily = trim(path_output) // "/" // filename_precipareal_domain_daily
        ! print *, file_precipareal_domain_daily

        filename_paths_xy = trim(prefix_outfiles) // filename_paths_xy
        file_paths_xy = trim(path_output) // "/" // filename_paths_xy
        ! print *, file_paths_xy

        filename_rhocolumn_grid = trim(prefix_outfiles) // filename_rhocolumn_grid
        file_rhocolumn_grid = trim(path_output) // "/" // filename_rhocolumn_grid

        filename_rhoslabs_grid = trim(prefix_outfiles) // filename_rhoslabs_grid
        file_rhoslabs_grid = trim(path_output) // "/" // filename_rhoslabs_grid

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
    subroutine allocate_global_arrays(i_file, min_day_array, newday1, max_day_array)
        use global_data
        integer, intent(out)  :: newday1
        integer, intent(inout):: max_day_array
        integer, intent(out)  :: min_day_array
        integer, intent(in)   :: i_file
        logical, save         :: is_first_readfile = .true.

        integer           :: n_datadt_file
        integer           :: max_day_array_old
        integer           :: min_dt_array, max_dt_array
        ! select 
        
        n_datadt_file = get_n_datadt_file_PP(i_file)
        print *,"n_datadt_file=",n_datadt_file
        ! end select
        


        if (is_first_readfile) then
            if (i_file == 1) then
                allocate(PP(bigx,bigy,n_datadt_file))
                allocate(ET(bigx,bigy,n_datadt_file))
                allocate(PW(bigx,bigy,nslabs,n_datadt_file))
                allocate(U3(bigx,bigy,nslabs, n_datadt_file* data_dt/UV_dt))     
                allocate(V3(bigx,bigy,nslabs, n_datadt_file* data_dt/UV_dt))
                allocate(PW_FLUX(bigx,bigy,nslabs-1,n_datadt_file))
                allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,n_datadt_file))
                max_day_array = n_datadt_file
                ! not necessary, but provided
                newday1 = 1
                min_day_array = 1
                ! ! end not necessary
            else
                allocate(PP(bigx,bigy,max_day_array+1:max_day_array+n_datadt_file))
                allocate(ET(bigx,bigy,max_day_array+1:max_day_array+n_datadt_file))
                allocate(PW(bigx,bigy,nslabs,max_day_array+1:max_day_array+n_datadt_file))
                allocate(U3(bigx,bigy,nslabs, (max_day_array)* data_dt/UV_dt + 1: (max_day_array + n_datadt_file)* data_dt/UV_dt))     
                allocate(V3(bigx,bigy,nslabs, (max_day_array)* data_dt/UV_dt + 1: (max_day_array + n_datadt_file)* data_dt/UV_dt))
                allocate(PW_FLUX(bigx,bigy,nslabs-1,max_day_array+1:max_day_array+n_datadt_file))
                allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,max_day_array+1:max_day_array+n_datadt_file))
                max_day_array = max_day_array + n_datadt_file
                ! not necessary, but provided
                newday1 = max_day_array+1
                min_day_array = max_day_array+1
                ! end not necessary
            end if
            is_first_readfile = .false.
        else 
            max_dt_array = max_day_array * data_dt/ UV_dt

            temp_PP = PP(:,:,max_day_array-n_datadt_tracing+1:max_day_array)
            temp_ET = ET(:,:,max_day_array-n_datadt_tracing+1:max_day_array)
            temp_PW = PW(:,:,:,max_day_array-n_datadt_tracing+1:max_day_array)
            temp_U3 = U3(:,:,:,max_dt_array-n_UVdt_tracing+1:max_dt_array)
            temp_V3 = V3(:,:,:,max_dt_array-n_UVdt_tracing+1:max_dt_array)
            temp_PW_FLUX = PW_FLUX(:,:,:,max_day_array-n_datadt_tracing+1:max_day_array)
            ! temp_MASK_PW_FLUX = MASK_PW_FLUX(:,:,:,max_day_array-n_datadt_tracing+1:max_day_array)

            deallocate(PP)
            deallocate(ET)
            deallocate(PW)
            deallocate(U3)
            deallocate(V3)
            deallocate(PW_FLUX)
            deallocate(MASK_PW_FLUX)
            ! min_day_array = day - n_datadt_tracing
            ! max_day_array = day + n_datadt_file -1 
            max_day_array_old = max_day_array
            newday1 = max_day_array_old + 1
            min_day_array = (max_day_array_old +1) - n_datadt_tracing
            max_day_array = (max_day_array_old +1) + n_datadt_file -1 
            ! min_dt_array = (min_day_array - 1) * 24/UV_dt + 1
            ! min_dt_array = (day- 1) * data_dt/UV_dt + 1 - n_UVdt_tracing
            min_dt_array = (max_day_array_old)* data_dt/UV_dt + 1 - n_UVdt_tracing
            max_dt_array = max_day_array * data_dt/UV_dt
            allocate(PP(bigx,bigy,min_day_array:max_day_array ))
            allocate(ET(bigx,bigy,min_day_array:max_day_array ))
            allocate(PW(bigx,bigy,nslabs,min_day_array:max_day_array))
            allocate(U3(bigx,bigy,nslabs, min_dt_array:max_dt_array))     
            allocate(V3(bigx,bigy,nslabs, min_dt_array:max_dt_array))
            allocate(PW_FLUX(bigx,bigy,nslabs-1,min_day_array:max_day_array))
            allocate(MASK_PW_FLUX(bigx,bigy,nslabs-1,min_day_array:max_day_array))
        
        end if

    end subroutine

    function get_n_datadt_file_PP(i_file) result(n_datadt)
        use global_data, only : nbytes_real,file_PP, bigx, bigy
        integer, intent(in)  :: i_file
        integer(8)           :: file_size
        integer              :: n_datadt
        
        INQUIRE(FILE=file_PP(i_file), SIZE=file_size)
        
        n_datadt = file_size / (nbytes_real * bigx * bigy) 
    end function

    subroutine check_and_update_globalarrays(day, max_iday_array, i_file)
        use global_data, only :    n_datadt_tracing
        integer, intent(in)     :: day
        integer, intent(inout)     :: max_iday_array
        integer, intent(inout)     :: i_file
        integer                    :: min_iday_array, newday1
        integer                    :: n_datadt_file


        do while (day > max_iday_array) !then
            i_file = i_file + 1
            print *,'Reading data from files', i_file
            n_datadt_file = get_n_datadt_file_PP(i_file)

            if ((max_iday_array + n_datadt_file) < (day - n_datadt_tracing)) then  ! to not load dataset if not necessary
                if (i_file == 1) then
                    max_iday_array = n_datadt_file
                else
                    max_iday_array = max_iday_array + n_datadt_file
                end if
                
                cycle
            end if

            call allocate_global_arrays(i_file,min_iday_array, newday1, max_iday_array)
            ! print *,"min_day_array= ",min_iday_array
            print *,"max_datadt_array= ",max_iday_array
            ! print *, "newday1 = ",newday1
            call read_data(newday1, i_file)  
            ! print *, "newday1 = ",newday1
            call daily_FLUX()
        end do
    end subroutine

    ! reads  MASK_TOPO, MAP_REGIONS
    subroutine read_masks()
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
    subroutine read_data(day, i_file)
        use global_data, only : PP, ET, PW, U3, V3, PW_FLUX, &
                                file_PP, file_ET, file_PW, file_U3, file_V3, file_PWflux,&
                                bigx, bigy, n_datadt, &
                                temp_PP, temp_ET,temp_PW,temp_U3, temp_V3, temp_PW_FLUX, n_datadt_tracing, &
                                UV_dt, data_dt, nslabs
        USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE, ieee_quiet_nan, ieee_value, IEEE_IS_NAN  ! for TEMPORAL SOLUTIOM

        integer, intent(in)   :: day, i_file
        integer               :: i,j,k
        integer               :: i_dt
        logical, save         :: is_first_readfile = .true.
        ! START REMOVE
        ! TEMPORAL SOLUTION
        integer  :: max_datadt_array, min_datadt_array,i_datadt
        real(4)  :: nan
        nan = ieee_value( nan, ieee_quiet_nan )
        ! END REMOVE
        ! real(4) :: startTime, stopTime
        i_dt = (day  -1 )* data_dt / UV_dt + 1

        ! call cpu_time(startTime)
        open(10, file = file_PP(i_file), status='old',form='unformatted',access='stream')
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) PP
            close(10)
        else
            PP(:,:,:day-1) = temp_PP
            read(10) PP(:,:,day:)
            close(10)
        end if

        
        open(10, file = file_ET(i_file), status='old',form='unformatted',access='stream')
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) ET
            close(10)
        else
            ET(:,:,:day-1) = temp_ET
            read(10) ET(:,:,day:)
            close(10)
        end if
        ! ET = ET / 8 ! MODIFIED2025

        write(*,'(A20)') 'ET/PP have been read'

        ! correction ET
        ET = max(ET, 0.0)
        ! do k = 1, n_datadt
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
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) PW
            close(10)
        else
            PW(:,:,:,:day-1) = temp_PW
            read(10) PW(:,:,:,day:)
            close(10)
        end if

        ! START REMOVE
        ! TEMPORAL SOLUTION ! there should not be negative PW (they only appeared rarely at nonCPM 600hPa, were very small too) (check preprocessing tool)
        max_datadt_array = ubound(PW,dim = 4)
        ! min_datadt_array = lbound(PW,dim = 4)
        ! print *,"day = ", day
        ! print *,"min_datadt_array = ",min_datadt_array
        ! print *,"max_datadt_array = ",max_datadt_array
        do i_datadt = day, max_datadt_array
            do  k = 1,nslabs
                do j = 1,bigy
                    do i = 1,bigx
                        if (PW(i,j,k,i_datadt) < 0) then
                            PW(i,j,k,i_datadt) = nan
                        end if
                    end do
                end do
            end do
        end do
        ! END REMOVE

        open(10,  file = file_U3(i_file), status='old',form='unformatted',access='stream')
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) U3
            close(10)
        else
            U3(:,:,:,:i_dt-1) = temp_U3
            read(10) U3(:,:,:,i_dt:)
            close(10)
        end if


        open(10,  file = file_V3(i_file), status='old',form='unformatted',access='stream')
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) V3
            close(10)
        else
            V3(:,:,:,:i_dt-1) = temp_V3
            read(10) V3(:,:,:,i_dt:)
            close(10)
        end if


        open(10,  file = file_PWflux(i_file), status='old',form='unformatted',access='stream')
        ! if (i_file == 1) then
        if (is_first_readfile) then
            read(10) PW_FLUX
            close(10)
        else
            PW_FLUX(:,:,:,:day-1) = temp_PW_FLUX
            read(10) PW_FLUX(:,:,:,day:)
            close(10)
        end if

        if (is_first_readfile) then
            is_first_readfile = .false.
        end if
        ! print *, U3(30,100,25,1)

    end subroutine


    ! reads domain
    subroutine read_targetregion()
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
        use global_data, only : PW_FLUX, MASK_PW_FLUX, UV_dt, bigx, bigy, nslabs, data_dt, UV_dt !, n_datadt
        ! use global_data, only : QLIMIT, W3U, W3D, PW_FLUX, MASK_PW_FLUX, &
        !                         n_datadt, UV_dt, bigx, bigy, n_datadt, NAN_value, nslabs
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

        if (data_dt .ne. UV_dt) then
            PW_FLUX = PW_FLUX / (data_dt/UV_dt)   ! ' converted to daily averages with mm/UV_dt unite.'
        end if


        ! do iday = 1, n_datadt
        !     istep_start = (iday-1)*24/UV_dt+1
        !     istep_end = iday*24/UV_dt

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
        !                             3600 * UV_dt           ! 3600 seg in 1 hour
        !                     end if
        !                 end do
        !                 if (flag1 == 0) then 
        !                     if (sum_day >=0) then
        !                         MASK_PW_FLUX(i,j,i_interslab, iday) = 1
        !                     else
        !                         MASK_PW_FLUX(i,j,i_interslab, iday) = 2
        !                     end if
        !                     PW_FLUX(i,j,i_interslab,iday) = sum_day/(24/UV_dt)     ! 24 hours in 1 day
        !                 end if
        !             end do
        !         end do
        !     end do
        ! end do
        print *, "Precipitable water (moisture?) flux (PW_FLUX) calculated as daily averages with mm/UV_dt units"
        ! print *, ' converted to daily averages with mm/UV_dt unite.'
    end subroutine 


    subroutine write_rhodomain_daily(rho_domain)
        use global_data, only: t_data_start,t_data_end, nregions, nslabs, file_rhodomain_daily 
        real(4), dimension(:,:,:), intent(in)            :: rho_domain
        integer                                          :: day, iday, islab, i_region
        integer, dimension(nregions)                     :: id_regions
        ! character(10), dimension(nregions)               :: name_regions

        
        do i_region = 1, nregions
            id_regions(i_region) = i_region
        end do

        open(10, file = file_rhodomain_daily, status = "REPLACE")

        write(10,"(A3,A,A4,*(', ', I10))") "day", ",","slab", id_regions 
        do day = t_data_start,t_data_end
            iday = day - t_data_start +1
            do islab = 1,nslabs
                write(10,"(I3,A,I4,*(', ', F10.6))") day, ",", islab, rho_domain(:,islab, iday)
            end do
        end do
        close(10)
    end subroutine


    subroutine write_rhocolumn_domain_daily(rhocolumn_domain)
        use global_data, only: t_data_start,t_data_end, nregions, file_rhocolumn_domain_daily, &
                               data_dt, datetime0
        use csv_module
        use datetime_module
        ! use 

        real(4), dimension(:,:), intent(in)              :: rhocolumn_domain
        integer                                          :: t_data, i_t_data, i_region!, iday
        integer, dimension(nregions)                     :: id_regions
        ! character(10), dimension(nregions)               :: name_regions
        type(csv_file) :: f
        logical :: status_ok
        type(datetime)                                   :: datetime_c

        do i_region = 1, nregions
            id_regions(i_region) = i_region
        end do

        ! set optional inputs:
        call f%initialize(verbose = .true., enclose_strings_in_quotes = .false.)
        ! open the file
        call f%open(file_rhocolumn_domain_daily,n_cols=size(id_regions)+1,&
                    status_ok=status_ok) ! default REPLACe not append, append=.true. for appending 
        ! add header
        call f%add("datetime")
        call f%add(id_regions, int_fmt = "(I7)", trim_str = .false.)
        ! call f%add("datetime")
        ! call f%add("id_region")
        ! call f%add("rho")
        ! call f%add("traced_precip")
        ! call f%add("precip")
        ! print "(I25)", 20

        call f%next_row()
        ! add some data:
        do t_data = t_data_start, t_data_end
            datetime_c = datetime0 + timedelta(hours = data_dt * (t_data - 1))
            call f%add(datetime_c%strftime("%Y-%m-%dT%H:%M:%S"))
            i_t_data = t_data - t_data_start + 1 
            do i_region = 1, nregions
                call f%add(rhocolumn_domain(i_region, i_t_data),real_fmt='(F7.5)')
            end do
            call f%next_row()
            ! do i_region = 1,nregions
            !     call f%add(datetime_c%strftime("%Y-%m-%dT%H:%M:%S"))
            !     call f%add(i_region, int_fmt = "(I7)")
            !     i_t_data = t_data - t_data_start + 1 
            !     call f%add(rhocolumn_domain(i_region, i_t_data),real_fmt='(F7.5)')
            !     call f%add(rhocolumn_domain(i_region, i_t_data),real_fmt='(F7.5)')
            !     ! call f%add()
            !     call f%next_row()
            ! end do
            ! call f%add(datetime_c%strftime("%Y-%m-%dT%H:%M:%S"))
            ! i_t_data = t_data - t_data_start + 1 
            ! call f%add(rhocolumn_domain(:, i_t_data),real_fmt='(F7.5)')
            ! call f%next_row()
        end do
        
        ! call f%add([1.0_wp,2.0_wp,3.0_wp],real_fmt='(F5.3)')
        ! call f%add(.true.)
        ! call f%next_row()
        ! call f%add([4.0_wp,5.0_wp,6.0_wp],real_fmt='(F5.3)')
        ! call f%add(.false.)
        ! call f%next_row()
        ! finished
        call f%close(status_ok)

        ! open(10, file = file_rhocolumn_domain_daily, status = "REPLACE" )
        ! write(10,"(A3,*(', ', I10))") "day", id_regions 
        ! do day = t_data_start,t_data_end
        !     iday = day - t_data_start +1
        !     write(10,"(I6,*(', ', F10.6))") day, rhocolumn_domain(:, iday)
        ! end do
        ! close(10)
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
        use global_data, only: t_data_end, t_data_start, file_precipareal_domain_daily, data_dt, datetime0
        use csv_module
        use datetime_module
        
        real(4), dimension(t_data_end-t_data_start+1)    :: PP_areal_days
        integer                                          :: t_data, i_t_data
        type(csv_file)                                   :: f
        logical                                          :: status_ok
        type(datetime)                                   :: datetime_c

        ! set optional inputs:
        call f%initialize(verbose = .true., enclose_strings_in_quotes = .false.)
        ! open the file
        call f%open(file_precipareal_domain_daily,n_cols=2,&
                    status_ok=status_ok) ! default REPLACe not append, append=.true. for appending 
        ! add header
        call f%add("datetime")
        call f%add("prareal")
        call f%next_row()

        do t_data = t_data_start, t_data_end
            datetime_c = datetime0 + timedelta(hours = data_dt * (t_data - 1))
            call f%add(datetime_c%strftime("%Y-%m-%dT%H:%M:%S"))
            i_t_data = t_data - t_data_start + 1 
            call f%add(PP_areal_days(i_t_data),real_fmt='(F9.5)')
            call f%next_row()
        end do
        call f%close(status_ok)

        ! call f%add("datetime")
        ! open(10, file = file_precipareal_domain_daily, status = "REPLACE") 
        ! write(10, "(A10,', ', A10)") adjustl("day"), adjustl("pr_areal")
        ! do t_data = t_data_start, t_data_end
        !     write(10, "(I10,', ', F12.6)")   t_data, PP_areal_days(iday-t_data_start + 1)
        ! end do
        ! close(10)
    end subroutine

    ! step: every how many timesteps of the tracing_dt, it will be written
    subroutine write_path(path_xy, length_path, islab, t_data, k, step,datetime0)
        use global_data, only:   t_data_start, t_data_end, file_paths_xy, domain_ij, delta_write_paths, &
                                 ij0_write_paths, tracing_dt
        use datetime_module
        use csv_module
        real(4), dimension(:,:)  :: path_xy
        integer, intent(in)      :: islab, t_data, length_path, k, step
        integer                  :: i_point
        logical                  :: status_ok
        logical, save            :: first_time = .true.
        type(datetime)           :: datetime0, datetime_tr
        type(csv_file),save      :: fpaths

        if (delta_write_paths  /= 1 ) then
            if ((mod(domain_ij(k,1) - ij0_write_paths, delta_write_paths) /=0) .or. &
                (mod(domain_ij(k,2) - ij0_write_paths, delta_write_paths) /=0)) then
                return
            end if
            ! print *, "k = ", k, ",  i = ", domain_ij(k,1), ",  j = ", domain_ij(k,2)
        end if
        
        ! set optional inputs:
        call fpaths%initialize(verbose = .true., enclose_strings_in_quotes = .false.)
        ! open the file
        if (first_time) then
            call fpaths%open(file_paths_xy,n_cols=7,&
                        status_ok=status_ok) ! default REPLACe not append, append=.true. for appending 
            first_time = .false.
        else
            call fpaths%open(file_paths_xy,n_cols=7,&
                        status_ok=status_ok, append = .true.) ! default REPLACe not append, append=.true. for appending 
        end if
        ! add header
        ! call fpaths%add(["t_data_start","datetime", "slab","point","k","X","Y"])
        call fpaths%add("dt_data_start")
        call fpaths%add("datetime")
        call fpaths%add("slab")
        call fpaths%add("point")
        call fpaths%add(["k","X","Y"])
        call fpaths%next_row()
        
        
        ! if (first_time) then
        !     open(151, file = file_paths_xy, status = "REPLACE") 
        !     write(151, "(A6,',', A20, ', ', A4,', ', A5, ', ', A6,', ',A12, ', ', A12)") &
        !             adjustl("t_data"), adjustl("datetime_start"),adjustl("slab"), adjustl("point"), adjustl("k"), adjustl("X"), adjustl("Y")
        !     first_time = .false.
        ! else
        !     ! open(151, file = file_paths_xy, status = "OLD", position = "APPEND")
        ! end if

        do i_point = 1, length_path
            if ((t_data >= 32) .and. (t_data <= 38)) then
              ! nothing
            else 
                cycle
            end if
            
            if (mod(i_point-1, step) == 0) then
                ! write(10, "(I6,',',I4, ', ', I5, ', ', I6 ,', ', F12.2, ', ', F12.2)")  &
                !                 iday, datetime_tr%strftime("%Y-%m-%dT%H:%M:%S"), islab, i_point, k, path_xy(1, i_point), path_xy(2, i_point)
                call fpaths%add(t_data)  
                datetime_tr = datetime0 - timedelta(seconds = tracing_dt * (i_point - 1))
                call fpaths%add(datetime_tr%strftime("%Y-%m-%dT%H:%M:%S"))
                call fpaths%add([islab,i_point,k])
                call fpaths%add([path_xy(1, i_point), path_xy(2, i_point)])
                call fpaths%next_row()
            end if
        end do

        call fpaths%close(status_ok)
        ! if (t_data == t_data_end) then
        !     call fpaths%close(status_ok)
        ! end if
        ! close(10)

    end subroutine


    subroutine write_rho_grid_sr(rhocolumn, datetime_c)
        
        use global_data, only:   t_data_start, t_data_end, nregions, domain_ij, &
                                 domsize, bigx, bigy, file_rhocolumn_grid, write_rho_grid_option, &
                                 filename_rhocolumn_grid_base, data_dt, filename_rhocolumn_grid, &
                                 path_output, prefix_outfiles

        use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN
        use, intrinsic :: iso_fortran_env, only: real32
        use datetime_module
        implicit none
        ! real(real32) :: nan
        real(4)                                           :: nan        
        logical, save                                     :: first_time = .true.
        integer(4), save                                  :: count, i, j, i_reg, k
        real(4), dimension(nregions,domsize),intent(in)   :: rhocolumn
        real(4), dimension(bigx, bigy, nregions)          :: rhocolumn_grid
        type(datetime), intent(in)                        :: datetime_c
        type(datetime)                                    :: datetime2
        integer, save                                     :: ndays_month, year, month
        integer,save                                      :: old_month = 0

        nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)
        ! old_month = 0
        if (write_rho_grid_option == 2) then
            
            month = datetime_c%getMonth()
            
            if (old_month .ne. month) then
                year = datetime_c%getYear()
                ndays_month = daysInMonth(month, year)
                datetime2 = datetime_c + timedelta(hours = ndays_month * 24  - data_dt)

                first_time = .true.
                
                filename_rhocolumn_grid = trim(filename_rhocolumn_grid_base) // "_" // &
                                         datetime_c%strftime("%Y-%m-%dT%H:%M:%S") // "_TO_" // &
                                         datetime2%strftime("%Y-%m-%dT%H:%M:%S") // ".dat"
                filename_rhocolumn_grid = trim(prefix_outfiles) // filename_rhocolumn_grid
                file_rhocolumn_grid = trim(path_output) // "/" // filename_rhocolumn_grid
            end if

            old_month = month
        end if



        rhocolumn_grid = nan
        do i_reg = 1,nregions
            do k = 1, domsize
                i=domain_ij(k,1)
                j=domain_ij(k,2) 
                rhocolumn_grid(i,j,i_reg) = rhocolumn(i_reg, k)  

                ! if ((count == t_data_start + 30 - 1) .and. i == 51 .and. j == 48) then
                !     print *, "count = ", count
                !     print *, "rho = ", rhocolumn_grid(51,48,1)
                !     print *, "k = ", k
                ! end if
            end do
        end do



        if (first_time) then
            open(120, file = file_rhocolumn_grid, access = "STREAM",&
                 form = "UNFORMATTED", status = "REPLACE") 
            write(120) rhocolumn_grid
            count = t_data_start
            first_time = .false.
        else
            write(120) rhocolumn_grid
            count = count + 1
        end if


        if (count == t_data_end) then
            close(120) 
        end if

        


    end subroutine



    subroutine write_rhoslabs_grid_sr(rhoslabs)
        
        use global_data, only:   t_data_start, t_data_end, nregions, domain_ij, &
                                 domsize, bigx, bigy, file_rhoslabs_grid, nslabs
        use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN, IEEE_IS_NAN
        use, intrinsic :: iso_fortran_env, only: real32
        implicit none
        ! real(real32) :: nan
        real(4)        :: nan        
        logical, save            :: first_time = .true.
        integer(4), save         :: count
        integer(4)               :: i, j, i_reg, k, islab
        real(4), dimension(nregions,domsize, nslabs)       :: rhoslabs
        real(4), dimension(bigx, bigy, nslabs, nregions)   :: rhoslabs_grid

        nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)

        rhoslabs_grid = nan
        do i_reg = 1,nregions
            do islab = 1, nslabs
                do k = 1, domsize
                    i=domain_ij(k,1)
                    j=domain_ij(k,2) 
                    ! if (.not. isnan(rhoslabs(i_reg, k,islab))) then
                    rhoslabs_grid(i,j,islab,i_reg) = rhoslabs(i_reg, k,islab)
                    ! end if
                end do
            end do
        end do

        if (first_time) then
            open(121, file = file_rhoslabs_grid, access = "STREAM",&
                 form = "UNFORMATTED", status = "REPLACE") 
            write(121) rhoslabs_grid
            count = t_data_start
            first_time = .false.
        else
            write(121) rhoslabs_grid
            count = count + 1
        end if


        if (count == t_data_end) then
            close(120) 
        end if

    end subroutine




END MODULE subroutines_IO