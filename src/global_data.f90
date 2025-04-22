MODULE global_data

    !=======================================================
    !---------------------- OTHER PARAMETERS --------------------
    !=======================================================
    ! use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN
    ! use, intrinsic :: iso_fortran_env, only: real32
    ! implicit none
    ! real(real32) :: nan
    ! nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)
    ! https://www.reddit.com/r/fortran/comments/bu41uz/how_to_assign_nan_to_a_variable_or_array_with/
    
    use datetime_module

    implicit none
    
    !=======================================================
    !---------------------- INPUTS -------------------------
    !=======================================================

    ! this section is used to define the dimensions of large domain and the time duration of available data
    integer, save :: bigx!=1471 !253 ! 1471 !1471  !number of grids in x and y direction
    integer, save :: bigy!=2027 !346 ! 2027 ! 2027
    integer, save :: n_datadt!= 123+15 !196 ! 120+15 months-01to04 no leapyear ! 123+15  months-05to08  ! 122+15  months-09to12   ! CHANGE 
    real(4), save :: dx!=4000.00  !unit(m), the distance of each grid
    real(4), save :: dy!=4000.00
    !__________________________starting dates and ending dates,core region
    integer, save :: t_data_start, t_data_end!= 123+15 !196  !123+15  !day1=16, day2=   !date starts and ends simulation
    ! character(len=19), save    :: datetime0_str
    type(datetime),save :: datetime0, datetime1
    integer, save :: domsize!=49509  !1386 nonCPM  !49509 CPM !49509  !number of grids within the monsoon region
    integer, save :: nregions != 8  ! 83148 cells landocean CPM  ! 31774 cells land CPM ! 8    !number of regions in map file    ! CHANGE 
    !______________________________________________________________________
    integer, save :: tracing_dt != 1800  !time interval of each-time back tracing
    integer, save :: max_tracing!=600 !1500 ! maximum times of back trajactory(>48*31)
    integer, save :: option_next_point != 1 ! 1 for iterative method, and 2 for direct method
    integer, save :: max_iteration!=101 ! maximum times of iteration
    real(4), save :: tol_error_iteration != 0.0001 
    integer, save :: solver
    ! integer, save :: n_datadt_warmup = 15 !days that to be excluded at the beginning    ! CHANGE 
    integer, save :: UV_dt! = 3  !time interval of velocity data (in hours)     ! CHANGE 
    integer, save :: data_dt! = 3  !time interval of  data 2 (PP, PW, ET) (in hours)     ! CHANGE 
    ! integer, save :: valve1=6  !times of tracing per unit velocity time interval (=3600*3/1800)
    ! integer, save :: valve2=48  !times of tracing per day (=3600*24/1800)   !REMOVE?
    ! integer, save :: ithyear=2018 ! seems doesnt do anything
    logical, save :: verbose! = .true.
    logical, save :: write_paths
    integer, save :: step_write_paths
    integer, save :: delta_write_paths! = 1
    integer, save :: ij0_write_paths! = 1
    logical, save :: write_rho_grid
    integer, save :: write_rho_grid_option
    integer, save :: nslabs 
    
    ! writing options
    ! logical, save :: write_trajectories = .true.
    integer, save :: NAN_value=-9.99E8 ! DONT MODIFY
    ! integer, save :: nslabs = 2  ! DONT MODIFY
    integer       :: nbytes_real = 4

    !!!!! derived variables 
    integer, save :: n_datadt_tracing
    integer, save :: n_UVdt_tracing


    !=======================================================
    !---------------------- INPUT FILES --------------------
    !=======================================================
    ! this sections sets the name of the files 
    character(len=150), save    :: path_input != '/data/keeling/a/erickkc2/f/SAAG3/01_inputs2LDRM/input_files_2LDRM_CPM-4km/'
    logical, save               :: listsfiles != .false.
    ! character(len=150), save    :: path_input = '/data/keeling/a/erickkc2/f/SAAG2/input_files_2LDRM-nonCPM_byyears__24km/'
    character(len=50), save     :: filename_regions != "WRF_regions1.dat"  !'WRF_regionscells.dat'  !"WRF_basins.dat" "WRF_regionscells_landocean.dat" 
    character(len=50), save     :: filename_topomask != 'mask_topo.dat'
    character(len=50), save     :: filename_PP != '2018_months05to08_PP24.dat'   ! 2018_months05to08_PP24.dat
    character(len=50), save     :: filename_ET != '2018_months05to08_ET24.dat'
    character(len=50), save     :: filename_PW != '2018_months05to08_PW24.dat'
    character(len=50), save     :: filename_U3 != '2018_months05to08_U.dat'
    character(len=50), save     :: filename_V3 != '2018_months05to08_V.dat'
    ! character(len=50), save     :: filename_W3U = '2018_WUP.dat'
    ! character(len=50), save     :: filename_W3D = '2018_WDOWN.dat'
    ! character(len=50), save     :: filename_QLIMIT = '2018_Q.dat'
    character(len=50), save     :: filename_PWflux != '2018_months05to08_PWflux24.dat'
    character(len=50), save     :: filename_domain_i != 'MW_il.dat'
    character(len=50), save     :: filename_domain_j != 'MW_jl.dat'


    !=======================================================
    !---------------------- OUTPUT FILES --------------------
    !=======================================================
    ! this sections sets the name of the files 
    ! character(len=150), save    :: path_output = '/data/keeling/a/erickkc2/f/SAAG2/test_4km_origmodel/output_basins_nonCPM/'
    ! character(len=150), save    :: path_output = '/data/keeling/a/erickkc2/f/SAAG2/test_4km_origmodel/output_cells_land/'
    character(len=150), save    :: path_output != '/data/keeling/a/erickkc2/f/SAAG3/02_2LDRM_results/CPM_4km/output_regions1/'
    character(len=100), save     :: prefix_outfiles
    character(len=100), save     :: filename_rhodomain_daily = 'rho_domain_daily.csv'
    character(len=100), save     :: filename_rhocolumn_domain_daily = 'rho_column_domain_daily.csv'
    character(len=100), save     :: filename_rhocolumn_domain_timeavg = 'rho_column_domain_timeavg.csv'
    character(len=100), save     :: filename_precipareal_domain_daily = 'precipareal_domain_daily.csv'
    character(len=100), save     :: filename_paths_xy = "paths_xy.csv"
    character(len=100), save     :: filename_rhocolumn_grid = "rho_grid.dat"
    character(len=100), save     :: filename_rhocolumn_grid_base = "rho_grid"
    character(len=100), save     :: filename_rhoslabs_grid = "rhoslabs_grid.dat"
    ! character(len=50), save     :: filename_rhoslabs_domain_timeavg = 'rho_slabs_domain_timeavg.csv'
    
    
    !=======================================================
    !---------------------- GLOBAL DATA --------------------
    !=======================================================

    character(len=200), save         :: file_regions, &! = trim(path_input) // filename_regions, & !  , file_topomask ! &
                                        file_topomask, &!= trim(path_input) // filename_topomask, &
                                        file_domain_i, &!= = trim(path_input) // filename_domain_i, &
                                        file_domain_j  ! = trim(path_input) // filename_domain_j

    character(len=200), dimension(:), allocatable, save ::  file_PP, &!= trim(path_input) // filename_PP, &
                                                            file_ET, &!= trim(path_input) // filename_ET, &
                                                            file_PW, &!=trim(path_input) // filename_PW, &
                                                            file_U3, &!= trim(path_input) // filename_U3, &
                                                            file_V3, &!= trim(path_input) // filename_V3, &
                                                            file_PWflux != = trim(path_input) // filename_PWflux, &
                                                

    character(len=200), save         :: file_rhodomain_daily, &! = trim(path_output) // filename_rhodomain_daily, &
                                        file_rhocolumn_domain_daily, &!= trim(path_output) // filename_rhocolumn_domain_daily, &
                                        file_rhocolumn_domain_timeavg, &!= trim(path_output) // filename_rhocolumn_domain_timeavg, &
                                        file_precipareal_domain_daily, & != trim(path_output) // filename_precipareal_domain_daily
                                        file_paths_xy, &
                                        file_rhocolumn_grid, &
                                        file_rhoslabs_grid



    integer, dimension(:,:,:),save, allocatable        :: MASK_TOPO
    integer, dimension(:,:),save, allocatable          :: MAP_REGIONS  
    real(4), dimension(:,:,:),save, allocatable        :: PP, ET
    real(4), dimension(:,:,:,:), save, allocatable     :: PW
    real(4), dimension(:,:,:,:),save, allocatable      :: U3, V3
    integer, dimension(:,:), save, allocatable   :: domain_ij  
    real(4), dimension(:,:,:,:),save, allocatable  :: PW_FLUX
    integer, dimension(:,:,:,:),save, allocatable  :: MASK_PW_FLUX 

    ! for storing the data for initial of initial max_tracing temporarily
    real(4), dimension(:,:,:),save, allocatable        :: temp_PP, temp_ET
    real(4), dimension(:,:,:,:), save, allocatable     :: temp_PW
    real(4), dimension(:,:,:,:),save, allocatable      :: temp_U3, temp_V3
    real(4), dimension(:,:,:,:),save, allocatable      :: temp_PW_FLUX
    ! integer, dimension(:,:,:,:),save, allocatable      :: temp_MASK_PW_FLUX 

    ! ################ OLD ##################!                     
    ! integer, dimension(bigx,bigy,nslabs-1),save       :: MASK_TOPO
    ! integer, dimension(bigx,bigy),save                :: MAP_REGIONS  
    ! real(4), dimension(bigx,bigy,n_datadt),save          :: PP, ET
    ! real(4), dimension(bigx,bigy,nslabs,n_datadt), save  :: PW
    ! real(4), dimension(bigx,bigy,nslabs, &
    !                     n_datadt*24/UV_dt),save    :: U3, V3
    ! integer, dimension(domsize,2), save               :: domain_ij  ! should be dimension(2, domsize) to be more efficient

    ! ! these will be calculated
    ! real(4), dimension(bigx,bigy,nslabs-1,n_datadt),save :: PW_FLUX
    ! integer, dimension(bigx,bigy,nslabs-1,n_datadt),save :: MASK_PW_FLUX 
    ! tracing_dt
    

 
END MODULE global_data

