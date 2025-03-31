MODULE subroutines_process

    implicit none

contains

    subroutine get_PP_domain(iday, PP_domain) 
        use global_data, only: domsize, domain_ij, PP
        integer, intent(in)                      :: iday
        real(4), dimension(domsize), intent(out) :: PP_domain
        integer                                  :: i,j,k

        do k = 1,domsize
            i = domain_ij(k,1)
            j = domain_ij(k,2)

            PP_domain(k) = PP(i,j, iday)
        end do
    end subroutine

    subroutine get_PW_domain(iday, PW_domain) 
        use global_data, only: domsize, domain_ij, PW, nslabs
        integer, intent(in)                              :: iday
        real(4), dimension(domsize, nslabs), intent(out) :: PW_domain
        integer                                          :: i,j,k, islab

        do  islab = 1, nslabs
            do k = 1,domsize
                i = domain_ij(k,1)
                j = domain_ij(k,2)
    
                PW_domain(k, islab) = PW(i,j, islab, iday)
            end do
        end do

    end subroutine

    subroutine average_in_time(rho_days, weights_days, n_datadt, nrhos, avg_rho)

        integer, intent(in)                            :: nrhos
        integer, intent(in)                            :: n_datadt
        real(4), dimension(nrhos,n_datadt), intent(in)    :: rho_days
        real(4), dimension(n_datadt), intent(in)          :: weights_days
        real(4), dimension(nrhos), intent(out)         :: avg_rho
        integer                                        :: i_rho, i
        integer                                        :: PP_total

        PP_total = sum(weights_days)        

        do i_rho = 1, nrhos 
            avg_rho(i_rho) = sum(rho_days(i_rho,:) * weights_days) / PP_total
        end do

    end subroutine

    
    subroutine calculate_column_rho(rho,PW_domain, size, n_rhos, nslabs, rho_average, verbose)
        USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
        real(4), dimension(n_rhos, size, nslabs), intent(in)  :: rho
        real(4), dimension(size, nslabs), intent(in)          :: PW_domain
        real(4), dimension(n_rhos, size), intent(out)         :: rho_average                   
        integer, intent(in)                                   :: size, n_rhos, nslabs
        integer                                               :: k, islab, i_rho
        real(4), dimension(nslabs)                            :: PW_values
        real(4), dimension(nslabs)                            :: PWflux_values
        real(4)                                               :: PW_total_value
        real(4), dimension(n_rhos)                            :: sum_temp
        ! logical, dimension(nslabs)                            :: isfinite_rho
        logical, dimension(nslabs)                            :: isvalue_PW, isfinite_rho
        logical, optional                                     :: verbose

        do k = 1,size
            PW_values = PW_domain(k,:)
            ! isvalue_PW = (.not.isnan(PW_values)) 
            ! PW_total_value = sum(PW_values, mask = isvalue_PW )

            isfinite_rho = IEEE_IS_FINITE(rho(1,k,:))  !! selecting one region since the mask is the same for all regions
            ! print *, isfinite_rho
            PW_total_value = sum(PW_values, mask = isfinite_rho )
            
            do i_rho = 1,n_rhos
                sum_temp(i_rho) = 0  
                do islab = 1,nslabs
                    ! if (isvalue_PW(islab)) then
                    if (isfinite_rho(islab)) then
                        sum_temp(i_rho) = sum_temp(i_rho)  + rho(i_rho, k, islab) * PW_values(islab)
                    end if
                end do        
            end do
            rho_average(:,k) = sum_temp / PW_total_value

            ! REMOVE
            if (present(verbose)) then
                if (verbose) then
                !if (i_rho == 4) then
                    if (k == 1817) then
                        print *, "k=",k,", PW_values=",PW_values, "PW_total=",PW_total_value,&
                            "rho=",rho(1, k, :), "rho_avg=",rho_average(1,k)
                    end if
                !end if   
                end if
            end if
            ! REMOVE
        end do
    end subroutine


    subroutine calculate_areal_rho(rho, PP_values, size, n_rhos, rho_domain)
        
        integer, intent(in)                         :: size, n_rhos
        real(4), dimension(n_rhos,size), intent(in) :: rho
        real(4), dimension(size), intent(in)        :: PP_values
        real(4), dimension(n_rhos), intent(out)     :: rho_domain
        real(4)                                     :: total_PP
        integer                                     :: k, i_rho
        real(4), dimension(n_rhos)                  :: sum_temp 
        logical, dimension(size)                 :: mask_values
        ! integer, intent(in), dimension(size), optional          :: mask_domain

        ! total_PP = sum(PP_values) ! when there is no NAN is gaurateed
        mask_values = (.not. isnan(PP_values)) .and.  (.not. isnan(rho(1,:)))   ! for all regions (n_rhos) is the same nan struture
        total_PP = sum(PP_values, MASK = mask_values)       

        sum_temp(:) = 0.0

        do k = 1,size
            do i_rho = 1, n_rhos
                ! if (.not. isnan(PP_values(k))) then
                if (mask_values(k)) then
                    sum_temp(i_rho) = sum_temp(i_rho) +  rho(i_rho,k) * PP_values(k)
                end if
            end do
        end do

        rho_domain = sum_temp / total_PP
    end subroutine
    
    subroutine identify_path_sections(path_ij, length_path, path_sections, n_sections)
        use derived_types
        use global_data, only: MAP_REGIONS 
        integer, dimension(:,:), intent(in)         :: path_ij
        integer, intent(in)                         :: length_path
        type(start_end), dimension(:), intent(out)  :: path_sections
        integer, intent(out)                        :: n_sections
        integer                                     :: time, id_region, &
                                                        id_region_old
        if (length_path == 0) then
            n_sections = 0 ! in case length_path == 0
            path_sections(1)%start_l = 0 ! just to avoid any confusion 
            path_sections(1)%end_l = 0 ! just to avoid any confusion 
            path_sections(1)%id_region = 0 
            return
        end if

        do time = 1,length_path
            id_region = MAP_REGIONS(path_ij(1,time), path_ij(2,time))
            
            if(time == 1)  then
                n_sections = 1
                path_sections(n_sections)%id_region  = id_region
                path_sections(n_sections)%start_l    = time
                id_region_old = id_region
                cycle
            end if

            if (id_region_old .ne. id_region) then
                n_sections = n_sections + 1
                path_sections(n_sections)%id_region  = id_region
                path_sections(n_sections)%start_l    = time
                path_sections(n_sections-1)%end_l    = time 
                id_region_old = id_region
            end if
        end do

        path_sections(n_sections)%end_l = length_path

    end subroutine

!=============================================================
! subroutine to do the integral along back-trajectory in upper slab
!_____________________________________________________________
    PURE subroutine SPTNQ_U(x,y1,y2,z,n,s)
    ! subroutine SPTNQ_U(x,y1,y2,z,n,s,verbose)
        use global_data, only: UV_dt
        ! REMOVE
        USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
        ! END REMOVE
        implicit none
        integer,intent(in):: n
        real(4),dimension(n),intent(in)::x,y1,y2
        integer,dimension(n),intent(in):: z
        real(4),intent(out) :: s
        integer :: i,j,k
        real(4) :: a,b1,b2, ewa1,ewa2, ew1,ew2
        real(8) :: ss1, ss2  
        ! if necessary use real( 16) but consider it is very slow
        ! https://community.intel.com/t5/Intel-Fortran-Compiler/Speeding-up-execution-of-REAL-16-programs/td-p/1033795
        real(4) :: bt1, bt2, sst1
        real(4) :: rho

        ! REMOVE
        ! logical, intent(in) :: verbose
        ! logical     :: is_first
        ! real(4)  :: ss2_old
        ! is_first = .true.
        ! end REMOVE

        s=0.
        ss1 = 0.0
        ss2 = 0.0
        sst1 = 0.0
        do i=n-1,1,-1
            rho = 0.0
            a=x(i+1)-x(i)           ! dt
            !-----for total column integration------
            bt1=y1(i)
            bt2=y1(i+1)
            sst1  = sst1 + a*(bt1+bt2)/(2*3600*UV_dt)  ! 3600 secs/h
            rho   = 1-exp(-sst1)
            !-----for upper layer integration-------
            b1 =y2(i)
            b2 =y2(i+1)
            ewa1=(b1+b2)/(2*3600*UV_dt)                ! 3600 secs/h
            ss1=ss1+a*ewa1
            ew1=b1*rho
            ew2=b2*rho
            if (z(i) == 0) then   ! only 1 layer
                ew1=b1
            end if
            if (z(i+1) == 0) then
                ew2=b2
            end if
            ewa2=(ew1+ew2)/(2*3600*UV_dt)              ! 3600 secs/h
            ! REMOVE
            ! ss2_old = ss2
            ! end REMOVE
            ss2=ss2+exp(ss1)*ewa2*a

            ! if (verbose .and. (.not.IEEE_IS_FINITE(ss2)) .and. is_first) then
            !        print *,"from SPTNQ_U: ", i, ss1, ewa2, a, ss2, sst1, "exp",exp(ss1) * ewa2*a, ss2_old, ss2 + exp(ss1) * ewa2*a
            !        is_first = .false.
            ! end if
        end do
        s = exp(-ss1)*ss2
        
    end subroutine


    !===========================================================
    ! subroutine to do the integral along back-trajectory in lower slab
    !-----------------------------------------------------------
    PURE subroutine SPTNQ_L(x,y1,y2,y3,z,n,s)
        use global_data, only: UV_dt
        implicit none
        integer,intent(in):: n
        real(4),dimension(n),intent(in)::x,y1,y2,y3
        integer,dimension(n),intent(in)::z
        real(4),intent(out) :: s
        integer:: i,j,k
        real(4) :: a,et1,et2,qw1,qw2
        real(4) :: ewa1,ewa2, ew1,ew2
        real(8) :: ss1, ss2
        real(4) :: sst1, bt1, bt2, rho
    
        s=0.
        ss1 = 0.0
        ss2 = 0.0
        sst1 = 0.0
        do i=n-1,1,-1
            a=x(i+1)-x(i)           ! dt
            !------for total column integration--------
            bt1 = y1(i)
            bt2 = y1(i+1)
            sst1 = sst1 + a*(bt1+bt2)/(2*3600*UV_dt)
            rho  = 1-exp(-sst1)
            !------for lower layer integration-------    
            et1=y2(i)
            et2=y2(i+1)
            qw1=y3(i)
            qw2=y3(i+1)
            if (z(i) == 0.0) then
                et1 = 0.0
                qw1 = 0.0
            end if
            if (z(i+1) == 0.0) then
                et2 = 0.0
                qw2 = 0.0
            end if
            ewa1=(et1+qw1+et2+qw2)/(2*3600*UV_dt)
            ss1=ss1+a*ewa1
            ew1=et1+qw1*rho
            ew2=et2+qw2*rho
            ewa2=(ew1+ew2)/(2*3600*UV_dt)
            ss2=ss2+exp(ss1)*ewa2*a
        end do
        s = exp(-ss1)*ss2
    end subroutine


    subroutine calculate_rho2_slab2__2LDRM(path_ij, path_sections, n_sections, i_UVdt, rho2,verbose_in)
        USE global_data, only: max_tracing, tracing_dt, nregions, UV_dt
        USE global_data, only:  PW, data_dt,ET, PW_flux ! remove 2 last ones
        USE derived_types
        integer, dimension(:,:), intent(in)       :: path_ij
        integer, intent(in)                       :: i_UVdt, n_sections
        type(start_end), dimension(:), intent(in) :: path_sections
        real, dimension(nregions), intent(out)    :: rho2
        integer                                   :: i_section, itime, itime_start, itime_end,&
                                                        id_region, i, j, i_UVdt_tracing,&
                                                        counter_tracing_dts, n_tracingdt_in_UVdt, &
                                                        length_section
        real(4),dimension(max_tracing)            :: E_PW_ratio
        ! real(4)                                 :: E_PW_ratio_last
        real(4),dimension(max_tracing)            :: FLUXUP_ratio
        ! real(4)                                 :: FLUXUP_ratio_last
        integer,dimension(max_tracing)            :: flag
        real(4)                                   :: s1
        real, dimension(max_tracing)              :: time_secs
        real, dimension(3,max_tracing)            :: various_reg
        real(4), dimension(nregions)              :: sum_region 
        !!!! UPDATE !!!! improve names varios_reg & s1
        ! remove
        integer  :: i_datadt
        real(4),  dimension(2)   :: PW_values
        real(4)                  :: PW_FLUX_value, ET_value

        ! en remove
        
        ! REMOVE 
        logical, intent(in), optional :: verbose_in
        logical                       :: verbose

        verbose = .false.
        if (present(verbose_in))  verbose = verbose_in
        ! END REMOVE

        do itime = 1,max_tracing
            !!!! UPDATE !!!!
            time_secs(itime) = (itime - 1) * tracing_dt          ! probably include in global data
        end do

        i_UVdt_tracing = i_UVdt
        counter_tracing_dts  = 0
        ! n_tracingdt_in_day = (24*60*60) / tracing_dt
        n_tracingdt_in_UVdt = (UV_dt*60*60) / tracing_dt 

        ! calculating ratios
        do itime = 1, path_sections(n_sections)%end_l
            i = path_ij(1, itime)
            j = path_ij(2, itime)
            
            if (counter_tracing_dts == n_tracingdt_in_UVdt ) then
                counter_tracing_dts = 0
                i_UVdt_tracing  = i_UVdt_tracing - 1
            end if
            counter_tracing_dts = counter_tracing_dts + 1

            call getratios_from_ij_slab2__2LDRM(i, j, i_UVdt_tracing, E_PW_ratio(itime), &
                                                FLUXUP_ratio(itime),flag(itime))

            ! REMOVE
            if (verbose) then
                if (itime >=1 .and. itime < 224) then
                    write(*,"('i_time = ',I0,'i_UVdt_trac=',I0, ', i= ',I0,', j= ',I0, ',&
                        & E_PW_ratio = ',F12.10, ', FLUXUP_ratio = ',F14.10,  'ndt=', I0 )") &
                        itime,i_UVdt_tracing, i, j, E_PW_ratio(itime), FLUXUP_ratio(itime), &
                        n_tracingdt_in_UVdt

                        i_datadt = 1 + (i_UVdt - 1) / (data_dt / UV_dt)   ! converting from i_UVdt to i_datadt
                        PW_values = PW(i, j, :, i_datadt)
                        ! ET_value = ET(i, j, i_datadt) 
                        ET_value = ET(i, j, i_datadt) / (data_dt / UV_dt)   ! MODIFIED 2025
                        PW_FLUX_value = PW_FLUX(i, j, 1 , i_datadt)      !!!! MODIFY
                    write(*,"   ('    i_datadt=', I0, ',  PW_values=', F8.5, F8.5, ',  ET_value=',F8.5, &
                             & ',  PW_FLUX_value=', F8.5)") &
                             i_datadt, PW_values(1), PW_values(2), ET_value, PW_FLUX_value
                end if
            end if
            ! END REMOVE
        end do

        ! calculating paths
        do i_section = 1, n_sections
            itime_start = path_sections(i_section)%start_l
            itime_end = path_sections(i_section)%end_l
            ! length_section = itime_end-itime_start+1

            length_section = itime_end  ! UPDATE

            ! call SPTNQ_U(time_secs(itime_start:itime_end), &
            !             E_PW_ratio(itime_start:itime_end), &
            !             FLUXUP_ratio(itime_start:itime_end), &
            !             flag(itime_start:itime_end), &
            !             itime_end-itime_start+1, &
            !             s1)

            ! REMOVE
            ! if  (verbose .and. i_section == 4) then
            !     do itime = itime_start,itime_end
            !         print *, "itime=" , time_secs(itime),", E_PW_ratio=",E_PW_ratio(itime), &
            !                 ", FLUXUP_ratio=",FLUXUP_ratio(itime), ", flag=", flag(itime)
            !     end do
            !     print *, "length_section: ", length_section
            ! end if
            ! END REMOVE

            call SPTNQ_U(time_secs(1:itime_end), &
                        E_PW_ratio(1:itime_end), &
                        FLUXUP_ratio(1:itime_end), &
                        flag(1:itime_end), &
                        length_section, &
                        s1)

            ! call SPTNQ_U(time_secs(1:itime_end), &
            !             E_PW_ratio(1:itime_end), &
            !             FLUXUP_ratio(1:itime_end), &
            !             flag(1:itime_end), &
            !             length_section, &
            !             s1,verbose)


            if (i_section == 1) then
                various_reg(1, i_section)=0.0
            else
                various_reg(1, i_section)=various_reg(2, i_section-1)
            end if
            various_reg(2,i_section) = s1
            various_reg(3,i_section) = various_reg(2,i_section) - various_reg(1,i_section)

            ! REMOVE
            ! if (verbose) then
            !     print *,"i_section=",i_section,", s1=",s1, "varios_re=",various_reg(3,i_section) 
            ! end if
            ! END REMOVE
        end do

        ! sum_region=0.0
        ! do i_section = 1,n_sections
        !   id_region = path_sections(k,i_section)%id_region
        !   sum_region(id_region) = sum_region(id_region)+max(various_reg(i_section,3),0.0)
        ! end do
        
        rho2 = 0.0
        do i_section = 1,n_sections
          id_region = path_sections(i_section)%id_region
          rho2(id_region) = rho2(id_region)+max(various_reg(3,i_section),0.0)
        end do

    end subroutine

    PURE subroutine calculate_rho1_slab1__2LDRM(path_ij, path_sections, n_sections,i_UVdt, rho1, verbose)
        USE global_data, only: max_tracing, tracing_dt, nregions, PW_FLUX, NAN_value, UV_dt, data_dt
        USE derived_types
        integer, dimension(:,:), intent(in)       :: path_ij
        integer, intent(in)                       :: i_UVdt, n_sections
        type(start_end), dimension(:), intent(in) :: path_sections
        real, dimension(nregions), intent(out)    :: rho1
        integer                                   :: i_section, itime, itime_start, itime_end,&
                                                        id_region, i, j, i_UVdt_tracing,&
                                                        counter_tracing_dts, n_tracingdt_in_UVdt,&
                                                        length_section
        real(4),dimension(max_tracing)            :: E_PW1_ratio, E_PW_ratio
        ! real(4)                                 :: E_PW_ratio_last
        real(4),dimension(max_tracing)            :: FLUXDOWN_ratio
        ! real(4)                                 :: FLUXDOWN_ratio_last
        integer,dimension(max_tracing)            :: flag
        real(4)                                   :: s1
        real, dimension(max_tracing)              :: time_secs
        real, dimension(3,max_tracing)            :: various_reg
        real(4), dimension(nregions)              :: sum_region 
        logical, optional, intent(in)             :: verbose ! REMOVE
        integer                                   :: itime2  ! REMOVE
        !!!! UPDATE !!!! improve names varios_reg & s1

        if (n_sections == 0) then
            rho1(:) = 0 ! REMOVE  ! with this 0s enter in the areal average. Should they ?
            ! rho1(:) = NAN_value ! problematic a true nan is needed 
            return
        end if

        do itime = 1,max_tracing
            !!!! UPDATE !!!!
            time_secs(itime) = (itime - 1) * tracing_dt          ! probably include in global data
        end do

        i_UVdt_tracing = i_UVdt
        counter_tracing_dts  = 0
        n_tracingdt_in_UVdt = (UV_dt*60*60) / tracing_dt

        ! calculating ratios
        do itime = 1, path_sections(n_sections)%end_l
            i = path_ij(1, itime)
            j = path_ij(2, itime)
                
            if (counter_tracing_dts == n_tracingdt_in_UVdt ) then
                counter_tracing_dts = 0
                i_UVdt_tracing  = i_UVdt_tracing - 1
            end if
            counter_tracing_dts = counter_tracing_dts + 1

            call getratios_from_ij_slab1__2LDRM(i, j, i_UVdt_tracing, E_PW1_ratio(itime), &
                                                E_PW_ratio(itime), FLUXDOWN_ratio(itime),flag(itime))
            
            ! REMOVE
            ! if (present(verbose)) then
            !     if (verbose) then
            !         ! if (i_section == 2 .and. itime > 45 .and. itime <= 50) then
            !             !print *, "i_section ", i_section
            !             print *, "itime = " ,itime, &
            !             "E_PW_ratio = ", E_PW_ratio(itime), &
            !             "E_PW1_ratio = ", E_PW1_ratio(itime), &
            !             "FLUXDOWN_ratio = ", FLUXDOWN_ratio(itime), &
            !             "PW_FLUX_value = ", PW_FLUX(i,j,1,iday_tracing), &
            !             ! "iday_tracing = ", iday_tracing, &
            !             ! "ntracingdt = ",n_tracingdt_in_day
            !             "i = ", i, &
            !             "j = ", j
            !         ! end if
            !     end if
            !  end if
            ! END REMOVE

        end do


        do i_section = 1, n_sections
            itime_start = path_sections(i_section)%start_l
            itime_end = path_sections(i_section)%end_l
            ! length_section = itime_end-itime_start+1

            length_section = itime_end  ! UPDATE
            call SPTNQ_L(time_secs(1:itime_end), & 
                        E_PW_ratio(1:itime_end), &
                        E_PW1_ratio(1:itime_end), &
                        FLUXDOWN_ratio(1:itime_end), &
                        flag(1:itime_end), &
                        length_section, &
                        s1)

            ! REMOVE
            ! if (present(verbose)) then
            !     if (verbose) then
            !         ! if (i_section == 2) then
            !             print *, "  "
            !             print *, "k = ", 1144 ,", i_section = ", i_section, &
            !                     ", start_time = ", itime_start, &
            !                     ", end_time = ", itime_end
            !             ! print *,"      time_track    = ", time_secs(1:itime_end)

            !             ! print *,"      EW_ratio      = ", E_PW_ratio(1:itime_end)
            !             ! print *,"      EWL_ratio     = ", E_PW1_ratio(1:itime_end)
            !             ! print *,"      QWD_ratio     = ", FLUXDOWN_ratio(1:itime_end)
            !             ! print *,"      PW_FLUX_value = ", PW_FLUX(i,j,1,iday) 
            !             ! print *,"      flag          = ", flag(1:itime_end) 
            !         ! end if
            !     end if
            ! end if
            ! end REMOVE

            if (i_section == 1) then
                various_reg(1, i_section)=0.0
            else
                various_reg(1, i_section)=various_reg(2, i_section-1)
            end if
            various_reg(2,i_section) = s1
            various_reg(3,i_section) = various_reg(2,i_section) - various_reg(1,i_section)
        end do

        ! sum_region=0.0
        ! do i_section = 1,n_sections
        !   id_region = path_sections(k,i_section)%id_region
        !   sum_region(id_region) = sum_region(id_region)+max(various_reg(i_section,3),0.0)
        ! end do

        ! REMOVE        
        ! if (present(verbose)) then
        !     if (verbose) then
        !         do itime = 45,50
        !             print *, "itime = " ,itime, &
        !                     "E_PW_ratio = ", E_PW_ratio(itime), &
        !                     "E_PW1_ratio = ", E_PW1_ratio(itime), &
        !                     "FLUXDOWN_ratio = ", FLUXDOWN_ratio(itime)
        !         end do
        !     end if
        ! end if
        ! end REMOVE

        rho1 = 0.0
        do i_section = 1,n_sections
          id_region = path_sections(i_section)%id_region
        !   if (id_region < 0) then
        !       print *,"i_section is ", i_section
        !       print *,"id_region is ", id_region
        !   end if
          rho1(id_region) = rho1(id_region)+max(various_reg(3,i_section),0.0)
        end do

        ! if (present(verbose)) then
        !     if (verbose) then
        !         print *, "various_reg(2,) = ", various_reg(2,1:n_sections)
        !     end if
        ! end if

    end subroutine



    PURE subroutine getratios_from_ij_slab2__2LDRM(i, j, i_UVdt, E_PW_ratio, FLUX_ratio, flag)
        use global_data, only : PW, PW_FLUX, ET, MASK_PW_FLUX, MASK_TOPO, U3, V3, &
                                nslabs, NAN_value, UV_dt, data_dt
        
        integer, intent(in)          :: i,j
        integer, intent(in)          :: i_UVdt
        real(4), intent(out)         :: E_PW_ratio, FLUX_ratio
        real(4), dimension(nslabs)   :: PW_values
        real(4)                      :: PW_FLUX_value
        integer                      :: mask1, mask2
        integer, intent(out)         :: flag
        real(4)                      :: FLUX_UP, PWsum, ET_value
        integer                      :: i_datadt

        ! call get_xy_from_ij(x, y, out_i, out_j)

        ! maybe create getter functions  :    get_PWvalues_from_ij_iUVdt(.....)
        i_datadt = 1 + (i_UVdt - 1) / (data_dt / UV_dt)   ! converting from i_UVdt to i_datadt
        PW_values = PW(i, j, :, i_datadt)
        ! ET_value = ET(i, j, i_datadt) 
        ET_value = ET(i, j, i_datadt) / (data_dt / UV_dt)   ! MODIFIED 2025
        PW_FLUX_value = PW_FLUX(i, j, 1 , i_datadt)      !!!! MODIFY

        ! ! should be better if by default it is required to have mask with the same number of slabs as the other variables
        ! if (islab < nslabs) then
        !     mask1 =  MASK_TOPO[out_i, out_j, islab] .ne. 0
        !     mask2 = MASK_PW_FLUX[out_i, out_j, islab, iday] .ne. 0 
        ! else  
        !     mask1 = .true.
        !     mask2 = .true.
        ! end if

        PWsum = sum(PW_values, mask = .NOT.(isnan(PW_values)))
          

        !!!! UPDATE !!!!\
        mask1 = MASK_TOPO(i, j, 1) 
        mask2= MASK_PW_FLUX(i, j, 1, i_datadt)
        
        ! PW_flux * (PWsum)/(PW1 * PW2)  (FLUX ratio)
        E_PW_ratio = ET_value / PWsum 
        if ((mask1 == 0) .or. (mask2== 0)) then ! 1 layer
            FLUX_UP = ET_value
            FLUX_ratio = FLUX_UP / PWsum      
            flag = 0    ! only 1-Layer
        else   ! 2 layers
            FLUX_UP = max(PW_FLUX_value,0.0)
            FLUX_ratio = (FLUX_UP / PW_values(2))*(PWsum/PW_values(1))    
            flag=mask2
        end if

        if (isnan(PWsum) .or. isnan(ET_value)) then
            flag = 5
        end if
    end subroutine


    PURE subroutine getratios_from_ij_slab1__2LDRM(i, j, i_UVdt, E_PW1_ratio, E_PW_ratio, FLUX_ratio,  flag)
        use global_data, only : PW, PW_FLUX, ET, MASK_PW_FLUX, MASK_TOPO, U3, V3, &
                                nslabs, NAN_value, UV_dt, data_dt
        
        integer, intent(in)          :: i,j
        integer, intent(in)          :: i_UVdt
        real(4), intent(out)         :: E_PW1_ratio, E_PW_ratio, FLUX_ratio
        real(4), dimension(nslabs)   :: PW_values
        real(4)                      :: PW_FLUX_value
        integer                      :: mask1, mask2
        integer, intent(out)         :: flag
        real(4)                      :: FLUX_DOWN, PWsum, ET_value
        integer                      :: i_datadt

        ! maybe create getter functions  :    get_PWvalues_from_ij_iUVdt(.....)
        i_datadt = 1 + (i_UVdt - 1) / (data_dt / UV_dt)   ! converting from i_UVdt to i_datadt
        PW_values = PW(i, j, :, i_datadt)
        ! ET_value = ET(i, j, i_datadt)
        ET_value = ET(i, j, i_datadt) / (data_dt / UV_dt)   ! MODIFIED 2025
        PW_FLUX_value = PW_FLUX(i, j, 1 , i_datadt)      

        PWsum = sum(PW_values, mask = .NOT.(isnan(PW_values)))
          
        !!!! UPDATE !!!!\
        mask1 = MASK_TOPO(i, j, 1) 
        mask2= MASK_PW_FLUX(i, j, 1, i_datadt)
        
        E_PW_ratio = ET_value / PWsum 
        if ((mask1 == 0) .or. (mask2 == 0)) then  ! 1 layer
            E_PW1_ratio = NAN_value
            FLUX_ratio  = NAN_value
            flag = 0
        else 
            FLUX_DOWN = min(PW_FLUX_value,0.0)*(-1.0)
            FLUX_ratio = (FLUX_DOWN/PW_values(1))*(PWsum/PW_values(2))
            flag = mask2
            E_PW1_ratio = ET_value / PW_values(1)
        end if

        if (isnan(PWsum) .or. isnan(ET_value)) then
            flag = 5
        end if
    end subroutine


    subroutine calculate_rhos_slabs__diffin(path_ij, path_sections, n_sections, i_UVdt,islab_out, rho, verbose_in)
        USE global_data, only: max_tracing, tracing_dt, nregions, UV_dt, nslabs, tracing_dt, UV_dt
        USE global_data, only:  PW, data_dt,ET, PW_flux ! remove 2 last ones
        USE global_data, only: solver
        USE derived_types
        USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE, ieee_quiet_nan, ieee_value, IEEE_IS_NAN
        integer, dimension(:,:), intent(in)       :: path_ij
        integer, intent(in)                       :: i_UVdt, n_sections, islab_out
        type(start_end), dimension(:), intent(in) :: path_sections
        real(4), dimension(nregions), intent(out)    :: rho
        integer                                   :: i_section, itime, itime_start, itime_end,&
                                                        id_region, i, j, i_UVdt_tracing,&
                                                        counter_tracing_dts, n_tracingdt_in_UVdt, &
                                                        length_section, i_datadt_tracing
        integer                                   :: length_path
        real(4), dimension(nslabs, nregions,max_tracing) :: rhoslabs_path ! maybe as output var
        real(4), dimension(max_tracing)           :: time_secs
        real(4), dimension(nregions)              :: sum_region 
        real(4)                                   :: ratio_stability
        real(4)                                   :: nan_value
        !!!! UPDATE !!!! improve names varios_reg & s1
        integer                       :: i_datadt, islab
        real(4),  dimension(nslabs)   :: PW_colvalues
        real(4)                       :: ET_value, rho_upperslab, rho_lowerslab, rho_slab, PWflux_down, PWflux_up
        real(4), dimension(nslabs-1)  :: PW_FLUX_colvalues,  PWflux_down_colvalues, PWflux_up_colvalues
        logical                       :: is_bottom_slab
        
        ! REMOVE 
        logical, intent(in), optional :: verbose_in
        logical                       :: verbose

        verbose = .false.
        if (present(verbose_in))  verbose = verbose_in
        ! END REMOVE

        nan_value = ieee_value( nan_value, ieee_quiet_nan )


        
        if (n_sections == 0) then
            rho(:) = nan_value !nan_value ! REMOVE  ! with this 0s enter in the areal average. Should they ?
            ! rho1(:) = NAN_value ! problematic a true nan is needed 
            return
        end if

        length_path = path_sections(n_sections)%end_l

        do itime = 1,max_tracing
            !!!! UPDATE !!!!
            time_secs(itime) = (itime - 1) * tracing_dt          ! probably include in global data
        end do

        i_UVdt_tracing = i_UVdt
        counter_tracing_dts  = 0
        ! n_tracingdt_in_day = (24*60*60) / tracing_dt
        n_tracingdt_in_UVdt = (UV_dt*60*60) / tracing_dt 

        ! calculating ratios
        ! i_UVdt_tracing = i_UVdt - (itime-1) / 6 
        
        i_section = n_sections
        
        rhoslabs_path = 0 ! implicitly also sets 0 to the first time step
        ! rhoslabs_path = nan_value !0 ! implicitly also sets 0 to the first time step
        ! rhoslabs_path(length_path,:,:) = 0

        do itime = length_path,1, -1  ! to start at the begginning of the path   (EXISTS IF itime = 1 later)
            i = path_ij(1, itime)
            j = path_ij(2, itime)

            
            i_UVdt_tracing = i_UVdt - (itime -1)/ n_tracingdt_in_UVdt
            i_datadt_tracing = 1 + (i_UVdt_tracing - 1) / (data_dt / UV_dt)

            if (itime < path_sections(i_section)%start_l) then 
                i_section = i_section - 1
            end if

            ! if (counter_tracing_dts == n_tracingdt_in_UVdt ) then
            !     counter_tracing_dts = 0
            !     i_UVdt_tracing  = i_UVdt_tracing - 1
            ! end if
            ! counter_tracing_dts = counter_tracing_dts + 1
            
            PW_colvalues = PW(i, j, :, i_datadt_tracing)
            ! ET_value = ET(i, j, i_datadt)
            ET_value = ET(i, j, i_datadt_tracing) / (data_dt / UV_dt)   ! MODIFIED 2025
            PW_FLUX_colvalues = PW_FLUX(i, j, : , i_datadt_tracing)     
            
            if (itime == 1) exit


            PWflux_down_colvalues = max(-PW_FLUX_colvalues,0.0)
            PWflux_up_colvalues = max(PW_FLUX_colvalues,0.0)
            
            do id_region = 1, nregions
                ! if (IEEE_IS_FINITE(PW_values(1))) then   ! why in some cases PW_values(1)) is finite but PWflux_value not?
                is_bottom_slab = .false.
                !!!! TOP SLAB
                ! this can be also included as a normal slab, making rho_upperslab = 0, but maybe less computationally efficient?
                if (IEEE_IS_FINITE(PW_FLUX_colvalues(nslabs-1)) .and. IEEE_IS_FINITE(PW_colvalues(nslabs-1))) then
                    PWflux_up = PWflux_up_colvalues(nslabs-1)
                    rho_lowerslab = rhoslabs_path(nslabs-1, id_region, itime)

                    ! if (ieee_is_nan(rho_lowerslab)) then
                    !     rho_lowerslab = 0
                    ! end if
                else 
                    is_bottom_slab  = .true.
                    PWflux_up = ET_value
                    if (id_region == path_sections(i_section)%id_region) then
                        rho_lowerslab = 1
                    else 
                        rho_lowerslab = 0
                    end if
                end if
                
                ! calculatin next rho
                ratio_stability = PWflux_up / PW_colvalues(nslabs)  * tracing_dt / (UV_dt * 60 * 60)
                if (ratio_stability < 1) then
                    rho_slab = rhoslabs_path( nslabs, id_region, itime)
                    rhoslabs_path( nslabs,id_region, itime - 1) = &
                        rho_slab + &
                        (rho_lowerslab - rho_slab ) * &
                        (PWflux_up / PW_colvalues(nslabs))  * tracing_dt / (UV_dt * 60 * 60)
                else 
                    if (solver == 3) then
                        rhoslabs_path(nslabs , id_region, itime - 1) = &
                            rho_lowerslab
                    else if (solver == 2) then
                        !!! DO SOMETHING 
                        !!! DECREASE DELTA T and RECALCULATE
                    end if
                end if

                ! START REMOVE
                ! if (verbose .and. (itime <= 32 .and. itime >=28) .and. id_region == 1) then
                if (verbose  .and. id_region == 1) then
                    print * , &!"t = ", itime, ", rho_slab =", rho_slab, &
                               itime, nslabs, ", rho_slab =", rho_slab, &
                               ", slab = ",nslabs,&
                               ", ratio_stab = ", ratio_stability, &
                               ", rho_ls = ", rho_lowerslab, &
                               ", PWflux_up = ", PWflux_up, ", PW = ", PW_colvalues(nslabs)
                end if
                ! END REMOVE

                !!!! OTHER SLABS
                
                do islab = nslabs-1,1,-1  
                    if (is_bottom_slab) exit
                    ! is bottom slab?
                    if (islab == 1) then 
                        is_bottom_slab = .true.
                    else if (IEEE_IS_NAN(PW_FLUX_colvalues(islab-1)) .or. IEEE_IS_NAN(PW_colvalues(islab-1))) then
                        is_bottom_slab = .true.
                    end if 
                    !!!! set flux and rho coming from lower slab
                    if (is_bottom_slab) then  ! bottom slab 
                        PWflux_up = ET_value

                        if (id_region == path_sections(i_section)%id_region) then
                            rho_lowerslab = 1
                        else 
                            rho_lowerslab = 0
                        end if
                    else   ! standard slab  
                        PWflux_up = PWflux_up_colvalues(islab-1)
                        rho_lowerslab = rhoslabs_path( islab-1, id_region,itime)
                        ! if (IEEE_IS_NAN(rho_lowerslab)) then
                        !     rho_lowerslab = 0
                        ! end if
                    end if
                    !!!! set flux and rho coming from upper slab
                    PWflux_down = PWflux_down_colvalues(islab)
                    rho_upperslab = rhoslabs_path(islab+1, id_region, itime)
                    ! if (IEEE_IS_NAN(rho_upperslab)) then
                    !     rho_upperslab = 0
                    ! end if

                    ! calculating next rho
                    ratio_stability = (PWflux_up + PWflux_down) / PW_colvalues(islab)  * tracing_dt / (UV_dt * 60 * 60)
                    if (ratio_stability < 1) then
                        rho_slab = rhoslabs_path( islab, id_region,itime)
                        ! if (IEEE_IS_NAN(rho_slab)) then
                        !     rho_slab = 0
                        ! end if
    
                        rhoslabs_path(islab, id_region, itime - 1) = &
                                rho_slab + &
                                (rho_lowerslab - rho_slab) *  &
                                PWflux_up / PW_colvalues(islab) * tracing_dt / (UV_dt * 60 * 60) + &
                                (rho_upperslab - rho_slab) *  &
                                PWflux_down / PW_colvalues(islab) * tracing_dt / (UV_dt * 60 * 60)
                    else
                        if (solver == 3) then
                            rhoslabs_path(islab, id_region,itime - 1) = & 
                                (rho_upperslab * PWflux_down + rho_lowerslab * PWflux_up) / &
                                (PWflux_down + PWflux_up)

                        else if (solver == 2) then
                            ! DO SOMETHING HERE
                            !!! DECREASE DELTA T and RECALCULATE
                        end if
                    end if

                    ! START REMOVE
                    if (verbose .and. id_region == 1) then
                    ! if (verbose .and. (itime <= 32 .and. itime >=28) .and. id_region == 1) then
                        print * ,&!, "t = ", &
                                itime, &
                                islab, &
                                ", rho_slab =", rho_slab, &
                                ! ", slab = ",islab,&
                                ", ratio_stab = ", ratio_stability, &
                                ! ", rho_ls = ", rho_lowerslab, &
                                ", rho_us = ", rho_upperslab, &
                                ! ", PWflux_up = ", PWflux_up, &
                                ", PWflux_down = ", PWflux_down,&
                                ", PW = ", PW_colvalues(islab)!,  "PWflux = ", PW_FLUX_colvalues(islab)
                    end if
                    ! END REMOVE

                    
                end do
            end do

        end do 

        ! FINAL OUTPUTS
        rho = rhoslabs_path(islab_out,:,1) ! first time step

        if (islab_out < nslabs) then
            if (IEEE_IS_NAN(PW_FLUX_colvalues(islab_out)) .or. IEEE_IS_NAN(PW_colvalues(islab_out))) then 
                rho = nan_value ! nan_value
            end if
        else 
            if (IEEE_IS_NAN(PW_colvalues(islab_out))) then
                rho = nan_value
            end if
        end if
        
        ! DIAGNOSTICS PRINT
        ! if (verbose) then
        !     print *, "length_path = ", length_path
        !     do  itime = length_path,1, -1  
        !         print *, itime, rhoslabs_path(itime, 1,2)  
        !     end do
        ! end if

    end subroutine





END MODULE subroutines_process