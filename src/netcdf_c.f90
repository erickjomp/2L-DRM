! Reading NetCDF files through the NetCDF-C library.
!
! Only the few NetCDF-C functions needed to read the inputs are bound here (bind(C)), so
! 2L-DRM needs only the NetCDF-C library (libnetcdf), which works with any Fortran
! compiler, and not NetCDF-Fortran, whose netcdf module must be built with the same
! compiler as the program. NetCDF-Fortran calls the same NetCDF-C functions.
!
! The NetCDF-C dimension order is the reverse of the Fortran one: a variable
! (time, slab, south_north, west_east) is read into a Fortran array
! (west_east, south_north, slab, time), the same memory order as the binary inputs.
module netcdf_c
    use, intrinsic :: iso_fortran_env, only: int8
    use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_float, c_double, c_char, &
                                            c_ptr, c_null_char, c_f_pointer, c_associated
    implicit none
    private

    public :: is_netcdf_file, nc_open_read, nc_close_file, nc_dimension_length, &
              nc_read_real, nc_read_double, nc_text_attribute, nc_int_attribute

    integer(c_int), parameter :: NC_NOERR = 0, NC_NOWRITE = 0, NC_GLOBAL = -1
    integer, parameter        :: max_dims = 16

    interface
        function nc_open(path, mode, ncidp) bind(C, name='nc_open')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value              :: mode
            integer(c_int), intent(out)        :: ncidp
            integer(c_int)                     :: nc_open
        end function
        function nc_close(ncid) bind(C, name='nc_close')
            import :: c_int
            integer(c_int), value :: ncid
            integer(c_int)        :: nc_close
        end function
        function nc_inq_dimid(ncid, name, idp) bind(C, name='nc_inq_dimid')
            import :: c_int, c_char
            integer(c_int), value              :: ncid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(out)        :: idp
            integer(c_int)                     :: nc_inq_dimid
        end function
        function nc_inq_dimlen(ncid, dimid, lenp) bind(C, name='nc_inq_dimlen')
            import :: c_int, c_size_t
            integer(c_int), value        :: ncid, dimid
            integer(c_size_t), intent(out) :: lenp
            integer(c_int)               :: nc_inq_dimlen
        end function
        function nc_inq_varid(ncid, name, varidp) bind(C, name='nc_inq_varid')
            import :: c_int, c_char
            integer(c_int), value              :: ncid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(out)        :: varidp
            integer(c_int)                     :: nc_inq_varid
        end function
        function nc_inq_varndims(ncid, varid, ndimsp) bind(C, name='nc_inq_varndims')
            import :: c_int
            integer(c_int), value       :: ncid, varid
            integer(c_int), intent(out) :: ndimsp
            integer(c_int)              :: nc_inq_varndims
        end function
        function nc_inq_vardimid(ncid, varid, dimidsp) bind(C, name='nc_inq_vardimid')
            import :: c_int
            integer(c_int), value       :: ncid, varid
            integer(c_int), intent(out) :: dimidsp(*)
            integer(c_int)              :: nc_inq_vardimid
        end function
        function nc_get_var_float(ncid, varid, ip) bind(C, name='nc_get_var_float')
            import :: c_int, c_float
            integer(c_int), value       :: ncid, varid
            real(c_float), intent(out)  :: ip(*)
            integer(c_int)              :: nc_get_var_float
        end function
        function nc_get_var_double(ncid, varid, ip) bind(C, name='nc_get_var_double')
            import :: c_int, c_double
            integer(c_int), value       :: ncid, varid
            real(c_double), intent(out) :: ip(*)
            integer(c_int)              :: nc_get_var_double
        end function
        function nc_inq_attlen(ncid, varid, name, lenp) bind(C, name='nc_inq_attlen')
            import :: c_int, c_char, c_size_t
            integer(c_int), value              :: ncid, varid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_size_t), intent(out)     :: lenp
            integer(c_int)                     :: nc_inq_attlen
        end function
        function nc_get_att_text(ncid, varid, name, value) bind(C, name='nc_get_att_text')
            import :: c_int, c_char
            integer(c_int), value               :: ncid, varid
            character(kind=c_char), intent(in)  :: name(*)
            character(kind=c_char), intent(out) :: value(*)
            integer(c_int)                      :: nc_get_att_text
        end function
        function nc_get_att_int(ncid, varid, name, ip) bind(C, name='nc_get_att_int')
            import :: c_int, c_char
            integer(c_int), value              :: ncid, varid
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(out)        :: ip
            integer(c_int)                     :: nc_get_att_int
        end function
        function nc_strerror(ncerr) bind(C, name='nc_strerror')
            import :: c_int, c_ptr
            integer(c_int), value :: ncerr
            type(c_ptr)           :: nc_strerror
        end function
    end interface

contains

    ! True if the file starts as a NetCDF file: "CDF" (classic formats) or "\x89HDF" (NetCDF-4)
    function is_netcdf_file(path) result(is_nc)
        character(len=*), intent(in) :: path
        logical                      :: is_nc
        integer(int8)                :: magic(4)
        integer                      :: unit, ios

        is_nc = .false.
        open(newunit=unit, file=path, status='old', form='unformatted', access='stream', &
             action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, iostat=ios) magic
        close(unit)
        if (ios /= 0) return
        ! "CDF" = 67 68 70; "\x89HDF" = 137 72 68 70 (137 is -119 as a signed byte)
        is_nc = all(magic(1:3) == [67_int8, 68_int8, 70_int8]) .or. &
                all(magic == [-119_int8, 72_int8, 68_int8, 70_int8])
    end function

    function nc_open_read(path) result(ncid)
        character(len=*), intent(in) :: path
        integer                      :: ncid
        integer(c_int)               :: id

        call check(nc_open(c_string(path), NC_NOWRITE, id), "Cannot open NetCDF file '" // path // "'")
        ncid = id
    end function

    subroutine nc_close_file(ncid)
        integer, intent(in) :: ncid
        call check(nc_close(int(ncid, c_int)), "Cannot close NetCDF file")
    end subroutine

    ! Length of a dimension; error if the file has no dimension with that name
    function nc_dimension_length(ncid, name, path) result(length)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: name, path
        integer                      :: length
        integer(c_int)               :: dimid
        integer(c_size_t)            :: dimlen

        call check(nc_inq_dimid(int(ncid, c_int), c_string(name), dimid), &
                   "No dimension '" // name // "' in '" // path // "'")
        call check(nc_inq_dimlen(int(ncid, c_int), dimid, dimlen), "Cannot read dimension '" // name // "'")
        length = int(dimlen)
    end function

    ! Reads a whole variable as float32 (converted by NetCDF-C if stored as another type)
    ! into values, which must have the Fortran shape var_shape (reverse of the NetCDF one).
    ! values is an explicit-size dummy, so any array or contiguous section can be passed.
    subroutine nc_read_real(ncid, name, path, var_shape, values)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: name, path
        integer, intent(in)          :: var_shape(:)
        real(c_float), intent(out)   :: values(product(var_shape))
        integer(c_int)               :: varid

        varid = variable_with_shape(ncid, name, path, var_shape)
        call check(nc_get_var_float(int(ncid, c_int), varid, values), &
                   "Cannot read variable '" // name // "' of '" // path // "'")
    end subroutine

    subroutine nc_read_double(ncid, name, path, var_shape, values)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: name, path
        integer, intent(in)          :: var_shape(:)
        real(c_double), intent(out)  :: values(product(var_shape))
        integer(c_int)               :: varid

        varid = variable_with_shape(ncid, name, path, var_shape)
        call check(nc_get_var_double(int(ncid, c_int), varid, values), &
                   "Cannot read variable '" // name // "' of '" // path // "'")
    end subroutine

    ! Text attribute of a variable (varname = '' for a global attribute); '' if missing
    function nc_text_attribute(ncid, varname, attname, path) result(text)
        integer, intent(in)              :: ncid
        character(len=*), intent(in)     :: varname, attname, path
        character(len=:), allocatable    :: text
        integer(c_int)                   :: varid
        integer(c_size_t)                :: attlen
        character(kind=c_char), allocatable :: buffer(:)
        integer                          :: i

        text = ''
        varid = variable_id(ncid, varname, path)
        if (nc_inq_attlen(int(ncid, c_int), varid, c_string(attname), attlen) /= NC_NOERR) return
        allocate(buffer(max(attlen, 1_c_size_t)))
        call check(nc_get_att_text(int(ncid, c_int), varid, c_string(attname), buffer), &
                   "Cannot read attribute '" // attname // "' of '" // path // "'")
        text = repeat(' ', int(attlen))
        do i = 1, int(attlen)
            text(i:i) = buffer(i)
        end do
    end function

    ! Integer global attribute; found = .false. if missing
    subroutine nc_int_attribute(ncid, attname, value, found)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: attname
        integer, intent(out)         :: value
        logical, intent(out)         :: found
        integer(c_int)               :: v

        found = nc_get_att_int(int(ncid, c_int), NC_GLOBAL, c_string(attname), v) == NC_NOERR
        value = 0
        if (found) value = v
    end subroutine


    function variable_id(ncid, name, path) result(varid)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: name, path
        integer(c_int)               :: varid

        if (len(name) == 0) then
            varid = NC_GLOBAL
        else
            call check(nc_inq_varid(int(ncid, c_int), c_string(name), varid), &
                       "No variable '" // name // "' in '" // path // "'")
        end if
    end function

    ! varid of a variable, after checking that its (Fortran-order) shape is var_shape
    function variable_with_shape(ncid, name, path, var_shape) result(varid)
        integer, intent(in)          :: ncid
        character(len=*), intent(in) :: name, path
        integer, intent(in)          :: var_shape(:)
        integer(c_int)               :: varid, ndims, dimids(max_dims)
        integer(c_size_t)            :: dimlen
        integer                      :: shape_file(max_dims), i
        character(len=200)           :: msg, shape_str, expected_str

        varid = variable_id(ncid, name, path)
        call check(nc_inq_varndims(int(ncid, c_int), varid, ndims), "Cannot inquire variable '" // name // "'")
        call check(nc_inq_vardimid(int(ncid, c_int), varid, dimids), "Cannot inquire variable '" // name // "'")
        do i = 1, ndims
            call check(nc_inq_dimlen(int(ncid, c_int), dimids(i), dimlen), "Cannot inquire variable '" // name // "'")
            shape_file(ndims - i + 1) = int(dimlen)   ! reverse: NetCDF-C order -> Fortran order
        end do
        if (ndims /= size(var_shape)) then
            write(msg, '(A,I0,A,I0,A)') " has ", ndims, " dimensions, expected ", size(var_shape), "."
        else if (any(shape_file(:ndims) /= var_shape)) then
            write(shape_str, '(*(I0,:,", "))') shape_file(:ndims)
            write(expected_str, '(*(I0,:,", "))') var_shape
            msg = " has shape (" // trim(shape_str) // "), expected (" // trim(expected_str) // &
                  ") (Fortran order: x, y, ..., time)."
        else
            return
        end if
        write(*, '(A)') "Variable '" // name // "' of '" // path // "'" // trim(msg)
        error stop
    end function

    subroutine check(status, message)
        integer(c_int), intent(in)   :: status
        character(len=*), intent(in) :: message
        if (status /= NC_NOERR) then
            write(*, '(A)') message // ": " // c_message(nc_strerror(status))
            error stop
        end if
    end subroutine

    function c_string(str) result(cstr)
        character(len=*), intent(in)  :: str
        character(kind=c_char, len=:), allocatable :: cstr
        cstr = trim(str) // c_null_char
    end function

    function c_message(ptr) result(text)
        type(c_ptr), intent(in)       :: ptr
        character(len=:), allocatable :: text
        character(kind=c_char), pointer :: chars(:)
        integer                       :: n, i

        text = ''
        if (.not. c_associated(ptr)) return
        call c_f_pointer(ptr, chars, [1000])
        do n = 0, 999
            if (chars(n + 1) == c_null_char) exit
        end do
        text = repeat(' ', n)
        do i = 1, n
            text(i:i) = chars(i)
        end do
    end function

end module netcdf_c
