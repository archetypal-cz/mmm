! Replacements for MS Fortran 5.x runtime routines used by MMM.
subroutine gettim(ihr, imin, isec, i100th)
  integer(2), intent(out) :: ihr, imin, isec, i100th
  integer :: v(8), ft, st
  character(len=16) :: env
  ! MMM_FIXED_TIME=HHMMSS makes the mystery number reproducible (tests)
  call get_environment_variable('MMM_FIXED_TIME', env, status=st)
  if (st == 0) then
    read(env, *) ft
    ihr = int(ft / 10000, 2); imin = int(mod(ft / 100, 100), 2)
    isec = int(mod(ft, 100), 2); i100th = 0
    return
  end if
  call date_and_time(values=v)
  ihr = int(v(5), 2); imin = int(v(6), 2); isec = int(v(7), 2)
  i100th = int(v(8) / 10, 2)
end subroutine gettim

! List-directed integer READ(*,*) aborts the program on non-numeric input
! (gfortran runtime error).  Read a line and fall back to 0 instead.
subroutine rdint(iv)
  integer(2), intent(out) :: iv
  character(len=80) :: line
  integer :: ios, tmp
  read(*, '(A)', iostat=ios) line
  if (ios < 0) stop
  read(line, *, iostat=ios) tmp
  if (ios /= 0) tmp = 0
  iv = int(tmp, 2)
end subroutine rdint
