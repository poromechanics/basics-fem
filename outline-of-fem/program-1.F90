program main
  implicit none

  real, parameter :: E0 = 22.22E+6
  real, parameter :: E = 200.0E+9
  real, parameter :: A = 1.402E-2
  real, parameter :: L = 12.0
  real, parameter :: resTol = 1.0E-6
  real :: f, df, lam, dlam
  integer, parameter :: maxIter = 100
  integer :: ii
  logical :: isconvg
  !!
  !! initial guess
  !!
  lam = 1.0; dlam = 0.0; isconvg = .FALSE.
  !!
  do ii = 1, maxIter
    f =   E0 &
      & + E0 * EXP( 2.0 * L * lam ) &
      & + A * E * lam &
      & - A * E * lam * EXP( 2.0 * L * lam )
    !!
    !! check convergence
    !!
    if( abs( f ) .LE. resTol ) then
      write( *, "(a)" ) "Convergence achieved"
      isconvg = .TRUE.
      exit
    else
      write( *, "(a, I6)" ) "Iteration = ", ii
      write( *, "(a, F16.6)" ) "residual = ", f
      write( *, "(a, F15.6)" ) "lam = ", lam
      write( *, * ) ""
      write( *, * ) ""
    end if
    !!
    df = &
      & 2.0 * E0 * L * EXP( 2.0 * L * lam ) &
      & + A * E &
      & - A * E * EXP( 2.0 * L * lam ) &
      & - 2.0 * A * E * L * lam * EXP( 2.0 * L * lam )
    !!
    !! calculate dlam
    !!
    dlam = -f / df
    !!
    !! update lam
    !!
    lam = lam + dlam
    !!
  end do
  !!
  !! print results
  !!
  if( isconvg ) then
    write( *, "(a, F12.6)" ) "lambda = ", lam
    write( *, "(a, F12.6)" ) "ks = ", lam*lam*A*E
  else
    write( *, "(a)" ) "No convergence, may be increase maxIter"
    write( *, "(a, F12.6)" ) "lambda = ", lam
    write( *, "(a, F12.6)" ) "ks = ", lam*lam*A*E
  end if
  !!
end program main