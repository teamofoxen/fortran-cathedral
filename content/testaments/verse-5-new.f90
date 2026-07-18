! Verse 5, Modern Testament: kinds by name. selected_real_kind
! arrived with Fortran 90; the named constants of iso_fortran_env
! (real32, real64, real128) with Fortran 2008.
program precise
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  real(real64) :: pi
  pi = 4.0_real64 * atan(1.0_real64)
  print '(f20.15)', pi
end program precise
