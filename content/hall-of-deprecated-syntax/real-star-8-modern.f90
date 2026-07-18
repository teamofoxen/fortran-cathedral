! The modern reading: precision requested by named kind.
program rstar
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  real(real64) :: x
  x = 1.0_real64 / 3.0_real64
  print *, x
end program rstar
