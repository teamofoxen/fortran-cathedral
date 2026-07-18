! Verse 3, Modern Testament: allocatable arrays and whole-array
! arithmetic (Fortran 90). The loops dissolve; arrays converse.
program arrays
  implicit none
  real, allocatable :: a(:), b(:), c(:)
  integer :: i
  allocate (a(5), b(5), c(5))
  a = [ (real(i), i = 1, 5) ]
  b = 2.0 * a
  c = a + b
  print '(5f6.1)', c
end program arrays
