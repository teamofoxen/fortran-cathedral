! Verse 4, Modern Testament: the module (Fortran 90). Shared state
! is named, typed, and reached through an explicit interface.
module state
  implicit none
  real :: temp = 288.15
  real :: press = 101.325
end module state

program sharing
  use state, only: temp, press
  implicit none
  print '(2(a, f8.3))', 't = ', temp, '  p = ', press
end program sharing
