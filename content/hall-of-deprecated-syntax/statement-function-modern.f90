! The modern reading: an internal function, checked and contained.
program stf
  implicit none
  print *, area(2.0)
contains
  pure function area(r) result(a)
    real, intent(in) :: r
    real :: a
    a = 3.14159 * r * r
  end function area
end program stf
