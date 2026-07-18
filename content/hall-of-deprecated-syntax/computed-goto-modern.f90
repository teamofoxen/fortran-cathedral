! The modern reading: select case names its branches.
program cgoto
  implicit none
  integer :: k
  k = 2
  select case (k)
  case (1)
    stop 'one'
  case (2)
    stop 'two'
  case (3)
    stop 'three'
  end select
end program cgoto
