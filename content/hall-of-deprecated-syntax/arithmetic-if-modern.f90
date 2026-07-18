! The modern reading: say the three cases outright.
program arif
  implicit none
  integer :: k
  k = 0
  if (k < 0) then
    stop 'negative'
  else if (k == 0) then
    stop 'zero'
  else
    stop 'positive'
  end if
end program arif
