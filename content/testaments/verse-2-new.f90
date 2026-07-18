! Verse 2, Modern Testament: the block do (Fortran 90), with
! exit and cycle in place of arithmetic and numbered faith.
program loops
  implicit none
  integer :: i, total
  total = 0
  do i = 1, 10
    if (mod(i, 2) == 0) cycle   ! pass over the even offerings
    total = total + i
    if (total > 20) exit        ! depart when enough is gathered
  end do
  print '(a, i0)', 'total: ', total
end program loops
