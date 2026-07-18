! The modern reading: characters live in character variables.
program holler
  implicit none
  character(len=8) :: greet
  greet = 'HELO WLD'
  print '(a)', greet
end program holler
