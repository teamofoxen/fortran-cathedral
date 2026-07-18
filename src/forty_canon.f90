!> forty_canon: fixed liturgical constants and exit codes.
!> Nothing in this module performs work. It merely remembers.
module forty_canon
  implicit none
  private
  public :: FORTY_VERSION, CANON_REPO_NAME, CANON_DESCRIPTION, CANON_BASE_URL
  public :: CANON_COMMIT_MSG, CANON_COMMIT_TRAILER
  public :: EXIT_OK, EXIT_FAIL, EXIT_USAGE, EXIT_ENV, EXIT_EXTERNAL, EXIT_DECLINED

  character(*), parameter :: FORTY_VERSION = '0.2.0'

  !> The intended canonical home of the generated site. Deployment is a
  !> later phase; the sitemap and robots.txt speak of this address in
  !> anticipation, which the Confessional does not conceal.
  character(*), parameter :: CANON_BASE_URL = &
    'https://teamofoxen.github.io/fortran-cathedral'

  character(*), parameter :: CANON_REPO_NAME = 'fortran-cathedral'
  character(*), parameter :: CANON_DESCRIPTION = &
    'The web, as IBM never intended. A cathedral of Fortran.'
  character(*), parameter :: CANON_COMMIT_MSG = 'PHASE 0: FORTY IS ORDAINED.'
  character(*), parameter :: CANON_COMMIT_TRAILER = &
    'Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>'

  integer, parameter :: EXIT_OK       = 0  ! the rite concluded
  integer, parameter :: EXIT_FAIL     = 1  ! general failure or wrong ground
  integer, parameter :: EXIT_USAGE    = 2  ! the invocation was malformed
  integer, parameter :: EXIT_ENV      = 3  ! required tooling is absent
  integer, parameter :: EXIT_EXTERNAL = 4  ! a delegated command failed
  integer, parameter :: EXIT_DECLINED = 5  ! confirmation was withheld
end module forty_canon
