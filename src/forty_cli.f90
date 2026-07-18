!> forty_cli: the parsing of invocations, and the vetting of names.
!> No I/O occurs here; judgment is rendered as data, so the trials
!> may examine it without ceremony.
module forty_cli
  use forty_util, only: string_t, to_lower, starts_with
  use forty_canon, only: CANON_REPO_NAME, CANON_DESCRIPTION
  implicit none
  private
  public :: cli_t, parse_cli
  public :: CMD_NONE, CMD_HELP, CMD_VERSION, CMD_STATUS, CMD_DOCTOR, CMD_BUILD
  public :: CMD_TEST, CMD_CONFESS, CMD_CLEAN, CMD_GITHUB, CMD_UNKNOWN
  public :: CMD_GENERATE, CMD_VALIDATE, CMD_OPEN, CMD_OFFER, CMD_ATONE, CMD_AUDIT
  public :: CMD_DEPLOY, CMD_INSPECT
  public :: SUB_NONE, SUB_STATUS, SUB_CONNECT, SUB_VERIFY, SUB_UNKNOWN
  public :: valid_repo_name, valid_owner_name, valid_description, valid_visibility
  public :: valid_commit_message

  integer, parameter :: CMD_NONE = 0, CMD_HELP = 1, CMD_VERSION = 2
  integer, parameter :: CMD_STATUS = 3, CMD_DOCTOR = 4, CMD_BUILD = 5
  integer, parameter :: CMD_TEST = 6, CMD_CONFESS = 7, CMD_CLEAN = 8
  integer, parameter :: CMD_GITHUB = 9, CMD_UNKNOWN = 99
  integer, parameter :: CMD_GENERATE = 11, CMD_VALIDATE = 12, CMD_OPEN = 13
  integer, parameter :: CMD_OFFER = 14, CMD_ATONE = 15, CMD_AUDIT = 16
  integer, parameter :: CMD_DEPLOY = 17, CMD_INSPECT = 18
  integer, parameter :: SUB_NONE = 0, SUB_STATUS = 1, SUB_CONNECT = 2
  integer, parameter :: SUB_VERIFY = 3, SUB_UNKNOWN = 99

  !> Characters cmd.exe might mistake for instructions. They are barred
  !> from descriptions; the Cathedral does not interpolate temptation.
  character(*), parameter :: FORBIDDEN_DESC = '"%`\|&<>^'

  type :: cli_t
    integer :: command = CMD_NONE
    integer :: subcommand = SUB_NONE
    logical :: dry_run = .false.
    logical :: assume_yes = .false.
    character(:), allocatable :: repo_name
    character(:), allocatable :: owner
    character(:), allocatable :: description
    character(:), allocatable :: visibility
    character(:), allocatable :: message
    character(:), allocatable :: rite
    character(:), allocatable :: errmsg
  end type cli_t

contains

  function parse_cli(argv) result(cli)
    type(string_t), intent(in) :: argv(:)
    type(cli_t) :: cli
    integer :: i
    character(:), allocatable :: a

    cli%repo_name = CANON_REPO_NAME
    cli%owner = ''
    cli%description = CANON_DESCRIPTION
    cli%visibility = 'public'
    cli%message = ''
    cli%rite = ''
    cli%errmsg = ''

    if (size(argv) == 0) then
      cli%command = CMD_HELP
      return
    end if

    a = to_lower(argv(1)%s)
    select case (a)
    case ('help', '--help', '-h'); cli%command = CMD_HELP
    case ('version', '--version'); cli%command = CMD_VERSION
    case ('status');  cli%command = CMD_STATUS
    case ('doctor');  cli%command = CMD_DOCTOR
    case ('build');   cli%command = CMD_BUILD
    case ('test');    cli%command = CMD_TEST
    case ('confess'); cli%command = CMD_CONFESS
    case ('clean');   cli%command = CMD_CLEAN
    case ('github');  cli%command = CMD_GITHUB
    case ('generate'); cli%command = CMD_GENERATE
    case ('validate'); cli%command = CMD_VALIDATE
    case ('open');     cli%command = CMD_OPEN
    case ('offer');    cli%command = CMD_OFFER
    case ('atone');    cli%command = CMD_ATONE
    case ('audit');    cli%command = CMD_AUDIT
    case ('deploy');   cli%command = CMD_DEPLOY
    case ('inspect');  cli%command = CMD_INSPECT
    case default
      cli%command = CMD_UNKNOWN
      cli%errmsg = 'UNKNOWN COMMAND: ' // argv(1)%s
      return
    end select

    i = 2
    if (cli%command == CMD_GITHUB) then
      if (size(argv) < 2) then
        cli%subcommand = SUB_UNKNOWN
        cli%errmsg = 'GITHUB REQUIRES A SUBCOMMAND: status | connect | verify'
        return
      end if
      select case (to_lower(argv(2)%s))
      case ('status');  cli%subcommand = SUB_STATUS
      case ('connect'); cli%subcommand = SUB_CONNECT
      case ('verify');  cli%subcommand = SUB_VERIFY
      case default
        cli%subcommand = SUB_UNKNOWN
        cli%errmsg = 'UNKNOWN GITHUB SUBCOMMAND: ' // argv(2)%s
        return
      end select
      i = 3
    end if

    if (cli%command == CMD_ATONE .or. cli%command == CMD_AUDIT) then
      if (size(argv) < 2) then
        if (cli%command == CMD_ATONE) then
          cli%errmsg = 'ATONE REQUIRES A RITE NAME: phase-1-manual-offering'
        else
          cli%errmsg = 'AUDIT REQUIRES A SUBJECT: provenance'
        end if
        return
      end if
      if (starts_with(argv(2)%s, '--')) then
        cli%errmsg = 'A SUBJECT MUST BE NAMED BEFORE THE OPTIONS.'
        return
      end if
      cli%rite = to_lower(argv(2)%s)
      i = 3
    end if

    if (cli%command == CMD_INSPECT) then
      if (size(argv) >= 2) then
        if (.not. starts_with(argv(2)%s, '--')) then
          cli%rite = to_lower(argv(2)%s)
          i = 3
        end if
      end if
    end if

    do while (i <= size(argv))
      a = argv(i)%s
      if (a == '--dry-run') then
        cli%dry_run = .true.
      else if (a == '--yes' .or. a == '-y') then
        cli%assume_yes = .true.
      else if (starts_with(a, '--name=')) then
        cli%repo_name = a(8:)
      else if (starts_with(a, '--owner=')) then
        cli%owner = a(9:)
      else if (starts_with(a, '--description=')) then
        cli%description = a(15:)
      else if (starts_with(a, '--message=')) then
        cli%message = a(11:)
      else if (starts_with(a, '--visibility=')) then
        cli%visibility = to_lower(a(14:))
      else
        cli%errmsg = 'UNKNOWN ARGUMENT: ' // a
        return
      end if
      i = i + 1
    end do
  end function parse_cli

  !> GitHub repository names: letters, digits, dot, underscore, hyphen.
  pure function valid_repo_name(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    integer :: i
    ok = .false.
    if (len(s) < 1 .or. len(s) > 100) return
    if (s == '.' .or. s == '..') return
    do i = 1, len(s)
      if (.not. is_name_char(s(i:i))) return
    end do
    ok = .true.
  end function valid_repo_name

  !> GitHub owners: letters, digits, hyphens; no hyphen at either end.
  pure function valid_owner_name(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    integer :: i
    ok = .false.
    if (len(s) < 1 .or. len(s) > 39) return
    if (s(1:1) == '-' .or. s(len(s):len(s)) == '-') return
    do i = 1, len(s)
      if (.not. (is_alnum(s(i:i)) .or. s(i:i) == '-')) return
    end do
    ok = .true.
  end function valid_owner_name

  !> Printable ASCII, bounded length, and none of cmd.exe's runes.
  pure function valid_description(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    integer :: i, c
    ok = .false.
    if (len(s) > 200) return
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (c < 32 .or. c > 126) return
      if (index(FORBIDDEN_DESC, s(i:i)) /= 0) return
    end do
    ok = .true.
  end function valid_description

  !> A commit message: nonempty, bounded, and free of cmd.exe's runes.
  pure function valid_commit_message(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    ok = (len_trim(s) > 0) .and. valid_description(s)
  end function valid_commit_message

  pure function valid_visibility(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    ok = (s == 'public' .or. s == 'private')
  end function valid_visibility

  pure function is_alnum(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    integer :: ic
    ic = iachar(c)
    r = (ic >= iachar('0') .and. ic <= iachar('9')) .or. &
        (ic >= iachar('a') .and. ic <= iachar('z')) .or. &
        (ic >= iachar('A') .and. ic <= iachar('Z'))
  end function is_alnum

  pure function is_name_char(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    r = is_alnum(c) .or. c == '.' .or. c == '_' .or. c == '-'
  end function is_name_char

end module forty_cli
