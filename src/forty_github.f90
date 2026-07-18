!> forty_github: the gatehouse, the verification, and the consecration rite.
!>
!> The rite is one transaction: Forty states every intended action in
!> full, obtains ONE confirmation, then performs the sequence, halting
!> at the first failure. No credential is requested, read, echoed, or
!> stored; the GitHub CLI keeps the keys.
module forty_github
  use forty_util, only: string_t, push_string, to_upper, int_to_str
  use forty_ui, only: say, verdict, lament, rule, blank, confirm
  use forty_paths, only: file_exists, quote
  use forty_run, only: run_result, run_cmd, run_live, tool_found, write_lines
  use forty_git, only: git_initialized, git_branch, git_remote_url, &
                       slug_from_url, valid_slug
  use forty_cli, only: cli_t, valid_repo_name, valid_owner_name, &
                       valid_description, valid_visibility
  use forty_canon, only: CANON_COMMIT_MSG, CANON_COMMIT_TRAILER, &
                         EXIT_OK, EXIT_FAIL, EXIT_USAGE, EXIT_ENV, &
                         EXIT_EXTERNAL, EXIT_DECLINED
  implicit none
  private
  public :: plan_step_t, build_connect_plan, parse_login
  public :: github_status, github_connect, github_verify

  type :: plan_step_t
    character(:), allocatable :: label
    character(:), allocatable :: command   ! empty for internal steps
    logical :: live = .true.               ! console inherited when run
    logical :: internal = .false.          ! performed by Forty's own hand
  end type plan_step_t

contains

  ! ---------------------------------------------------------------- status

  subroutine github_status(exit_code)
    integer, intent(out) :: exit_code
    type(run_result) :: rr
    logical :: ok_git, ok_gh, found
    character(:), allocatable :: path, login, url, branch

    call say('THE GATEHOUSE REPORT.')
    call rule()

    ok_git = tool_found('git', path)
    if (ok_git) then
      call verdict('GIT.', path)
    else
      call verdict('GIT.', 'MISSING.')
    end if
    ok_gh = tool_found('gh', path)
    if (ok_gh) then
      call verdict('GH.', path)
    else
      call verdict('GH.', 'MISSING.')
    end if

    if (ok_gh) then
      rr = run_cmd('gh auth status')
      if (rr%launched .and. rr%exit_code == 0) then
        login = parse_login(rr%out)
        if (len(login) > 0) then
          call verdict('AUTH.', 'RECOGNIZED AS ' // login // '.')
        else
          call verdict('AUTH.', 'RECOGNIZED.')
        end if
      else
        call verdict('AUTH.', 'ABSENT. THE CLI KEEPS THE KEYS: gh auth login')
      end if
    end if

    if (git_initialized()) then
      branch = git_branch()
      if (len(branch) == 0) branch = '(UNREADABLE)'
      call verdict('REPOSITORY.', 'INITIATED. BRANCH ' // branch // '.')
      call git_remote_url(found, url)
      if (found) then
        call verdict('REMOTE.', url)
        call blank()
        call say('THE REMOTE STANDS.')
      else
        call verdict('REMOTE.', 'ABSENT.')
        call blank()
        call say('AWAITING CONSECRATION.')
      end if
    else
      call verdict('REPOSITORY.', 'UNINITIATED. THE GROUND IS UNCONSECRATED.')
      call blank()
      call say('AWAITING CONSECRATION.')
    end if

    if (ok_git .and. ok_gh) then
      exit_code = EXIT_OK
    else
      exit_code = EXIT_ENV
    end if
  end subroutine github_status

  ! ---------------------------------------------------------------- verify

  subroutine github_verify(exit_code)
    integer, intent(out) :: exit_code
    logical :: found
    character(:), allocatable :: url, branch, slug
    type(run_result) :: rr
    integer :: i

    if (.not. git_initialized()) then
      call lament('GIT IS UNINITIATED. THERE IS NOTHING TO VERIFY.')
      exit_code = EXIT_FAIL
      return
    end if
    call git_remote_url(found, url)
    if (.not. found) then
      call lament('NO REMOTE IS CONFIGURED. AWAITING CONSECRATION.')
      exit_code = EXIT_FAIL
      return
    end if
    call verdict('REMOTE.', url)
    branch = git_branch()
    if (len(branch) > 0) call verdict('BRANCH.', branch)

    slug = slug_from_url(url)
    if (len(slug) == 0 .or. .not. valid_slug(slug)) then
      call lament('THE REMOTE IS NOT A RECOGNIZABLE GITHUB DWELLING: ' // url)
      exit_code = EXIT_FAIL
      return
    end if
    rr = run_cmd('gh repo view ' // slug // ' --json nameWithOwner --jq .nameWithOwner')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('GITHUB DOES NOT ACKNOWLEDGE ' // slug // '.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) then
        call verdict('GITHUB SEES.', rr%out(i)%s)
        exit
      end if
    end do
    call blank()
    call say('THE REMOTE IS CANONICAL.')
    exit_code = EXIT_OK
  end subroutine github_verify

  ! --------------------------------------------------------------- connect

  !> The complete rite as data. Pure assembly; no execution, no I/O.
  function build_connect_plan(name, owner, desc, vis, git_ready, ignore_present) &
    result(steps)
    character(*), intent(in) :: name, owner, desc, vis
    logical, intent(in) :: git_ready, ignore_present
    type(plan_step_t), allocatable :: steps(:)

    allocate (steps(0))
    if (.not. git_ready) then
      call push_step(steps, 'CONSECRATE THE GROUND', 'git init -b main', &
                     live=.true., internal=.false.)
    end if
    if (.not. ignore_present) then
      call push_step(steps, 'INSCRIBE .gitignore (build/, dist/)', '', &
                     live=.false., internal=.true.)
    end if
    call push_step(steps, 'GATHER THE WORKS', 'git add -A', &
                   live=.true., internal=.false.)
    call push_step(steps, 'SEAL THE FIRST COMMIT', &
                   'git commit -m "' // CANON_COMMIT_MSG // '" -m "' // &
                   CANON_COMMIT_TRAILER // '"', &
                   live=.true., internal=.false.)
    call push_step(steps, 'RAISE THE REMOTE AND PUSH', &
                   'gh repo create ' // owner // '/' // name // ' --' // vis // &
                   ' --description "' // desc // &
                   '" --source . --remote origin --push', &
                   live=.true., internal=.false.)
    call push_step(steps, 'VERIFY THE REMOTE', 'git remote get-url origin', &
                   live=.false., internal=.false.)
    call push_step(steps, 'VERIFY THE CANON', &
                   'gh repo view ' // owner // '/' // name // &
                   ' --json nameWithOwner --jq .nameWithOwner', &
                   live=.false., internal=.false.)
  end function build_connect_plan

  subroutine github_connect(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    type(run_result) :: rr
    type(plan_step_t), allocatable :: steps(:)
    character(:), allocatable :: path, login, owner, url
    logical :: found, ok
    integer :: i, j

    ! The instruments.
    if (.not. tool_found('git', path)) then
      call lament('GIT IS MISSING. THE RITE CANNOT PROCEED.')
      exit_code = EXIT_ENV
      return
    end if
    if (.not. tool_found('gh', path)) then
      call lament('THE GITHUB CLI IS MISSING. THE RITE CANNOT PROCEED.')
      exit_code = EXIT_ENV
      return
    end if

    ! The keys. Forty never touches them; he only asks whether they turn.
    rr = run_cmd('gh auth status')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('GITHUB DOES NOT KNOW YOU. THE CLI OWNS ALL CREDENTIALS.')
      call say('AUTHENTICATE IN YOUR OWN TERMINAL, WHERE THE CLI CAN SPEAK')
      call say('TO YOU DIRECTLY:  gh auth login')
      call say('THEN RETURN, AND THE RITE WILL RESUME.')
      exit_code = EXIT_ENV
      return
    end if
    login = parse_login(rr%out)

    ! Perhaps the work is already done.
    if (git_initialized()) then
      call git_remote_url(found, url)
      if (found) then
        call say('THE REMOTE ALREADY STANDS: ' // url)
        call say('NOTHING WAS PERFORMED. CONSULT: forty github verify')
        exit_code = EXIT_OK
        return
      end if
    end if

    owner = cli%owner
    if (len(owner) == 0) owner = login
    if (len(owner) == 0) then
      call lament('NO OWNER COULD BE DISCERNED. STATE ONE: --owner=NAME')
      exit_code = EXIT_FAIL
      return
    end if

    ! The vetting of names, before any of them touches a command line.
    if (.not. valid_repo_name(cli%repo_name)) then
      call lament('THE REPOSITORY NAME IS NOT FIT TO SPEAK: ' // cli%repo_name)
      exit_code = EXIT_USAGE
      return
    end if
    if (.not. valid_owner_name(owner)) then
      call lament('THE OWNER NAME IS NOT FIT TO SPEAK: ' // owner)
      exit_code = EXIT_USAGE
      return
    end if
    if (.not. valid_description(cli%description)) then
      call lament('THE DESCRIPTION CONTAINS FORBIDDEN RUNES OR EXCESS LENGTH.')
      exit_code = EXIT_USAGE
      return
    end if
    if (.not. valid_visibility(cli%visibility)) then
      call lament('VISIBILITY MUST BE public OR private, NOT: ' // cli%visibility)
      exit_code = EXIT_USAGE
      return
    end if

    steps = build_connect_plan(cli%repo_name, owner, cli%description, &
                               cli%visibility, git_initialized(), &
                               file_exists('.gitignore'))

    ! The complete rite, stated before anything is performed.
    call say('THE INTENDED RITE:')
    call rule()
    do i = 1, size(steps)
      if (steps(i)%internal) then
        call say('  ' // int_to_str(i) // '. ' // steps(i)%label)
        call say('       (FORTY WRITES THE FILE HIMSELF: build/ AND dist/ KEPT OUT.)')
      else
        call say('  ' // int_to_str(i) // '. ' // steps(i)%label)
        call say('       ' // steps(i)%command)
      end if
    end do
    call rule()
    call say('REPOSITORY: ' // owner // '/' // cli%repo_name // &
             '  (' // to_upper(cli%visibility) // ')')
    call say('THIS RITE CREATES A ' // to_upper(cli%visibility) // &
             ' REPOSITORY AND PUSHES ALL COMMITTED WORKS.')
    call blank()

    if (cli%dry_run) then
      call say('THE RITE REMAINS UNPERFORMED (--dry-run).')
      exit_code = EXIT_OK
      return
    end if

    ! One confirmation for the whole transaction. No approval theater.
    if (.not. confirm('PERFORM THE COMPLETE RITE?', cli%assume_yes)) then
      call say('THE RITE IS DEFERRED. THE CATHEDRAL IS PATIENT.')
      exit_code = EXIT_DECLINED
      return
    end if

    do i = 1, size(steps)
      call blank()
      call say('STEP ' // int_to_str(i) // ': ' // steps(i)%label)
      if (steps(i)%internal) then
        call inscribe_gitignore(ok)
        if (.not. ok) then
          call lament('THE RITE HALTS AT STEP ' // int_to_str(i) // &
                      ': THE FILE COULD NOT BE WRITTEN.')
          exit_code = EXIT_FAIL
          return
        end if
      else if (steps(i)%live) then
        rr = run_live(steps(i)%command)
        if (.not. rr%launched .or. rr%exit_code /= 0) then
          call halt_report(i, steps(i)%label, rr%exit_code)
          exit_code = EXIT_EXTERNAL
          return
        end if
      else
        rr = run_cmd(steps(i)%command)
        if (.not. rr%launched .or. rr%exit_code /= 0) then
          call halt_report(i, steps(i)%label, rr%exit_code)
          exit_code = EXIT_EXTERNAL
          return
        end if
        do j = 1, size(rr%out)
          if (len_trim(rr%out(j)%s) > 0) call say('  ' // rr%out(j)%s)
        end do
      end if
    end do

    call blank()
    call say('CONNECTION CONSECRATED.')
    call say('THE REMOTE IS CANONICAL.')
    exit_code = EXIT_OK
  end subroutine github_connect

  ! ------------------------------------------------------------- servants

  subroutine push_step(steps, label, command, live, internal)
    type(plan_step_t), allocatable, intent(inout) :: steps(:)
    character(*), intent(in) :: label, command
    logical, intent(in) :: live, internal
    type(plan_step_t), allocatable :: tmp(:)
    integer :: n
    if (.not. allocated(steps)) allocate (steps(0))
    n = size(steps)
    allocate (tmp(n + 1))
    tmp(1:n) = steps
    tmp(n + 1)%label = label
    tmp(n + 1)%command = command
    tmp(n + 1)%live = live
    tmp(n + 1)%internal = internal
    call move_alloc(tmp, steps)
  end subroutine push_step

  subroutine halt_report(step_no, label, code)
    integer, intent(in) :: step_no, code
    character(*), intent(in) :: label
    call lament('THE RITE HALTS AT STEP ' // int_to_str(step_no) // ': ' // &
                label // ' (EXIT ' // int_to_str(code) // ').')
    call say('NOTHING FURTHER WAS ATTEMPTED. MEND THE FAULT AND RETURN.')
  end subroutine halt_report

  subroutine inscribe_gitignore(ok)
    logical, intent(out) :: ok
    type(string_t), allocatable :: lines(:)
    call push_string(lines, '# KEPT OUTSIDE THE CANON. INSCRIBED BY FORTY.')
    call push_string(lines, 'build/')
    call push_string(lines, 'dist/')
    call write_lines('.gitignore', lines, ok)
  end subroutine inscribe_gitignore

  !> Find the account name in `gh auth status` output without touching
  !> anything resembling a token.
  function parse_login(lines) result(login)
    type(string_t), intent(in) :: lines(:)
    character(:), allocatable :: login
    integer :: i, p, q
    character(:), allocatable :: rest
    login = ''
    do i = 1, size(lines)
      p = index(lines(i)%s, ' account ')
      if (p > 0) then
        rest = lines(i)%s(p + len(' account '):)
        q = index(rest, ' ')
        if (q > 1) then
          login = rest(1:q - 1)
        else
          login = trim(rest)
        end if
        return
      end if
    end do
  end function parse_login

end module forty_github
