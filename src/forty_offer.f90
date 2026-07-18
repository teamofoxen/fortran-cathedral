!> forty_offer: the offering rite. The normal, ongoing Git transaction —
!> add, commit, push, verify — owned by Forty from ground inspection to
!> final accord. One confirmation covers the whole offering. Credentials
!> remain entirely with Git and the GitHub CLI's credential helper;
!> Forty owns sequence, validation, interpretation, and verdict.
!>
!> This rite exists because its absence is a recorded transgression.
module forty_offer
  use forty_util, only: string_t, push_string, int_to_str, starts_with
  use forty_ui, only: say, lament, rule, blank, confirm, ask_line
  use forty_run, only: run_result, run_cmd, run_live
  use forty_git, only: git_initialized, git_branch, git_remote_url, &
                       slug_from_url, valid_slug
  use forty_github, only: plan_step_t, push_step
  use forty_cli, only: cli_t, valid_commit_message
  use forty_canon, only: CANON_COMMIT_TRAILER, EXIT_OK, EXIT_FAIL, &
                         EXIT_USAGE, EXIT_EXTERNAL, EXIT_DECLINED
  implicit none
  private
  public :: run_offer, build_offer_plan, check_offer_ground
  public :: categorize_porcelain, offering_acceptable, porcelain_path

contains

  ! ---------------------------------------------------------- pure parts

  !> The three commands of the offering, as data. The accord
  !> verification that follows them is displayed alongside.
  function build_offer_plan(message) result(steps)
    character(*), intent(in) :: message
    type(plan_step_t), allocatable :: steps(:)
    allocate (steps(0))
    call push_step(steps, 'GATHER THE OFFERING', 'git add -A', &
                   live=.true., internal=.false.)
    call push_step(steps, 'SEAL THE OFFERING', &
                   'git commit -m "' // message // '" -m "' // &
                   CANON_COMMIT_TRAILER // '"', &
                   live=.true., internal=.false.)
    call push_step(steps, 'LIFT IT TO THE REMOTE', 'git push', &
                   live=.true., internal=.false.)
  end function build_offer_plan

  !> The path named by one `git status --porcelain` line: the status
  !> pair, a space, then the path — rename targets and quotes unwrapped.
  pure function porcelain_path(line) result(p)
    character(*), intent(in) :: line
    character(:), allocatable :: p
    integer :: arrow
    p = ''
    if (len(line) < 4) return
    p = line(4:)
    arrow = index(p, ' -> ')
    if (arrow > 0) p = p(arrow + 4:)
    if (len(p) >= 2) then
      if (p(1:1) == '"' .and. p(len(p):len(p)) == '"') p = p(2:len(p) - 1)
    end if
  end function porcelain_path

  !> Summarize the working tree's porcelain report.
  subroutine categorize_porcelain(lines, n_mod, n_new, n_del, n_ren, paths)
    type(string_t), intent(in) :: lines(:)
    integer, intent(out) :: n_mod, n_new, n_del, n_ren
    type(string_t), allocatable, intent(out) :: paths(:)
    integer :: i
    character(2) :: xy
    n_mod = 0; n_new = 0; n_del = 0; n_ren = 0
    allocate (paths(0))
    do i = 1, size(lines)
      if (len_trim(lines(i)%s) < 4) cycle
      xy = lines(i)%s(1:2)
      if (xy == '??') then
        n_new = n_new + 1
      else if (index(xy, 'R') > 0) then
        n_ren = n_ren + 1
      else if (index(xy, 'D') > 0) then
        n_del = n_del + 1
      else
        n_mod = n_mod + 1
      end if
      call push_string(paths, porcelain_path(lines(i)%s))
    end do
  end subroutine categorize_porcelain

  !> Generated or operational residue may not enter an offering. If the
  !> ignores have failed and the yards stand in the porcelain, the whole
  !> offering is refused.
  subroutine offering_acceptable(paths, ok, offending)
    type(string_t), intent(in) :: paths(:)
    logical, intent(out) :: ok
    character(:), allocatable, intent(out) :: offending
    integer :: i
    ok = .true.
    offending = ''
    do i = 1, size(paths)
      if (starts_with(paths(i)%s, 'build/') .or. &
          starts_with(paths(i)%s, 'dist/') .or. &
          starts_with(paths(i)%s, 'build\') .or. &
          starts_with(paths(i)%s, 'dist\')) then
        ok = .false.
        offending = paths(i)%s
        return
      end if
    end do
  end subroutine offering_acceptable

  !> Is the ground fit for an offering? Initialized, remoted, canonical.
  subroutine check_offer_ground(ready, why)
    logical, intent(out) :: ready
    character(:), allocatable, intent(out) :: why
    logical :: found
    character(:), allocatable :: url, slug
    ready = .false.
    why = ''
    if (.not. git_initialized()) then
      why = 'GIT IS UNINITIATED. THE GROUND IS UNCONSECRATED.'
      return
    end if
    call git_remote_url(found, url)
    if (.not. found) then
      why = 'NO REMOTE IS CONFIGURED. CONSECRATE FIRST: forty github connect'
      return
    end if
    slug = slug_from_url(url)
    if (len(slug) == 0 .or. .not. valid_slug(slug)) then
      why = 'THE REMOTE IS NOT A RECOGNIZABLE GITHUB DWELLING: ' // url
      return
    end if
    ready = .true.
  end subroutine check_offer_ground

  ! ------------------------------------------------------------ the rite

  subroutine run_offer(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    type(run_result) :: rr, ra, rb
    type(plan_step_t), allocatable :: steps(:)
    type(string_t), allocatable :: paths(:)
    character(:), allocatable :: why, message, offending, branch
    character(:), allocatable :: local_head, remote_head
    logical :: ready, ok, acceptable
    integer :: i, n_mod, n_new, n_del, n_ren

    ! The ground.
    call check_offer_ground(ready, why)
    if (.not. ready) then
      call lament(why)
      exit_code = EXIT_FAIL
      return
    end if

    ! A provided message is judged before anything else is inspected.
    message = cli%message
    if (len(message) > 0) then
      if (.not. valid_commit_message(message)) then
        call lament('THE MESSAGE IS NOT FIT TO SEAL: FORBIDDEN RUNES OR EMPTINESS.')
        exit_code = EXIT_USAGE
        return
      end if
    end if

    ! The tree.
    rr = run_cmd('git status --porcelain')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE TREE COULD NOT BE READ.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call categorize_porcelain(rr%out, n_mod, n_new, n_del, n_ren, paths)
    if (size(paths) == 0) then
      call say('THERE IS NOTHING TO OFFER. ALL WORKS ARE SEALED.')
      exit_code = EXIT_OK
      return
    end if

    call say('THE OFFERING TABLE:')
    call rule()
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) call say('  ' // rr%out(i)%s)
    end do
    call rule()
    call say('MODIFIED: ' // int_to_str(n_mod) // '.  NEW: ' // &
             int_to_str(n_new) // '.  DELETED: ' // int_to_str(n_del) // &
             '.  RENAMED: ' // int_to_str(n_ren) // '.')

    ! The refusal of residue.
    call offering_acceptable(paths, acceptable, offending)
    if (.not. acceptable) then
      call lament('GENERATED RESIDUE STANDS IN THE OFFERING: ' // offending)
      call say('THE OFFERING IS REFUSED. RESTORE THE IGNORES AND RETURN.')
      exit_code = EXIT_FAIL
      return
    end if

    ! The message, if not yet given.
    if (len(message) == 0) then
      call ask_line('THE MESSAGE TO SEAL UPON IT: ', message, ok)
      if (.not. ok .or. .not. valid_commit_message(message)) then
        call lament('NO FIT MESSAGE WAS GIVEN. THE OFFERING WAITS.')
        exit_code = EXIT_USAGE
        return
      end if
    end if

    branch = git_branch()
    if (len(branch) == 0) then
      call lament('THE BRANCH COULD NOT BE NAMED.')
      exit_code = EXIT_FAIL
      return
    end if

    ! The complete intended sequence.
    steps = build_offer_plan(message)
    call blank()
    call say('THE INTENDED OFFERING:')
    call rule()
    do i = 1, size(steps)
      call say('  ' // int_to_str(i) // '. ' // steps(i)%label)
      call say('       ' // steps(i)%command)
    end do
    call say('  THEN THE ACCORD IS VERIFIED:')
    call say('       git rev-parse HEAD')
    call say('       git rev-parse origin/' // branch)
    call rule()
    call say('BRANCH: ' // branch)
    call blank()

    if (cli%dry_run) then
      call say('THE OFFERING REMAINS UNPERFORMED (--dry-run).')
      exit_code = EXIT_OK
      return
    end if

    ! One confirmation for the whole transaction.
    if (.not. confirm('PERFORM THE COMPLETE OFFERING?', cli%assume_yes)) then
      call say('THE OFFERING IS WITHHELD. THE CATHEDRAL IS PATIENT.')
      exit_code = EXIT_DECLINED
      return
    end if

    do i = 1, size(steps)
      call blank()
      call say('STEP ' // int_to_str(i) // ': ' // steps(i)%label)
      rr = run_live(steps(i)%command)
      if (.not. rr%launched .or. rr%exit_code /= 0) then
        call lament('THE OFFERING HALTS AT STEP ' // int_to_str(i) // ': ' // &
                    steps(i)%label // ' (EXIT ' // int_to_str(rr%exit_code) // ').')
        call say('NOTHING FURTHER WAS ATTEMPTED. MEND THE FAULT AND RETURN.')
        exit_code = EXIT_EXTERNAL
        return
      end if
    end do

    ! The accord.
    call blank()
    call say('VERIFYING THE ACCORD...')
    ra = run_cmd('git rev-parse HEAD')
    rb = run_cmd('git rev-parse origin/' // branch)
    local_head = ''
    remote_head = ''
    if (ra%launched .and. ra%exit_code == 0 .and. size(ra%out) > 0) then
      local_head = trim(ra%out(1)%s)
    end if
    if (rb%launched .and. rb%exit_code == 0 .and. size(rb%out) > 0) then
      remote_head = trim(rb%out(1)%s)
    end if
    if (len(local_head) == 0 .or. local_head /= remote_head) then
      call lament('THE ACCORD IS BROKEN: LOCAL AND REMOTE DIFFER.')
      call say('  LOCAL:  ' // local_head)
      call say('  REMOTE: ' // remote_head)
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('  ' // local_head)
    call blank()
    call say('THE OFFERING IS RECEIVED.')
    call say('LOCAL AND REMOTE ARE OF ONE ACCORD.')
    exit_code = EXIT_OK
  end subroutine run_offer

end module forty_offer
