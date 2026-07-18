!> forty_atone: the rite of restitution. A recorded transgression whose
!> effects entered the canonical remote is expiated — never erased —
!> by a forward-only pair of commits formed with git plumbing:
!>
!>   W: parent = HEAD, tree = the tree before the offense (withdrawal)
!>   R: parent = W,    tree = the canonical tree before expiation
!>
!> main then advances atomically from HEAD to R and is pushed without
!> force. History gains the truth twice over; it loses nothing. The
!> checked-out working tree never passes through the withdrawn state,
!> because only refs move and R's tree equals the tree already on disk.
module forty_atone
  use forty_util, only: string_t, push_string, int_to_str
  use forty_ui, only: say, lament, rule, blank, confirm
  use forty_run, only: run_result, run_cmd, run_live, version_line, &
                       read_all_lines, write_lines
  use forty_git, only: git_initialized, git_branch, git_remote_url, is_hash
  use forty_cli, only: cli_t
  use forty_confess, only: transgression_t, ledger_transgressions
  use forty_offer, only: run_offer
  use cathedral_generate, only: run_generate
  use cathedral_validate, only: run_validate
  use forty_canon, only: CANON_OFFENDING_COMMIT, CANON_COMMIT_TRAILER, &
                         EXIT_OK, EXIT_FAIL, EXIT_USAGE, EXIT_EXTERNAL, &
                         EXIT_DECLINED
  implicit none
  private
  public :: run_atone, perform_restitution, expiate_ledger_lines
  public :: MSG_WITHDRAW, MSG_ANEW, MSG_EXPIATED, STATUS_EXPIATED

  character(*), parameter :: MSG_WITHDRAW = &
    'EXPIATION: THE MANUAL OFFERING IS WITHDRAWN.'
  character(*), parameter :: MSG_ANEW = &
    'EXPIATION: FORTY PRESENTS THE CATHEDRAL ANEW.'
  character(*), parameter :: MSG_EXPIATED = &
    'PHASE 1.2: THE STAIN IS EXPIATED.'
  character(*), parameter :: STATUS_OLD = 'Historical. Disclosed. Not erasable.'
  character(*), parameter :: STATUS_EXPIATED = 'EXPIATED, NOT ERASED.'

contains

  !> The named rite, bound to the canonical offense, followed by the
  !> public confession: regenerate, validate, and offer the ledger.
  subroutine run_atone(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    character(:), allocatable :: w, r
    type(cli_t) :: ocli
    integer :: code

    if (cli%rite /= 'phase-1-manual-offering') then
      call lament('UNKNOWN ATONEMENT: ' // cli%rite)
      call say('KNOWN RITES: phase-1-manual-offering')
      exit_code = EXIT_USAGE
      return
    end if

    call perform_restitution(cli, CANON_OFFENDING_COMMIT, w, r, code)
    if (code /= EXIT_OK) then
      exit_code = code
      return
    end if
    if (len(w) == 0) then
      ! A dry run, or a stain already expiated: nothing further to do.
      exit_code = EXIT_OK
      return
    end if

    ! The public confession follows the restitution.
    call blank()
    call run_generate(code)
    if (code /= EXIT_OK) then
      call lament('THE CONFESSIONAL COULD NOT BE REGENERATED. THE RITE PAUSES.')
      exit_code = code
      return
    end if
    call run_validate(code)
    if (code /= EXIT_OK) then
      call lament('THE REGENERATED FABRIC IS UNSOUND. THE RITE PAUSES.')
      exit_code = code
      return
    end if

    ! The ledger's new truth is offered through Forty's own machinery.
    ! The rite's single confirmation covers this concluding offering.
    call blank()
    ocli%message = MSG_EXPIATED
    ocli%assume_yes = .true.
    ocli%dry_run = .false.
    call run_offer(ocli, code)
    if (code /= EXIT_OK) then
      call lament('THE EXPIATION RECORD COULD NOT BE OFFERED. THE RITE PAUSES.')
      exit_code = code
      return
    end if

    call blank()
    call say('THE MANUAL OFFERING HAS BEEN WITHDRAWN.')
    call say('THE CATHEDRAL HAS BEEN PRESENTED ANEW BY FORTY.')
    call say('THE TRANSGRESSION REMAINS IN THE RECORD.')
    call say('THE STAIN IS EXPIATED.')
    call say('GO, AND PUSH MANUALLY NO MORE.')
    exit_code = EXIT_OK
  end subroutine run_atone

  !> The core rite, parameterized by the offending commit so the trials
  !> may perform it in fixture repositories against local bare remotes.
  !> On success w_out and r_out carry the two expiation commits; both
  !> come back empty for a dry run or an already-expiated stain.
  subroutine perform_restitution(cli, offender, w_out, r_out, exit_code)
    type(cli_t), intent(in) :: cli
    character(*), intent(in) :: offender
    character(:), allocatable, intent(out) :: w_out, r_out
    integer, intent(out) :: exit_code
    type(run_result) :: rr
    type(string_t), allocatable :: ledger(:), newledger(:)
    type(transgression_t), allocatable :: trans(:)
    logical :: found, wellformed, ok, changed
    integer :: i, idx
    character(:), allocatable :: branch, url, head, omain
    character(:), allocatable :: pre_tree, canon_tree, w, r, probe

    w_out = ''
    r_out = ''
    exit_code = EXIT_FAIL

    ! 1. The ground.
    if (.not. git_initialized()) then
      call lament('GIT IS UNINITIATED. THERE IS NOTHING TO ATONE UPON.')
      return
    end if
    branch = git_branch()
    if (branch /= 'main') then
      call lament('THE RITE IS PERFORMED ON main ALONE. THIS IS: ' // branch)
      return
    end if
    call git_remote_url(found, url)
    if (.not. found) then
      call lament('NO CANONICAL REMOTE EXISTS. RESTITUTION NEEDS A REMOTE.')
      return
    end if

    ! 2. Clean and of one accord.
    rr = run_cmd('git status --porcelain')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE TREE COULD NOT BE READ.')
      return
    end if
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) then
        call lament('THE TREE IS UNCLEAN. OFFER OR SWEEP BEFORE ATONING.')
        return
      end if
    end do
    head = version_line('git rev-parse HEAD')
    omain = version_line('git rev-parse origin/' // branch)
    if (.not. is_hash(head) .or. .not. is_hash(omain)) then
      call lament('THE HEADS COULD NOT BE NAMED.')
      return
    end if
    if (head /= omain) then
      call lament('LOCAL AND REMOTE ARE NOT OF ONE ACCORD. RECONCILE FIRST.')
      return
    end if

    ! 3. The ledger remembers the offense.
    call read_all_lines('HERESY_LEDGER.md', ledger)
    if (size(ledger) == 0) then
      call lament('THE LEDGER ITSELF IS MISSING.')
      return
    end if
    call ledger_transgressions(ledger, trans, wellformed)
    if (.not. wellformed) then
      call lament('THE OPERATIONAL CHAPTER IS MISSING OR MALFORMED.')
      return
    end if
    idx = 0
    do i = 1, size(trans)
      if (trans(i)%commit == offender) idx = i
    end do
    if (idx == 0) then
      call lament('THE LEDGER DOES NOT RECORD THIS OFFENSE: ' // offender)
      return
    end if
    if (index(trans(idx)%status, 'EXPIATED') > 0) then
      call say('THE STAIN IS ALREADY EXPIATED. ATONE NO FURTHER.')
      exit_code = EXIT_OK
      return
    end if

    ! 4. The offense stands in our history.
    rr = run_cmd('git cat-file -t ' // offender)
    if (.not. rr%launched .or. rr%exit_code /= 0 .or. size(rr%out) == 0) then
      call lament('THE OFFENDING COMMIT IS NOT FOUND: ' // offender)
      return
    end if
    if (rr%out(1)%s /= 'commit') then
      call lament('THE NAMED OBJECT IS NOT A COMMIT: ' // offender)
      return
    end if
    rr = run_cmd('git merge-base --is-ancestor ' // offender // ' HEAD')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE OFFENSE IS NOT IN OUR HISTORY. THERE IS NOTHING TO WITHDRAW.')
      return
    end if

    ! 5. The trees are resolved and recorded.
    pre_tree = version_line('git rev-parse ' // offender // '~1:')
    if (.not. is_hash(pre_tree)) then
      call lament('THE OFFENSE HAS NO PREDECESSOR TREE. THE RITE CANNOT FORM.')
      return
    end if
    canon_tree = version_line('git rev-parse HEAD:')
    if (.not. is_hash(canon_tree)) then
      call lament('THE CANONICAL TREE COULD NOT BE NAMED.')
      return
    end if

    ! 6. The complete intended rite.
    call say('THE RITE OF RESTITUTION.')
    call rule()
    call say('THE OFFENSE:              ' // offender)
    call say('TREE BEFORE THE OFFENSE:  ' // pre_tree)
    call say('CANONICAL TREE (NOW):     ' // canon_tree)
    call say('CURRENT HEAD:             ' // head)
    call blank()
    call say('THE INTENDED RITE:')
    call say('  1. WITHDRAW.  git commit-tree ' // pre_tree)
    call say('       -p ' // head)
    call say('       -m "' // MSG_WITHDRAW // '"')
    call say('  2. PRESENT ANEW.  git commit-tree ' // canon_tree)
    call say('       -p (THE WITHDRAWAL, ONCE NAMED)')
    call say('       -m "' // MSG_ANEW // '"')
    call say('  3. ADVANCE main ATOMICALLY.  git update-ref refs/heads/main')
    call say('       (RE-OFFERING) (EXPECTING ' // head // ')')
    call say('  4. LIFT.  git push  (NO FORCE. FORWARD ONLY.)')
    call say('  5. VERIFY TREES, PARENTAGE, AND THE ACCORD.')
    call say('  6. THE LEDGER BECOMES: ' // STATUS_EXPIATED)
    call rule()
    call say('NO HISTORY IS ERASED, AMENDED, SQUASHED, OR REWRITTEN.')
    call say('THE TRANSGRESSION REMAINS VISIBLE FOREVER.')
    call blank()

    if (cli%dry_run) then
      call say('THE RITE REMAINS UNPERFORMED (--dry-run).')
      exit_code = EXIT_OK
      return
    end if

    ! One confirmation for the whole transaction.
    if (.not. confirm('PERFORM THE COMPLETE RITE OF RESTITUTION?', &
                      cli%assume_yes)) then
      call say('THE STAIN REMAINS. THE CATHEDRAL IS PATIENT.')
      exit_code = EXIT_DECLINED
      return
    end if

    ! 7. The withdrawal is formed. Nothing has moved yet.
    call blank()
    w = version_line('git commit-tree ' // pre_tree // ' -p ' // head // &
                     ' -m "' // MSG_WITHDRAW // '" -m "' // &
                     CANON_COMMIT_TRAILER // '"')
    if (.not. is_hash(w)) then
      call lament('THE WITHDRAWAL COULD NOT BE FORMED. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('THE WITHDRAWAL IS FORMED:  ' // w)

    ! 8. The re-offering is formed. Still nothing has moved.
    r = version_line('git commit-tree ' // canon_tree // ' -p ' // w // &
                     ' -m "' // MSG_ANEW // '" -m "' // &
                     CANON_COMMIT_TRAILER // '"')
    if (.not. is_hash(r)) then
      call lament('THE RE-OFFERING COULD NOT BE FORMED. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('THE RE-OFFERING IS FORMED: ' // r)

    ! Verified before any ref advances: trees and parentage.
    probe = version_line('git rev-parse ' // r // ':')
    if (probe /= canon_tree) then
      call lament('THE RE-OFFERED TREE IS NOT THE CANONICAL TREE. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    probe = version_line('git rev-parse ' // w // ':')
    if (probe /= pre_tree) then
      call lament('THE WITHDRAWN TREE IS NOT THE PREDECESSOR TREE. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    probe = version_line('git rev-parse ' // r // '~1')
    if (probe /= w) then
      call lament('THE RE-OFFERING DOES NOT FOLLOW THE WITHDRAWAL. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    probe = version_line('git rev-parse ' // w // '~1')
    if (probe /= head) then
      call lament('THE WITHDRAWAL DOES NOT FOLLOW THE PRESENT. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if

    ! 9. main advances atomically, expecting its old position.
    rr = run_cmd('git update-ref refs/heads/main ' // r // ' ' // head)
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('main COULD NOT BE ADVANCED. NOTHING HAS MOVED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('main HAS ADVANCED: ' // head(1:8) // ' -> ' // r(1:8))

    rr = run_live('git push')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE LIFT FAILED. main STANDS AT THE RE-OFFERING LOCALLY;')
      call say('THE REMOTE HAS NOT MOVED. MEND THE WAY AND ATONE AGAIN.')
      exit_code = EXIT_EXTERNAL
      return
    end if

    ! 10-11. The accord, the trees, and the delivery of both commits.
    call blank()
    call say('VERIFYING THE RESTITUTION...')
    omain = version_line('git rev-parse origin/' // branch)
    if (omain /= r) then
      call lament('THE REMOTE DID NOT RECEIVE THE RE-OFFERING.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    probe = version_line('git rev-parse origin/' // branch // '~1')
    if (probe /= w) then
      call lament('THE REMOTE DID NOT RECEIVE THE WITHDRAWAL.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    probe = version_line('git rev-parse HEAD:')
    if (probe /= canon_tree) then
      call lament('THE FINAL TREE IS NOT THE CANONICAL TREE.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('  RE-OFFERED TREE IDENTICAL TO THE CANONICAL TREE: ' // canon_tree)
    call say('  REMOTE HOLDS THE WITHDRAWAL AND THE RE-OFFERING.')
    call say('  LOCAL AND REMOTE ARE OF ONE ACCORD: ' // r)

    ! 12-13. The ledger's status becomes EXPIATED, NOT ERASED, and the
    ! full record of the means is inscribed.
    call expiate_ledger_lines(ledger, offender, w, r, newledger, changed)
    if (.not. changed) then
      call lament('THE LEDGER COULD NOT BE BROUGHT TO ITS NEW TRUTH.')
      exit_code = EXIT_FAIL
      return
    end if
    call write_lines('HERESY_LEDGER.md', newledger, ok)
    if (.not. ok) then
      call lament('THE LEDGER COULD NOT BE WRITTEN.')
      exit_code = EXIT_FAIL
      return
    end if
    call say('THE LEDGER NOW READS: ' // STATUS_EXPIATED)

    w_out = w
    r_out = r
    exit_code = EXIT_OK
  end subroutine perform_restitution

  !> The ledger's transition, as a pure transform the trials can judge:
  !> the offending row's status becomes EXPIATED, NOT ERASED, and an
  !> expiation-record chapter is inscribed before the rules.
  subroutine expiate_ledger_lines(lines, offender, w, r, out_lines, changed)
    type(string_t), intent(in) :: lines(:)
    character(*), intent(in) :: offender, w, r
    type(string_t), allocatable, intent(out) :: out_lines(:)
    logical, intent(out) :: changed
    integer :: i, p
    logical :: replaced, inserted
    character(:), allocatable :: line

    allocate (out_lines(0))
    replaced = .false.
    inserted = .false.
    do i = 1, size(lines)
      line = lines(i)%s
      if (.not. inserted .and. trim(adjustl(line)) == '## Rules') then
        call append_expiation_chapter(out_lines, offender, w, r)
        inserted = .true.
      end if
      if (.not. replaced .and. index(line, offender) > 0) then
        p = index(line, STATUS_OLD)
        if (p > 0) then
          line = line(1:p - 1) // STATUS_EXPIATED // line(p + len(STATUS_OLD):)
          replaced = .true.
        end if
      end if
      call push_string(out_lines, line)
    end do
    if (.not. inserted) then
      call append_expiation_chapter(out_lines, offender, w, r)
      inserted = .true.
    end if
    changed = replaced
  end subroutine expiate_ledger_lines

  subroutine append_expiation_chapter(out_lines, offender, w, r)
    type(string_t), allocatable, intent(inout) :: out_lines(:)
    character(*), intent(in) :: offender, w, r
    call push_string(out_lines, '## Expiation record')
    call push_string(out_lines, '')
    call push_string(out_lines, 'The stain of the manual Phase 1 offering has been ' // &
                     'expiated by restitution.')
    call push_string(out_lines, 'Nothing was erased, amended, squashed, or ' // &
                     'rewritten; the rite is forward-only.')
    call push_string(out_lines, '')
    call push_string(out_lines, '| Field | Value |')
    call push_string(out_lines, '|---|---|')
    call push_string(out_lines, '| Expiated transgression | `' // offender // '` |')
    call push_string(out_lines, '| Withdrawal commit | `' // w // '` |')
    call push_string(out_lines, '| Canonical re-offering commit | `' // r // '` |')
    call push_string(out_lines, '| Means | Forward-only withdrawal and re-offering ' // &
                     'formed with git commit-tree; main advanced atomically with ' // &
                     'git update-ref and pushed without force |')
    call push_string(out_lines, '| History | The original transgression remains ' // &
                     'permanently in history |')
    call push_string(out_lines, '')
  end subroutine append_expiation_chapter

end module forty_atone
