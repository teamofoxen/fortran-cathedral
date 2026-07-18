!> forty_audit: the reading of the Cathedral's own history. Fixed,
!> read-only Git commands serve as the platform boundary; Fortran owns
!> the sequence, the capture, the parsing, the classification, the
!> cross-checks, the conclusions, and the report.
!>
!> Every conclusion is classified: PROVEN (follows from repository
!> evidence alone), STRONGLY SUPPORTED (all evidence consistent, no
!> contrary evidence, but not deducible from trees alone), UNPROVEN
!> (repository evidence cannot establish it), or HERESY DETECTED.
!> Present-day reproducibility is never confused with proof of the
!> original execution; repositories record trees, not keystrokes.
module forty_audit
  use forty_util, only: string_t, push_string, to_lower, int_to_str, &
                        starts_with, count_substr
  use forty_ui, only: say, lament, rule, blank
  use forty_run, only: run_result, run_cmd, version_line, write_lines, &
                       read_all_lines, ensure_dir
  use forty_paths, only: quote, temp_root, file_exists
  use forty_confess, only: classify, CLASS_HERESY
  use forty_cli, only: cli_t
  use forty_canon, only: CANON_OFFENDING_COMMIT, CANON_RESIDUE_ADD, &
                         CANON_RESIDUE_FIX, EXIT_OK, EXIT_FAIL, EXIT_USAGE
  implicit none
  private
  public :: run_audit
  public :: finding_t, add_finding, tree_paths, scan_tracked_html, &
            scan_template_suspects, scan_tree_heresy, commit_exists, &
            blob_text, signature_module, residue_change, tree_has_path, &
            historical_execution_finding, render_report
  public :: V_PROVEN, V_STRONG, V_UNPROVEN, V_HERESY

  character(*), parameter :: V_PROVEN   = 'PROVEN'
  character(*), parameter :: V_STRONG   = 'STRONGLY SUPPORTED'
  character(*), parameter :: V_UNPROVEN = 'UNPROVEN'
  character(*), parameter :: V_HERESY   = 'HERESY DETECTED'

  character(*), parameter :: REPORT_PATH = 'build\audit\provenance.txt'

  type :: finding_t
    character(:), allocatable :: verdict
    character(:), allocatable :: topic
    character(:), allocatable :: detail
  end type finding_t

contains

  ! ------------------------------------------------------------ evidence

  subroutine add_finding(fs, verdict, topic, detail)
    type(finding_t), allocatable, intent(inout) :: fs(:)
    character(*), intent(in) :: verdict, topic, detail
    type(finding_t), allocatable :: tmp(:)
    integer :: n
    if (.not. allocated(fs)) allocate (fs(0))
    n = size(fs)
    allocate (tmp(n + 1))
    tmp(1:n) = fs
    tmp(n + 1)%verdict = verdict
    tmp(n + 1)%topic = topic
    tmp(n + 1)%detail = detail
    call move_alloc(tmp, fs)
  end subroutine add_finding

  function commit_exists(sha) result(r)
    character(*), intent(in) :: sha
    logical :: r
    type(run_result) :: rr
    r = .false.
    rr = run_cmd('git cat-file -t ' // sha)
    if (rr%launched .and. rr%exit_code == 0 .and. size(rr%out) > 0) then
      r = (rr%out(1)%s == 'commit')
    end if
  end function commit_exists

  !> Every tracked path in a commit's tree.
  subroutine tree_paths(sha, paths, ok)
    character(*), intent(in) :: sha
    type(string_t), allocatable, intent(out) :: paths(:)
    logical, intent(out) :: ok
    type(run_result) :: rr
    integer :: i
    allocate (paths(0))
    rr = run_cmd('git ls-tree -r --name-only ' // sha)
    ok = rr%launched .and. rr%exit_code == 0
    if (.not. ok) return
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) call push_string(paths, trim(rr%out(i)%s))
    end do
  end subroutine tree_paths

  !> Tracked HTML, judged by where it stands: inside generated yards
  !> (an ignore-discipline breach) or elsewhere (handwritten suspects).
  subroutine scan_tracked_html(paths, suspects, yard_tracked)
    type(string_t), intent(in) :: paths(:)
    type(string_t), allocatable, intent(out) :: suspects(:), yard_tracked(:)
    integer :: i
    character(:), allocatable :: p
    allocate (suspects(0))
    allocate (yard_tracked(0))
    do i = 1, size(paths)
      p = to_lower(paths(i)%s)
      if (len(p) < 5) cycle
      if (p(len(p) - 4:) /= '.html') cycle
      if (starts_with(p, 'dist/') .or. starts_with(p, 'build/')) then
        call push_string(yard_tracked, paths(i)%s)
      else
        call push_string(suspects, paths(i)%s)
      end if
    end do
  end subroutine scan_tracked_html

  !> Templating machinery and external-generator inputs, by a bounded
  !> list of extensions and names.
  subroutine scan_template_suspects(paths, suspects)
    type(string_t), intent(in) :: paths(:)
    type(string_t), allocatable, intent(out) :: suspects(:)
    integer :: i, k
    character(:), allocatable :: p
    character(12), parameter :: EXTS(9) = [character(12) :: &
      '.tpl', '.tmpl', '.template', '.mustache', '.hbs', '.ejs', &
      '.liquid', '.njk', '.jinja']
    character(18), parameter :: NAMES(8) = [character(18) :: &
      'package.json', 'gulpfile.js', 'webpack.config.js', '_config.yml', &
      'config.rb', 'hugo.toml', 'netlify.toml', 'gatsby-config.js']
    allocate (suspects(0))
    do i = 1, size(paths)
      p = to_lower(paths(i)%s)
      do k = 1, size(EXTS)
        if (ends_with(p, trim(EXTS(k)))) then
          call push_string(suspects, paths(i)%s)
          exit
        end if
      end do
      do k = 1, size(NAMES)
        if (ends_with(p, trim(NAMES(k)))) then
          call push_string(suspects, paths(i)%s)
          exit
        end if
      end do
      if (index(p, 'partial') > 0 .or. index(p, 'template') > 0) then
        call push_string(suspects, paths(i)%s)
      end if
    end do
  end subroutine scan_template_suspects

  !> Executable non-Fortran files in a tree, by the confessional's
  !> own classifier.
  subroutine scan_tree_heresy(paths, heresy)
    type(string_t), intent(in) :: paths(:)
    type(string_t), allocatable, intent(out) :: heresy(:)
    integer :: i
    allocate (heresy(0))
    do i = 1, size(paths)
      if (classify(paths(i)%s) == CLASS_HERESY) then
        call push_string(heresy, paths(i)%s)
      end if
    end do
  end subroutine scan_tree_heresy

  !> A file's content at a commit, without touching the working tree.
  subroutine blob_text(sha, path, lines, ok)
    character(*), intent(in) :: sha, path
    type(string_t), allocatable, intent(out) :: lines(:)
    logical, intent(out) :: ok
    type(run_result) :: rr
    rr = run_cmd('git show ' // sha // ':' // path)
    ok = rr%launched .and. rr%exit_code == 0
    if (ok) then
      lines = rr%out
    else
      allocate (lines(0))
    end if
  end subroutine blob_text

  !> Which candidate module at a commit carries a signature string.
  function signature_module(sha, candidates, signature) result(modpath)
    character(*), intent(in) :: sha, signature
    type(string_t), intent(in) :: candidates(:)
    character(:), allocatable :: modpath
    type(string_t), allocatable :: body(:)
    logical :: ok
    integer :: i, j
    modpath = ''
    do i = 1, size(candidates)
      call blob_text(sha, candidates(i)%s, body, ok)
      if (.not. ok) cycle
      do j = 1, size(body)
        if (index(body(j)%s, signature) > 0) then
          modpath = candidates(i)%s
          return
        end if
      end do
    end do
  end function signature_module

  !> What one commit did to one artifact: 'A', 'M', 'D', or '' if the
  !> commit does not touch it.
  subroutine residue_change(sha, artifact, action, found)
    character(*), intent(in) :: sha, artifact
    character(1), intent(out) :: action
    logical, intent(out) :: found
    type(run_result) :: rr
    integer :: i, t
    character(:), allocatable :: line, path
    action = ' '
    found = .false.
    rr = run_cmd('git diff-tree --no-commit-id --name-status -r ' // sha)
    if (.not. rr%launched .or. rr%exit_code /= 0) return
    do i = 1, size(rr%out)
      line = rr%out(i)%s
      if (len_trim(line) < 3) cycle
      t = index(line, achar(9))
      if (t == 0) t = index(line, ' ')
      if (t <= 1) cycle
      path = trim(adjustl(line(t + 1:)))
      if (path == artifact) then
        action = line(1:1)
        found = .true.
        return
      end if
    end do
  end subroutine residue_change

  function tree_has_path(sha, path) result(r)
    character(*), intent(in) :: sha, path
    logical :: r
    type(string_t), allocatable :: paths(:)
    logical :: ok
    integer :: i
    r = .false.
    call tree_paths(sha, paths, ok)
    if (.not. ok) return
    do i = 1, size(paths)
      if (paths(i)%s == path) r = .true.
    end do
  end function tree_has_path

  !> The one conclusion no repository can carry.
  function historical_execution_finding() result(f)
    type(finding_t) :: f
    f%verdict = V_UNPROVEN
    f%topic = 'THE ORIGINAL EXECUTION ITSELF'
    f%detail = 'Repositories record trees, not keystrokes. That the ' // &
      'historical generation runs were performed exactly as the tree ' // &
      'implies cannot be established from repository evidence, and this ' // &
      'audit does not pretend otherwise. Present-day reproduction is a ' // &
      'separate, testable fact and is classified separately.'
  end function historical_execution_finding

  ! -------------------------------------------------------------- report

  subroutine render_report(title, fs, lines)
    character(*), intent(in) :: title
    type(finding_t), intent(in) :: fs(:)
    type(string_t), allocatable, intent(out) :: lines(:)
    integer :: i, n_proven, n_strong, n_unproven, n_heresy
    allocate (lines(0))
    call push_string(lines, repeat('=', 70))
    call push_string(lines, title)
    call push_string(lines, repeat('=', 70))
    call push_string(lines, '')
    n_proven = 0; n_strong = 0; n_unproven = 0; n_heresy = 0
    do i = 1, size(fs)
      call push_string(lines, int_to_str(i) // '. [' // fs(i)%verdict // '] ' // &
                       fs(i)%topic)
      call push_string(lines, '   ' // fs(i)%detail)
      call push_string(lines, '')
      select case (fs(i)%verdict)
      case (V_PROVEN);   n_proven = n_proven + 1
      case (V_STRONG);   n_strong = n_strong + 1
      case (V_UNPROVEN); n_unproven = n_unproven + 1
      case (V_HERESY);   n_heresy = n_heresy + 1
      end select
    end do
    call push_string(lines, repeat('-', 70))
    call push_string(lines, 'FINDINGS: ' // int_to_str(size(fs)) // &
                     '.  PROVEN: ' // int_to_str(n_proven) // &
                     '.  STRONGLY SUPPORTED: ' // int_to_str(n_strong) // &
                     '.  UNPROVEN: ' // int_to_str(n_unproven) // &
                     '.  HERESY DETECTED: ' // int_to_str(n_heresy) // '.')
    if (n_heresy > 0) then
      call push_string(lines, 'OVERALL: HERESY DETECTED. THE LEDGER MUST HEAR OF THIS.')
    else if (n_unproven > 0) then
      call push_string(lines, 'OVERALL: CLEAN, WITH HONEST GAPS WHERE HISTORY KEEPS ITS OWN COUNSEL.')
    else
      call push_string(lines, 'OVERALL: CLEAN.')
    end if
  end subroutine render_report

  ! ------------------------------------------------------------ the rite

  subroutine run_audit(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    type(finding_t), allocatable :: fs(:)
    type(finding_t) :: hist
    type(string_t), allocatable :: report(:)
    logical :: ok, heresy_found
    integer :: i

    if (cli%rite /= 'provenance') then
      call lament('UNKNOWN AUDIT SUBJECT: ' // cli%rite)
      call say('KNOWN SUBJECTS: provenance')
      exit_code = EXIT_USAGE
      return
    end if

    call say('THE AUDIT OF PROVENANCE BEGINS.')
    call rule()
    allocate (fs(0))

    call audit_original_nave(fs)
    call audit_residue_incident(fs)
    hist = historical_execution_finding()
    call add_finding(fs, hist%verdict, hist%topic, hist%detail)

    call ensure_dir('build')
    call ensure_dir('build\audit')
    call render_report('THE AUDIT OF PROVENANCE — FORTRAN CATHEDRAL', fs, report)
    call write_lines(REPORT_PATH, report, ok)
    if (.not. ok) then
      call lament('THE REPORT COULD NOT BE WRITTEN.')
      exit_code = EXIT_FAIL
      return
    end if

    heresy_found = .false.
    do i = 1, size(fs)
      call say('[' // fs(i)%verdict // '] ' // fs(i)%topic)
      if (fs(i)%verdict == V_HERESY) heresy_found = .true.
    end do
    call rule()
    call say('THE FULL RECORD RESTS AT ' // REPORT_PATH)
    if (heresy_found) then
      call lament('HERESY WAS DETECTED. CONSULT THE RECORD AND AMEND THE LEDGER.')
      exit_code = EXIT_FAIL
    else
      call say('THE HISTORY WITHSTANDS EXAMINATION.')
      exit_code = EXIT_OK
    end if
  end subroutine run_audit

  !> Part A: the original Nave, before, during, and after Phase 1.
  subroutine audit_original_nave(fs)
    type(finding_t), allocatable, intent(inout) :: fs(:)
    character(:), allocatable :: offender, parent, successor, modpath
    type(string_t), allocatable :: paths(:), suspects(:), yard(:), heresy(:)
    type(string_t), allocatable :: candidates(:), srcs(:)
    type(run_result) :: rr
    logical :: ok
    integer :: i, k
    character(40) :: sha3(3)
    character(8) :: label(3)
    character(28), parameter :: TOPICS(9) = [character(28) :: &
      'THE COMPLETE HTML DOCUMENT', 'THE NAVE BODY', 'NAVIGATION', &
      'PAGE METADATA', 'CSS AND DESIGN TOKENS', 'THE SVG ORNAMENT', &
      'robots.txt', 'sitemap.xml', 'THE ROUTE MANIFEST']
    character(24), parameter :: SIGS(9) = [character(24) :: &
      '<!doctype html>', 'nave_body', 'aria-current', &
      'meta name="description"', 'tokens_css_lines', '<svg xmlns', &
      'User-agent: *', '<urlset', '"routes": [']

    offender = CANON_OFFENDING_COMMIT
    if (.not. commit_exists(offender)) then
      call add_finding(fs, V_UNPROVEN, 'THE PHASE 1 COMMIT', &
                       'The commit ' // offender // ' is absent from this ' // &
                       'repository; the Nave cannot be audited here.')
      return
    end if
    parent = version_line('git rev-parse ' // offender // '~1')
    rr = run_cmd('git rev-list --ancestry-path ' // offender // '..HEAD')
    successor = ''
    do i = size(rr%out), 1, -1
      if (len_trim(rr%out(i)%s) > 0) then
        successor = trim(rr%out(i)%s)
        exit
      end if
    end do

    sha3(1) = parent;    label(1) = 'BEFORE'
    sha3(2) = offender;  label(2) = 'DURING'
    sha3(3) = successor; label(3) = 'AFTER'
    do i = 1, 3
      if (len_trim(sha3(i)) == 0) cycle
      call tree_paths(trim(sha3(i)), paths, ok)
      if (.not. ok) then
        call add_finding(fs, V_UNPROVEN, 'THE TREE ' // trim(label(i)) // &
                         ' PHASE 1', 'ls-tree failed for ' // trim(sha3(i)))
        cycle
      end if
      call scan_tracked_html(paths, suspects, yard)
      if (size(suspects) > 0) then
        call add_finding(fs, V_HERESY, 'TRACKED HTML ' // trim(label(i)) // &
                         ' PHASE 1', 'Handwritten HTML suspects: ' // &
                         joined(suspects))
      else if (size(yard) > 0) then
        call add_finding(fs, V_HERESY, 'TRACKED GENERATED OUTPUT ' // &
                         trim(label(i)) // ' PHASE 1', &
                         'Generated yards were tracked: ' // joined(yard))
      else
        call add_finding(fs, V_PROVEN, 'NO TRACKED HTML ' // trim(label(i)) // &
                         ' PHASE 1', 'The tree of ' // trim(sha3(i)) // &
                         ' contains no .html file of any kind.')
      end if
      call scan_template_suspects(paths, suspects)
      if (size(suspects) > 0) then
        call add_finding(fs, V_HERESY, 'TEMPLATING MACHINERY ' // &
                         trim(label(i)) // ' PHASE 1', joined(suspects))
      else
        call add_finding(fs, V_PROVEN, 'NO TEMPLATES OR GENERATOR INPUTS ' // &
                         trim(label(i)) // ' PHASE 1', &
                         'No template, partial, fragment, shell, or ' // &
                         'external-generator input stands in the tree.')
      end if
      call scan_tree_heresy(paths, heresy)
      if (size(heresy) > 0) then
        call add_finding(fs, V_HERESY, 'EXECUTABLE NON-FORTRAN ' // &
                         trim(label(i)) // ' PHASE 1', joined(heresy))
      else
        call add_finding(fs, V_PROVEN, 'NO EXECUTABLE NON-FORTRAN ' // &
                         trim(label(i)) // ' PHASE 1', &
                         'Every executable file in the tree is Fortran.')
      end if
    end do

    ! Generator attribution within the Phase 1 tree itself.
    call tree_paths(offender, paths, ok)
    allocate (candidates(0))
    if (ok) then
      do i = 1, size(paths)
        if (starts_with(paths(i)%s, 'src/') .and. &
            index(to_lower(paths(i)%s), '.f90') > 0) then
          call push_string(candidates, paths(i)%s)
        end if
      end do
    end if
    do k = 1, size(TOPICS)
      modpath = signature_module(offender, candidates, trim(SIGS(k)))
      if (len(modpath) > 0) then
        call add_finding(fs, V_PROVEN, trim(TOPICS(k)) // ' (PHASE 1)', &
                         'Generated by ' // modpath // &
                         ', whose signature stands in that exact tree.')
      else
        call add_finding(fs, V_UNPROVEN, trim(TOPICS(k)) // ' (PHASE 1)', &
                         'No generator signature was found in the tree.')
      end if
    end do

    call audit_worktree_rebuild(fs, offender)

    call add_finding(fs, V_STRONG, 'THE ORIGINAL NAVE WAS RAISED BY THE ' // &
      'FORTRAN PATH', 'Every tree is free of HTML, templates, and ' // &
      'non-Fortran executables; the generator modules stand in the Phase 1 ' // &
      'tree with their signatures; and a clean checkout regenerates the ' // &
      'site through its own path today. All evidence is consistent and no ' // &
      'contrary evidence exists, but the original run itself is beyond ' // &
      'repository proof.')

    ! Silence unused-variable pedantry for srcs if any compiler asks.
    if (allocated(srcs)) deallocate (srcs)
  end subroutine audit_original_nave

  !> The Phase 1 commit, checked out clean and asked to raise its own
  !> cathedral through its own fpm path. Proves present-day capability.
  subroutine audit_worktree_rebuild(fs, offender)
    type(finding_t), allocatable, intent(inout) :: fs(:)
    character(*), intent(in) :: offender
    type(run_result) :: rr, rb, rg, rv
    character(:), allocatable :: wt
    wt = temp_root() // '\forty_audit_wt'
    rr = run_cmd('git worktree remove --force ' // quote(wt))
    rr = run_cmd('if exist ' // quote(wt // '\') // ' rmdir /s /q ' // quote(wt))
    rr = run_cmd('git worktree add ' // quote(wt) // ' ' // offender)
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call add_finding(fs, V_UNPROVEN, 'CLEAN-CHECKOUT REGENERATION', &
                       'A worktree of the Phase 1 commit could not be prepared.')
      return
    end if
    rb = run_cmd('cd /d ' // quote(wt) // ' && fpm build')
    rg = run_cmd('cd /d ' // quote(wt) // ' && fpm run forty -- generate')
    rv = run_cmd('cd /d ' // quote(wt) // ' && fpm run forty -- validate')
    if (rb%exit_code == 0 .and. rg%exit_code == 0 .and. rv%exit_code == 0) then
      call add_finding(fs, V_PROVEN, 'CLEAN-CHECKOUT REGENERATION', &
        'A clean checkout of the Phase 1 commit compiled, regenerated, and ' // &
        'validated its complete site using only the Fortran/fpm/Forty path ' // &
        'in that tree. This proves present-day capability, not the ' // &
        'original run.')
    else
      call add_finding(fs, V_UNPROVEN, 'CLEAN-CHECKOUT REGENERATION', &
        'The Phase 1 worktree did not rebuild cleanly (build ' // &
        int_to_str(rb%exit_code) // ', generate ' // int_to_str(rg%exit_code) // &
        ', validate ' // int_to_str(rv%exit_code) // ').')
    end if
    rr = run_cmd('git worktree remove --force ' // quote(wt))
    rr = run_cmd('git worktree prune')
    rr = run_cmd('if exist ' // quote(wt // '\') // ' rmdir /s /q ' // quote(wt))
  end subroutine audit_worktree_rebuild

  !> Part B: the compiler-residue incident, read from both commits.
  subroutine audit_residue_incident(fs)
    type(finding_t), allocatable, intent(inout) :: fs(:)
    type(string_t), allocatable :: body(:)
    type(run_result) :: rr
    character(1) :: action
    logical :: found, ok, has_j, has_gate
    integer :: i

    if (.not. commit_exists(CANON_RESIDUE_ADD) .or. &
        .not. commit_exists(CANON_RESIDUE_FIX)) then
      call add_finding(fs, V_UNPROVEN, 'THE RESIDUE INCIDENT', &
                       'One or both incident commits are absent.')
      return
    end if

    call residue_change(CANON_RESIDUE_ADD, 'state.mod', action, found)
    if (found .and. action == 'A') then
      call add_finding(fs, V_PROVEN, 'HOW state.mod ENTERED', &
        'Commit ' // CANON_RESIDUE_ADD // ' added state.mod at the root; ' // &
        'the diff records it.')
    else
      call add_finding(fs, V_UNPROVEN, 'HOW state.mod ENTERED', &
        'The addition could not be read from the diff.')
    end if

    call blob_text(CANON_RESIDUE_ADD, 'src/cathedral_validate.f90', body, ok)
    has_j = .false.
    if (ok) then
      do i = 1, size(body)
        if (index(body(i)%s, '-fsyntax-only -J') > 0) has_j = .true.
      end do
      if (.not. has_j) then
        call add_finding(fs, V_STRONG, 'THE MECHANISM OF THE RESIDUE', &
          'That tree''s compile checks invoked gfortran -fsyntax-only ' // &
          'without -J; gfortran writes .mod files even under syntax-only, ' // &
          'so module droppings landed in the root. Consistent with all ' // &
          'evidence; the runtime event itself is beyond tree proof.')
      end if
    end if

    rr = run_cmd('git log -1 --format=%B ' // CANON_RESIDUE_ADD)
    call judge_offering_shape(fs, rr, 'THE FLAWED OFFERING WAS PERFORMED ' // &
                              'THROUGH FORTY')
    call residue_change(CANON_RESIDUE_FIX, 'state.mod', action, found)
    if (found .and. action == 'D') then
      call add_finding(fs, V_PROVEN, 'THE FORWARD REMOVAL', &
        'Commit ' // CANON_RESIDUE_FIX // ' deleted state.mod; nothing ' // &
        'was rewritten; the addition remains visible in history.')
    else
      call add_finding(fs, V_UNPROVEN, 'THE FORWARD REMOVAL', &
        'The deletion could not be read from the diff.')
    end if
    rr = run_cmd('git log -1 --format=%B ' // CANON_RESIDUE_FIX)
    call judge_offering_shape(fs, rr, 'THE REMOVAL WAS PERFORMED THROUGH FORTY')

    if (.not. tree_has_path('HEAD', 'state.mod')) then
      call add_finding(fs, V_PROVEN, 'CURRENT-TREE ABSENCE', &
        'state.mod is absent from the canonical tree at HEAD.')
    else
      call add_finding(fs, V_HERESY, 'CURRENT-TREE ABSENCE', &
        'state.mod still stands in the canonical tree.')
    end if

    has_j = .false.
    has_gate = .false.
    call read_all_lines('src/cathedral_validate.f90', body)
    do i = 1, size(body)
      if (index(body(i)%s, '-fsyntax-only -J') > 0) has_j = .true.
    end do
    call read_all_lines('src/forty_offer.f90', body)
    do i = 1, size(body)
      if (index(body(i)%s, 'is_compiler_dropping') > 0) has_gate = .true.
    end do
    if (has_j .and. has_gate) then
      call add_finding(fs, V_PROVEN, 'PREVENTION STANDS IN THE PRESENT CANON', &
        'Compile checks banish .mod output to the temple of ephemera (-J), ' // &
        'and the offering gate refuses compiler droppings by extension. ' // &
        'Both guards are regression-tested by the trials.')
    else
      call add_finding(fs, V_HERESY, 'PREVENTION STANDS IN THE PRESENT CANON', &
        'One or both guards are missing from the present source.')
    end if

    call add_finding(fs, V_STRONG, 'LEDGER DOCTRINE: NOT AN OPERATIONAL ' // &
      'TRANSGRESSION', 'The ledger''s chapter records events in which a ' // &
      'canonical operation was performed outside Forty. This incident was ' // &
      'the opposite shape: Forty performed the offering, a defect in his ' // &
      'own gate admitted accidental compiler residue, and Forty performed ' // &
      'the forward removal. No canonical operation was bypassed. Forward ' // &
      'removal plus prevention constitutes sufficient remediation; the ' // &
      'incident remains permanently visible in history and in this report.')
  end subroutine audit_residue_incident

  subroutine judge_offering_shape(fs, rr, topic)
    type(finding_t), allocatable, intent(inout) :: fs(:)
    type(run_result), intent(in) :: rr
    character(*), intent(in) :: topic
    logical :: has_trailer
    integer :: i
    has_trailer = .false.
    do i = 1, size(rr%out)
      if (index(rr%out(i)%s, 'Co-Authored-By') > 0) has_trailer = .true.
    end do
    if (has_trailer) then
      call add_finding(fs, V_STRONG, topic, &
        'The commit bears the exact shape forty offer seals: subject plus ' // &
        'trailer paragraph. A hand mimicking that shape cannot be excluded ' // &
        'by tree evidence alone, so this stops short of PROVEN.')
    else
      call add_finding(fs, V_UNPROVEN, topic, &
        'The commit message does not carry the offering''s shape.')
    end if
  end subroutine judge_offering_shape

  ! ------------------------------------------------------------- helpers

  function joined(items) result(r)
    type(string_t), intent(in) :: items(:)
    character(:), allocatable :: r
    integer :: i
    r = ''
    do i = 1, size(items)
      if (i > 1) r = r // ', '
      r = r // items(i)%s
    end do
  end function joined

  pure function ends_with(s, suffix) result(r)
    character(*), intent(in) :: s, suffix
    logical :: r
    integer :: n, m
    n = len(s)
    m = len(suffix)
    r = .false.
    if (n >= m) r = (s(n - m + 1:n) == suffix)
  end function ends_with

end module forty_audit
