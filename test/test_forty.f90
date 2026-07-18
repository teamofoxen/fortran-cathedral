!> The trials of Forty. Pure logic is examined directly; external
!> discipline is examined with harmless local commands. No trial
!> touches a network or mutates a remote. Fixtures live in %TEMP%.
module test_kit
  use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
  implicit none
  integer :: n_pass = 0
  integer :: n_fail = 0

contains

  subroutine check(cond, label)
    logical, intent(in) :: cond
    character(*), intent(in) :: label
    if (cond) then
      n_pass = n_pass + 1
    else
      n_fail = n_fail + 1
      write (error_unit, '(a)') 'TRIAL FAILED: ' // label
    end if
  end subroutine check

  subroutine check_str(actual, expected, label)
    character(*), intent(in) :: actual, expected, label
    if (actual == expected) then
      n_pass = n_pass + 1
    else
      n_fail = n_fail + 1
      write (error_unit, '(a)') 'TRIAL FAILED: ' // label // &
        '  [GOT: "' // actual // '" WANTED: "' // expected // '"]'
    end if
  end subroutine check_str

  subroutine summary()
    write (output_unit, '(a,i0,a,i0,a)') 'TRIALS: ', n_pass, ' PASSED, ', &
      n_fail, ' FAILED.'
    if (n_fail == 0) then
      write (output_unit, '(a)') 'THE TRIALS ARE PASSED.'
    end if
  end subroutine summary

end module test_kit

program test_forty
  use test_kit
  use forty_util
  use forty_cli
  use forty_paths, only: quote, temp_root, set_cwd
  use forty_run, only: run_result, run_cmd, read_all_lines, write_lines, &
                       delete_file, version_line
  use forty_confess, only: classify, ledger_entries, transgression_t, &
                           ledger_transgressions, split_cells, &
                           expiation_t, ledger_expiation, list_repo_files, &
                           CLASS_FORTRAN, CLASS_DECLARATIVE, CLASS_HERESY, CLASS_OTHER
  use cathedral_highlight, only: highlight_line
  use cathedral_testaments, only: verse_t, verses
  use, intrinsic :: iso_fortran_env, only: real64
  use cathedral_why, only: cite_t, cites
  use cathedral_blas, only: axpy_ref, gemv_ref, gemm_ref, fmt_num, &
                            exhibit_axpy, exhibit_gemv, exhibit_gemm, blas_cites
  use cathedral_validate, only: extract_hrefs
  use forty_deploy, only: deploy_preflight, build_deploy_tree, deploy_parent, &
                          make_deploy_commit, push_deploy, deploy_needed, &
                          tree_manifest, gate_production, parse_pages_response
  use cathedral_hall, only: hall_exhibit_t, hall_exhibits, probe_verdict_t, &
                            run_probe, normalize_diag, category_text, &
                            CAT_CLEAN, CAT_WARN, CAT_EXTENSION, CAT_REJECTED
  use forty_inspect, only: measure_t, harness_lines, browser_command, &
                           path_to_url, parse_measure, judge_page, &
                           render_inspection
  use forty_audit, only: finding_t, add_finding, tree_paths, scan_tracked_html, &
                         scan_template_suspects, scan_tree_heresy, commit_exists, &
                         signature_module, residue_change, tree_has_path, &
                         historical_execution_finding, render_report, &
                         V_PROVEN, V_HERESY
  use forty_offer, only: build_offer_plan, check_offer_ground, &
                         categorize_porcelain, offering_acceptable, &
                         porcelain_path, run_offer
  use forty_git, only: slug_from_url, valid_slug, is_hash
  use forty_github, only: plan_step_t, build_connect_plan, parse_login
  use forty_canon, only: CANON_DESCRIPTION, CANON_BASE_URL
  use forty_ui, only: set_muted, set_scripted_confirm, confirm_consult_count, &
                      SCRIPT_NONE, SCRIPT_NO
  use forty_buildops, only: dir_present
  use forty_atone, only: perform_restitution, expiate_ledger_lines, STATUS_EXPIATED
  use cathedral_html, only: escape_html, escape_json
  use cathedral_routes, only: route_t, routes, append_nav
  use cathedral_generate, only: run_generate
  use cathedral_validate, only: run_validate
  implicit none

  character(:), allocatable :: saved_root
  integer, parameter :: ROW_OFFENDER = 1, ROW_NONE = 2, ROW_GHOST = 3, ROW_ROOT = 4
  character(40), parameter :: GHOST_HASH = repeat('f', 40)

  call capture_root()
  call trial_parse()
  call trial_validators()
  call trial_strings()
  call trial_classify()
  call trial_ledger()
  call trial_fixture_counting()
  call trial_slugs()
  call trial_plan()
  call trial_login()
  call trial_run_discipline()
  call trial_escaping()
  call trial_dir_present()
  call trial_count_substr()
  call trial_routes_and_nav()
  call trial_site_generation()
  call trial_determinism()
  call trial_validator_teeth()
  call trial_highlighter()
  call trial_testaments()
  call trial_html_purity()
  call trial_why_wing()
  call trial_blas_arithmetic()
  call trial_blas_page()
  call trial_rebuild_from_nothing()
  call trial_transgressions()
  call trial_commit_messages()
  call trial_offer_plan()
  call trial_porcelain()
  call trial_residue()
  call trial_offer_ground_and_accord()
  call trial_offer_discipline()
  call trial_expiate_transform()
  call trial_restitution_happy()
  call trial_restitution_dry_and_decline()
  call trial_restitution_refusals()
  call trial_restitution_severed()
  call trial_audit_capabilities()
  call trial_deploy_engine()
  call trial_deploy_planning()
  call trial_hall_probes()
  call trial_hall_page()
  call trial_tape_measure()

  call summary()
  if (n_fail > 0) call exit(1)

contains

  function one(a) result(v)
    character(*), intent(in) :: a
    type(string_t), allocatable :: v(:)
    allocate (v(1))
    v(1)%s = a
  end function one

  subroutine trial_parse()
    type(string_t), allocatable :: argv(:)
    type(cli_t) :: c

    allocate (argv(0))
    c = parse_cli(argv)
    call check(c%command == CMD_HELP, 'NO ARGUMENTS SUMMONS THE PROCLAMATION')

    c = parse_cli(one('doctor'))
    call check(c%command == CMD_DOCTOR .and. len(c%errmsg) == 0, 'DOCTOR PARSES')

    c = parse_cli(one('CONFESS'))
    call check(c%command == CMD_CONFESS, 'COMMANDS ARE HEARD IN ANY CASE')

    c = parse_cli(one('transgress'))
    call check(c%command == CMD_UNKNOWN .and. len(c%errmsg) > 0, &
               'UNKNOWN COMMANDS ARE REBUKED')

    c = parse_cli(one('github'))
    call check(len(c%errmsg) > 0, 'GITHUB ALONE IS INSUFFICIENT')

    deallocate (argv); allocate (argv(2))
    argv(1)%s = 'github'; argv(2)%s = 'banish'
    c = parse_cli(argv)
    call check(len(c%errmsg) > 0, 'UNKNOWN SUBCOMMANDS ARE REBUKED')

    deallocate (argv); allocate (argv(6))
    argv(1)%s = 'github'; argv(2)%s = 'connect'
    argv(3)%s = '--dry-run'; argv(4)%s = '--name=chapel'
    argv(5)%s = '--owner=teamofoxen'; argv(6)%s = '--visibility=PRIVATE'
    c = parse_cli(argv)
    call check(c%command == CMD_GITHUB .and. c%subcommand == SUB_CONNECT, &
               'CONNECT PARSES')
    call check(c%dry_run, 'DRY RUN IS NOTED')
    call check_str(c%repo_name, 'chapel', 'NAME OPTION')
    call check_str(c%owner, 'teamofoxen', 'OWNER OPTION')
    call check_str(c%visibility, 'private', 'VISIBILITY IS LOWERED')

    deallocate (argv); allocate (argv(2))
    argv(1)%s = 'build'; argv(2)%s = '-y'
    c = parse_cli(argv)
    call check(c%assume_yes, 'CONSENT BY DECREE IS NOTED')

    deallocate (argv); allocate (argv(2))
    argv(1)%s = 'status'; argv(2)%s = '--bogus'
    c = parse_cli(argv)
    call check(len(c%errmsg) > 0, 'STRANGE ARGUMENTS ARE REBUKED')
  end subroutine trial_parse

  subroutine trial_validators()
    call check(valid_repo_name('fortran-cathedral'), 'CANON NAME IS FIT')
    call check(valid_repo_name('Name.ok_1'), 'MODEST PUNCTUATION IS FIT')
    call check(.not. valid_repo_name(''), 'EMPTINESS IS NOT A NAME')
    call check(.not. valid_repo_name('a b'), 'SPACES ARE NOT FIT')
    call check(.not. valid_repo_name('..'), 'DOTS OF ASCENT ARE REFUSED')
    call check(.not. valid_repo_name('a/b'), 'SLASHES ARE NOT FIT')

    call check(valid_owner_name('teamofoxen'), 'THE OWNER IS FIT')
    call check(valid_owner_name('A1-b'), 'HYPHENATED OWNERS ARE FIT')
    call check(.not. valid_owner_name('-x'), 'LEADING HYPHENS ARE REFUSED')
    call check(.not. valid_owner_name('x-'), 'TRAILING HYPHENS ARE REFUSED')
    call check(.not. valid_owner_name('a_b'), 'UNDERSCORED OWNERS ARE REFUSED')

    call check(valid_description(CANON_DESCRIPTION), 'THE CANON DESCRIPTION IS FIT')
    call check(valid_description(''), 'SILENCE IS PERMITTED')
    call check(.not. valid_description('has "quotes"'), 'QUOTES ARE FORBIDDEN RUNES')
    call check(.not. valid_description('100% pure'), 'PERCENT IS A FORBIDDEN RUNE')
    call check(.not. valid_description('a&b'), 'AMPERSAND IS A FORBIDDEN RUNE')
    call check(.not. valid_description('x<y'), 'ANGLES ARE FORBIDDEN RUNES')
    call check(.not. valid_description(repeat('a', 201)), 'EXCESS IS REFUSED')

    call check(valid_visibility('public') .and. valid_visibility('private'), &
               'BOTH VISIBILITIES ARE KNOWN')
    call check(.not. valid_visibility('internal'), 'OTHER VISIBILITIES ARE NOT')
  end subroutine trial_validators

  subroutine trial_strings()
    call check_str(to_lower('MiXeD'), 'mixed', 'LOWERING')
    call check_str(to_upper('mixed'), 'MIXED', 'RAISING')
    call check_str(basename('a\b\c.f90'), 'c.f90', 'BASENAME OF THE FAITHFUL SLASH')
    call check_str(basename('a/b/x'), 'x', 'BASENAME OF THE OTHER FAITH')
    call check_str(basename('c.f90'), 'c.f90', 'BASENAME WITHOUT PILGRIMAGE')
    call check_str(extension_of('c.F90'), 'f90', 'EXTENSIONS ARE LOWERED')
    call check_str(extension_of('.gitignore'), '', 'DOTFILES WEAR HOODS')
    call check_str(extension_of('noext'), '', 'THE EXTENSIONLESS')
    call check_str(extension_of('a.tar.gz'), 'gz', 'THE LAST DOT PREVAILS')
    call check_str(trim_cr('abc' // achar(13)), 'abc', 'CARRIAGES ARE RETURNED')
    call check(starts_with('build\x.o', 'build\'), 'PREFIXES ARE SEEN')
    call check(.not. starts_with('x', 'build\'), 'SHORT STRINGS DO NOT LIE')
    call check_str(int_to_str(42), '42', 'NUMBERS BECOME WORDS')
    call check_str(quote('C:\A B'), '"C:\A B"', 'PATHS ARE QUOTED FOR CMD')
  end subroutine trial_strings

  subroutine trial_classify()
    call check(classify('src\forty_ui.f90') == CLASS_FORTRAN, 'F90 IS SCRIPTURE')
    call check(classify('CLAUDE.md') == CLASS_DECLARATIVE, 'MARKDOWN IS DECLARATIVE')
    call check(classify('heresy\x.js') == CLASS_HERESY, 'JAVASCRIPT IS HERESY')
    call check(classify('X.PY') == CLASS_HERESY, 'HERESY HIDES IN NO CASE')
    call check(classify('a.png') == CLASS_DECLARATIVE, 'IMAGES ARE DECLARATIVE')
    call check(classify('.gitignore') == CLASS_DECLARATIVE, 'DOT-CONFIGS ARE DECLARATIVE')
    call check(classify('a.zzz') == CLASS_OTHER, 'THE UNKNOWN AWAITS REVIEW')
    call check(classify('scripts\deploy.ps1') == CLASS_HERESY, 'POWERSHELL IS HERESY')
  end subroutine trial_classify

  subroutine trial_ledger()
    type(string_t), allocatable :: lines(:), entries(:)

    allocate (lines(4))
    lines(1)%s = '## Current ledger'
    lines(2)%s = '| File or component | Language | Executable lines | Purpose | Why | Removal |'
    lines(3)%s = '|---|---:|---:|---|---|---|'
    lines(4)%s = '| None | - | 0 | - | - | - |'
    call ledger_entries(lines, entries)
    call check(size(entries) == 0, 'A PURE LEDGER YIELDS NO ENTRIES')

    deallocate (lines); allocate (lines(8))
    lines(1)%s = '## Current ledger'
    lines(2)%s = '| File or component | Language | Executable lines | Purpose | Why | Removal |'
    lines(3)%s = '|---|---:|---:|---|---|---|'
    lines(4)%s = '| `heresy/x.js` | JavaScript | 12 | pump | boundary | purge |'
    lines(5)%s = 'Prose outside the table is ignored.'
    lines(6)%s = '## Operational transgressions'
    lines(7)%s = '|---|---|---|---:|---|---|---|'
    lines(8)%s = '| 2026-07-18 | An event | `abc1234` | 0 | why | how | Historical. |'
    call ledger_entries(lines, entries)
    call check(size(entries) == 1, 'ONE SIN, ONE ENTRY')
    if (size(entries) == 1) then
      call check_str(entries(1)%s, 'heresy/x.js', 'THE SIN IS NAMED WITHOUT BACKTICKS')
    end if
  end subroutine trial_ledger

  subroutine trial_transgressions()
    type(string_t), allocatable :: lines(:), cells(:), ledger(:)
    type(transgression_t), allocatable :: trans(:)
    logical :: wellformed
    integer :: i
    logical :: found

    call split_cells('| a | `b` |  c  |', cells)
    call check(size(cells) == 3, 'THREE CELLS ARE CUT FROM THE ROW')
    if (size(cells) == 3) then
      call check_str(cells(2)%s, 'b', 'BACKTICKS ARE SHED')
      call check_str(cells(3)%s, 'c', 'CELLS ARE TRIMMED')
    end if

    allocate (lines(4))
    lines(1)%s = '## Operational transgressions'
    lines(2)%s = '| Date | Event | Commit | Executable non-Fortran lines introduced | Why | Remediation | Status |'
    lines(3)%s = '|---|---|---|---:|---|---|---|'
    lines(4)%s = '| 2026-07-18 | Manual push | `abc1234def` | 0 | haste | forty offer | Historical. |'
    call ledger_transgressions(lines, trans, wellformed)
    call check(wellformed, 'A FULL ROW IS WELL-FORMED')
    call check(size(trans) == 1, 'ONE TRANSGRESSION IS READ')
    if (size(trans) == 1) then
      call check_str(trans(1)%commit, 'abc1234def', 'THE COMMIT IS NAMED')
      call check_str(trans(1)%exec_lines, '0', 'THE LINE COUNT IS NUMERIC')
      call check_str(trans(1)%status, 'Historical.', 'THE STATUS ENDURES')
    end if

    lines(4)%s = '| 2026-07-18 | Too few cells | `abc` | 0 |'
    call ledger_transgressions(lines, trans, wellformed)
    call check(.not. wellformed, 'A TRUNCATED ROW IS CONDEMNED')

    deallocate (lines); allocate (lines(1))
    lines(1)%s = 'No chapter here.'
    call ledger_transgressions(lines, trans, wellformed)
    call check(.not. wellformed, 'A MISSING CHAPTER IS CONDEMNED')

    call read_all_lines('HERESY_LEDGER.md', ledger)
    call ledger_transgressions(ledger, trans, wellformed)
    call check(wellformed, 'THE TRUE LEDGER''S CHAPTER IS WELL-FORMED')
    call check(size(trans) >= 1, 'THE TRUE LEDGER REMEMBERS AT LEAST ONE')
    found = .false.
    do i = 1, size(trans)
      if (index(trans(i)%commit, 'd2c9f0be') > 0) found = .true.
    end do
    call check(found, 'THE PHASE 1 TRANSGRESSION IS PERMANENTLY NAMED')
  end subroutine trial_transgressions

  subroutine trial_commit_messages()
    call check(valid_commit_message('PHASE 1.1: FORTY RECEIVES THE OFFERING.'), &
               'THE CANONICAL MESSAGE IS FIT')
    call check(.not. valid_commit_message(''), 'AN EMPTY MESSAGE IS NOT')
    call check(.not. valid_commit_message('   '), 'BLANKS ALONE ARE NOT')
    call check(.not. valid_commit_message('with "quotes"'), &
               'QUOTED MESSAGES ARE REFUSED')
    call check(.not. valid_commit_message('100% done'), &
               'PERCENT IS REFUSED IN MESSAGES')
    call check(.not. valid_commit_message(repeat('m', 201)), &
               'EXCESSIVE MESSAGES ARE REFUSED')
  end subroutine trial_commit_messages

  subroutine trial_offer_plan()
    type(plan_step_t), allocatable :: steps(:)
    steps = build_offer_plan('SEAL THE WORK.')
    call check(size(steps) == 3, 'THE OFFERING HAS THREE COMMANDS')
    if (size(steps) == 3) then
      call check_str(steps(1)%command, 'git add -A', 'FIRST IT GATHERS')
      call check(index(steps(2)%command, 'git commit -m "SEAL THE WORK."') == 1, &
                 'THEN IT SEALS WITH THE GIVEN WORDS')
      call check(index(steps(2)%command, 'Co-Authored-By') > 0, &
                 'THE TRAILER RIDES WITH THE SEAL')
      call check_str(steps(3)%command, 'git push', 'THEN IT LIFTS')
    end if
  end subroutine trial_offer_plan

  subroutine trial_porcelain()
    type(string_t), allocatable :: lines(:), paths(:)
    integer :: n_mod, n_new, n_del, n_ren
    call check_str(porcelain_path(' M src/a.f90'), 'src/a.f90', &
                   'THE MODIFIED PATH IS READ')
    call check_str(porcelain_path('?? new.f90'), 'new.f90', &
                   'THE NEW PATH IS READ')
    call check_str(porcelain_path('R  old.md -> new.md'), 'new.md', &
                   'THE RENAME YIELDS ITS TARGET')
    call check_str(porcelain_path(' M "a b.txt"'), 'a b.txt', &
                   'QUOTED PATHS ARE UNWRAPPED')
    allocate (lines(4))
    lines(1)%s = ' M src/a.f90'
    lines(2)%s = '?? src/new.f90'
    lines(3)%s = 'D  gone.md'
    lines(4)%s = 'R  old.md -> new.md'
    call categorize_porcelain(lines, n_mod, n_new, n_del, n_ren, paths)
    call check(n_mod == 1 .and. n_new == 1 .and. n_del == 1 .and. n_ren == 1, &
               'THE TABLE IS COUNTED TRULY')
    call check(size(paths) == 4, 'EVERY PATH IS GATHERED')
  end subroutine trial_porcelain

  subroutine trial_residue()
    type(string_t), allocatable :: lines(:)
    logical :: ok
    character(:), allocatable :: offending
    allocate (lines(2))
    lines(1)%s = ' M src/forty_offer.f90'
    lines(2)%s = ' M HERESY_LEDGER.md'
    call offering_acceptable(lines, ok, offending)
    call check(ok, 'HONEST WORKS ARE ACCEPTED')
    deallocate (lines); allocate (lines(2))
    lines(1)%s = ' M src/ok.f90'
    lines(2)%s = '?? build/sneaky.o'
    call offering_acceptable(lines, ok, offending)
    call check(.not. ok, 'YARD RESIDUE IS REFUSED')
    call check_str(offending, 'build/sneaky.o', 'THE OFFENDER IS NAMED')
    deallocate (lines); allocate (lines(1))
    lines(1)%s = '?? dist/index.html'
    call offering_acceptable(lines, ok, offending)
    call check(.not. ok, 'PORCH RESIDUE IS REFUSED')
    deallocate (lines); allocate (lines(1))
    lines(1)%s = '?? state.mod'
    call offering_acceptable(lines, ok, offending)
    call check(.not. ok, 'COMPILER DROPPINGS ARE REFUSED WHEREVER THEY FALL')
    deallocate (lines); allocate (lines(1))
    lines(1)%s = 'A  src/thing.o'
    call offering_acceptable(lines, ok, offending)
    call check(.not. ok, 'OBJECT FILES ARE REFUSED IN ANY CLOISTER')
    deallocate (lines); allocate (lines(1))
    lines(1)%s = 'D  state.mod'
    call offering_acceptable(lines, ok, offending)
    call check(ok, 'THE DEPARTURE OF RESIDUE IS CLEANSING, NOT DEFILEMENT')
  end subroutine trial_residue

  subroutine trial_offer_ground_and_accord()
    logical :: ready
    character(:), allocatable :: why
    type(run_result) :: ra, rb
    call check_offer_ground(ready, why)
    call check(ready, 'THE CONSECRATED GROUND IS FIT FOR OFFERINGS')
    call check(len(why) == 0, 'NO COMPLAINT IS RAISED AGAINST IT')
    ra = run_cmd('git rev-parse HEAD')
    rb = run_cmd('git rev-parse origin/main')
    call check(ra%exit_code == 0 .and. rb%exit_code == 0 .and. &
               size(ra%out) > 0 .and. size(rb%out) > 0, &
               'BOTH HEADS CAN BE NAMED')
    if (size(ra%out) > 0 .and. size(rb%out) > 0) then
      call check(ra%out(1)%s == rb%out(1)%s, &
                 'THE ACCORD HOLDS IN THE TRUE REPOSITORY')
    end if
  end subroutine trial_offer_ground_and_accord

  subroutine trial_offer_discipline()
    type(cli_t) :: cli
    type(run_result) :: before, after
    integer :: code
    ! An unfit message is refused before any inspection of the tree.
    cli%message = 'bad "message"'
    cli%dry_run = .true.
    call set_muted(.true.)
    call run_offer(cli, code)
    call set_muted(.false.)
    call check(code == 2, 'AN UNFIT MESSAGE IS A USAGE FAULT')
    ! A dry run plans everything and performs nothing.
    before = run_cmd('git rev-parse HEAD')
    cli%message = 'A FIT MESSAGE FOR A DRY RUN.'
    cli%dry_run = .true.
    call set_muted(.true.)
    call run_offer(cli, code)
    call set_muted(.false.)
    call check(code == 0, 'THE DRY RUN CONCLUDES IN PEACE')
    after = run_cmd('git rev-parse HEAD')
    call check(size(before%out) > 0 .and. size(after%out) > 0, &
               'HEAD CAN BE NAMED TWICE')
    if (size(before%out) > 0 .and. size(after%out) > 0) then
      call check(before%out(1)%s == after%out(1)%s, &
                 'THE DRY RUN SEALED NOTHING')
    end if
  end subroutine trial_offer_discipline

  subroutine trial_fixture_counting()
    type(string_t), allocatable :: lines(:), got(:)
    character(:), allocatable :: fixture
    logical :: ok

    fixture = temp_root() // '\forty_trial_fixture.txt'
    allocate (lines(5))
    lines(1)%s = 'first'
    lines(2)%s = ''
    lines(3)%s = 'second'
    lines(4)%s = '   '
    lines(5)%s = 'third'
    call write_lines(fixture, lines, ok)
    call check(ok, 'FIXTURES MAY BE WRITTEN TO THE APPOINTED PLACE')
    call read_all_lines(fixture, got)
    call check(size(got) == 5, 'ALL FIVE LINES RETURN FROM THE APPOINTED PLACE')
    call check(count_nonblank(got) == 3, 'THREE LINES ARE EXECUTABLE; BLANKS ARE INNOCENT')
    call delete_file(fixture)
  end subroutine trial_fixture_counting

  subroutine trial_slugs()
    call check_str(slug_from_url('https://github.com/teamofoxen/fortran-cathedral.git'), &
                   'teamofoxen/fortran-cathedral', 'HTTPS URLS YIELD SLUGS')
    call check_str(slug_from_url('git@github.com:teamofoxen/x'), &
                   'teamofoxen/x', 'SSH URLS YIELD SLUGS')
    call check_str(slug_from_url('https://gitlab.com/x/y'), '', &
                   'FOREIGN DWELLINGS YIELD NOTHING')
    call check(valid_slug('a/b'), 'A MODEST SLUG IS FIT')
    call check(.not. valid_slug('ab'), 'A SLUG WITHOUT A SLASH IS NOT')
    call check(.not. valid_slug('a/b/c'), 'A SLUG WITH TWO SLASHES IS NOT')
  end subroutine trial_slugs

  subroutine trial_plan()
    type(plan_step_t), allocatable :: steps(:)
    character(:), allocatable :: expected

    steps = build_connect_plan('fortran-cathedral', 'teamofoxen', &
                               CANON_DESCRIPTION, 'public', .false., .false.)
    call check(size(steps) == 7, 'THE FULL RITE HAS SEVEN STEPS')
    if (size(steps) == 7) then
      call check_str(steps(1)%command, 'git init -b main', 'STEP 1 CONSECRATES THE GROUND')
      call check(steps(2)%internal, 'STEP 2 IS BY FORTY''S OWN HAND')
      call check_str(steps(3)%command, 'git add -A', 'STEP 3 GATHERS THE WORKS')
      call check(index(steps(4)%command, 'PHASE 0: FORTY IS ORDAINED.') > 0, &
                 'STEP 4 SEALS THE COMMIT')
      expected = 'gh repo create teamofoxen/fortran-cathedral --public' // &
                 ' --description "' // CANON_DESCRIPTION // &
                 '" --source . --remote origin --push'
      call check_str(steps(5)%command, expected, 'STEP 5 IS EXACTLY AS PROCLAIMED')
      call check_str(steps(6)%command, 'git remote get-url origin', &
                     'STEP 6 VERIFIES THE REMOTE')
      call check(index(steps(7)%command, &
                 'gh repo view teamofoxen/fortran-cathedral') > 0, &
                 'STEP 7 VERIFIES THE CANON')
    end if

    steps = build_connect_plan('fortran-cathedral', 'teamofoxen', &
                               CANON_DESCRIPTION, 'public', .true., .true.)
    call check(size(steps) == 5, 'A PREPARED GROUND SHORTENS THE RITE')
    if (size(steps) == 5) then
      call check_str(steps(1)%command, 'git add -A', &
                     'THE SHORT RITE BEGINS BY GATHERING')
    end if
  end subroutine trial_plan

  subroutine trial_login()
    type(string_t), allocatable :: lines(:)
    allocate (lines(2))
    lines(1)%s = 'github.com'
    lines(2)%s = '  Logged in to github.com account teamofoxen (keyring)'
    call check_str(parse_login(lines), 'teamofoxen', 'THE SOUL IS RECOGNIZED')
    lines(2)%s = 'no accounts here'
    call check_str(parse_login(lines), '', 'ABSENCE IS REPORTED AS SILENCE')
  end subroutine trial_login

  subroutine trial_run_discipline()
    type(run_result) :: rr
    rr = run_cmd('git --version')
    call check(rr%launched .and. rr%exit_code == 0, 'GIT ANSWERS WHEN CALLED')
    call check(size(rr%out) > 0, 'ITS ANSWER IS CAPTURED')
    if (size(rr%out) > 0) then
      call check(index(rr%out(1)%s, 'git version') > 0, 'THE ANSWER IS SENSIBLE')
    end if
    rr = run_cmd('git --no-such-flag-xyz')
    call check(rr%launched .and. rr%exit_code /= 0, &
               'FAILURE IS REPORTED, NOT CONCEALED')
  end subroutine trial_run_discipline

  subroutine trial_escaping()
    call check_str(escape_html('a<b & "c" ''d''>e'), &
                   'a&lt;b &amp; &quot;c&quot; &#39;d&#39;&gt;e', &
                   'THE FIVE PERILOUS CHARACTERS ARE ENTOMBED')
    call check_str(escape_html('plain text'), 'plain text', &
                   'INNOCENT TEXT PASSES UNTOUCHED')
    call check_str(escape_json('say "x" to \y'), 'say \"x\" to \\y', &
                   'JSON QUOTES AND SLASHES ARE BOUND')
  end subroutine trial_escaping

  subroutine trial_dir_present()
    call check(dir_present('src'), 'A STANDING DIRECTORY IS SEEN')
    call check(.not. dir_present('no_such_crypt_xyz'), &
               'AN ABSENT DIRECTORY IS NOT IMAGINED')
  end subroutine trial_dir_present

  subroutine trial_count_substr()
    call check(count_substr('abcabcab', 'abc') == 2, 'COUNTING IS EXACT')
    call check(count_substr('aaaa', 'aa') == 2, 'COUNTING DOES NOT OVERLAP')
    call check(count_substr('abc', 'xyz') == 0, 'ABSENCE COUNTS AS ZERO')
    call check(count_substr('abc', '') == 0, 'EMPTINESS IS NOT COUNTED')
  end subroutine trial_count_substr

  subroutine trial_routes_and_nav()
    type(route_t), allocatable :: rs(:)
    type(string_t), allocatable :: nav(:)
    character(:), allocatable :: doc
    integer :: i
    rs = routes()
    call check(size(rs) == 6, 'SIX ROUTES STAND IN THE REGISTRY')
    if (size(rs) == 6) then
      call check_str(rs(1)%file, 'index.html', 'THE NAVE IS THE INDEX')
      call check_str(rs(2)%file, 'why-it-still-stands.html', 'WHY HAS ITS DOOR')
      call check_str(rs(3)%file, 'testaments.html', 'THE TESTAMENTS HAVE THEIR DOOR')
      call check_str(rs(4)%file, 'book-of-blas.html', 'THE BOOK OF BLAS HAS ITS DOOR')
      call check_str(rs(5)%file, 'hall-of-deprecated-syntax.html', 'THE HALL HAS ITS DOOR')
      call check_str(rs(6)%file, 'confessional.html', 'THE CONFESSIONAL HAS ITS DOOR')
    end if
    allocate (nav(0))
    call append_nav(nav, 'nave')
    doc = ''
    do i = 1, size(nav)
      doc = doc // nav(i)%s // achar(10)
    end do
    call check(count_substr(doc, 'aria-current="page"') == 1, &
               'ONE PLACE IS HELD IN THE NAV')
    call check(count_substr(doc, 'href="index.html" aria-current') == 1, &
               'THE HELD PLACE IS THE ACTIVE PAGE')
    call check(count_substr(doc, 'href="confessional.html"') == 1, &
               'THE OTHER DOOR IS OFFERED')
  end subroutine trial_routes_and_nav

  subroutine trial_site_generation()
    integer :: code
    type(string_t), allocatable :: lines(:)
    character(:), allocatable :: doc
    integer :: i
    call set_muted(.true.)
    call run_generate(code)
    call set_muted(.false.)
    call check(code == 0, 'THE CATHEDRAL RISES ON COMMAND')
    call check(exists('dist\index.html') .and. exists('dist\confessional.html'), &
               'BOTH PAGES STAND')
    call check(exists('dist\assets\tokens.css') .and. &
               exists('dist\assets\cathedral.css') .and. &
               exists('dist\assets\ornament.svg') .and. &
               exists('dist\robots.txt') .and. exists('dist\sitemap.xml') .and. &
               exists('dist\routes.json') .and. exists('dist\.nojekyll'), &
               'ALL SEVEN WORKS ARE LAID')
    doc = slurp_file('dist\index.html')
    call check(count_substr(doc, 'n &gt; 0') >= 1, &
               'THE CODE EXHIBIT IS ESCAPED IN REAL CONTENT')
    call check(count_substr(to_lower(doc), '<script') == 0, &
               'NO SCRIPT TAINTS THE NAVE')
    doc = slurp_file('dist\sitemap.xml')
    call check(count_substr(doc, CANON_BASE_URL // '/index.html') == 1, &
               'THE MAP KNOWS THE NAVE')
    doc = slurp_file('dist\confessional.html')
    call check(count_substr(doc, 'd2c9f0be63f28b7ecf136c1b9b81a7bd993132db') >= 1, &
               'THE CONFESSIONAL DISPLAYS THE TRANSGRESSION''S COMMIT')
    call check(count_substr(doc, 'The operational record') >= 1, &
               'THE OPERATIONAL RECORD HAS ITS SECTION')
    call set_muted(.true.)
    call run_validate(code)
    call set_muted(.false.)
    call check(code == 0, 'THE SURVEYOR FINDS THE FABRIC SOUND')
  end subroutine trial_site_generation

  subroutine trial_determinism()
    type(string_t), allocatable :: first(:), second(:)
    character(40), parameter :: works(8) = [character(40) :: &
      'dist\index.html', 'dist\confessional.html', &
      'dist\testaments.html', 'dist\why-it-still-stands.html', &
      'dist\book-of-blas.html', 'dist\hall-of-deprecated-syntax.html', &
      'dist\assets\ornament.svg', 'dist\routes.json']
    integer :: code, w, i
    logical :: same
    do w = 1, size(works)
      call read_all_lines(trim(works(w)), first)
      call set_muted(.true.)
      call run_generate(code)
      call set_muted(.false.)
      call read_all_lines(trim(works(w)), second)
      same = (size(first) == size(second)) .and. (size(first) > 0)
      if (same) then
        do i = 1, size(first)
          if (first(i)%s /= second(i)%s) same = .false.
        end do
      end if
      call check(same, 'REGENERATION IS DETERMINISTIC: ' // trim(works(w)))
    end do
  end subroutine trial_determinism

  subroutine trial_validator_teeth()
    integer :: code
    call delete_file('dist\robots.txt')
    call set_muted(.true.)
    call run_validate(code)
    call set_muted(.false.)
    call check(code /= 0, 'THE SURVEYOR REFUSES A BREACHED FABRIC')
    call set_muted(.true.)
    call run_generate(code)
    call run_validate(code)
    call set_muted(.false.)
    call check(code == 0, 'REGENERATION HEALS THE BREACH')
  end subroutine trial_validator_teeth

  subroutine trial_highlighter()
    character(:), allocatable :: h
    h = highlight_line('C     A FIXED-FORM SERMON', .true.)
    call check(index(h, '<span class="hl-c">') == 1, &
               'COLUMN ONE CONSECRATES THE FIXED LINE')
    h = highlight_line('  print *, ''pilgrim''  ! greet', .false.)
    call check(index(h, '<span class="hl-k">print</span>') > 0, &
               'KEYWORDS RECEIVE THEIR VESTMENTS')
    call check(index(h, '<span class="hl-s">&#39;pilgrim&#39;</span>') > 0, &
               'STRINGS ARE ROBED AND ESCAPED')
    call check(index(h, '<span class="hl-c">! greet</span>') > 0, &
               'TRAILING COMMENTS ARE DIMMED')
    h = highlight_line('x = 42.5', .false.)
    call check(index(h, '<span class="hl-n">42.5</span>') > 0, &
               'NUMBERS ARE COUNTED')
    h = highlight_line('s = ''<script>alert(1)</script>''', .false.)
    call check(index(h, '<script') == 0, 'NO RAW SCRIPT ESCAPES THE EXHIBIT')
    call check(index(h, '&lt;script&gt;') > 0, 'THE HOSTILE TEXT IS ENTOMBED')
  end subroutine trial_highlighter

  subroutine trial_testaments()
    type(verse_t), allocatable :: vs(:)
    type(run_result) :: rr
    character(:), allocatable :: page, nave, broken
    type(string_t), allocatable :: blines(:)
    logical :: ok
    integer :: v

    vs = verses()
    call check(size(vs) == 5, 'FIVE VERSES STAND IN THE REGISTRY')
    do v = 1, size(vs)
      call check(exists(vs(v)%old_file) .and. exists(vs(v)%new_file), &
                 'BOTH SCROLLS EXIST: ' // vs(v)%id)
      rr = run_cmd('gfortran -fsyntax-only -J ' // quote(temp_root()) // &
                   ' ' // quote(vs(v)%old_file))
      call check(rr%exit_code == 0, 'THE OLD SCROLL COMPILES: ' // vs(v)%id)
      rr = run_cmd('gfortran -fsyntax-only -J ' // quote(temp_root()) // &
                   ' ' // quote(vs(v)%new_file))
      call check(rr%exit_code == 0, 'THE NEW SCROLL COMPILES: ' // vs(v)%id)
    end do
    call check(.not. exists('state.mod'), &
               'THE COMPILE CHECKS LEAVE NO DROPPINGS IN THE TREE')

    page = slurp_file('dist\testaments.html')
    do v = 1, size(vs)
      call check(count_substr(page, 'id="verse-' // vs(v)%id // '"') == 1, &
                 'THE PAGE HOLDS VERSE ' // vs(v)%id)
    end do
    call check(count_substr(page, '<meta name="generator" content="FORTY ') == 1, &
               'THE TESTAMENTS BEAR THE VERGER''S MARK')
    call check(count_substr(page, 'class="timeline"') == 1, &
               'THE LIVING STANDARD IS CHARTED')
    nave = slurp_file('dist\index.html')
    call check(count_substr(nave, 'href="testaments.html"') >= 1, &
               'THE NAVE POINTS TO THE TESTAMENTS')

    broken = temp_root() // '\forty_broken_verse.f90'
    allocate (blines(3))
    blines(1)%s = 'program broken'
    blines(2)%s = '  this is not fortran at all'
    blines(3)%s = 'end program broken'
    call write_lines(broken, blines, ok)
    rr = run_cmd('gfortran -fsyntax-only ' // quote(broken))
    call check(rr%exit_code /= 0, 'A BROKEN VERSE IS REFUSED BY THE COMPILER')
    call delete_file(broken)
  end subroutine trial_testaments

  subroutine trial_html_purity()
    type(string_t), allocatable :: files(:)
    logical :: ok, pure_tree
    integer :: i
    call list_repo_files(files, ok)
    call check(ok, 'THE GROUNDS CAN BE WALKED FOR THE DOCTRINE')
    pure_tree = .true.
    do i = 1, size(files)
      if (len(files(i)%s) >= 5) then
        if (to_lower(files(i)%s(len(files(i)%s) - 4:)) == '.html') then
          pure_tree = .false.
        end if
      end if
    end do
    call check(pure_tree, 'NO HANDWRITTEN HTML STANDS IN THE SOURCE TREE')
  end subroutine trial_html_purity

  subroutine trial_why_wing()
    type(cite_t), allocatable :: cs(:)
    type(string_t), allocatable :: urls(:), srclines(:)
    character(:), allocatable :: page, sources, u
    integer :: i, j
    logical :: ok_all

    cs = cites()
    call check(size(cs) == 14, 'FOURTEEN SOURCES STAND IN THE CITATION REGISTRY')
    page = slurp_file('dist\why-it-still-stands.html')
    call check(count_substr(page, 'id="why-arrays"') == 1 .and. &
               count_substr(page, 'id="why-compilers"') == 1 .and. &
               count_substr(page, 'id="why-validated"') == 1 .and. &
               count_substr(page, 'id="why-libraries"') == 1 .and. &
               count_substr(page, 'id="why-institutions"') == 1, &
               'ALL FIVE PILLARS STAND ON THE PAGE')
    call check(count_substr(page, 'id="why-myths"') == 1, 'THE MYTHS ARE ANSWERED')
    call check(count_substr(page, 'class="timeline"') == 1, &
               'THE UNBROKEN LINE IS CHARTED ON THE WHY WING')
    do i = 1, size(cs)
      call check(count_substr(page, 'id="src-' // int_to_str(i) // '"') == 1, &
                 'SOURCE ' // int_to_str(i) // ' IS RENDERED WITH ITS ANCHOR')
    end do
    ! Every #src reference resolves to a rendered anchor.
    call extract_hrefs(page, urls)
    ok_all = .true.
    do j = 1, size(urls)
      u = urls(j)%s
      if (len(u) < 6) cycle
      if (u(1:5) /= '#src-') cycle
      if (count_substr(page, 'id="' // u(2:) // '"') /= 1) ok_all = .false.
    end do
    call check(ok_all, 'EVERY CITATION MARK FINDS ITS SOURCE')
    ! Every external link is recorded in the declarative source record.
    call read_all_lines('content\why-it-still-stands\SOURCES.md', srclines)
    sources = ''
    do i = 1, size(srclines)
      sources = sources // srclines(i)%s // achar(10)
    end do
    ok_all = .true.
    do j = 1, size(urls)
      u = urls(j)%s
      if (index(u, '://') == 0) cycle
      if (count_substr(sources, u) < 1) ok_all = .false.
    end do
    call check(ok_all, 'EVERY EXTERNAL LINK STANDS IN SOURCES.md')
    call check(count_substr(page, 'href="testaments.html"') >= 1, &
               'THE WHY WING POINTS FORWARD TO THE TESTAMENTS')
  end subroutine trial_why_wing

  subroutine trial_blas_arithmetic()
    real(real64) :: alpha
    real(real64), allocatable :: x(:), y(:), r(:), gy(:), a(:, :), b(:, :), c(:, :)
    logical :: ok

    ! AXPY: 2*[1,2,3,4] + [10,20,30,40] = [12,24,36,48].
    call exhibit_axpy(alpha, x, y, r, ok)
    call check(ok .and. size(r) == 4, 'AXPY COMPUTES A LENGTH-4 RESULT')
    call check(nearly(r(1), 12.0_real64) .and. nearly(r(2), 24.0_real64) .and. &
               nearly(r(3), 36.0_real64) .and. nearly(r(4), 48.0_real64), &
               'AXPY: 2X + Y IS EXACTLY RIGHT')
    call axpy_ref(alpha, x, [1.0_real64, 2.0_real64], r, ok)
    call check(.not. ok, 'AXPY REFUSES MISMATCHED LENGTHS')

    ! GEMV: [[1,2,3],[4,5,6]] * [7,8,9] = [50,122]; A is 2x3, non-square.
    call exhibit_gemv(a, x, gy, ok)
    call check(ok .and. size(a, 1) == 2 .and. size(a, 2) == 3, &
               'GEMV WORKS ON A NON-SQUARE MATRIX')
    call check(nearly(gy(1), 50.0_real64) .and. nearly(gy(2), 122.0_real64), &
               'GEMV: AX IS EXACTLY RIGHT')
    call gemv_ref(a, [1.0_real64, 2.0_real64], gy, ok)
    call check(.not. ok, 'GEMV REFUSES MISMATCHED DIMENSIONS')

    ! GEMM: (2x3)(3x2) = [[22,28],[49,64]].
    call exhibit_gemm(a, b, c, ok)
    call check(ok .and. size(c, 1) == 2 .and. size(c, 2) == 2, &
               'GEMM YIELDS THE 2X2 PRODUCT OF RECTANGULAR OPERANDS')
    call check(nearly(c(1, 1), 22.0_real64) .and. nearly(c(1, 2), 28.0_real64) .and. &
               nearly(c(2, 1), 49.0_real64) .and. nearly(c(2, 2), 64.0_real64), &
               'GEMM: AB IS EXACTLY RIGHT')
    call gemm_ref(a, c, b, ok)
    call check(.not. ok, 'GEMM REFUSES DISAGREEING INNER DIMENSIONS')
    call check_str(fmt_num(49.0_real64), '49.0', 'NUMBERS ARE DRESSED CONSISTENTLY')
  end subroutine trial_blas_arithmetic

  subroutine trial_blas_page()
    character(:), allocatable :: page, sources, u
    type(string_t), allocatable :: urls(:), srclines(:)
    integer :: i, j
    logical :: ok_all

    page = slurp_file('dist\book-of-blas.html')
    call check(count_substr(page, 'id="blas-level1"') == 1 .and. &
               count_substr(page, 'id="blas-level2"') == 1 .and. &
               count_substr(page, 'id="blas-level3"') == 1, &
               'THE THREE LEVELS STAND ON THE PAGE')
    call check(count_substr(page, 'id="blas-vs-lapack"') == 1 .and. &
               count_substr(page, 'id="blas-chain"') == 1 .and. &
               count_substr(page, 'id="blas-glossary"') == 1, &
               'LAPACK, THE CHAIN, AND THE GLOSSARY STAND')
    call check(count_substr(page, '>12.0<') >= 1 .and. &
               count_substr(page, '>122.0<') >= 1 .and. &
               count_substr(page, '>64.0<') >= 1, &
               'THE COMPUTED NUMBERS STAND IN THE TABLES')
    call check(count_substr(page, 'reference arithmetic') >= 1, &
               'THE PAGE CONFESSES ITS OWN ARITHMETIC HONESTLY')
    call check(count_substr(page, 'href="testaments.html"') >= 1 .and. &
               count_substr(page, 'href="why-it-still-stands.html"') >= 1, &
               'THE BOOK POINTS TO ITS SIBLING WINGS')
    call check(count_substr(page, '<svg') == 3, &
               'THREE OPERATION MAPS ARE DRAWN INLINE')

    call read_all_lines('content\book-of-blas\SOURCES.md', srclines)
    sources = ''
    do i = 1, size(srclines)
      sources = sources // srclines(i)%s // achar(10)
    end do
    call extract_hrefs(page, urls)
    ok_all = .true.
    do j = 1, size(urls)
      u = urls(j)%s
      if (index(u, '://') == 0) cycle
      if (count_substr(sources, u) < 1) ok_all = .false.
    end do
    call check(ok_all, 'EVERY EXTERNAL LINK OF THE BOOK STANDS IN ITS RECORD')
  end subroutine trial_blas_page

  pure function nearly(a, b) result(r)
    real(real64), intent(in) :: a, b
    logical :: r
    r = abs(a - b) < 1.0e-9_real64
  end function nearly

  subroutine trial_hall_probes()
    type(hall_exhibit_t), allocatable :: xs(:)
    type(probe_verdict_t) :: v
    integer, parameter :: EXPECT(6) = [CAT_EXTENSION, CAT_EXTENSION, &
                                       CAT_WARN, CAT_WARN, CAT_CLEAN, &
                                       CAT_EXTENSION]
    integer :: i

    xs = hall_exhibits()
    call check(size(xs) == 6, 'SIX EXHIBITS STAND IN THE HALL REGISTRY')
    do i = 1, size(xs)
      call check(exists(xs(i)%old_file) .and. exists(xs(i)%modern_file), &
                 'HALL SCROLLS EXIST: ' // xs(i)%id)
      call run_probe(xs(i)%old_file, v)
      call check(v%ok, 'THE PROBE RETURNS: ' // xs(i)%id)
      call check(v%category == EXPECT(i), &
                 'THE TESTIMONY CATEGORY IS TRUE: ' // xs(i)%id)
      if (EXPECT(i) /= CAT_CLEAN) then
        call check(len(v%evidence) > 0 .and. index(v%evidence, ' at (') == 0, &
                   'THE EVIDENCE IS NORMALIZED: ' // xs(i)%id)
      end if
      call run_probe(xs(i)%modern_file, v)
      call check(v%ok .and. v%category == CAT_CLEAN, &
                 'THE MODERN READING IS CLEAN: ' // xs(i)%id)
    end do
    call check_str(normalize_diag('C:\x\y.f:6:13:  Warning: Obsolescent ' // &
                   'feature: Computed GOTO at (1)'), &
                   'Warning: Obsolescent feature: Computed GOTO', &
                   'DIAGNOSTICS SHED THEIR COORDINATES')
    call check(category_text(CAT_REJECTED) == 'REJECTED OUTRIGHT', &
               'THE FOURTH FATE HAS ITS NAME')
    call check(.not. exists('state.mod'), &
               'THE PROBES LEFT NO DROPPINGS AT THE ROOT')
  end subroutine trial_hall_probes

  subroutine trial_hall_page()
    character(:), allocatable :: page, sources, u
    type(string_t), allocatable :: urls(:), srclines(:)
    type(hall_exhibit_t), allocatable :: xs(:)
    integer :: i, j
    logical :: ok_all

    page = slurp_file('dist\hall-of-deprecated-syntax.html')
    call check(count_substr(page, 'id="hall-legend"') == 1, &
               'THE FIVE FATES ARE TAUGHT ON THE PAGE')
    xs = hall_exhibits()
    do i = 1, size(xs)
      call check(count_substr(page, 'id="hall-' // xs(i)%id // '"') == 1, &
                 'THE PAGE HOLDS EXHIBIT ' // xs(i)%id)
    end do
    call check(count_substr(page, 'ACCEPTED CLEANLY UNDER -std=f2018') >= 6, &
               'CLEAN TESTIMONY APPEARS FOR EVERY MODERN READING')
    call check(count_substr(page, 'REJECTED UNDER -std=f2018') >= 3, &
               'THE EXTENSION TESTIMONY APPEARS THRICE')
    call check(count_substr(page, 'ACCEPTED WITH WARNING UNDER -std=f2018') >= 2, &
               'THE OBSOLESCENT TESTIMONY APPEARS TWICE')
    call check(count_substr(page, 'href="testaments.html"') >= 1 .and. &
               count_substr(page, 'href="why-it-still-stands.html"') >= 1, &
               'THE HALL POINTS TO ITS SIBLING WINGS')
    call read_all_lines('content\hall-of-deprecated-syntax\SOURCES.md', srclines)
    sources = ''
    do i = 1, size(srclines)
      sources = sources // srclines(i)%s // achar(10)
    end do
    call extract_hrefs(page, urls)
    ok_all = .true.
    do j = 1, size(urls)
      u = urls(j)%s
      if (index(u, '://') == 0) cycle
      if (count_substr(sources, u) < 1) ok_all = .false.
    end do
    call check(ok_all, 'EVERY EXTERNAL LINK OF THE HALL STANDS IN ITS RECORD')
  end subroutine trial_hall_page

  subroutine trial_tape_measure()
    type(string_t), allocatable :: hl(:), out(:), failures(:), report(:)
    type(measure_t) :: m
    type(route_t), allocatable :: rs(:)
    type(run_result) :: rr
    character(:), allocatable :: doc, cmd
    integer :: i, n_checks

    call harness_lines('file:///C:/x/dist/index.html', 375, hl)
    doc = ''
    do i = 1, size(hl)
      doc = doc // hl(i)%s // achar(10)
    end do
    call check(index(doc, 'width:375px') > 0, 'THE HARNESS SETS THE VIEWPORT')
    call check(index(doc, 'file:///C:/x/dist/index.html') > 0, &
               'THE HARNESS AIMS AT THE TARGET')
    call check(index(doc, 'FORTY-MEASURE-BEGIN') > 0, &
               'THE HARNESS SPEAKS THE MEASUREMENT TONGUE')

    cmd = browser_command('C:\Program Files\Edge\msedge.exe', &
                          'file:///C:/h.html', 'C:\p')
    call check(index(cmd, '--headless=new') > 0 .and. &
               index(cmd, '--dump-dom') > 0 .and. &
               index(cmd, '--virtual-time-budget') > 0, &
               'THE SUMMONS CARRIES THE OFFICIAL FLAGS')
    call check(starts_with(cmd, 'call "') .and. &
               cmd(len(cmd):len(cmd)) == '"', &
               'THE SUMMONS WEARS THE CALL PREFIX AGAINST QUOTE-STRIPPING')

    call check_str(path_to_url('C:\A B\dist\x.html'), &
                   'file:///C:/A%20B/dist/x.html', 'PATHS BECOME URLS')

    allocate (out(6))
    out(1)%s = 'noise before'
    out(2)%s = 'FORTY-MEASURE-BEGIN'
    out(3)%s = 'OK: YES'
    out(4)%s = 'SCROLLW: 375'
    out(5)%s = 'OVERFLOW: NO'
    out(6)%s = 'FORTY-MEASURE-END'
    call parse_measure(out, m)
    call check(m%ok .and. m%scrollw == 375 .and. .not. m%overflow, &
               'A GOOD MEASUREMENT IS READ')
    ! The dumped page also carries the harness's own script source,
    ! where both markers share one line; the tape must not read it.
    deallocate (out); allocate (out(7))
    out(1)%s = '<pre id="out">FORTY-MEASURE-BEGIN'
    out(2)%s = 'OK: YES'
    out(3)%s = 'SCROLLW: 740'
    out(4)%s = 'FORTY-MEASURE-END</pre>'
    out(5)%s = '<script>'
    out(6)%s = 'function emit(s){x.textContent="FORTY-MEASURE-BEGIN\n"' // &
               '+s+"\nFORTY-MEASURE-END";}'
    out(7)%s = '</script>'
    call parse_measure(out, m)
    call check(m%ok .and. m%scrollw == 740, &
               'THE SCRIPT''S OWN SCRIPTURE DOES NOT CONFUSE THE TAPE')
    deallocate (out); allocate (out(1))
    out(1)%s = 'no markers here'
    call parse_measure(out, m)
    call check(.not. m%ok .and. index(m%errmsg, 'NO MEASUREMENT') > 0, &
               'A MISSING MEASUREMENT IS NOT INVENTED')
    deallocate (out); allocate (out(4))
    out(1)%s = 'FORTY-MEASURE-BEGIN'
    out(2)%s = 'OK: NO'
    out(3)%s = 'ERR: TIMEOUT'
    out(4)%s = 'FORTY-MEASURE-END'
    call parse_measure(out, m)
    call check(.not. m%ok .and. m%errmsg == 'TIMEOUT', &
               'A FAILED MEASUREMENT CONFESSES ITS REASON')

    rs = routes()
    call good_measure(m, rs(1), 375)
    allocate (failures(0))
    n_checks = 0
    call judge_page(m, rs(1), 375, size(rs), 0, 0, n_checks, failures)
    call check(size(failures) == 0 .and. n_checks >= 12, &
               'A SOUND PAGE PASSES EVERY MEASUREMENT')
    call good_measure(m, rs(1), 375)
    m%overflow = .true.
    deallocate (failures); allocate (failures(0)); n_checks = 0
    call judge_page(m, rs(1), 375, size(rs), 0, 0, n_checks, failures)
    call check(size(failures) == 1, 'OVERFLOW IS A FAULT AND IS COUNTED')
    call good_measure(m, rs(1), 375)
    m%marker = ''
    deallocate (failures); allocate (failures(0)); n_checks = 0
    call judge_page(m, rs(1), 375, size(rs), 0, 0, n_checks, failures)
    call check(size(failures) == 1, 'A MISSING MARK IS A FAULT')
    call good_measure(m, rs(1), 375)
    m%links = 'ghost.html'
    deallocate (failures); allocate (failures(0)); n_checks = 0
    call judge_page(m, rs(1), 375, size(rs), 0, 0, n_checks, failures)
    call check(size(failures) == 1, 'A DOOR TO NOWHERE IS A FAULT')

    call good_measure(m, rs(1), 375)
    deallocate (failures); allocate (failures(0)); n_checks = 0
    call judge_page(m, rs(1), 375, size(rs), 0, 0, n_checks, failures)
    call render_inspection('trial', 1, n_checks, failures, report)
    doc = ''
    do i = 1, size(report)
      doc = doc // report(i)%s // achar(10)
    end do
    call check(index(doc, 'THE FABRIC HAS BEEN MEASURED.') > 0 .and. &
               index(doc, 'FAULTS: 0') > 0, &
               'THE TAPE RENDERS ITS PROCLAMATION')

    rr = run_cmd('call "C:\no\such\browser.exe" --dump-dom "x"')
    call check(rr%launched .and. rr%exit_code /= 0, &
               'A MISSING BROWSER FAILS HONESTLY')
  end subroutine trial_tape_measure

  subroutine good_measure(m, rt, width)
    type(measure_t), intent(out) :: m
    type(route_t), intent(in) :: rt
    integer, intent(in) :: width
    m%ok = .true.
    m%errmsg = ''
    m%title = rt%title
    m%scrollw = width - 15
    m%clientw = width - 15
    m%overflow = .false.
    m%sheets = 2
    m%sheetrules = '2,58'
    m%nav = 6
    m%active = rt%nav
    m%marker = 'FORTY 0.7.0'
    m%svgs = 0
    m%tables = 0
    m%pres = 2
    m%pres_contained = .true.
    m%h1 = rt%title
    if (width <= 600) then
      m%fontsize = '16px'
    else
      m%fontsize = '17px'
    end if
    m%links = 'why-it-still-stands.html|testaments.html'
  end subroutine good_measure

  subroutine trial_rebuild_from_nothing()
    type(run_result) :: rr
    integer :: code
    rr = run_cmd('rmdir /s /q dist')
    call check(.not. exists('dist\index.html'), 'THE PORCH IS RAZED FOR THE TRIAL')
    call set_muted(.true.)
    call run_generate(code)
    call set_muted(.false.)
    call check(code == 0, 'THE CATHEDRAL RISES FROM NOTHING')
    call set_muted(.true.)
    call run_validate(code)
    call set_muted(.false.)
    call check(code == 0, 'AND IS FOUND SOUND, REPRODUCED BY FORTRAN ALONE')
  end subroutine trial_rebuild_from_nothing

  subroutine trial_deploy_engine()
    character(:), allocatable :: fx, remote, tree1, tree2, tree3, tree4
    character(:), allocatable :: c1, c2, c3, why
    type(string_t), allocatable :: names(:), glines(:)
    type(run_result) :: rr
    logical :: ok, ready, has_parent
    character(:), allocatable :: parent
    integer :: i

    fx = temp_root() // '\forty_deploy_fx'
    remote = temp_root() // '\forty_deploy_fx_remote.git'
    call set_cwd(saved_root, ok)
    rr = run_cmd('if exist ' // quote(fx // '\') // ' rmdir /s /q ' // quote(fx))
    rr = run_cmd('if exist ' // quote(remote // '\') // ' rmdir /s /q ' // quote(remote))
    rr = run_cmd('mkdir ' // quote(fx))
    rr = run_cmd('git init -q --bare ' // quote(remote))
    call set_cwd(fx, ok)
    call check(ok, 'THE DEPLOY FIXTURE GROUND IS ENTERED')
    rr = run_cmd('git init -q -b main')
    rr = run_cmd('git config user.email trials@cathedral.local')
    rr = run_cmd('git config user.name "The Trials"')
    allocate (glines(3))
    glines(1)%s = 'dist/'
    glines(2)%s = 'build/'
    glines(3)%s = '*.mod'
    call write_lines('.gitignore', glines, ok)
    rr = run_cmd('mkdir src')
    call write_one('src\junk.f90', 'program junk')
    call write_one('a.md', 'prose')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "BASE"')
    rr = run_cmd('git remote add origin ' // quote(remote))
    rr = run_cmd('git push -q -u origin main')
    rr = run_cmd('mkdir dist')
    rr = run_cmd('mkdir dist\assets')
    call write_one('dist\index.html', '<p>nave</p>')
    call write_one('dist\blas.html', '<p>blas</p>')
    call write_one('dist\assets\cathedral.css', 'body{}')
    rr = run_cmd('mkdir build')
    call write_one('build\junk.o', 'not really an object')
    call write_one('state.mod', 'module droppings')

    call deploy_preflight(ready, why)
    call check(ready, 'THE DEPLOY GROUND IS FIT WHEN CLEAN AND SYNCED')

    call build_deploy_tree(tree1, ok)
    call check(ok, 'THE ARK IS BUILT FROM THE PORCH')
    call tree_manifest(tree1, names, ok)
    call check(ok .and. size(names) == 3, 'THE ARK CARRIES EXACTLY THE PORCH')
    if (size(names) == 3) then
      call check_str(names(1)%s, 'assets/cathedral.css', 'THE STYLESHEET BOARDS')
      call check_str(names(2)%s, 'blas.html', 'THE BOOK BOARDS')
      call check_str(names(3)%s, 'index.html', 'THE NAVE BOARDS')
    end if
    ok = .true.
    do i = 1, size(names)
      if (index(names(i)%s, 'junk') > 0 .or. index(names(i)%s, '.mod') > 0) ok = .false.
    end do
    call check(ok, 'NO SOURCE, YARD, OR DROPPING BOARDS THE ARK')

    call deploy_parent(parent, has_parent)
    call check(.not. has_parent, 'NO PUBLIC LINE EXISTS BEFORE THE FIRST OPENING')
    call check(version_line('git rev-parse refs/remotes/origin/gh-pages') == '', &
               'THE TREE ALONE MOVED NO BRANCH')

    call make_deploy_commit(tree1, '', .false., 'abcd1234', c1, ok)
    call check(ok, 'THE FIRST SEAL IS FORMED')
    call check(version_line('git rev-parse ' // c1 // ':') == tree1, &
               'THE SEAL CARRIES THE ARK')
    call check(version_line('git rev-parse ' // c1 // '~1') == '', &
               'THE FIRST OPENING HAS NO ANCESTOR')
    call push_deploy(c1, ok)
    call check(ok, 'THE FIRST LIFT SUCCEEDS')
    call check(version_line('git rev-parse refs/remotes/origin/gh-pages') == c1, &
               'THE PUBLIC LINE IS BORN AT THE SEAL')

    call write_one('dist\index.html', '<p>renewed nave</p>')
    call build_deploy_tree(tree2, ok)
    call check(ok .and. tree2 /= tree1, 'A CHANGED PORCH YIELDS A CHANGED ARK')
    call check(deploy_needed(tree2, c1), 'A CHANGED ARK NEEDS A NEW OPENING')
    call make_deploy_commit(tree2, c1, .true., 'abcd1234', c2, ok)
    call check(ok .and. version_line('git rev-parse ' // c2 // '~1') == c1, &
               'THE SECOND SEAL FOLLOWS THE FIRST')
    call push_deploy(c2, ok)
    call check(ok .and. version_line('git rev-parse refs/remotes/origin/gh-pages') == c2, &
               'THE PUBLIC LINE ADVANCES WITHOUT FORCE')

    call build_deploy_tree(tree3, ok)
    call check(ok .and. tree3 == tree2, 'AN UNCHANGED PORCH REBUILDS THE SAME ARK')
    call check(.not. deploy_needed(tree3, c2), &
               'AN UNCHANGED CATHEDRAL NEEDS NO SECOND OPENING')

    call write_one('src\dirty.f90', 'program dirty')
    call deploy_preflight(ready, why)
    call check(.not. ready .and. index(why, 'UNCLEAN') > 0, &
               'AN UNCLEAN TREE REFUSES THE DOORS')
    rr = run_cmd('del src\dirty.f90')
    call write_one('b.md', 'more prose')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "AHEAD"')
    call deploy_preflight(ready, why)
    call check(.not. ready .and. index(why, 'ACCORD') > 0, &
               'AN UNSYNCED main REFUSES THE DOORS')
    rr = run_cmd('git push -q')
    call deploy_preflight(ready, why)
    call check(ready, 'THE GROUND IS FIT AGAIN ONCE RECONCILED')

    rr = run_cmd('rmdir /s /q ' // quote(remote))
    call write_one('dist\blas.html', '<p>renewed blas</p>')
    call build_deploy_tree(tree4, ok)
    call make_deploy_commit(tree4, c2, .true., 'abcd1234', c3, ok)
    call check(ok, 'A SEAL CAN FORM EVEN WHEN THE REMOTE IS GONE')
    call push_deploy(c3, ok)
    call check(.not. ok, 'A SEVERED REMOTE FAILS THE LIFT HONESTLY')

    call set_cwd(saved_root, ok)
  end subroutine trial_deploy_engine

  subroutine trial_deploy_planning()
    character(:), allocatable :: url, branch, doc
    logical :: okj
    call check(gate_production([0, 0, 1, 0, 0]) == 'validate', &
               'THE GATE NAMES THE REFUSING STAGE')
    call check(gate_production([1, 0, 0, 0, 0]) == 'build', &
               'THE GATE NAMES THE FIRST REFUSAL')
    call check(len(gate_production([0, 0, 0, 0, 0])) == 0, &
               'A CLEAN PATH PASSES THE GATE')
    doc = '{"url":"https://api.github.com/repos/x/y/pages",' // &
          '"status":"built",' // &
          '"html_url":"https://teamofoxen.github.io/fortran-cathedral/",' // &
          '"source":{"branch":"gh-pages","path":"/"}}'
    call parse_pages_response(doc, url, branch, okj)
    call check(okj, 'THE PAGES RESPONSE IS PARSED')
    call check_str(url, 'https://teamofoxen.github.io/fortran-cathedral/', &
                   'THE PUBLIC ADDRESS IS READ')
    call check_str(branch, 'gh-pages', 'THE SERVING BRANCH IS READ')
    call parse_pages_response('{"nope": 1}', url, branch, okj)
    call check(.not. okj, 'A STRANGE RESPONSE IS NOT PRETENDED UNDERSTOOD')
  end subroutine trial_deploy_planning

  ! ------------------------------------------------ the restitution trials

  subroutine capture_root()
    type(run_result) :: rr
    rr = run_cmd('cd')
    saved_root = rr%out(1)%s
  end subroutine capture_root

  !> Forge an isolated fixture repository with a local bare remote:
  !> C0 (base) -> C_OFF (offense) -> C_NOW (present), pushed and synced.
  !> The fixture's ledger records the offense per row_mode. Leaves the
  !> working directory INSIDE the fixture; callers return to saved_root.
  subroutine make_fixture(tag, row_mode, c0, c_off, c_now, fx, remote)
    character(*), intent(in) :: tag
    integer, intent(in) :: row_mode
    character(:), allocatable, intent(out) :: c0, c_off, c_now, fx, remote
    type(run_result) :: rr
    type(string_t), allocatable :: lines(:)
    logical :: ok
    character(:), allocatable :: row_hash

    fx = temp_root() // '\forty_rite_' // tag
    remote = temp_root() // '\forty_rite_' // tag // '_remote.git'
    call set_cwd(saved_root, ok)
    rr = run_cmd('if exist ' // quote(fx // '\') // ' rmdir /s /q ' // quote(fx))
    rr = run_cmd('if exist ' // quote(remote // '\') // ' rmdir /s /q ' // quote(remote))
    rr = run_cmd('mkdir ' // quote(fx))
    rr = run_cmd('git init -q --bare ' // quote(remote))
    call set_cwd(fx, ok)
    call check(ok, 'THE FIXTURE GROUND IS ENTERED (' // tag // ')')
    rr = run_cmd('git init -q -b main')
    rr = run_cmd('git config user.email trials@cathedral.local')
    rr = run_cmd('git config user.name "The Trials"')
    call write_one('a.txt', 'phase zero stone')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "C0"')
    c0 = version_line('git rev-parse HEAD')
    call write_one('a.txt', 'offense stone')
    call write_one('b.txt', 'offense extra')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "OFFENSE"')
    c_off = version_line('git rev-parse HEAD')

    select case (row_mode)
    case (ROW_OFFENDER); row_hash = c_off
    case (ROW_GHOST);    row_hash = GHOST_HASH
    case (ROW_ROOT);     row_hash = c0
    case default;        row_hash = ''
    end select
    allocate (lines(0))
    call push_string(lines, '## Current ledger')
    call push_string(lines, '| File or component | Language | Executable lines | Purpose | Why | Removal |')
    call push_string(lines, '|---|---:|---:|---|---|---|')
    call push_string(lines, '| None | - | 0 | - | - | - |')
    call push_string(lines, '')
    call push_string(lines, '## Operational transgressions')
    call push_string(lines, '| Date | Event | Commit | Executable non-Fortran lines introduced | Why | Remediation | Status |')
    call push_string(lines, '|---|---|---|---:|---|---|---|')
    if (row_mode /= ROW_NONE) then
      call push_string(lines, '| 2026-07-18 | A manual offering | `' // row_hash // &
                       '` | 0 | haste | forty offer | Historical. Disclosed. Not erasable. |')
    end if
    call push_string(lines, '')
    call push_string(lines, '## Rules')
    call push_string(lines, 'None here.')
    call write_lines('HERESY_LEDGER.md', lines, ok)

    call write_one('c.txt', 'the present stone')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "NOW"')
    c_now = version_line('git rev-parse HEAD')
    rr = run_cmd('git remote add origin ' // quote(remote))
    rr = run_cmd('git push -q -u origin main')
    call check(version_line('git rev-parse origin/main') == c_now, &
               'THE FIXTURE REMOTE IS SYNCED (' // tag // ')')
  end subroutine make_fixture

  subroutine write_one(path, text)
    character(*), intent(in) :: path, text
    type(string_t), allocatable :: lines(:)
    logical :: ok
    allocate (lines(1))
    lines(1)%s = text
    call write_lines(path, lines, ok)
  end subroutine write_one

  function fixture_cli(yes, dry) result(c)
    logical, intent(in) :: yes, dry
    type(cli_t) :: c
    c%assume_yes = yes
    c%dry_run = dry
    c%message = ''
    c%rite = 'phase-1-manual-offering'
  end function fixture_cli

  function ledger_doc() result(doc)
    character(:), allocatable :: doc
    type(string_t), allocatable :: lines(:)
    integer :: i
    call read_all_lines('HERESY_LEDGER.md', lines)
    doc = ''
    do i = 1, size(lines)
      doc = doc // lines(i)%s // achar(10)
    end do
  end function ledger_doc

  subroutine trial_expiate_transform()
    type(string_t), allocatable :: lines(:), out(:)
    type(expiation_t) :: exp
    type(transgression_t), allocatable :: trans(:)
    logical :: changed, found, wellformed
    character(40), parameter :: OFF = repeat('a', 40)
    character(40), parameter :: WH = repeat('b', 40)
    character(40), parameter :: RH = repeat('c', 40)
    character(:), allocatable :: doc
    integer :: i

    allocate (lines(0))
    call push_string(lines, '## Operational transgressions')
    call push_string(lines, '| Date | Event | Commit | Executable non-Fortran lines introduced | Why | Remediation | Status |')
    call push_string(lines, '|---|---|---|---:|---|---|---|')
    call push_string(lines, '| 2026-07-18 | Manual push | `' // OFF // &
                     '` | 0 | haste | forty offer | Historical. Disclosed. Not erasable. |')
    call push_string(lines, '')
    call push_string(lines, '## Rules')
    call push_string(lines, 'Prose.')
    call expiate_ledger_lines(lines, OFF, WH, RH, out, changed)
    call check(changed, 'THE TRANSFORM REPORTS ITS WORK')
    doc = ''
    do i = 1, size(out)
      doc = doc // out(i)%s // achar(10)
    end do
    call check(count_substr(doc, 'EXPIATED, NOT ERASED.') == 1, &
               'THE STATUS TRANSITIONS EXACTLY ONCE')
    call check(count_substr(doc, 'Historical. Disclosed. Not erasable.') == 0, &
               'THE OLD STATUS DEPARTS')
    call check(index(doc, '## Expiation record') > 0 .and. &
               index(doc, '## Expiation record') < index(doc, '## Rules'), &
               'THE RECORD IS INSCRIBED BEFORE THE RULES')
    call ledger_transgressions(out, trans, wellformed)
    call check(wellformed .and. size(trans) == 1, 'THE TRANSFORMED CHAPTER STILL PARSES')
    call ledger_expiation(out, exp, found)
    call check(found, 'THE EXPIATION RECORD PARSES')
    if (found) then
      call check_str(exp%withdrawal, WH, 'THE WITHDRAWAL IS RECORDED')
      call check_str(exp%reoffering, RH, 'THE RE-OFFERING IS RECORDED')
      call check(len(exp%means) > 0 .and. len(exp%history) > 0, &
                 'MEANS AND HISTORY ARE STATED')
    end if
    call expiate_ledger_lines(out, OFF, WH, RH, lines, changed)
    call check(.not. changed, 'AN EXPIATED LEDGER DOES NOT TRANSITION TWICE')
  end subroutine trial_expiate_transform

  subroutine trial_restitution_happy()
    character(:), allocatable :: c0, coff, cnow, fx, remote
    character(:), allocatable :: w, r, w2, r2, tree_c0, tree_now, doc
    type(cli_t) :: c
    type(run_result) :: rr
    integer :: code, i
    logical :: ok, clean

    call make_fixture('happy', ROW_OFFENDER, c0, coff, cnow, fx, remote)
    tree_c0 = version_line('git rev-parse ' // coff // '~1:')
    tree_now = version_line('git rev-parse HEAD:')
    c = fixture_cli(.true., .false.)
    call set_scripted_confirm(SCRIPT_NONE)
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code == 0, 'THE FIXTURE RESTITUTION CONCLUDES')
    call check(is_hash(w) .and. is_hash(r), 'BOTH EXPIATION COMMITS ARE NAMED')
    call check(confirm_consult_count() == 1, 'ONE CONFIRMATION SERVED THE WHOLE RITE')
    call check(version_line('git rev-parse HEAD') == r, 'HEAD STANDS AT THE RE-OFFERING')
    call check(version_line('git rev-parse HEAD~1') == w, 'THE WITHDRAWAL PRECEDES IT')
    call check(version_line('git rev-parse HEAD~2') == cnow, &
               'THE PRESENT PRECEDES THE WITHDRAWAL: FORWARD ONLY')
    call check(version_line('git rev-parse ' // w // ':') == tree_c0, &
               'THE WITHDRAWN TREE IS THE PREDECESSOR TREE')
    call check(version_line('git rev-parse ' // r // ':') == tree_now, &
               'THE RE-OFFERED TREE IS THE CANONICAL TREE, BYTE FOR BYTE')
    call check(version_line('git rev-parse origin/main') == r, &
               'THE REMOTE RECEIVED THE RE-OFFERING')
    call check(version_line('git rev-parse origin/main~1') == w, &
               'THE REMOTE RECEIVED THE WITHDRAWAL')
    ! After the rite, exactly one uncommitted change exists: the ledger's
    ! new truth, awaiting its offering. The checked-out files themselves
    ! never passed through the withdrawn state.
    rr = run_cmd('git status --porcelain')
    clean = .true.
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) then
        if (index(rr%out(i)%s, 'HERESY_LEDGER.md') == 0) clean = .false.
      end if
    end do
    call check(clean, 'ONLY THE LEDGER AWAITS ITS OFFERING')
    call check(exists('c.txt'), &
               'THE TREE NEVER PASSED THROUGH THE WITHDRAWN STATE')
    doc = ledger_doc()
    call check(index(doc, 'EXPIATED, NOT ERASED.') > 0 .and. &
               index(doc, w) > 0 .and. index(doc, r) > 0, &
               'THE FIXTURE LEDGER RECORDS THE FULL RESTITUTION')
    ! Seal the ledger as the real rite's concluding offering would, then
    ! confirm an expiated stain atones no further.
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "RECORD"')
    rr = run_cmd('git push -q')
    call set_muted(.true.)
    call perform_restitution(c, coff, w2, r2, code)
    call set_muted(.false.)
    call check(code == 0 .and. len(w2) == 0, 'AN EXPIATED STAIN ATONES NO FURTHER')
    call set_cwd(saved_root, ok)
  end subroutine trial_restitution_happy

  subroutine trial_restitution_dry_and_decline()
    character(:), allocatable :: c0, coff, cnow, fx, remote, w, r, doc
    type(cli_t) :: c
    integer :: code
    logical :: ok

    call make_fixture('dry', ROW_OFFENDER, c0, coff, cnow, fx, remote)
    c = fixture_cli(.false., .true.)
    call set_scripted_confirm(SCRIPT_NONE)
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code == 0 .and. len(w) == 0, 'THE DRY RITE CONCLUDES IN PEACE')
    call check(confirm_consult_count() == 0, 'THE DRY RITE ASKS NOTHING')
    call check(version_line('git rev-parse HEAD') == cnow, 'THE DRY RITE MOVED NOTHING')
    call check(version_line('git rev-parse origin/main') == cnow, &
               'THE DRY RITE LIFTED NOTHING')
    doc = ledger_doc()
    call check(index(doc, 'Not erasable.') > 0 .and. index(doc, 'EXPIATED') == 0, &
               'THE DRY RITE INSCRIBED NOTHING')

    call set_scripted_confirm(SCRIPT_NO)
    c = fixture_cli(.false., .false.)
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code == 5, 'A WITHHELD CONFIRMATION DEFERS THE RITE')
    call check(confirm_consult_count() == 1, 'EXACTLY ONE CONFIRMATION WAS SOUGHT')
    call check(version_line('git rev-parse HEAD') == cnow, 'DEFERRAL MOVED NOTHING')
    call set_scripted_confirm(SCRIPT_NONE)
    call set_cwd(saved_root, ok)
  end subroutine trial_restitution_dry_and_decline

  subroutine trial_restitution_refusals()
    character(:), allocatable :: c0, coff, cnow, fx, remote, w, r
    type(cli_t) :: c
    type(run_result) :: rr
    integer :: code
    logical :: ok

    ! The ledger does not record the offense.
    call make_fixture('norec', ROW_NONE, c0, coff, cnow, fx, remote)
    c = fixture_cli(.true., .false.)
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'AN UNRECORDED OFFENSE CANNOT BE ATONED')
    call check(version_line('git rev-parse HEAD') == cnow, 'AND NOTHING MOVED (NOREC)')

    ! The recorded commit does not exist.
    call make_fixture('ghost', ROW_GHOST, c0, coff, cnow, fx, remote)
    call set_muted(.true.)
    call perform_restitution(c, GHOST_HASH, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'A GHOST OFFENSE CANNOT BE ATONED')
    call check(version_line('git rev-parse HEAD') == cnow, 'AND NOTHING MOVED (GHOST)')

    ! The tree is unclean.
    call make_fixture('dirty', ROW_OFFENDER, c0, coff, cnow, fx, remote)
    call write_one('uncommitted.txt', 'dust')
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'AN UNCLEAN TREE REFUSES THE RITE')
    call check(version_line('git rev-parse HEAD') == cnow, 'AND NOTHING MOVED (DIRTY)')

    ! Local and remote are not of one accord.
    call make_fixture('ahead', ROW_OFFENDER, c0, coff, cnow, fx, remote)
    call write_one('d.txt', 'unpushed stone')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "AHEAD"')
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'A BROKEN ACCORD REFUSES THE RITE')
    call check(version_line('git rev-parse origin/main') == cnow, &
               'AND THE REMOTE NEVER MOVED (AHEAD)')

    ! The offense is the root commit: no predecessor tree exists.
    call make_fixture('root', ROW_ROOT, c0, coff, cnow, fx, remote)
    call set_scripted_confirm(SCRIPT_NONE)
    call set_muted(.true.)
    call perform_restitution(c, c0, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'A ROOT OFFENSE HAS NO PREDECESSOR TO RESTORE')
    call check(confirm_consult_count() == 0, &
               'THE FAULT WAS FOUND BEFORE ANY CONFIRMATION')
    call check(version_line('git rev-parse HEAD') == cnow, 'AND NOTHING MOVED (ROOT)')
    call set_cwd(saved_root, ok)
  end subroutine trial_restitution_refusals

  subroutine trial_restitution_severed()
    character(:), allocatable :: c0, coff, cnow, fx, remote, w, r, doc
    type(cli_t) :: c
    type(run_result) :: rr
    integer :: code
    logical :: ok

    call make_fixture('sever', ROW_OFFENDER, c0, coff, cnow, fx, remote)
    rr = run_cmd('rmdir /s /q ' // quote(remote))
    c = fixture_cli(.true., .false.)
    call set_muted(.true.)
    call perform_restitution(c, coff, w, r, code)
    call set_muted(.false.)
    call check(code /= 0, 'A SEVERED REMOTE HALTS THE RITE AT THE LIFT')
    doc = ledger_doc()
    call check(index(doc, 'EXPIATED') == 0, &
               'THE LEDGER WAITS WHEN THE LIFT FAILS')
    call set_cwd(saved_root, ok)
  end subroutine trial_restitution_severed

  subroutine trial_audit_capabilities()
    character(:), allocatable :: fx, c1, c2, c3, modpath
    type(string_t), allocatable :: paths(:), sus(:), yard(:), heresy(:)
    type(string_t), allocatable :: cands(:), report(:)
    type(finding_t), allocatable :: fs(:)
    type(finding_t) :: hf
    type(run_result) :: rr
    character(1) :: action
    logical :: ok, found
    integer :: i

    ! An isolated fixture with a pure commit, a defiled commit, and a
    ! cleansing commit. No remote exists; nothing can be touched.
    fx = temp_root() // '\forty_audit_fx'
    call set_cwd(saved_root, ok)
    rr = run_cmd('if exist ' // quote(fx // '\') // ' rmdir /s /q ' // quote(fx))
    rr = run_cmd('mkdir ' // quote(fx))
    call set_cwd(fx, ok)
    call check(ok, 'THE AUDIT FIXTURE GROUND IS ENTERED')
    rr = run_cmd('git init -q -b main')
    rr = run_cmd('git config user.email trials@cathedral.local')
    rr = run_cmd('git config user.name "The Trials"')
    rr = run_cmd('mkdir src')
    call write_one('src\gen.f90', '! <!doctype html> and User-agent: * live here')
    call write_one('notes.md', 'prose only')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "C1 PURE"')
    c1 = version_line('git rev-parse HEAD')
    call write_one('page.html', '<p>handwritten</p>')
    rr = run_cmd('mkdir dist')
    call write_one('dist\out.html', '<p>tracked output</p>')
    rr = run_cmd('mkdir templates')
    call write_one('templates\x.tpl', 'a template')
    call write_one('junk.mod', 'module droppings')
    call write_one('script.py', 'print(1)')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "C2 DEFILED"')
    c2 = version_line('git rev-parse HEAD')
    rr = run_cmd('del junk.mod')
    rr = run_cmd('git add -A')
    rr = run_cmd('git commit -q -m "C3 CLEANSED"')
    c3 = version_line('git rev-parse HEAD')

    call tree_paths(c1, paths, ok)
    call check(ok .and. size(paths) == 2, 'THE PURE TREE IS READ')
    call scan_tracked_html(paths, sus, yard)
    call check(size(sus) == 0 .and. size(yard) == 0, 'A PURE TREE SHOWS NO HTML')
    call scan_template_suspects(paths, sus)
    call check(size(sus) == 0, 'A PURE TREE SHOWS NO TEMPLATING MACHINERY')
    call scan_tree_heresy(paths, heresy)
    call check(size(heresy) == 0, 'A PURE TREE SHOWS NO EXECUTABLE HERESY')

    call tree_paths(c2, paths, ok)
    call scan_tracked_html(paths, sus, yard)
    call check(size(sus) == 1 .and. size(yard) == 1, &
               'HANDWRITTEN HTML AND TRACKED OUTPUT ARE TOLD APART')
    if (size(sus) == 1) then
      call check_str(sus(1)%s, 'page.html', 'THE HANDWRITTEN SUSPECT IS NAMED')
    end if
    call scan_template_suspects(paths, sus)
    found = .false.
    do i = 1, size(sus)
      if (index(sus(i)%s, 'x.tpl') > 0) found = .true.
    end do
    call check(found, 'THE TEMPLATE IS DETECTED')
    call scan_tree_heresy(paths, heresy)
    found = .false.
    do i = 1, size(heresy)
      if (index(heresy(i)%s, 'script.py') > 0) found = .true.
    end do
    call check(found, 'THE EXECUTABLE HERESY IS DETECTED')

    allocate (cands(1))
    cands(1)%s = 'src/gen.f90'
    modpath = signature_module(c1, cands, '<!doctype html>')
    call check_str(modpath, 'src/gen.f90', 'THE GENERATOR SIGNATURE IS ATTRIBUTED')
    modpath = signature_module(c1, cands, 'no_such_sig_xyz')
    call check(len(modpath) == 0, 'AN ABSENT SIGNATURE ATTRIBUTES NOTHING')

    call check(.not. commit_exists(GHOST_HASH), &
               'A MISSING HISTORICAL COMMIT IS REPORTED MISSING')

    call residue_change(c2, 'junk.mod', action, found)
    call check(found .and. action == 'A', 'THE RESIDUE''S ARRIVAL IS READ')
    call residue_change(c3, 'junk.mod', action, found)
    call check(found .and. action == 'D', 'THE RESIDUE''S DEPARTURE IS READ')
    call check(.not. tree_has_path('HEAD', 'junk.mod'), &
               'THE CURRENT TREE IS FREE OF THE RESIDUE')
    call check(tree_has_path('HEAD', 'page.html'), &
               'PRESENCE IS READ AS TRULY AS ABSENCE')

    hf = historical_execution_finding()
    call check(hf%verdict == 'UNPROVEN', &
               'HISTORY''S OWN EXECUTION IS HONESTLY UNPROVEN')

    allocate (fs(0))
    call add_finding(fs, V_PROVEN, 'A CLEAN THING', 'It is clean.')
    call add_finding(fs, V_HERESY, 'A FOUND SIN', 'It is found.')
    call render_report('TRIAL REPORT', fs, report)
    found = .false.
    do i = 1, size(report)
      if (index(report(i)%s, '[HERESY DETECTED] A FOUND SIN') > 0) found = .true.
    end do
    call check(found, 'THE REPORT CLASSIFIES ITS FINDINGS')
    found = .false.
    do i = 1, size(report)
      if (index(report(i)%s, 'HERESY DETECTED: 1.') > 0) found = .true.
    end do
    call check(found, 'THE REPORT COUNTS ITS VERDICTS')
    rr = run_cmd('if not exist build\audit\ mkdir build\audit')
    call write_lines('build\audit\provenance.txt', report, ok)
    call check(ok .and. exists('build\audit\provenance.txt'), &
               'THE REPORT RESTS IN ITS IGNORED CRYPT')
    call set_cwd(saved_root, ok)
  end subroutine trial_audit_capabilities

  function exists(path) result(r)
    character(*), intent(in) :: path
    logical :: r
    inquire (file=path, exist=r)
  end function exists

  function slurp_file(path) result(doc)
    character(*), intent(in) :: path
    character(:), allocatable :: doc
    type(string_t), allocatable :: lines(:)
    integer :: i
    call read_all_lines(path, lines)
    doc = ''
    do i = 1, size(lines)
      doc = doc // lines(i)%s // achar(10)
    end do
  end function slurp_file

end program test_forty
