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
  use forty_paths, only: quote, temp_root
  use forty_run, only: run_result, run_cmd, read_all_lines, write_lines, delete_file
  use forty_confess, only: classify, ledger_entries, transgression_t, &
                           ledger_transgressions, split_cells, &
                           CLASS_FORTRAN, CLASS_DECLARATIVE, CLASS_HERESY, CLASS_OTHER
  use forty_offer, only: build_offer_plan, check_offer_ground, &
                         categorize_porcelain, offering_acceptable, &
                         porcelain_path, run_offer
  use forty_git, only: slug_from_url, valid_slug
  use forty_github, only: plan_step_t, build_connect_plan, parse_login
  use forty_canon, only: CANON_DESCRIPTION, CANON_BASE_URL
  use forty_ui, only: set_muted
  use forty_buildops, only: dir_present
  use cathedral_html, only: escape_html, escape_json
  use cathedral_routes, only: route_t, routes, append_nav
  use cathedral_generate, only: run_generate
  use cathedral_validate, only: run_validate
  implicit none

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
  call trial_transgressions()
  call trial_commit_messages()
  call trial_offer_plan()
  call trial_porcelain()
  call trial_residue()
  call trial_offer_ground_and_accord()
  call trial_offer_discipline()

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
    type(string_t), allocatable :: paths(:)
    logical :: ok
    character(:), allocatable :: offending
    allocate (paths(2))
    paths(1)%s = 'src/forty_offer.f90'
    paths(2)%s = 'HERESY_LEDGER.md'
    call offering_acceptable(paths, ok, offending)
    call check(ok, 'HONEST WORKS ARE ACCEPTED')
    deallocate (paths); allocate (paths(2))
    paths(1)%s = 'src/ok.f90'
    paths(2)%s = 'build/sneaky.o'
    call offering_acceptable(paths, ok, offending)
    call check(.not. ok, 'YARD RESIDUE IS REFUSED')
    call check_str(offending, 'build/sneaky.o', 'THE OFFENDER IS NAMED')
    deallocate (paths); allocate (paths(1))
    paths(1)%s = 'dist/index.html'
    call offering_acceptable(paths, ok, offending)
    call check(.not. ok, 'PORCH RESIDUE IS REFUSED')
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
    call check(size(rs) == 2, 'TWO ROUTES STAND IN THE REGISTRY')
    if (size(rs) == 2) then
      call check_str(rs(1)%file, 'index.html', 'THE NAVE IS THE INDEX')
      call check_str(rs(2)%file, 'confessional.html', 'THE CONFESSIONAL HAS ITS DOOR')
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
               exists('dist\routes.json'), 'ALL SIX WORKS ARE LAID')
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
    character(24), parameter :: works(4) = [character(24) :: &
      'dist\index.html', 'dist\confessional.html', &
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
