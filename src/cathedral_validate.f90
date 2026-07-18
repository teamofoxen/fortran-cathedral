!> cathedral_validate: the survey of the raised fabric. Confirms every
!> route's page exists and is sound, every asset is present, the sitemap
!> and manifest agree with the registry, and — enforced, not asserted —
!> that no script tag exists anywhere in the porch.
module cathedral_validate
  use forty_util, only: string_t, push_string, to_lower, starts_with, &
                        count_substr, int_to_str
  use forty_ui, only: say, lament, rule
  use forty_paths, only: file_exists, quote, temp_root
  use forty_run, only: run_result, run_cmd, read_all_lines, tool_found
  use forty_canon, only: EXIT_OK, EXIT_FAIL, CANON_BASE_URL
  use forty_confess, only: transgression_t, ledger_transgressions, &
                           expiation_t, ledger_expiation, &
                           list_repo_files, heresy_summary
  use cathedral_routes, only: route_t, routes
  use cathedral_testaments, only: verse_t, verses
  implicit none
  private
  public :: run_validate, extract_hrefs

  integer :: n_checks = 0
  integer :: n_breach = 0

contains

  subroutine run_validate(exit_code)
    integer, intent(out) :: exit_code
    type(route_t), allocatable :: rs(:)
    character(:), allocatable :: doc, other_file, sm, rj
    integer :: i, j

    n_checks = 0
    n_breach = 0
    rs = routes()

    call say('THE SURVEY OF THE FABRIC BEGINS.')
    call rule()

    call check(file_exists('dist\index.html'), 'THE NAVE STANDS (dist\index.html)')
    call check(file_exists('dist\assets\tokens.css'), 'THE TOKENS ARE LAID')
    call check(file_exists('dist\assets\cathedral.css'), 'THE STYLESHEET IS LAID')
    call check(file_exists('dist\assets\ornament.svg'), 'THE ROSE WINDOW IS SET')
    call check(file_exists('dist\robots.txt'), 'THE CRAWLERS ARE ADDRESSED')
    call check(file_exists('dist\sitemap.xml'), 'THE MAP IS DRAWN')
    call check(file_exists('dist\routes.json'), 'THE MANIFEST IS KEPT')

    do i = 1, size(rs)
      call check(file_exists('dist\' // rs(i)%file), &
                 'ROUTE STANDS: ' // rs(i)%file)
      doc = slurp('dist\' // rs(i)%file)
      call check(count_substr(doc, '<!doctype html>') == 1, &
                 rs(i)%file // ': ONE DOCTYPE, HUMBLE AND LOWERCASE')
      call check(count_substr(doc, '<html lang="en">') == 1, &
                 rs(i)%file // ': THE TONGUE IS DECLARED')
      call check(count_substr(doc, '<title>') == 1, &
                 rs(i)%file // ': A TITLE IS GIVEN')
      call check(count_substr(doc, 'aria-current="page"') == 1, &
                 rs(i)%file // ': EXACTLY ONE PLACE IN THE NAV IS HELD')
      call check(count_substr(doc, '<h1>') == 1, &
                 rs(i)%file // ': ONE HEADING REIGNS')
      call check(count_substr(doc, 'assets/cathedral.css') == 1, &
                 rs(i)%file // ': THE STYLESHEET IS SUMMONED ONCE')
      call check(count_substr(doc, '</html>') == 1, &
                 rs(i)%file // ': THE DOCUMENT IS CLOSED')
      do j = 1, size(rs)
        if (i /= j) then
          other_file = rs(j)%file
          call check(count_substr(doc, 'href="' // other_file // '"') >= 1, &
                     rs(i)%file // ': THE WAY TO ' // other_file // ' IS OPEN')
        end if
      end do
    end do

    call check(porch_is_free_of_scripts(), &
               'NO SCRIPT TAG EXISTS ANYWHERE IN THE PORCH (ENFORCED)')

    sm = slurp('dist\sitemap.xml')
    call check(count_substr(sm, '<urlset') == 1, 'THE MAP BEARS ITS SEAL')
    do i = 1, size(rs)
      call check(count_substr(sm, '<loc>' // CANON_BASE_URL // '/' // &
                 rs(i)%file // '</loc>') == 1, &
                 'THE MAP NAMES ' // rs(i)%file // ' EXACTLY ONCE')
    end do

    rj = slurp('dist\routes.json')
    do i = 1, size(rs)
      call check(count_substr(rj, '"slug": "' // rs(i)%slug // '"') == 1, &
                 'THE MANIFEST NAMES ' // rs(i)%slug // ' EXACTLY ONCE')
    end do

    doc = slurp('dist\robots.txt')
    call check(count_substr(doc, 'User-agent: *') == 1, 'ALL AGENTS ARE ADDRESSED')
    call check(count_substr(doc, 'Sitemap: ') == 1, 'THE MAP IS PROCLAIMED TO THEM')

    call check_operational_record()
    call check_generation_doctrine(rs)
    call check_verses()
    call check_links(rs)

    call rule()
    if (n_breach == 0) then
      call say('THE FABRIC IS SOUND. ' // int_to_str(n_checks) // ' CHECKS UPHELD.')
      exit_code = EXIT_OK
    else
      call lament('THE FABRIC IS BREACHED: ' // int_to_str(n_breach) // ' OF ' // &
                  int_to_str(n_checks) // ' CHECKS FAILED.')
      exit_code = EXIT_FAIL
    end if
  end subroutine run_validate

  !> Every transgression the Ledger records must be displayed, by
  !> commit hash, in the public Confessional. History does not hide.
  subroutine check_operational_record()
    type(string_t), allocatable :: ledger(:)
    type(transgression_t), allocatable :: trans(:)
    type(expiation_t) :: exp
    logical :: wellformed, exp_found, any_expiated
    character(:), allocatable :: page
    integer :: i
    call read_all_lines('HERESY_LEDGER.md', ledger)
    call ledger_transgressions(ledger, trans, wellformed)
    call check(wellformed, 'THE OPERATIONAL CHAPTER IS PRESENT AND WELL-FORMED')
    page = slurp('dist\confessional.html')
    any_expiated = .false.
    do i = 1, size(trans)
      if (index(trans(i)%status, 'EXPIATED') > 0) any_expiated = .true.
      if (len(trans(i)%commit) >= 7) then
        call check(count_substr(page, trans(i)%commit) >= 1, &
                   'THE CONFESSIONAL DISPLAYS TRANSGRESSION ' // int_to_str(i) // &
                   ' (' // trans(i)%commit(1:7) // ')')
      end if
    end do
    if (any_expiated) then
      call ledger_expiation(ledger, exp, exp_found)
      call check(exp_found, 'THE EXPIATION RECORD ACCOMPANIES THE EXPIATED STAIN')
      if (exp_found) then
        call check(count_substr(page, exp%withdrawal) >= 1, &
                   'THE CONFESSIONAL DISPLAYS THE WITHDRAWAL COMMIT')
        call check(count_substr(page, exp%reoffering) >= 1, &
                   'THE CONFESSIONAL DISPLAYS THE RE-OFFERING COMMIT')
        call check(count_substr(page, 'EXPIATED, NOT ERASED') >= 1, &
                   'THE CONFESSIONAL PROCLAIMS: EXPIATED, NOT ERASED')
      end if
    end if
  end subroutine check_operational_record

  !> The doctrine: all HTML is generated by Fortran. No handwritten
  !> .html may stand in the source tree, every page in the porch must be
  !> named in the route registry, every page must bear the verger's
  !> generation mark, and no non-Fortran executable may exist to have
  !> participated. Byte-reproducibility is enforced by the trials.
  subroutine check_generation_doctrine(rs)
    type(route_t), intent(in) :: rs(:)
    type(string_t), allocatable :: files(:)
    type(run_result) :: rr
    logical :: ok, found, pure_tree
    integer :: i, j, h_files, h_lines, n_pages
    character(:), allocatable :: doc

    call list_repo_files(files, ok)
    pure_tree = ok
    if (ok) then
      do i = 1, size(files)
        if (ends_ci(files(i)%s, '.html')) pure_tree = .false.
      end do
    end if
    call check(pure_tree, 'NO HANDWRITTEN HTML STANDS IN THE SOURCE TREE')

    call heresy_summary(h_files, h_lines)
    call check(h_files == 0 .and. h_lines == 0, &
               'THE GENERATION PATH IS FORTRAN ALONE (0 HERESY LINES)')

    do i = 1, size(rs)
      doc = slurp('dist\' // rs(i)%file)
      call check(count_substr(doc, '<meta name="generator" content="FORTY ') == 1, &
                 rs(i)%file // ': BEARS THE VERGER''S MARK')
    end do

    rr = run_cmd('dir /b dist\*.html')
    n_pages = 0
    if (rr%launched .and. rr%exit_code == 0) then
      do i = 1, size(rr%out)
        if (len_trim(rr%out(i)%s) == 0) cycle
        n_pages = n_pages + 1
        found = .false.
        do j = 1, size(rs)
          if (to_lower(trim(rr%out(i)%s)) == to_lower(rs(j)%file)) found = .true.
        end do
        call check(found, 'THE REGISTRY NAMES ' // trim(rr%out(i)%s))
      end do
    end if
    call check(n_pages == size(rs), 'THE PORCH HOLDS EXACTLY THE REGISTERED PAGES')
  end subroutine check_generation_doctrine

  !> The Rite of Compilation: every verse of the Testaments must exist,
  !> appear on its page, and satisfy GFortran's judgment.
  subroutine check_verses()
    type(verse_t), allocatable :: vs(:)
    type(run_result) :: rr
    character(:), allocatable :: page, gpath
    integer :: v

    call check(tool_found('gfortran', gpath), 'THE COMPILER ATTENDS THE SURVEY')
    page = slurp('dist\testaments.html')
    vs = verses()
    do v = 1, size(vs)
      call check(file_exists(vs(v)%old_file), &
                 'VERSE ' // vs(v)%id // ': THE OLD SCROLL EXISTS')
      call check(file_exists(vs(v)%new_file), &
                 'VERSE ' // vs(v)%id // ': THE NEW SCROLL EXISTS')
      call check(count_substr(page, 'id="verse-' // vs(v)%id // '"') == 1, &
                 'VERSE ' // vs(v)%id // ': STANDS ON THE PAGE EXACTLY ONCE')
      ! -J banishes .mod droppings to the temple of ephemera; even a
      ! syntax-only blessing writes module files, and the tree stays clean.
      rr = run_cmd('gfortran -fsyntax-only -J ' // quote(temp_root()) // &
                   ' ' // quote(vs(v)%old_file))
      call check(rr%launched .and. rr%exit_code == 0, &
                 'VERSE ' // vs(v)%id // ': THE OLD TESTAMENT COMPILES')
      rr = run_cmd('gfortran -fsyntax-only -J ' // quote(temp_root()) // &
                   ' ' // quote(vs(v)%new_file))
      call check(rr%launched .and. rr%exit_code == 0, &
                 'VERSE ' // vs(v)%id // ': THE MODERN TESTAMENT COMPILES')
      call check(.not. file_exists('state.mod'), &
                 'VERSE ' // vs(v)%id // ': NO DROPPINGS FOUL THE TREE')
    end do
  end subroutine check_verses

  !> Every href value in a document, in order of appearance.
  subroutine extract_hrefs(doc, urls)
    character(*), intent(in) :: doc
    type(string_t), allocatable, intent(out) :: urls(:)
    integer :: pos, hit, close
    character(*), parameter :: MARK = 'href="'
    allocate (urls(0))
    pos = 1
    do
      if (pos > len(doc)) exit
      hit = index(doc(pos:), MARK)
      if (hit == 0) exit
      hit = pos + hit - 1 + len(MARK)
      close = index(doc(hit:), '"')
      if (close == 0) exit
      if (close > 1) call push_string(urls, doc(hit:hit + close - 2))
      pos = hit + close
    end do
  end subroutine extract_hrefs

  !> Every internal door must open, and every external link on the Why
  !> wing must stand in its declarative source record.
  subroutine check_links(rs)
    type(route_t), intent(in) :: rs(:)
    type(string_t), allocatable :: urls(:), srclines(:)
    character(:), allocatable :: doc, u, sources
    integer :: i, j, frag
    logical :: ok_all

    do i = 1, size(rs)
      doc = slurp('dist\' // rs(i)%file)
      call extract_hrefs(doc, urls)
      ok_all = .true.
      do j = 1, size(urls)
        u = urls(j)%s
        if (index(u, '://') > 0) cycle
        if (len(u) == 0) cycle
        if (u(1:1) == '#') cycle
        frag = index(u, '#')
        if (frag > 0) u = u(1:frag - 1)
        if (len(u) < 6) cycle
        if (to_lower(u(len(u) - 4:)) /= '.html') cycle
        if (.not. file_exists('dist\' // u)) ok_all = .false.
      end do
      call check(ok_all, rs(i)%file // ': ALL INTERNAL DOORS OPEN')
    end do

    call read_all_lines('content\why-it-still-stands\SOURCES.md', srclines)
    sources = ''
    do i = 1, size(srclines)
      sources = sources // srclines(i)%s // achar(10)
    end do
    doc = slurp('dist\why-it-still-stands.html')
    call extract_hrefs(doc, urls)
    ok_all = .true.
    do j = 1, size(urls)
      u = urls(j)%s
      if (index(u, '://') == 0) cycle
      if (count_substr(sources, u) < 1) ok_all = .false.
    end do
    call check(ok_all, 'EVERY EXTERNAL LINK ON THE WHY WING STANDS IN ITS SOURCE RECORD')
    call check(size(urls) > 0, 'THE WHY WING SPEAKS WITH CITATIONS, NOT ASSERTIONS')
  end subroutine check_links

  pure function ends_ci(s, suffix) result(r)
    character(*), intent(in) :: s, suffix
    logical :: r
    integer :: n, m
    n = len_trim(s)
    m = len(suffix)
    r = .false.
    if (n >= m) r = (to_lower(s(n - m + 1:n)) == to_lower(suffix))
  end function ends_ci

  subroutine check(cond, label)
    logical, intent(in) :: cond
    character(*), intent(in) :: label
    n_checks = n_checks + 1
    if (cond) then
      call say('  UPHELD: ' // label)
    else
      n_breach = n_breach + 1
      call lament('BREACHED: ' // label)
    end if
  end subroutine check

  !> Walk every file in dist\ and refuse any script tag, in any case.
  function porch_is_free_of_scripts() result(clean)
    logical :: clean
    type(run_result) :: rr
    character(:), allocatable :: doc
    integer :: i
    clean = .true.
    rr = run_cmd('dir /b /s /a:-d dist')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      clean = .false.
      return
    end if
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) == 0) cycle
      doc = to_lower(slurp(rr%out(i)%s))
      if (count_substr(doc, '<script') > 0) clean = .false.
    end do
  end function porch_is_free_of_scripts

  !> A whole file as one string, newline-joined.
  function slurp(path) result(doc)
    character(*), intent(in) :: path
    character(:), allocatable :: doc
    type(string_t), allocatable :: lines(:)
    integer :: i
    call read_all_lines(path, lines)
    doc = ''
    do i = 1, size(lines)
      doc = doc // lines(i)%s // achar(10)
    end do
  end function slurp

end module cathedral_validate
