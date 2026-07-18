!> cathedral_validate: the survey of the raised fabric. Confirms every
!> route's page exists and is sound, every asset is present, the sitemap
!> and manifest agree with the registry, and — enforced, not asserted —
!> that no script tag exists anywhere in the porch.
module cathedral_validate
  use forty_util, only: string_t, to_lower, starts_with, count_substr, int_to_str
  use forty_ui, only: say, lament, rule
  use forty_paths, only: file_exists
  use forty_run, only: run_result, run_cmd, read_all_lines
  use forty_canon, only: EXIT_OK, EXIT_FAIL, CANON_BASE_URL
  use forty_confess, only: transgression_t, ledger_transgressions
  use cathedral_routes, only: route_t, routes
  implicit none
  private
  public :: run_validate

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
    logical :: wellformed
    character(:), allocatable :: page
    integer :: i
    call read_all_lines('HERESY_LEDGER.md', ledger)
    call ledger_transgressions(ledger, trans, wellformed)
    call check(wellformed, 'THE OPERATIONAL CHAPTER IS PRESENT AND WELL-FORMED')
    page = slurp('dist\confessional.html')
    do i = 1, size(trans)
      if (len(trans(i)%commit) >= 7) then
        call check(count_substr(page, trans(i)%commit) >= 1, &
                   'THE CONFESSIONAL DISPLAYS TRANSGRESSION ' // int_to_str(i) // &
                   ' (' // trans(i)%commit(1:7) // ')')
      end if
    end do
  end subroutine check_operational_record

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
