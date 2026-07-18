!> cathedral_hall: the Hall of Deprecated Syntax. Six historical
!> features, each with its scroll, its purpose, its precise standards
!> classification, its modern reading, and — because this Cathedral
!> does not assert what it can measure — the compiler's own testimony,
!> taken at generation time by Forty's probes and re-taken by the
!> validator. Old syntax is not flattened into one word: the Hall
!> teaches the five distinct fates a feature can meet.
module cathedral_hall
  use forty_util, only: string_t, push_string, int_to_str
  use forty_run, only: run_result, run_cmd, read_all_lines
  use forty_paths, only: quote, temp_root, file_exists
  use cathedral_html, only: escape_html
  use cathedral_highlight, only: highlight_line
  implicit none
  private
  public :: hall_exhibit_t, hall_exhibits, hall_body, hall_cites, cite_t
  public :: probe_verdict_t, run_probe, normalize_diag, category_text
  public :: CAT_CLEAN, CAT_WARN, CAT_EXTENSION, CAT_REJECTED

  integer, parameter :: CAT_CLEAN = 1, CAT_WARN = 2
  integer, parameter :: CAT_EXTENSION = 3, CAT_REJECTED = 4

  character(*), parameter :: HALL_DIR = 'content\hall-of-deprecated-syntax'

  type :: probe_verdict_t
    integer :: category = 0
    character(:), allocatable :: evidence
    logical :: ok = .false.
  end type probe_verdict_t

  type :: hall_exhibit_t
    character(:), allocatable :: id
    character(:), allocatable :: plaque
    character(:), allocatable :: old_file
    character(:), allocatable :: modern_file
    character(:), allocatable :: purpose
    character(:), allocatable :: classification
    character(:), allocatable :: replacement
  end type hall_exhibit_t

  type :: cite_t
    character(:), allocatable :: label
    character(:), allocatable :: url
  end type cite_t

contains

  ! -------------------------------------------------- the compiler probe

  !> Two invocations judge a scroll: GFortran under -std=f2018, and
  !> GFortran in its merciful default. The categories are derived from
  !> the exits and diagnostics, never from the standards prose.
  subroutine run_probe(path, v)
    character(*), intent(in) :: path
    type(probe_verdict_t), intent(out) :: v
    type(run_result) :: r_def, r_std
    logical :: def_ok, std_ok, std_warn
    character(:), allocatable :: diag
    r_def = run_cmd('gfortran -fsyntax-only -J ' // quote(temp_root()) // &
                    ' ' // quote(path))
    r_std = run_cmd('gfortran -std=f2018 -fsyntax-only -J ' // &
                    quote(temp_root()) // ' ' // quote(path))
    v%ok = r_def%launched .and. r_std%launched
    v%evidence = ''
    if (.not. v%ok) return
    def_ok = (r_def%exit_code == 0)
    std_ok = (r_std%exit_code == 0)
    std_warn = has_mark(r_std, 'Warning:')
    if (std_ok .and. .not. std_warn) then
      v%category = CAT_CLEAN
    else if (std_ok) then
      v%category = CAT_WARN
      call first_mark(r_std, 'Warning:', diag)
      v%evidence = normalize_diag(diag)
    else if (def_ok) then
      v%category = CAT_EXTENSION
      call first_mark(r_std, 'Error:', diag)
      v%evidence = normalize_diag(diag)
    else
      v%category = CAT_REJECTED
      call first_mark(r_def, 'Error:', diag)
      v%evidence = normalize_diag(diag)
    end if
  end subroutine run_probe

  function has_mark(rr, mark) result(r)
    type(run_result), intent(in) :: rr
    character(*), intent(in) :: mark
    logical :: r
    integer :: i
    r = .false.
    do i = 1, size(rr%out)
      if (index(rr%out(i)%s, mark) > 0) r = .true.
    end do
  end function has_mark

  subroutine first_mark(rr, mark, diag)
    type(run_result), intent(in) :: rr
    character(*), intent(in) :: mark
    character(:), allocatable, intent(out) :: diag
    integer :: i
    diag = ''
    do i = 1, size(rr%out)
      if (index(rr%out(i)%s, mark) > 0) then
        diag = rr%out(i)%s
        return
      end if
    end do
  end subroutine first_mark

  !> Strip file paths and source coordinates from a diagnostic, leaving
  !> the stable judgment itself.
  function normalize_diag(line) result(r)
    character(*), intent(in) :: line
    character(:), allocatable :: r
    integer :: p, q
    r = trim(line)
    p = index(r, 'Warning:')
    if (p == 0) p = index(r, 'Error:')
    if (p > 0) r = r(p:)
    q = index(r, ' at (1)')
    if (q > 0) r = r(1:q - 1)
    r = trim(r)
  end function normalize_diag

  function category_text(cat) result(r)
    integer, intent(in) :: cat
    character(:), allocatable :: r
    select case (cat)
    case (CAT_CLEAN)
      r = 'ACCEPTED CLEANLY UNDER -std=f2018'
    case (CAT_WARN)
      r = 'ACCEPTED WITH WARNING UNDER -std=f2018'
    case (CAT_EXTENSION)
      r = 'REJECTED UNDER -std=f2018; ACCEPTED AS AN EXTENSION BY DEFAULT'
    case (CAT_REJECTED)
      r = 'REJECTED OUTRIGHT'
    case default
      r = 'UNMEASURED'
    end select
  end function category_text

  ! ------------------------------------------------------- the registry

  function hall_exhibits() result(xs)
    type(hall_exhibit_t), allocatable :: xs(:)
    allocate (xs(6))

    xs(1)%id = 'arithmetic-if'
    xs(1)%plaque = 'The Arithmetic IF'
    xs(1)%old_file = HALL_DIR // '\arithmetic-if.f'
    xs(1)%modern_file = HALL_DIR // '\arithmetic-if-modern.f90'
    xs(1)%purpose = 'One test, three destinations: negative, zero, or ' // &
      'positive, each named by a statement label. On the IBM 704, whose ' // &
      'hardware compared against zero in a single breath, this was the ' // &
      'natural shape of a branch.'
    xs(1)%classification = 'Deleted &#8212; declared obsolescent in ' // &
      'Fortran 90 and deleted outright in Fortran 2018' // cite(1) // '.'
    xs(1)%replacement = 'The block IF: <code>if &#8230; else if &#8230; ' // &
      'else</code>, which says the three cases in words.'

    xs(2)%id = 'hollerith'
    xs(2)%plaque = 'Hollerith Constants'
    xs(2)%old_file = HALL_DIR // '\hollerith.f'
    xs(2)%modern_file = HALL_DIR // '\hollerith-modern.f90'
    xs(2)%purpose = 'Characters counted into numeric storage &#8212; ' // &
      '<code>4HHELO</code> is the four characters HELO packed into an ' // &
      'integer &#8212; from the age before Fortran had a character type ' // &
      'at all.'
    xs(2)%classification = 'Deleted &#8212; Fortran 66 practice removed ' // &
      'from the FORTRAN 77 standard (kept only in an appendix); the ' // &
      'related H edit descriptor lingered until its deletion in Fortran ' // &
      '95' // cite(2) // '. Lives on as a documented compiler ' // &
      'extension' // cite(4) // '.'
    xs(2)%replacement = 'The character type, standard since FORTRAN 77: ' // &
      'characters live in character variables.'

    xs(3)%id = 'computed-goto'
    xs(3)%plaque = 'The Computed GO TO'
    xs(3)%old_file = HALL_DIR // '\computed-goto.f'
    xs(3)%modern_file = HALL_DIR // '\computed-goto-modern.f90'
    xs(3)%purpose = 'Branch by position: an integer selects the k-th ' // &
      'label from a list. A jump table written by hand, and the ' // &
      'ancestor of every dispatch switch.'
    xs(3)%classification = 'Obsolescent &#8212; since Fortran 95, and ' // &
      'still in the standard today' // cite(2) // '.'
    xs(3)%replacement = 'The <code>select case</code> construct, which ' // &
      'names its branches and needs no labels' // cite(2) // '.'

    xs(4)%id = 'statement-function'
    xs(4)%plaque = 'The Statement Function'
    xs(4)%old_file = HALL_DIR // '\statement-function.f'
    xs(4)%modern_file = HALL_DIR // '\statement-function-modern.f90'
    xs(4)%purpose = 'A one-line function defined among the declarations ' // &
      '&#8212; compact, convenient, and subject to famously nonintuitive ' // &
      'restrictions.'
    xs(4)%classification = 'Obsolescent &#8212; since Fortran 95; ' // &
      'wholly superseded' // cite(2) // '.'
    xs(4)%replacement = 'The internal function after <code>contains</code>: ' // &
      'checked, typed, and free of the old restrictions' // cite(2) // cite(3) // '.'

    xs(5)%id = 'plain-goto'
    xs(5)%plaque = 'The Plain GO TO'
    xs(5)%old_file = HALL_DIR // '\plain-goto.f'
    xs(5)%modern_file = HALL_DIR // '\plain-goto-modern.f90'
    xs(5)%purpose = 'The unconditional jump: control moves to the named ' // &
      'label, no questions asked. Every loop in this Hall''s era was ' // &
      'built from it.'
    xs(5)%classification = 'Currently standard &#8212; the unconditional ' // &
      'GO TO has never been declared obsolescent or deleted; no standards ' // &
      'source lists it among the retired' // cite(6) // '. Style retired ' // &
      'it; the standard did not. The Hall includes it precisely so ' // &
      '&#8220;old&#8221; is not mistaken for &#8220;deprecated&#8221;.'
    xs(5)%replacement = 'Structured constructs &#8212; <code>do</code>, ' // &
      '<code>exit</code>, <code>cycle</code>, <code>select case</code> ' // &
      '&#8212; wherever they say the intent better.'

    xs(6)%id = 'real-star-8'
    xs(6)%plaque = 'REAL*8'
    xs(6)%old_file = HALL_DIR // '\real-star-8.f'
    xs(6)%modern_file = HALL_DIR // '\real-star-8-modern.f90'
    xs(6)%purpose = 'Precision by byte count, in the vendor dialects of ' // &
      'the seventies &#8212; so widespread that generations mistook it ' // &
      'for scripture.'
    xs(6)%classification = 'Never standard &#8212; a vendor custom in no ' // &
      'edition of the standard, carried today as a documented compiler ' // &
      'extension' // cite(5) // '.'
    xs(6)%replacement = 'Kind parameters: <code>selected_real_kind</code> ' // &
      '(Fortran 90) or the named constants of <code>iso_fortran_env</code> ' // &
      '(Fortran 2008).'
  end function hall_exhibits

  function hall_cites() result(cs)
    type(cite_t), allocatable :: cs(:)
    allocate (cs(6))
    cs(1)%label = 'NAG Fortran 2018 overview'
    cs(1)%url = 'https://support.nag.com/nagware/np/r72_doc/nag_f2018.html'
    cs(2)%label = 'Backward and forward compatibility, Fortran 77 to 90 (NSC)'
    cs(2)%url = 'https://www.nsc.liu.se/~boein/f77to90/a4.html'
    cs(3)%label = 'Fortran Wiki: Modernizing Old Fortran'
    cs(3)%url = 'https://fortranwiki.org/fortran/show/Modernizing+Old+Fortran'
    cs(4)%label = 'GNU Fortran manual: Hollerith constants support'
    cs(4)%url = 'https://gcc.gnu.org/onlinedocs/gfortran/Hollerith-constants-support.html'
    cs(5)%label = 'GNU Fortran manual: Extensions'
    cs(5)%url = 'https://gcc.gnu.org/onlinedocs/gfortran/Extensions.html'
    cs(6)%label = 'WG5: earlier Fortran standards'
    cs(6)%url = 'https://wg5-fortran.org/fearlier.html'
  end function hall_cites

  ! ---------------------------------------------------------- the page

  subroutine hall_body(body, ok)
    type(string_t), allocatable, intent(inout) :: body(:)
    logical, intent(out) :: ok
    type(hall_exhibit_t), allocatable :: xs(:)
    type(cite_t), allocatable :: cs(:)
    type(probe_verdict_t) :: v_old, v_new
    integer :: i

    ok = .false.
    xs = hall_exhibits()

    call para(body, 'Languages retire their past in different ways, and ' // &
      'flattening them all into &#8220;deprecated&#8221; teaches nothing. ' // &
      'This hall keeps six exhibits, each labeled with its precise fate. ' // &
      'Five fates are possible:')
    call push_string(body, '<dl class="glossary" id="hall-legend">')
    call gloss(body, 'Currently standard', 'In the present standard, ' // &
      'unretired &#8212; however unfashionable.')
    call gloss(body, 'Obsolescent', 'Still standard, formally discouraged; ' // &
      'better means exist and removal may come.')
    call gloss(body, 'Deleted', 'Removed from a named edition of the ' // &
      'standard. Compilers may still accept it for the old scriptures.')
    call gloss(body, 'Never standard, historically common', 'Vendor custom ' // &
      'so widespread it was mistaken for the standard.')
    call gloss(body, 'Compiler extension', 'Accepted by a compiler beyond ' // &
      'the standard, documented as such.')
    call push_string(body, '</dl>')
    call para(body, 'Every classification below is cited, and every scroll ' // &
      'carries the compiler&#39;s own testimony &#8212; measured by this ' // &
      'site&#39;s generator invoking GFortran at build time, under ' // &
      '<code>-std=f2018</code> and in its merciful default, then measured ' // &
      'again by the validator. The Cathedral does not assert what it can ' // &
      'probe.')

    do i = 1, size(xs)
      if (.not. file_exists(xs(i)%old_file)) return
      if (.not. file_exists(xs(i)%modern_file)) return
      call run_probe(xs(i)%old_file, v_old)
      call run_probe(xs(i)%modern_file, v_new)
      if (.not. (v_old%ok .and. v_new%ok)) return

      call push_string(body, '<section class="verse">')
      call push_string(body, '<h2 id="hall-' // xs(i)%id // '">' // &
                       xs(i)%plaque // '</h2>')
      if (.not. emit_scroll(body, 'The historical scroll', &
                            xs(i)%old_file, .true.)) return
      call para(body, xs(i)%purpose)
      call push_string(body, '<dl class="glossary">')
      call gloss(body, 'Classification', xs(i)%classification)
      call gloss(body, 'The modern preference', xs(i)%replacement)
      call gloss(body, 'The compiler''s testimony', &
                 category_text(v_old%category) // evidence_html(v_old))
      call push_string(body, '</dl>')
      if (.not. emit_scroll(body, 'The modern reading', &
                            xs(i)%modern_file, .false.)) return
      call para(body, '<em>The modern reading&#39;s testimony:</em> ' // &
                category_text(v_new%category) // evidence_html(v_new) // '.')
      call push_string(body, '</section>')
    end do

    call push_string(body, '<h2 id="hall-sources">Sources</h2>')
    call push_string(body, '<ol class="sources">')
    cs = hall_cites()
    do i = 1, size(cs)
      call push_string(body, '  <li id="src-' // int_to_str(i) // '"><a href="' // &
                       cs(i)%url // '">' // cs(i)%label // '</a></li>')
    end do
    call push_string(body, '</ol>')
    call para(body, 'Continue through the Cathedral: ' // &
      '<a href="testaments.html">Old Testament / Modern Testament</a> for ' // &
      'the two source forms side by side, or ' // &
      '<a href="why-it-still-stands.html">Why It Still Stands</a> for the ' // &
      'wider case.')
    ok = .true.
  end subroutine hall_body

  function evidence_html(v) result(r)
    type(probe_verdict_t), intent(in) :: v
    character(:), allocatable :: r
    r = ''
    if (len(v%evidence) > 0) then
      r = ' &#8212; <code>' // escape_html(v%evidence) // '</code>'
    end if
  end function evidence_html

  function emit_scroll(body, caption, path, fixed_form) result(ok)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: caption, path
    logical, intent(in) :: fixed_form
    logical :: ok
    type(string_t), allocatable :: lines(:)
    integer :: i
    ok = .false.
    call read_all_lines(path, lines)
    if (size(lines) == 0) return
    call push_string(body, '<figure>')
    call push_string(body, '  <figcaption>' // caption // '</figcaption>')
    call push_string(body, '<pre><code>' // highlight_line(lines(1)%s, fixed_form))
    do i = 2, size(lines)
      call push_string(body, highlight_line(lines(i)%s, fixed_form))
    end do
    call push_string(body, '</code></pre>')
    call push_string(body, '</figure>')
    ok = .true.
  end function emit_scroll

  function cite(n) result(r)
    integer, intent(in) :: n
    character(:), allocatable :: r
    r = '<sup class="cite"><a href="#src-' // int_to_str(n) // '">[' // &
        int_to_str(n) // ']</a></sup>'
  end function cite

  subroutine gloss(body, term, def)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: term, def
    call push_string(body, '  <dt>' // term // '</dt>')
    call push_string(body, '  <dd>' // def // '</dd>')
  end subroutine gloss

  subroutine para(lines, html)
    type(string_t), allocatable, intent(inout) :: lines(:)
    character(*), intent(in) :: html
    call push_string(lines, '<p>' // html // '</p>')
  end subroutine para

end module cathedral_hall
