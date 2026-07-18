!> cathedral_testaments: the wing where two liturgies of the same
!> language face one another. Exhibit code lives as real Fortran source
!> under content\testaments\ — declarative input to the generator, and
!> compiled by the validator so no verse can quietly rot. The registry
!> here, like the route registry, is data owned by Fortran.
!>
!> Accuracy discipline: every dated claim in the commentary below is
!> verified and cited in content/testaments/SOURCES.md.
module cathedral_testaments
  use forty_util, only: string_t, push_string
  use forty_run, only: read_all_lines
  use cathedral_highlight, only: highlight_line
  implicit none
  private
  public :: verse_t, verses, testaments_body, TESTAMENTS_DIR, standards_timeline

  character(*), parameter :: TESTAMENTS_DIR = 'content\testaments'

  type :: verse_t
    character(:), allocatable :: id
    character(:), allocatable :: title
    character(:), allocatable :: old_file
    character(:), allocatable :: new_file
    character(:), allocatable :: commentary
  end type verse_t

contains

  function verses() result(vs)
    type(verse_t), allocatable :: vs(:)
    allocate (vs(5))

    vs(1)%id = 'shape'
    vs(1)%title = 'The shape of the source'
    vs(1)%old_file = TESTAMENTS_DIR // '\verse-1-old.f'
    vs(1)%new_file = TESTAMENTS_DIR // '\verse-1-new.f90'
    vs(1)%commentary = 'Fixed form is a punched card wearing a standard: ' // &
      'comment marks in column 1, statement labels in columns 1&#8211;5, a ' // &
      'continuation mark in column 6, statements confined to columns 7 ' // &
      'through 72, and silence beyond. Free source form, introduced with ' // &
      'Fortran 90, released the line from the card; code begins where it ' // &
      'is needed, <code>!</code> opens a comment, and <code>&amp;</code> ' // &
      'continues.'

    vs(2)%id = 'loops'
    vs(2)%title = 'Loops and labels'
    vs(2)%old_file = TESTAMENTS_DIR // '\verse-2-old.f'
    vs(2)%new_file = TESTAMENTS_DIR // '\verse-2-new.f90'
    vs(2)%commentary = 'The old loop marched to a numbered statement. The ' // &
      'block <code>do &#8230; end do</code> of Fortran 90, with ' // &
      '<code>exit</code> and <code>cycle</code>, replaced the numbers with ' // &
      'structure. The labeled DO is obsolescent under Fortran 2018; its ' // &
      'shared-termination cousins &#8212; and the arithmetic IF &#8212; ' // &
      'were deleted from that standard outright, though compilers still ' // &
      'accept them for the sake of the old scriptures.'

    vs(3)%id = 'arrays'
    vs(3)%title = 'Arrays, element by element or whole'
    vs(3)%old_file = TESTAMENTS_DIR // '\verse-3-old.f'
    vs(3)%new_file = TESTAMENTS_DIR // '\verse-3-new.f90'
    vs(3)%commentary = 'Arrays are the reason Fortran survives, and Fortran ' // &
      '90 let them speak for themselves: whole-array arithmetic, array ' // &
      'constructors, and allocatable arrays sized at run time. What the ' // &
      'Old Testament wrote as three loops, the Modern Testament writes as ' // &
      'three sentences of algebra &#8212; which is also what the compiler ' // &
      'optimizers prefer to read.'

    vs(4)%id = 'sharing'
    vs(4)%title = 'Sharing data'
    vs(4)%old_file = TESTAMENTS_DIR // '\verse-4-old.f'
    vs(4)%new_file = TESTAMENTS_DIR // '\verse-4-new.f90'
    vs(4)%commentary = 'The COMMON block shared raw storage by position: ' // &
      'every program unit redeclared it, and no one checked their ' // &
      'agreement. The module, introduced with Fortran 90, shares by name ' // &
      'through an explicit, compiler-checked interface. COMMON, ' // &
      'EQUIVALENCE, and BLOCK DATA were declared obsolescent in Fortran ' // &
      '2018; the module is why nobody mourns.'

    vs(5)%id = 'precise'
    vs(5)%title = 'Precision, by decree or by name'
    vs(5)%old_file = TESTAMENTS_DIR // '\verse-5-old.f'
    vs(5)%new_file = TESTAMENTS_DIR // '\verse-5-new.f90'
    vs(5)%commentary = 'DOUBLE PRECISION has been standard since the early ' // &
      'books, and the widely seen <code>REAL*8</code> never was &#8212; a ' // &
      'vendor dialect mistaken for scripture. Fortran 90 brought ' // &
      '<code>selected_real_kind</code>; Fortran 2008 added the named kind ' // &
      'constants <code>real32</code>, <code>real64</code>, and ' // &
      '<code>real128</code> in <code>iso_fortran_env</code>, so precision ' // &
      'is requested by meaning rather than by folklore.'
  end function verses

  !> The page body: introduction, five paired exhibits, the timeline,
  !> and the honest note on obsolescence. ok goes false if any exhibit
  !> file cannot be read; the generator halts honestly.
  subroutine testaments_body(body, ok)
    type(string_t), allocatable, intent(inout) :: body(:)
    logical, intent(out) :: ok
    type(verse_t), allocatable :: vs(:)
    integer :: v

    ok = .false.
    vs = verses()

    call para(body, 'The stereotype of Fortran is a fixed-form deck from ' // &
      '1977: capital letters, numbered statements, columns ruled like a ' // &
      'ledger. The stereotype is real &#8212; and it is also a museum ' // &
      'piece. Fortran has been revised by international standard nine ' // &
      'times, and the modern language is free-form, modular, and checked. ' // &
      'This wing reads the two side by side.')
    call para(body, 'Every exhibit below is real code from this ' // &
      'repository, read from disk by the generator, dressed by a Fortran ' // &
      'syntax highlighter, and syntax-checked by GFortran during ' // &
      '<code>forty validate</code>. Every verse compiles.')

    do v = 1, size(vs)
      call push_string(body, '<section class="verse">')
      call push_string(body, '<h2 id="verse-' // vs(v)%id // '">' // &
                       vs(v)%title // '</h2>')
      call push_string(body, '<div class="pair">')
      if (.not. emit_exhibit(body, 'Old Testament &#183; fixed form', &
                             vs(v)%old_file, .true.)) return
      if (.not. emit_exhibit(body, 'Modern Testament &#183; free form', &
                             vs(v)%new_file, .false.)) return
      call push_string(body, '</div>')
      call para(body, vs(v)%commentary)
      call push_string(body, '</section>')
    end do

    call push_string(body, '<h2 id="the-living-standard">The living standard</h2>')
    call para(body, 'Nine revisions, one unbroken line. Years give the ' // &
      'publication of each standard; the first entry is the language&#39;s ' // &
      'delivery by IBM.')
    call standards_timeline(body)
    call para(body, 'The standard retires its past with care: features are ' // &
      'first declared <em>obsolescent</em> (still standard, discouraged), ' // &
      'and only rarely <em>deleted</em>. Compilers, serving decades of ' // &
      'working scripture, generally accept even the deleted. That patience ' // &
      'is not weakness. It is why fifty-year-old physics still compiles.')
    call para(body, 'Sources for every dated claim on this page are ' // &
      'recorded in the repository at ' // &
      '<a href="https://github.com/teamofoxen/fortran-cathedral/blob/main/' // &
      'content/testaments/SOURCES.md">content/testaments/SOURCES.md</a>.')

    ok = .true.
  end subroutine testaments_body

  function emit_exhibit(body, caption, path, fixed_form) result(ok)
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
  end function emit_exhibit

  !> The nine-revision line, shared by every wing that charts it.
  subroutine standards_timeline(body)
    type(string_t), allocatable, intent(inout) :: body(:)
    call push_string(body, '<ol class="timeline">')
    call timeline_item(body, 'FORTRAN', '1957')
    call timeline_item(body, 'FORTRAN 66', '1966')
    call timeline_item(body, 'FORTRAN 77', '1978')
    call timeline_item(body, 'Fortran 90', '1991')
    call timeline_item(body, 'Fortran 95', '1997')
    call timeline_item(body, 'Fortran 2003', '2004')
    call timeline_item(body, 'Fortran 2008', '2010')
    call timeline_item(body, 'Fortran 2018', '2018')
    call timeline_item(body, 'Fortran 2023', '2023')
    call push_string(body, '</ol>')
  end subroutine standards_timeline

  subroutine timeline_item(body, name, year)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: name, year
    call push_string(body, '  <li>' // name // ' <span class="year">' // &
                     year // '</span></li>')
  end subroutine timeline_item

  subroutine para(lines, html)
    type(string_t), allocatable, intent(inout) :: lines(:)
    character(*), intent(in) :: html
    call push_string(lines, '<p>' // html // '</p>')
  end subroutine para

end module cathedral_testaments
