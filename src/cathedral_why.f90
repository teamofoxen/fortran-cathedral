!> cathedral_why: the wing that answers the visitor's first honest
!> question. Every factual claim is cited; every citation resolves to a
!> numbered source rendered at the foot of the page; and every source
!> is cross-checked by the validator against the declarative record at
!> content/why-it-still-stands/SOURCES.md. Claims that could not be
!> authoritatively supported were not made.
module cathedral_why
  use forty_util, only: string_t, push_string, int_to_str
  use cathedral_testaments, only: standards_timeline
  implicit none
  private
  public :: cite_t, cites, why_body

  type :: cite_t
    character(:), allocatable :: label
    character(:), allocatable :: url
  end type cite_t

contains

  !> The numbered sources, in citation order. The URLs here must appear
  !> verbatim in content/why-it-still-stands/SOURCES.md; the validator
  !> enforces that agreement.
  function cites() result(cs)
    type(cite_t), allocatable :: cs(:)
    allocate (cs(14))
    cs(1)%label = 'WG5: earlier Fortran standards'
    cs(1)%url = 'https://wg5-fortran.org/fearlier.html'
    cs(2)%label = 'fortran-lang compiler index'
    cs(2)%url = 'https://fortran-lang.org/compilers/'
    cs(3)%label = 'GNU Fortran (GCC), official page'
    cs(3)%url = 'https://gcc.gnu.org/fortran/'
    cs(4)%label = 'Intel Fortran Compiler release notes (2025)'
    cs(4)%url = 'https://www.intel.com/content/www/us/en/developer/articles/release-notes/fortran-compiler/2025.html'
    cs(5)%label = 'LLVM Flang documentation'
    cs(5)%url = 'https://flang.llvm.org/docs/'
    cs(6)%label = 'Netlib LAPACK'
    cs(6)%url = 'https://www.netlib.org/lapack/'
    cs(7)%label = 'SciPy: low-level LAPACK functions'
    cs(7)%url = 'https://docs.scipy.org/doc/scipy/reference/linalg.lapack.html'
    cs(8)%label = 'GNU Fortran manual: ISO_C_BINDING'
    cs(8)%url = 'https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html'
    cs(9)%label = 'ECMWF: an open-source IFS'
    cs(9)%url = 'https://www.ecmwf.int/en/newsletter/171/news/open-source-integrated-forecasting-system'
    cs(10)%label = 'ECMWF OpenIFS project'
    cs(10)%url = 'https://www.ecmwf.int/en/research/projects/openifs'
    cs(11)%label = 'NCAR CESM tutorial: Fortran'
    cs(11)%url = 'https://ncar.github.io/CESM-Tutorial/notebooks/resources/fortran.html'
    cs(12)%label = 'WRF model repository'
    cs(12)%url = 'https://github.com/wrf-model/WRF'
    cs(13)%label = 'NASA Software Catalog: FUN3D'
    cs(13)%url = 'https://software.nasa.gov/software/LAR-20188-1'
    cs(14)%label = 'NASA: Fortran 90 conversion course'
    cs(14)%url = 'https://www.nccs.nasa.gov/sites/default/docs/tutorials/f90studentnotes.pdf'
  end function cites

  subroutine why_body(body)
    type(string_t), allocatable, intent(inout) :: body(:)
    type(cite_t), allocatable :: cs(:)
    integer :: i

    call para(body, 'Fortran is not standing because nobody got around to ' // &
      'demolishing it. It is standing because it is load-bearing. The ' // &
      'language holds up a measurable share of the numerical software that ' // &
      'forecasts weather, models climate, simulates aircraft, and runs on ' // &
      'the world&#39;s largest computers &#8212; and it holds that weight ' // &
      'for reasons that are concrete, unromantic, and worth understanding.')
    call para(body, 'This page states those reasons plainly, with sources. ' // &
      'Where an authoritative source could not be found, the claim was not ' // &
      'made.')

    call h2(body, 'why-myths', 'Myth and reality')
    call push_string(body, '<table>')
    call push_string(body, '  <caption>Four common myths, answered</caption>')
    call push_string(body, '  <thead>')
    call push_string(body, '    <tr><th scope="col">Myth</th><th scope="col">Reality</th></tr>')
    call push_string(body, '  </thead>')
    call push_string(body, '  <tbody>')
    call myth(body, 'Fortran is dead.', 'The ISO standard has been revised ' // &
      'continuously through Fortran 2023' // cite(1) // ', and at least ' // &
      'three major compilers are under active development' // cite(2) // '.')
    call myth(body, 'Fortran means FORTRAN 77.', 'Modern Fortran is ' // &
      'free-form, modular, and checked. See the exhibits in ' // &
      '<a href="testaments.html">Old Testament / Modern Testament</a>, ' // &
      'where both liturgies compile.')
    call myth(body, 'It survives only as untouched legacy.', 'Operational ' // &
      'systems written primarily in Fortran &#8212; among them ECMWF&#39;s ' // &
      'IFS' // cite(9) // ' and NCAR&#39;s CESM' // cite(11) // ' &#8212; ' // &
      'are actively developed today.')
    call myth(body, 'Python replaced it.', 'Much of scientific Python ' // &
      'stands on compiled numerical kernels; SciPy exposes low-level ' // &
      'interfaces to the LAPACK library directly' // cite(7) // '. The ' // &
      'snake often rides the ox.')
    call push_string(body, '  </tbody>')
    call push_string(body, '</table>')

    call h2(body, 'why-arrays', 'Pillar I: arrays as first-class citizens')
    call para(body, 'Fortran&#39;s subject matter is the array. ' // &
      'Multidimensional arrays are part of the language&#39;s core grammar, ' // &
      'and since Fortran 90 they converse directly: whole-array ' // &
      'arithmetic, array constructors, and allocatable arrays sized at run ' // &
      'time' // cite(14) // '. Code written this way says what the ' // &
      'mathematics means, and leaves the compiler an unusually clear view ' // &
      'of what may be optimized.')

    call h2(body, 'why-compilers', 'Pillar II: mature optimizing compilers')
    call para(body, 'A language is only as alive as its compilers. Fortran ' // &
      'has several under active development: GFortran within GCC' // &
      cite(3) // ', Intel&#39;s LLVM-based ifx with Fortran 2018 support ' // &
      'and selected 2023 features' // cite(4) // ', and LLVM Flang' // &
      cite(5) // ', with a wider field catalogued by the community' // &
      cite(2) // '. Decades of investment in these optimizers is a ' // &
      'compounding asset that a rewrite forfeits on day one.')

    call h2(body, 'why-validated', 'Pillar III: validated code is capital')
    call para(body, 'Scientific code is not merely written; it is ' // &
      'validated &#8212; against experiments, observations, and decades of ' // &
      'operational use. The standard retires features with unusual care, ' // &
      'as the <a href="testaments.html">Testaments</a> wing shows, and ' // &
      'compilers keep faith with old scripture. The result: numerical ' // &
      'code validated over generations still compiles, and institutions ' // &
      'do not casually discard capital of that kind.')

    call h2(body, 'why-libraries', 'Pillar IV: the foundations are Fortran')
    call para(body, 'The reference implementation of LAPACK &#8212; the ' // &
      'linear-algebra package beneath a great deal of numerical software ' // &
      '&#8212; is written in Fortran 90 and published at Netlib' // &
      cite(6) // '. Higher ecosystems reach down to it: SciPy documents ' // &
      'its own low-level LAPACK interfaces' // cite(7) // '. And the ' // &
      'traffic flows both ways &#8212; standard C interoperability ' // &
      '(<code>iso_c_binding</code>) makes Fortran kernels callable from ' // &
      'the wider world' // cite(8) // '. This wing&#39;s successor, the ' // &
      'Book of BLAS, will treat these foundations at length.')

    call h2(body, 'why-institutions', 'Pillar V: institutional continuity')
    call para(body, 'The institutions that predict and simulate the ' // &
      'physical world say plainly what their systems are built from. ' // &
      'ECMWF describes the IFS suite as written primarily in modern ' // &
      'Fortran' // cite(9) // ' and offers OpenIFS to research ' // &
      'institutions' // cite(10) // '. NCAR&#39;s CESM climate model is ' // &
      'written mostly in Fortran' // cite(11) // ', and the WRF ' // &
      'atmospheric model&#39;s codebase is predominantly Fortran' // &
      cite(12) // '. NASA catalogues FUN3D, a Navier&#8211;Stokes solver ' // &
      'used across aeronautics, as written in Fortran' // cite(13) // '. ' // &
      'Weather, climate, aerospace, and the machines they run on: this is ' // &
      'the continuity that keeps the language funded, taught, and compiled.')

    call h2(body, 'why-timeline', 'The unbroken line')
    call para(body, 'Nine revisions of one standard, each carrying the old ' // &
      'code forward' // cite(1) // ':')
    call standards_timeline(body)

    call h2(body, 'why-architecture', 'A note on this page')
    call para(body, 'In keeping with the Cathedral&#39;s premise, this ' // &
      'page &#8212; its markup, navigation, citations, and the numbered ' // &
      'sources below &#8212; was assembled and emitted by Fortran. The ' // &
      'claims above are also recorded declaratively in the repository, ' // &
      'and the site&#39;s validator refuses any external link on this ' // &
      'page that does not appear in that record.')

    call h2(body, 'why-sources', 'Sources')
    call push_string(body, '<ol class="sources">')
    cs = cites()
    do i = 1, size(cs)
      call push_string(body, '  <li id="src-' // int_to_str(i) // '"><a href="' // &
                       cs(i)%url // '">' // cs(i)%label // '</a></li>')
    end do
    call push_string(body, '</ol>')
  end subroutine why_body

  ! ------------------------------------------------------------- helpers

  function cite(n) result(r)
    integer, intent(in) :: n
    character(:), allocatable :: r
    r = '<sup class="cite"><a href="#src-' // int_to_str(n) // '">[' // &
        int_to_str(n) // ']</a></sup>'
  end function cite

  subroutine myth(body, m, reality)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: m, reality
    call push_string(body, '    <tr><th scope="row">' // m // '</th><td>' // &
                     reality // '</td></tr>')
  end subroutine myth

  subroutine h2(lines, anchor, text)
    type(string_t), allocatable, intent(inout) :: lines(:)
    character(*), intent(in) :: anchor, text
    call push_string(lines, '<h2 id="' // anchor // '">' // text // '</h2>')
  end subroutine h2

  subroutine para(lines, html)
    type(string_t), allocatable, intent(inout) :: lines(:)
    character(*), intent(in) :: html
    call push_string(lines, '<p>' // html // '</p>')
  end subroutine para

end module cathedral_why
