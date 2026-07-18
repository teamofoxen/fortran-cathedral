!> cathedral_blas: the Book of BLAS. The wing's arithmetic is performed
!> here, by Fortran, at generation time — transparent reference
!> implementations of the operations the BLAS standardizes, with
!> dimensional validation. The same procedures feed the page, the
!> validator's cross-checks, and the trials, so a displayed number can
!> never drift from the arithmetic that produced it.
!>
!> Honesty discipline: the page states plainly that these results come
!> from the generator's own reference arithmetic, not from a linked
!> optimized BLAS library. Accuracy discipline: every historical and
!> ecosystem claim is cited in content/book-of-blas/SOURCES.md.
module cathedral_blas
  use, intrinsic :: iso_fortran_env, only: real64
  use forty_util, only: string_t, push_string, int_to_str
  implicit none
  private
  public :: axpy_ref, gemv_ref, gemm_ref, fmt_num
  public :: exhibit_axpy, exhibit_gemv, exhibit_gemm
  public :: blas_body, cite_t, blas_cites

  type :: cite_t
    character(:), allocatable :: label
    character(:), allocatable :: url
  end type cite_t

contains

  ! ------------------------------------------------- reference arithmetic

  !> y := alpha*x + y, the Level 1 archetype. Sizes must agree.
  subroutine axpy_ref(alpha, x, y, r, ok)
    real(real64), intent(in) :: alpha, x(:), y(:)
    real(real64), allocatable, intent(out) :: r(:)
    logical, intent(out) :: ok
    ok = (size(x) == size(y))
    if (.not. ok) then
      allocate (r(0))
      return
    end if
    r = alpha * x + y
  end subroutine axpy_ref

  !> y := A*x, the Level 2 archetype. cols(A) must equal size(x).
  subroutine gemv_ref(a, x, y, ok)
    real(real64), intent(in) :: a(:, :), x(:)
    real(real64), allocatable, intent(out) :: y(:)
    logical, intent(out) :: ok
    integer :: i
    ok = (size(a, 2) == size(x))
    if (.not. ok) then
      allocate (y(0))
      return
    end if
    allocate (y(size(a, 1)))
    do i = 1, size(a, 1)
      y(i) = dot_product(a(i, :), x)
    end do
  end subroutine gemv_ref

  !> C := A*B, the Level 3 archetype. cols(A) must equal rows(B).
  subroutine gemm_ref(a, b, c, ok)
    real(real64), intent(in) :: a(:, :), b(:, :)
    real(real64), allocatable, intent(out) :: c(:, :)
    logical, intent(out) :: ok
    ok = (size(a, 2) == size(b, 1))
    if (.not. ok) then
      allocate (c(0, 0))
      return
    end if
    c = matmul(a, b)
  end subroutine gemm_ref

  function fmt_num(v) result(r)
    real(real64), intent(in) :: v
    character(:), allocatable :: r
    character(32) :: buf
    write (buf, '(f0.1)') v
    r = trim(buf)
  end function fmt_num

  ! ----------------------------------------------- the canonical exhibits

  !> One source of truth for each exhibit's operands and results,
  !> shared by the page, the validator, and the trials.
  subroutine exhibit_axpy(alpha, x, y, r, ok)
    real(real64), intent(out) :: alpha
    real(real64), allocatable, intent(out) :: x(:), y(:), r(:)
    logical, intent(out) :: ok
    alpha = 2.0_real64
    x = real([1, 2, 3, 4], real64)
    y = real([10, 20, 30, 40], real64)
    call axpy_ref(alpha, x, y, r, ok)
  end subroutine exhibit_axpy

  subroutine exhibit_gemv(a, x, y, ok)
    real(real64), allocatable, intent(out) :: a(:, :), x(:), y(:)
    logical, intent(out) :: ok
    allocate (a(2, 3))
    a(1, :) = real([1, 2, 3], real64)
    a(2, :) = real([4, 5, 6], real64)
    x = real([7, 8, 9], real64)
    call gemv_ref(a, x, y, ok)
  end subroutine exhibit_gemv

  subroutine exhibit_gemm(a, b, c, ok)
    real(real64), allocatable, intent(out) :: a(:, :), b(:, :), c(:, :)
    logical, intent(out) :: ok
    allocate (a(2, 3), b(3, 2))
    a(1, :) = real([1, 2, 3], real64)
    a(2, :) = real([4, 5, 6], real64)
    b(1, :) = real([1, 2], real64)
    b(2, :) = real([3, 4], real64)
    b(3, :) = real([5, 6], real64)
    call gemm_ref(a, b, c, ok)
  end subroutine exhibit_gemm

  ! -------------------------------------------------------- the citations

  function blas_cites() result(cs)
    type(cite_t), allocatable :: cs(:)
    allocate (cs(10))
    cs(1)%label = 'Netlib BLAS'
    cs(1)%url = 'https://www.netlib.org/blas/'
    cs(2)%label = 'Netlib BLAS FAQ'
    cs(2)%url = 'https://www.netlib.org/blas/faq.html'
    cs(3)%label = 'LAPACK Working Note 81: Level 1, 2, and 3 BLAS'
    cs(3)%url = 'https://www.netlib.org/lapack/lawn81-3.0/node7.html'
    cs(4)%label = 'Netlib LAPACK'
    cs(4)%url = 'https://www.netlib.org/lapack/'
    cs(5)%label = 'LAPACK Users&#39; Guide: LAPACK and the BLAS'
    cs(5)%url = 'https://www.netlib.org/lapack/lug/node11.html'
    cs(6)%label = 'LAPACK Users&#39; Guide: The BLAS as the Key to Portability'
    cs(6)%url = 'https://www.netlib.org/lapack/lug/node65.html'
    cs(7)%label = 'NumPy manual: Linear algebra'
    cs(7)%url = 'https://numpy.org/doc/stable/reference/routines.linalg.html'
    cs(8)%label = 'SciPy manual: low-level LAPACK functions'
    cs(8)%url = 'https://docs.scipy.org/doc/scipy/reference/linalg.lapack.html'
    cs(9)%label = 'OpenBLAS documentation'
    cs(9)%url = 'https://www.openmathlib.org/OpenBLAS/docs/faq/'
    cs(10)%label = 'Intel oneMKL Developer Reference (Fortran)'
    cs(10)%url = 'https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-fortran/2024-0/overview.html'
  end function blas_cites

  ! ------------------------------------------------------------- the page

  subroutine blas_body(body, ok)
    type(string_t), allocatable, intent(inout) :: body(:)
    logical, intent(out) :: ok
    type(cite_t), allocatable :: cs(:)
    real(real64) :: alpha
    real(real64), allocatable :: x(:), y(:), r(:), a(:, :), b(:, :), c(:, :)
    real(real64), allocatable :: gx(:), gy(:)
    logical :: ok1, ok2, ok3
    integer :: i

    ok = .false.

    call para(body, 'BLAS &#8212; the Basic Linear Algebra Subprograms ' // &
      '&#8212; is not a single program. It is a published specification ' // &
      'of low-level vector and matrix operations, with a Fortran ' // &
      'reference implementation kept at Netlib' // cite(1) // '. Its ' // &
      'genius is the separation of interface from implementation: code ' // &
      'written against the standard routine names can run on the plain ' // &
      'reference Fortran, or on a machine-tuned library that implements ' // &
      'the same interface many times faster' // cite(2) // '. That ' // &
      'bargain &#8212; portable mathematics, replaceable speed &#8212; is ' // &
      'why so much numerical software chose to stand on it.')
    call para(body, 'The operations are organized in three levels, named ' // &
      'in order of their arrival and their appetite: vector operations ' // &
      '(Level 1, specified in 1979), matrix&#8211;vector operations ' // &
      '(Level 2, 1988), and matrix&#8211;matrix operations (Level 3, ' // &
      '1990)' // cite(2) // cite(3) // '. Each exhibit below is computed, ' // &
      'laid into its tables, and drawn into its map by this site&#39;s ' // &
      'own Fortran at generation time.')

    ! ---------------------------------------------------------- Level 1
    call h2(body, 'blas-level1', 'Level 1: AXPY, the vector verse')
    call para(body, 'Level 1 works on vectors: dot products, norms, and ' // &
      'the archetype AXPY &#8212; <em>a x plus y</em>, ' // &
      '<code>y &#8592; &#945;x + y</code>' // cite(3) // '. In the ' // &
      'naming convention, a type prefix chooses precision: SAXPY in ' // &
      'single, DAXPY in double' // cite(2) // '. Here is the operation, ' // &
      'with &#945; = ' // fmt_num(2.0_real64) // ':')
    call exhibit_axpy(alpha, x, y, r, ok1)
    if (.not. ok1) return
    call vector_table(body, 'Operand x (length ' // int_to_str(size(x)) // ')', x)
    call vector_table(body, 'Operand y (length ' // int_to_str(size(y)) // ')', y)
    call vector_table(body, 'Result &#945;x + y, computed by the generator', r)
    call shape_map(body, 'alpha times x plus y equals the result: three ' // &
                   'vectors of length 4', &
                   size(x), 1, '&#945;x', '+', size(y), 1, 'y', '=', size(r), 1, '&#945;x+y')
    call para(body, 'Level 1 does O(n) arithmetic on O(n) data: every ' // &
      'number is touched about once. Useful, ubiquitous &#8212; and ' // &
      'memory-bound almost by definition.')

    ! ---------------------------------------------------------- Level 2
    call h2(body, 'blas-level2', 'Level 2: GEMV, the matrix&#8211;vector verse')
    call para(body, 'Level 2 works between a matrix and a vector: the ' // &
      'archetype GEMV computes <code>y &#8592; A x</code>' // cite(3) // &
      '. The matrix below is deliberately not square &#8212; two rows, ' // &
      'three columns &#8212; because the interface is general:')
    call exhibit_gemv(a, gx, gy, ok2)
    if (.not. ok2) return
    call matrix_table(body, 'Operand A (' // dims(a) // ')', a)
    call vector_table(body, 'Operand x (length ' // int_to_str(size(gx)) // ')', gx)
    call vector_table(body, 'Result y = Ax, computed by the generator', gy)
    call shape_map(body, 'A two-by-three matrix times a length-three ' // &
                   'vector equals a length-two vector', &
                   size(a, 1), size(a, 2), 'A', '&#215;', size(gx), 1, 'x', &
                   '=', size(gy), 1, 'y')
    call para(body, 'Dimensional law: the columns of A must equal the ' // &
      'length of x. This site&#39;s generator refuses mismatched shapes, ' // &
      'and its trials prove the refusal. Level 2 performs O(n&#178;) ' // &
      'arithmetic on O(n&#178;) data &#8212; still roughly one visit per ' // &
      'number.')

    ! ---------------------------------------------------------- Level 3
    call h2(body, 'blas-level3', 'Level 3: GEMM, the matrix&#8211;matrix verse')
    call para(body, 'Level 3 works between matrices: the archetype GEMM ' // &
      'computes <code>C &#8592; A B</code>' // cite(3) // '. Again the ' // &
      'operands are rectangular; only the inner dimensions must agree:')
    call exhibit_gemm(a, b, c, ok3)
    if (.not. ok3) return
    call matrix_table(body, 'Operand A (' // dims(a) // ')', a)
    call matrix_table(body, 'Operand B (' // dims(b) // ')', b)
    call matrix_table(body, 'Result C = AB (' // dims(c) // '), computed by the generator', c)
    call shape_map(body, 'A two-by-three matrix times a three-by-two ' // &
                   'matrix equals a two-by-two matrix', &
                   size(a, 1), size(a, 2), 'A', '&#215;', size(b, 1), size(b, 2), 'B', &
                   '=', size(c, 1), size(c, 2), 'C')
    call para(body, 'Here the economics change. Multiplying an ' // &
      'n&#215;n pair performs O(n&#179;) arithmetic on only O(n&#178;) ' // &
      'data &#8212; many operations per number moved. That surplus is ' // &
      'what optimized implementations spend: blocks of the matrices can ' // &
      'be staged in fast cache and reused, so a tuned GEMM approaches ' // &
      'what the arithmetic units can actually sustain. This is why ' // &
      'blocked algorithms put matrix&#8211;matrix work in their inner ' // &
      'loops' // cite(6) // ', and why Level 3 is the level modern ' // &
      'hardware loves best.')

    call para(body, '<strong>What computed these numbers:</strong> the ' // &
      'tables above were calculated at generation time by this ' // &
      'site&#39;s own Fortran generator, using transparent reference ' // &
      'arithmetic &#8212; the same operations the BLAS standardizes ' // &
      '&#8212; not by a linked optimized BLAS library. The site&#39;s ' // &
      'validator recomputes every exhibit and refuses the page if a ' // &
      'displayed value disagrees.')

    ! ------------------------------------------------------ BLAS vs LAPACK
    call h2(body, 'blas-vs-lapack', 'BLAS and LAPACK')
    call para(body, 'BLAS supplies the bricks; LAPACK builds the ' // &
      'cathedral. LAPACK &#8212; the Linear Algebra PACKage, written in ' // &
      'Fortran' // cite(4) // ' &#8212; provides the higher operations ' // &
      'of scientific computing: solving linear systems, least squares, ' // &
      'eigenvalues, singular values. Its routines are written so that as ' // &
      'much computation as possible is performed by calls to the BLAS, ' // &
      'and it was designed at the outset to exploit Level 3' // cite(5) // &
      '. Install a faster BLAS beneath it, and LAPACK gets faster ' // &
      'without changing a line &#8212; the key to its portability' // &
      cite(6) // '.')

    ! ---------------------------------------------------------- the chain
    call h2(body, 'blas-chain', 'The call chain')
    call para(body, 'A user of a high-level environment may never see ' // &
      'these names, yet may stand on them. One common shape of the ' // &
      'descent:')
    call push_string(body, '<ol class="chain">')
    call push_string(body, '  <li>A researcher calls a high-level linear-algebra ' // &
      'routine &#8212; NumPy&#39;s and SciPy&#39;s, for instance, are ' // &
      'documented as relying on BLAS and LAPACK' // cite(7) // cite(8) // '.</li>')
    call push_string(body, '  <li>The environment dispatches to LAPACK drivers ' // &
      'for factorizations and solves' // cite(5) // '.</li>')
    call push_string(body, '  <li>LAPACK casts the heavy work as BLAS calls, ' // &
      'Level 3 wherever it can' // cite(5) // cite(6) // '.</li>')
    call push_string(body, '  <li>The BLAS interface is served by whatever ' // &
      'implementation is installed: the reference Fortran from Netlib' // &
      cite(1) // ', or an optimized library such as OpenBLAS' // cite(9) // &
      ' or Intel oneMKL' // cite(10) // '.</li>')
    call push_string(body, '</ol>')

    ! ----------------------------------------------------------- glossary
    call h2(body, 'blas-glossary', 'Glossary')
    call push_string(body, '<dl class="glossary">')
    call gloss(body, 'BLAS', 'Basic Linear Algebra Subprograms: a ' // &
      'standardized set of low-level vector and matrix routines' // cite(1) // '.')
    call gloss(body, 'Reference implementation', 'The plain, portable ' // &
      'Fortran realization of the BLAS kept at Netlib; correct ' // &
      'everywhere, tuned for nowhere' // cite(2) // '.')
    call gloss(body, 'Optimized implementation', 'A library implementing ' // &
      'the same interface with architecture-specific tuning, such as ' // &
      'OpenBLAS or oneMKL' // cite(9) // cite(10) // '.')
    call gloss(body, 'Level 1 / 2 / 3', 'The operation classes: ' // &
      'vector&#8211;vector, matrix&#8211;vector, and matrix&#8211;matrix' // &
      cite(3) // '.')
    call gloss(body, 'AXPY, GEMV, GEMM', 'The archetypal routine of each ' // &
      'level; a precision prefix (S, D, C, Z) completes the name' // cite(2) // '.')
    call gloss(body, 'LAPACK', 'The Linear Algebra PACKage: ' // &
      'factorizations, solvers, and eigenproblems built on the BLAS' // &
      cite(4) // cite(5) // '.')
    call gloss(body, 'Arithmetic intensity', 'Operations performed per ' // &
      'datum moved; the resource Level 3 has in surplus and optimized ' // &
      'libraries exploit' // cite(6) // '.')
    call gloss(body, 'Blocked algorithm', 'An algorithm reorganized to ' // &
      'work on submatrices so its inner loops become Level 3 calls' // &
      cite(6) // '.')
    call push_string(body, '</dl>')

    ! ------------------------------------------------------------ sources
    call h2(body, 'blas-sources', 'Sources')
    call push_string(body, '<ol class="sources">')
    cs = blas_cites()
    do i = 1, size(cs)
      call push_string(body, '  <li id="src-' // int_to_str(i) // '"><a href="' // &
                       cs(i)%url // '">' // cs(i)%label // '</a></li>')
    end do
    call push_string(body, '</ol>')

    call para(body, 'Continue through the Cathedral: ' // &
      '<a href="why-it-still-stands.html">Why It Still Stands</a> for the ' // &
      'wider case, or <a href="testaments.html">Old Testament / Modern ' // &
      'Testament</a> to read the language itself.')

    ok = .true.
  end subroutine blas_body

  ! --------------------------------------------------- rendering servants

  subroutine vector_table(body, caption, v)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: caption
    real(real64), intent(in) :: v(:)
    integer :: i
    character(:), allocatable :: hdr, row
    call push_string(body, '<table>')
    call push_string(body, '  <caption>' // caption // '</caption>')
    hdr = '    <tr><td></td>'
    row = '    <tr><th scope="row">value</th>'
    do i = 1, size(v)
      hdr = hdr // '<th scope="col">' // int_to_str(i) // '</th>'
      row = row // '<td class="num">' // fmt_num(v(i)) // '</td>'
    end do
    call push_string(body, '  <thead>')
    call push_string(body, hdr // '</tr>')
    call push_string(body, '  </thead>')
    call push_string(body, '  <tbody>')
    call push_string(body, row // '</tr>')
    call push_string(body, '  </tbody>')
    call push_string(body, '</table>')
  end subroutine vector_table

  subroutine matrix_table(body, caption, a)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: caption
    real(real64), intent(in) :: a(:, :)
    integer :: i, j
    character(:), allocatable :: line
    call push_string(body, '<table>')
    call push_string(body, '  <caption>' // caption // '</caption>')
    line = '    <tr><td></td>'
    do j = 1, size(a, 2)
      line = line // '<th scope="col">' // int_to_str(j) // '</th>'
    end do
    call push_string(body, '  <thead>')
    call push_string(body, line // '</tr>')
    call push_string(body, '  </thead>')
    call push_string(body, '  <tbody>')
    do i = 1, size(a, 1)
      line = '    <tr><th scope="row">' // int_to_str(i) // '</th>'
      do j = 1, size(a, 2)
        line = line // '<td class="num">' // fmt_num(a(i, j)) // '</td>'
      end do
      call push_string(body, line // '</tr>')
    end do
    call push_string(body, '  </tbody>')
    call push_string(body, '</table>')
  end subroutine matrix_table

  !> An operation map: three shapes and two operators, drawn to their
  !> true proportions from the exhibit's actual dimensions.
  subroutine shape_map(body, aria, r1, c1, l1, op1, r2, c2, l2, op2, r3, c3, l3)
    type(string_t), allocatable, intent(inout) :: body(:)
    character(*), intent(in) :: aria, l1, op1, l2, op2, l3
    integer, intent(in) :: r1, c1, r2, c2, r3, c3
    integer, parameter :: U = 20, GAP = 40, PAD = 12, LABEL_H = 34
    integer :: w1, h1, w2, h2, w3, h3, maxh, total_w, total_h
    integer :: x1, x2, x3, y1, y2, y3

    w1 = c1 * U; h1 = r1 * U
    w2 = c2 * U; h2 = r2 * U
    w3 = c3 * U; h3 = r3 * U
    maxh = max(h1, h2, h3)
    total_w = PAD + w1 + GAP + w2 + GAP + w3 + PAD
    total_h = PAD + maxh + LABEL_H
    x1 = PAD
    x2 = x1 + w1 + GAP
    x3 = x2 + w2 + GAP
    y1 = PAD + (maxh - h1) / 2
    y2 = PAD + (maxh - h2) / 2
    y3 = PAD + (maxh - h3) / 2

    call push_string(body, '<figure class="opmap">')
    call push_string(body, '<svg viewBox="0 0 ' // int_to_str(total_w) // ' ' // &
                     int_to_str(total_h) // '" role="img" aria-label="' // &
                     aria // '">')
    call draw_box(body, x1, y1, w1, h1, r1, c1, l1)
    call draw_op(body, x1 + w1 + GAP / 2, PAD + maxh / 2, op1)
    call draw_box(body, x2, y2, w2, h2, r2, c2, l2)
    call draw_op(body, x2 + w2 + GAP / 2, PAD + maxh / 2, op2)
    call draw_box(body, x3, y3, w3, h3, r3, c3, l3)
    call push_string(body, '</svg>')
    call push_string(body, '</figure>')
  end subroutine shape_map

  subroutine draw_box(body, x, y, w, h, rows, cols, label)
    type(string_t), allocatable, intent(inout) :: body(:)
    integer, intent(in) :: x, y, w, h, rows, cols
    character(*), intent(in) :: label
    integer, parameter :: U = 20
    integer :: k
    call push_string(body, '  <rect x="' // int_to_str(x) // '" y="' // &
                     int_to_str(y) // '" width="' // int_to_str(w) // &
                     '" height="' // int_to_str(h) // &
                     '" fill="none" stroke="#ffb000" stroke-width="2"/>')
    do k = 1, cols - 1
      call push_string(body, '  <line x1="' // int_to_str(x + k * U) // '" y1="' // &
                       int_to_str(y) // '" x2="' // int_to_str(x + k * U) // &
                       '" y2="' // int_to_str(y + h) // &
                       '" stroke="#ffb000" stroke-width="0.5"/>')
    end do
    do k = 1, rows - 1
      call push_string(body, '  <line x1="' // int_to_str(x) // '" y1="' // &
                       int_to_str(y + k * U) // '" x2="' // int_to_str(x + w) // &
                       '" y2="' // int_to_str(y + k * U) // &
                       '" stroke="#ffb000" stroke-width="0.5"/>')
    end do
    call push_string(body, '  <text x="' // int_to_str(x + w / 2) // '" y="' // &
                     int_to_str(y + h + 16) // '" fill="#ffb000" ' // &
                     'font-family="monospace" font-size="12" ' // &
                     'text-anchor="middle">' // label // ' ' // &
                     int_to_str(rows) // '&#215;' // int_to_str(cols) // '</text>')
  end subroutine draw_box

  subroutine draw_op(body, x, y, op)
    type(string_t), allocatable, intent(inout) :: body(:)
    integer, intent(in) :: x, y
    character(*), intent(in) :: op
    call push_string(body, '  <text x="' // int_to_str(x) // '" y="' // &
                     int_to_str(y + 5) // '" fill="#ffb000" ' // &
                     'font-family="monospace" font-size="16" ' // &
                     'text-anchor="middle">' // op // '</text>')
  end subroutine draw_op

  function dims(a) result(r)
    real(real64), intent(in) :: a(:, :)
    character(:), allocatable :: r
    r = int_to_str(size(a, 1)) // '&#215;' // int_to_str(size(a, 2))
  end function dims

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

end module cathedral_blas
