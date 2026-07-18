!> cathedral_highlight: a Fortran-source highlighter written in Fortran,
!> because the exhibits deserve vestments and the Mandate is clear.
!>
!> Bounded by design: line-based, no cross-line strings, a fixed keyword
!> list, and a modest number scanner that does not chase exponents. Every
!> character of every token passes through escape_html; nothing reaches
!> the page unexamined. Color is ornament here, never meaning.
module cathedral_highlight
  use cathedral_html, only: escape_html
  use forty_util, only: to_lower
  implicit none
  private
  public :: highlight_line

  character(12), parameter :: KEYWORDS(*) = [character(12) :: &
    'program', 'end', 'subroutine', 'function', 'module', 'use', &
    'implicit', 'none', 'integer', 'real', 'double', 'precision', &
    'parameter', 'common', 'allocatable', 'allocate', 'do', 'continue', &
    'if', 'then', 'else', 'exit', 'cycle', 'call', 'write', 'print', &
    'format', 'intent', 'only', 'intrinsic', 'contains', 'result', &
    'dimension', 'character', 'logical', 'stop', 'return']

contains

  function highlight_line(raw, fixed_form) result(html)
    character(*), intent(in) :: raw
    logical, intent(in) :: fixed_form
    character(:), allocatable :: html
    integer :: i, n, start
    character(1) :: ch, quote

    html = ''
    n = len(raw)
    if (n == 0) return

    ! Fixed form: a mark in column 1 consecrates the whole line.
    if (fixed_form) then
      ch = raw(1:1)
      if (ch == 'C' .or. ch == 'c' .or. ch == '*') then
        html = span('hl-c', raw)
        return
      end if
    end if

    i = 1
    do while (i <= n)
      ch = raw(i:i)
      if (ch == '!') then
        html = html // span('hl-c', raw(i:))
        return
      else if (ch == "'" .or. ch == '"') then
        quote = ch
        start = i
        i = i + 1
        do while (i <= n)
          if (raw(i:i) == quote) then
            if (i < n) then
              if (raw(i + 1:i + 1) == quote) then
                i = i + 2
                cycle
              end if
            end if
            exit
          end if
          i = i + 1
        end do
        if (i > n) i = n
        html = html // span('hl-s', raw(start:i))
        i = i + 1
      else if (is_word_start(ch)) then
        start = i
        do while (i <= n)
          if (.not. is_word_char(raw(i:i))) exit
          i = i + 1
        end do
        if (is_keyword(raw(start:i - 1))) then
          html = html // span('hl-k', raw(start:i - 1))
        else
          html = html // escape_html(raw(start:i - 1))
        end if
      else if (is_digit(ch)) then
        start = i
        do while (i <= n)
          if (.not. (is_digit(raw(i:i)) .or. raw(i:i) == '.')) exit
          i = i + 1
        end do
        html = html // span('hl-n', raw(start:i - 1))
      else
        html = html // escape_html(ch)
        i = i + 1
      end if
    end do
  end function highlight_line

  function span(class, text) result(r)
    character(*), intent(in) :: class, text
    character(:), allocatable :: r
    r = '<span class="' // class // '">' // escape_html(text) // '</span>'
  end function span

  pure function is_word_start(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    integer :: ic
    ic = iachar(c)
    r = (ic >= iachar('a') .and. ic <= iachar('z')) .or. &
        (ic >= iachar('A') .and. ic <= iachar('Z')) .or. c == '_'
  end function is_word_start

  pure function is_word_char(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    r = is_word_start(c) .or. is_digit(c)
  end function is_word_char

  pure function is_digit(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    integer :: ic
    ic = iachar(c)
    r = (ic >= iachar('0') .and. ic <= iachar('9'))
  end function is_digit

  function is_keyword(word) result(r)
    character(*), intent(in) :: word
    logical :: r
    character(:), allocatable :: lw
    integer :: k
    r = .false.
    lw = to_lower(word)
    do k = 1, size(KEYWORDS)
      if (lw == trim(KEYWORDS(k))) then
        r = .true.
        return
      end if
    end do
  end function is_keyword

end module cathedral_highlight
