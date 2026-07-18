!> forty_util: strings, lists, and small mercies.
module forty_util
  implicit none
  private
  public :: string_t, push_string
  public :: to_lower, to_upper, starts_with, trim_cr, int_to_str
  public :: basename, extension_of, count_nonblank, count_substr

  !> A single owned string. Fortran's answer to a very old question.
  type :: string_t
    character(:), allocatable :: s
  end type string_t

contains

  !> Append one value to a growable list of strings.
  subroutine push_string(list, value)
    type(string_t), allocatable, intent(inout) :: list(:)
    character(*), intent(in) :: value
    type(string_t), allocatable :: tmp(:)
    integer :: n
    if (.not. allocated(list)) allocate(list(0))
    n = size(list)
    allocate(tmp(n + 1))
    tmp(1:n) = list
    tmp(n + 1)%s = value
    call move_alloc(tmp, list)
  end subroutine push_string

  pure function to_lower(s) result(r)
    character(*), intent(in) :: s
    character(len(s)) :: r
    integer :: i, c
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (c >= iachar('A') .and. c <= iachar('Z')) then
        r(i:i) = achar(c + 32)
      else
        r(i:i) = s(i:i)
      end if
    end do
  end function to_lower

  pure function to_upper(s) result(r)
    character(*), intent(in) :: s
    character(len(s)) :: r
    integer :: i, c
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (c >= iachar('a') .and. c <= iachar('z')) then
        r(i:i) = achar(c - 32)
      else
        r(i:i) = s(i:i)
      end if
    end do
  end function to_upper

  pure function starts_with(s, prefix) result(r)
    character(*), intent(in) :: s, prefix
    logical :: r
    r = .false.
    if (len(s) >= len(prefix)) r = (s(1:len(prefix)) == prefix)
  end function starts_with

  !> Strip trailing carriage returns and trailing blanks.
  !> Captured command output on Windows deserves this courtesy.
  pure function trim_cr(s) result(r)
    character(*), intent(in) :: s
    character(:), allocatable :: r
    integer :: n
    n = len_trim(s)
    do while (n > 0)
      if (s(n:n) == achar(13)) then
        n = n - 1
      else
        exit
      end if
    end do
    r = s(1:n)
  end function trim_cr

  function int_to_str(i) result(r)
    integer, intent(in) :: i
    character(:), allocatable :: r
    character(32) :: buf
    write (buf, '(i0)') i
    r = trim(buf)
  end function int_to_str

  !> The final path component, after the last separator of either faith.
  pure function basename(path) result(r)
    character(*), intent(in) :: path
    character(:), allocatable :: r
    integer :: i, cut
    cut = 0
    do i = len(path), 1, -1
      if (path(i:i) == '\' .or. path(i:i) == '/') then
        cut = i
        exit
      end if
    end do
    r = path(cut + 1:)
  end function basename

  !> Lowercased extension without the dot. Empty when there is none.
  !> A leading-dot configuration file (.gitignore) has no extension;
  !> it is a name wearing a hood.
  pure function extension_of(name) result(r)
    character(*), intent(in) :: name
    character(:), allocatable :: r
    integer :: dot
    dot = index(name, '.', back=.true.)
    if (dot <= 1) then
      r = ''
    else
      r = to_lower(name(dot + 1:))
    end if
  end function extension_of

  !> Count nonblank lines. This is the declared metric of executable heresy.
  pure function count_nonblank(lines) result(n)
    type(string_t), intent(in) :: lines(:)
    integer :: n, i
    n = 0
    do i = 1, size(lines)
      if (allocated(lines(i)%s)) then
        if (len_trim(lines(i)%s) > 0) n = n + 1
      end if
    end do
  end function count_nonblank

  !> Non-overlapping occurrences of sub within s.
  pure function count_substr(s, sub) result(n)
    character(*), intent(in) :: s, sub
    integer :: n, p, start
    n = 0
    if (len(sub) == 0) return
    start = 1
    do
      if (start > len(s)) exit
      p = index(s(start:), sub)
      if (p == 0) exit
      n = n + 1
      start = start + p - 1 + len(sub)
    end do
  end function count_substr

end module forty_util
