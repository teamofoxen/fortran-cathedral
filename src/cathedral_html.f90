!> cathedral_html: the escaping of text before it enters the sanctuary.
!> Untrusted or special characters are converted to entities; nothing
!> passes into a page or manifest unexamined.
module cathedral_html
  implicit none
  private
  public :: escape_html, escape_json

contains

  !> For HTML text nodes and double-quoted attribute values alike.
  pure function escape_html(s) result(r)
    character(*), intent(in) :: s
    character(:), allocatable :: r
    integer :: i
    r = ''
    do i = 1, len(s)
      select case (s(i:i))
      case ('&');  r = r // '&amp;'
      case ('<');  r = r // '&lt;'
      case ('>');  r = r // '&gt;'
      case ('"');  r = r // '&quot;'
      case ("'");  r = r // '&#39;'
      case default; r = r // s(i:i)
      end select
    end do
  end function escape_html

  !> For double-quoted JSON strings.
  pure function escape_json(s) result(r)
    character(*), intent(in) :: s
    character(:), allocatable :: r
    integer :: i
    r = ''
    do i = 1, len(s)
      select case (s(i:i))
      case ('\');  r = r // '\\'
      case ('"');  r = r // '\"'
      case default; r = r // s(i:i)
      end select
    end do
  end function escape_json

end module cathedral_html
