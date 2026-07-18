!> forty_ui: the voice of the verger.
!> Solemn, aligned, line-printer safe. ASCII only, as the ancestors intended.
module forty_ui
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, input_unit
  use forty_canon, only: FORTY_VERSION
  use forty_util, only: to_lower
  implicit none
  private
  public :: say, verdict, lament, rule, blank, banner, confirm, set_muted

  !> The trials sometimes ask the verger to work in silence.
  logical, save :: muted = .false.

contains

  subroutine set_muted(m)
    logical, intent(in) :: m
    muted = m
  end subroutine set_muted

  subroutine say(text)
    character(*), intent(in) :: text
    if (muted) return
    write (output_unit, '(a)') text
  end subroutine say

  !> A two-column pronouncement: a short verdict, then the evidence.
  subroutine verdict(head, detail)
    character(*), intent(in) :: head, detail
    character(26) :: pad
    if (muted) return
    if (len_trim(detail) == 0) then
      write (output_unit, '(a)') trim(head)
    else
      pad = head
      write (output_unit, '(a,2x,a)') pad, trim(detail)
    end if
  end subroutine verdict

  subroutine lament(text)
    character(*), intent(in) :: text
    if (muted) return
    write (error_unit, '(a)') 'FORTY: ' // text
  end subroutine lament

  subroutine rule()
    if (muted) return
    write (output_unit, '(a)') repeat('-', 60)
  end subroutine rule

  subroutine blank()
    if (muted) return
    write (output_unit, '(a)') ''
  end subroutine blank

  subroutine banner()
    call say('FORTY ' // FORTY_VERSION)
    call say('VERGER OF THE FORTRAN CATHEDRAL')
    call rule()
  end subroutine banner

  !> One question, one answer. EOF or silence is a refusal;
  !> the Cathedral does not presume consent.
  function confirm(prompt, assume_yes) result(agreed)
    character(*), intent(in) :: prompt
    logical, intent(in) :: assume_yes
    logical :: agreed
    character(64) :: reply
    integer :: ios
    character(:), allocatable :: ans
    if (assume_yes) then
      call say(prompt // ' [y/N]  CONFIRMED BY DECREE (--yes).')
      agreed = .true.
      return
    end if
    write (output_unit, '(a)', advance='no') prompt // ' [y/N] '
    read (input_unit, '(a)', iostat=ios) reply
    if (ios /= 0) then
      call blank()
      call say('NO ANSWER CAME. THE RITE IS WITHHELD.')
      agreed = .false.
      return
    end if
    ans = trim(adjustl(to_lower(reply)))
    agreed = (ans == 'y' .or. ans == 'yes')
  end function confirm

end module forty_ui
