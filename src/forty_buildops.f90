!> forty_buildops: building, testing, and the sweeping of the yard.
!> Compilation is delegated to fpm; the verdicts and the stamp are Forty's.
module forty_buildops
  use forty_util, only: string_t, push_string, to_lower, int_to_str
  use forty_ui, only: say, lament, blank, confirm
  use forty_run, only: run_result, run_cmd, run_live, ensure_dir, write_lines
  use forty_paths, only: BUILD_OPS_DIR, BUILD_BIN_DIR, STAMP_PATH, ORDAINED_EXE, quote
  use forty_canon, only: EXIT_OK, EXIT_EXTERNAL, EXIT_DECLINED
  implicit none
  private
  public :: run_build, run_test, run_clean

contains

  subroutine run_build(exit_code)
    integer, intent(out) :: exit_code
    type(run_result) :: rr, rloc, rcopy
    character(:), allocatable :: src, ts
    integer :: i

    call say('COMPILING THE CATHEDRAL...')
    rr = run_live('fpm build')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE BUILD HAS FALLEN. FPM EXIT: ' // int_to_str(rr%exit_code))
      exit_code = EXIT_EXTERNAL
      return
    end if

    call ensure_dir(BUILD_OPS_DIR)
    call ensure_dir(BUILD_BIN_DIR)

    ! Keep an ordained copy at a stable path, that the faithful need not
    ! memorize fpm's hashed corridors.
    src = ''
    rloc = run_cmd('dir /b /s /a:-d build\forty.exe')
    if (rloc%launched .and. rloc%exit_code == 0) then
      do i = 1, size(rloc%out)
        if (index(to_lower(rloc%out(i)%s), '\app\') > 0) then
          src = rloc%out(i)%s
          exit
        end if
      end do
    end if
    if (len(src) > 0) then
      rcopy = run_cmd('copy /y ' // quote(src) // ' ' // quote(ORDAINED_EXE))
      if (.not. rcopy%launched .or. rcopy%exit_code /= 0) then
        call say('THE ORDAINED COPY COULD NOT BE RENEWED.')
        call say('(IS FORTY HIMSELF STANDING IN THE DOORWAY? RUN: fpm run forty -- build)')
      end if
    end if

    ts = timestamp()
    call write_stamp(ts)
    call say('BUILD BLESSED: ' // ts)
    call say('THE ORDAINED BINARY RESTS AT ' // ORDAINED_EXE)
    exit_code = EXIT_OK
  end subroutine run_build

  subroutine run_test(exit_code)
    integer, intent(out) :: exit_code
    type(run_result) :: rr
    call say('THE TRIALS BEGIN.')
    rr = run_live('fpm test')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('THE TRIALS HAVE FOUND US WANTING. FPM EXIT: ' // &
                  int_to_str(rr%exit_code))
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('THE TRIALS ARE PASSED.')
    exit_code = EXIT_OK
  end subroutine run_test

  subroutine run_clean(assume_yes, exit_code)
    logical, intent(in) :: assume_yes
    integer, intent(out) :: exit_code
    type(run_result) :: rr, rsurv
    integer :: i
    logical :: only_forty

    rr = run_cmd('if exist build\ (echo PRESENT) else (echo ABSENT)')
    if (size(rr%out) > 0) then
      if (rr%out(1)%s == 'ABSENT') then
        call say('THE YARD IS ALREADY SWEPT.')
        exit_code = EXIT_OK
        return
      end if
    end if

    call say('CLEAN WILL REMOVE: build\  (COMPILED ARTIFACTS, STAMPS, AND')
    call say('THE ORDAINED BINARY). NOTHING ELSE WILL BE TOUCHED.')
    if (.not. confirm('SWEEP THE YARD?', assume_yes)) then
      call say('THE YARD REMAINS.')
      exit_code = EXIT_DECLINED
      return
    end if

    rr = run_cmd('rmdir /s /q build')
    if (rr%launched .and. rr%exit_code == 0) then
      call say('THE YARD IS SWEPT.')
      exit_code = EXIT_OK
      return
    end if

    ! A locked forty.exe cannot sweep himself out of existence. If he is
    ! the only survivor, the sweeping is accepted as morally complete.
    rsurv = run_cmd('dir /b /s /a:-d build')
    only_forty = (rsurv%launched .and. rsurv%exit_code == 0 .and. size(rsurv%out) > 0)
    do i = 1, size(rsurv%out)
      if (.not. ends_with_ci(rsurv%out(i)%s, '\forty.exe')) only_forty = .false.
    end do
    if (only_forty) then
      call say('THE YARD IS SWEPT, SAVE FOR THE VERGER''S OWN SHOES.')
      call say('(A RUNNING FORTY CANNOT REMOVE HIMSELF. SWEEP AGAIN FROM OUTSIDE.)')
      exit_code = EXIT_OK
    else
      call lament('THE YARD RESISTS. rmdir EXIT: ' // int_to_str(rr%exit_code))
      exit_code = EXIT_EXTERNAL
    end if
  end subroutine run_clean

  function timestamp() result(ts)
    character(:), allocatable :: ts
    integer :: v(8)
    character(19) :: buf
    call date_and_time(values=v)
    write (buf, '(i4.4,a,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i2.2)') &
      v(1), '-', v(2), '-', v(3), v(5), ':', v(6), ':', v(7)
    ts = buf
  end function timestamp

  subroutine write_stamp(ts)
    character(*), intent(in) :: ts
    type(string_t), allocatable :: lines(:)
    logical :: ok
    call push_string(lines, ts)
    call push_string(lines, 'FPM BUILD EXIT: 0')
    call push_string(lines, 'STAMPED BY FORTY. THE ARRAYS REMAIN CONTIGUOUS.')
    call write_lines(STAMP_PATH, lines, ok)
  end subroutine write_stamp

  pure function ends_with_ci(s, suffix) result(r)
    character(*), intent(in) :: s, suffix
    logical :: r
    integer :: n, m
    n = len_trim(s)
    m = len(suffix)
    r = .false.
    if (n >= m) r = (to_lower(s(n - m + 1:n)) == to_lower(suffix))
  end function ends_with_ci

end module forty_buildops
