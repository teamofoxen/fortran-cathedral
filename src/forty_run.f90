!> forty_run: the only door through which external tools are addressed.
!> Commands are fixed literals joined with validated tokens. No untrusted
!> text is ever interpolated. Every exit status is read and believed.
module forty_run
  use forty_util, only: string_t, push_string, trim_cr, int_to_str
  use forty_paths, only: quote, temp_root
  implicit none
  private
  public :: run_result, run_cmd, run_live, tool_found, version_line
  public :: ensure_dir, read_all_lines, write_lines, delete_file

  type :: run_result
    logical :: launched = .false.   ! the command could be started at all
    integer :: exit_code = -1
    type(string_t), allocatable :: out(:)  ! captured lines (run_cmd only)
  end type run_result

  integer, save :: cap_counter = 0

contains

  !> Run and capture stdout+stderr via a temp file, since Fortran predates
  !> the notion that a language should hand you a pipe.
  function run_cmd(cmd) result(rr)
    character(*), intent(in) :: cmd
    type(run_result) :: rr
    character(:), allocatable :: cap, full
    integer :: xs, cs
    character(256) :: msg
    cap = next_capture_path()
    full = cmd // ' > ' // quote(cap) // ' 2>&1'
    xs = 0; cs = 0; msg = ''
    call execute_command_line(full, wait=.true., exitstat=xs, cmdstat=cs, cmdmsg=msg)
    rr%launched = (cs == 0)
    rr%exit_code = xs
    call read_all_lines(cap, rr%out)
    call delete_file(cap)
    if (.not. allocated(rr%out)) allocate (rr%out(0))
  end function run_cmd

  !> Run with the console inherited, so delegated tools may speak for
  !> themselves. Output is not captured; the exit status is.
  function run_live(cmd) result(rr)
    character(*), intent(in) :: cmd
    type(run_result) :: rr
    integer :: xs, cs
    character(256) :: msg
    xs = 0; cs = 0; msg = ''
    call execute_command_line(cmd, wait=.true., exitstat=xs, cmdstat=cs, cmdmsg=msg)
    rr%launched = (cs == 0)
    rr%exit_code = xs
    allocate (rr%out(0))
  end function run_live

  !> Is a tool on the PATH? Asks `where`, the registrar of this platform.
  function tool_found(name, path_out) result(found)
    character(*), intent(in) :: name
    character(:), allocatable, intent(out) :: path_out
    logical :: found
    type(run_result) :: rr
    rr = run_cmd('where ' // name)
    found = rr%launched .and. rr%exit_code == 0 .and. size(rr%out) > 0
    if (found) then
      path_out = rr%out(1)%s
    else
      path_out = ''
    end if
  end function tool_found

  !> First nonempty line of a command's output; typically its version.
  function version_line(cmd) result(ver)
    character(*), intent(in) :: cmd
    character(:), allocatable :: ver
    type(run_result) :: rr
    integer :: i
    ver = ''
    rr = run_cmd(cmd)
    if (.not. rr%launched .or. rr%exit_code /= 0) return
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) then
        ver = trim(rr%out(i)%s)
        return
      end if
    end do
  end function version_line

  subroutine ensure_dir(path)
    character(*), intent(in) :: path
    type(run_result) :: rr
    rr = run_cmd('if not exist ' // quote(path // '\') // ' mkdir ' // quote(path))
  end subroutine ensure_dir

  subroutine read_all_lines(path, lines)
    character(*), intent(in) :: path
    type(string_t), allocatable, intent(out) :: lines(:)
    integer :: u, ios
    character(8192) :: buf
    allocate (lines(0))
    open (newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) return
    do
      read (u, '(a)', iostat=ios) buf
      if (ios /= 0) exit
      call push_string(lines, trim_cr(trim(buf)))
    end do
    close (u)
  end subroutine read_all_lines

  subroutine write_lines(path, lines, ok)
    character(*), intent(in) :: path
    type(string_t), intent(in) :: lines(:)
    logical, intent(out) :: ok
    integer :: u, ios, i
    ok = .false.
    open (newunit=u, file=path, status='replace', action='write', iostat=ios)
    if (ios /= 0) return
    do i = 1, size(lines)
      write (u, '(a)') lines(i)%s
    end do
    close (u)
    ok = .true.
  end subroutine write_lines

  subroutine delete_file(path)
    character(*), intent(in) :: path
    integer :: u, ios
    open (newunit=u, file=path, status='old', iostat=ios)
    if (ios == 0) close (u, status='delete')
  end subroutine delete_file

  function next_capture_path() result(p)
    character(:), allocatable :: p
    integer :: v(8)
    call date_and_time(values=v)
    cap_counter = cap_counter + 1
    p = temp_root() // '\forty_cap_' // int_to_str(v(7)*1000 + v(8)) // &
        '_' // int_to_str(cap_counter) // '.txt'
  end function next_capture_path

end module forty_run
