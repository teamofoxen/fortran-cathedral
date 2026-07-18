!> forty_paths: where things are, and how to speak of them to cmd.exe.
!> Operational residue (stamps, captures) lives under build\ or %TEMP%,
!> never in the working tree. The tree stays clean; the yard takes the dust.
module forty_paths
  implicit none
  private
  public :: file_exists, in_cathedral_root, quote, temp_root, set_cwd
  public :: BUILD_OPS_DIR, BUILD_BIN_DIR, STAMP_PATH, ORDAINED_EXE

  character(*), parameter :: BUILD_OPS_DIR = 'build\forty'
  character(*), parameter :: BUILD_BIN_DIR = 'build\bin'
  character(*), parameter :: STAMP_PATH    = 'build\forty\stamp.txt'
  character(*), parameter :: ORDAINED_EXE  = 'build\bin\forty.exe'

contains

  function file_exists(path) result(r)
    character(*), intent(in) :: path
    logical :: r
    inquire (file=path, exist=r)
  end function file_exists

  !> Forty serves one building. He checks the cornerstones before acting.
  function in_cathedral_root() result(r)
    logical :: r
    r = file_exists('CLAUDE.md') .and. file_exists('FORTY.md') &
        .and. file_exists('fpm.toml')
  end function in_cathedral_root

  !> Wrap a path for cmd.exe. Paths containing double quotes are refused
  !> upstream; this project's ground truly does contain a space.
  pure function quote(path) result(r)
    character(*), intent(in) :: path
    character(:), allocatable :: r
    r = '"' // path // '"'
  end function quote

  !> Change the working directory. Used by the trials to enter their
  !> fixture repositories; the GNU chdir extension serves.
  subroutine set_cwd(dir, ok)
    character(*), intent(in) :: dir
    logical, intent(out) :: ok
    integer :: st
    call chdir(dir, st)
    ok = (st == 0)
  end subroutine set_cwd

  !> The operating system's appointed place for ephemera.
  function temp_root() result(r)
    character(:), allocatable :: r
    character(1024) :: buf
    integer :: l, stat
    call get_environment_variable('TEMP', buf, length=l, status=stat)
    if (stat == 0 .and. l > 0) then
      r = buf(1:l)
    else
      ! No TEMP on Windows would be remarkable. Degrade into the yard.
      r = BUILD_OPS_DIR
    end if
  end function temp_root

end module forty_paths
