!> forty_doctor: examination of the fabric. Four instruments are required
!> for the work: a compiler, fpm, git, and the GitHub CLI.
module forty_doctor
  use forty_ui, only: say, verdict, lament, rule, blank
  use forty_run, only: run_result, run_cmd, tool_found, version_line
  use forty_canon, only: EXIT_OK, EXIT_ENV
  implicit none
  private
  public :: run_doctor

contains

  subroutine run_doctor(exit_code)
    integer, intent(out) :: exit_code
    logical :: ok_compiler, ok_fpm, ok_git, ok_gh
    type(run_result) :: rr

    call say('THE DOCTOR EXAMINES THE FABRIC.')
    call rule()
    ok_compiler = report_tool('gfortran', 'gfortran --version', 'COMPILER')
    ok_fpm      = report_tool('fpm',      'fpm --version',      'FPM')
    ok_git      = report_tool('git',      'git --version',      'GIT')
    ok_gh       = report_tool('gh',       'gh --version',       'GH')

    if (ok_gh) then
      rr = run_cmd('gh auth status')
      if (rr%launched .and. rr%exit_code == 0) then
        call verdict('GH AUTH: RECOGNIZED.', '')
      else
        call verdict('GH AUTH: ABSENT.', 'THE CLI KEEPS THE KEYS: gh auth login')
      end if
    end if

    call blank()
    if (ok_compiler .and. ok_fpm .and. ok_git .and. ok_gh) then
      call say('THE TOOLING IS SOUND.')
      exit_code = EXIT_OK
    else
      call say('THE TOOLING IS INCOMPLETE. THE WORK WAITS.')
      exit_code = EXIT_ENV
    end if
  end subroutine run_doctor

  function report_tool(name, vercmd, label) result(found)
    character(*), intent(in) :: name, vercmd, label
    logical :: found
    character(:), allocatable :: path, ver
    found = tool_found(name, path)
    if (found) then
      ver = version_line(vercmd)
      if (len(ver) == 0) ver = '(VERSION WITHHELD)'
      call verdict(label // ' FOUND.', ver)
      call say('    AT ' // path)
    else
      call verdict(label // ' MISSING.', 'THE PATH DOES NOT PROVIDE IT.')
    end if
  end function report_tool

end module forty_doctor
