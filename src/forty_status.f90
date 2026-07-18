!> forty_status: the state of the Cathedral, read aloud.
module forty_status
  use forty_util, only: string_t, int_to_str
  use forty_ui, only: say, verdict, rule, blank, banner
  use forty_paths, only: file_exists, STAMP_PATH
  use forty_run, only: read_all_lines
  use forty_git, only: git_initialized, git_branch, git_remote_url
  use forty_confess, only: list_repo_files, classify, heresy_summary, CLASS_FORTRAN
  use forty_canon, only: EXIT_OK
  implicit none
  private
  public :: run_status

  character(24), parameter :: CANON_BOOKS(6) = [character(24) :: &
    'CLAUDE.md', 'VISION.md', 'BUILD_RULES.md', 'FORTY.md', &
    'HERESY_LEDGER.md', 'README.md']

contains

  subroutine run_status(exit_code)
    integer, intent(out) :: exit_code
    type(string_t), allocatable :: files(:), stamp(:)
    logical :: ok, found
    integer :: i, n_books, n_fortran, h_files, h_lines
    character(:), allocatable :: branch, url, missing

    call banner()

    n_books = 0
    missing = ''
    do i = 1, size(CANON_BOOKS)
      if (file_exists(trim(CANON_BOOKS(i)))) then
        n_books = n_books + 1
      else
        missing = missing // ' ' // trim(CANON_BOOKS(i))
      end if
    end do
    if (n_books == size(CANON_BOOKS)) then
      call verdict('CANON.', int_to_str(n_books) // ' OF ' // &
                   int_to_str(size(CANON_BOOKS)) // ' BOOKS PRESENT.')
    else
      call verdict('CANON.', int_to_str(n_books) // ' OF ' // &
                   int_to_str(size(CANON_BOOKS)) // ' BOOKS. MISSING:' // missing)
    end if

    n_fortran = 0
    call list_repo_files(files, ok)
    if (ok) then
      do i = 1, size(files)
        if (classify(files(i)%s) == CLASS_FORTRAN) n_fortran = n_fortran + 1
      end do
    end if
    call verdict('SCRIPTURE.', int_to_str(n_fortran) // ' FORTRAN FILES.')

    call heresy_summary(h_files, h_lines)
    if (h_lines == 0) then
      call verdict('HERESY.', '0 EXECUTABLE LINES. RIGHTEOUS.')
    else
      call verdict('HERESY.', int_to_str(h_lines) // ' EXECUTABLE LINES IN ' // &
                   int_to_str(h_files) // ' FILES.')
    end if

    if (git_initialized()) then
      branch = git_branch()
      if (len(branch) == 0) branch = '(UNREADABLE)'
      call verdict('GIT.', 'INITIATED. BRANCH ' // branch // '.')
      call git_remote_url(found, url)
      if (found) then
        call verdict('REMOTE.', url)
      else
        call verdict('REMOTE.', 'ABSENT. AWAITING CONSECRATION.')
      end if
    else
      call verdict('GIT.', 'UNINITIATED. THE GROUND IS UNCONSECRATED.')
    end if

    if (file_exists(STAMP_PATH)) then
      call read_all_lines(STAMP_PATH, stamp)
      if (size(stamp) > 0) then
        call verdict('LAST BLESSED BUILD.', stamp(1)%s)
      end if
    else
      call verdict('BUILD.', 'NO BUILD HAS BEEN BLESSED.')
    end if

    call blank()
    call say('THE CATHEDRAL STANDS.')
    exit_code = EXIT_OK
  end subroutine run_status

end module forty_status
