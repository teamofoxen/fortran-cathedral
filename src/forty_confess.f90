!> forty_confess: the measurement of heresy and the audit of the ledger.
!> The metric is declared plainly: one executable line is one nonblank
!> line in a file whose language is judged heretical by extension.
module forty_confess
  use forty_util, only: string_t, push_string, to_lower, starts_with, &
                        basename, extension_of, count_nonblank, int_to_str
  use forty_run, only: run_result, run_cmd, read_all_lines
  use forty_ui, only: say, verdict, lament, rule, blank
  use forty_canon, only: EXIT_OK, EXIT_FAIL
  implicit none
  private
  public :: CLASS_FORTRAN, CLASS_DECLARATIVE, CLASS_HERESY, CLASS_OTHER
  public :: classify, ledger_entries, list_repo_files, run_confess, heresy_summary

  integer, parameter :: CLASS_FORTRAN = 1, CLASS_DECLARATIVE = 2
  integer, parameter :: CLASS_HERESY = 3, CLASS_OTHER = 4

contains

  !> Judgment by extension, per the classification rules of the Ledger.
  pure function classify(relpath) result(class)
    character(*), intent(in) :: relpath
    integer :: class
    character(:), allocatable :: base, ext
    base = basename(relpath)
    ext = extension_of(base)
    if (ext == '') then
      if (starts_with(base, '.')) then
        class = CLASS_DECLARATIVE   ! dot-config files: declarative hoods
      else
        class = CLASS_OTHER
      end if
      return
    end if
    select case (ext)
    case ('f90', 'f95', 'f03', 'f08', 'f', 'for', 'f77', 'fpp')
      class = CLASS_FORTRAN
    case ('md', 'markdown', 'html', 'htm', 'css', 'json', 'yml', 'yaml', &
          'xml', 'svg', 'toml', 'txt', 'csv', 'ini', 'cfg', 'png', 'jpg', &
          'jpeg', 'gif', 'ico', 'webp', 'woff', 'woff2', 'ttf', 'pdf')
      class = CLASS_DECLARATIVE
    case ('js', 'mjs', 'cjs', 'ts', 'tsx', 'jsx', 'py', 'pyw', 'rb', 'sh', &
          'bash', 'zsh', 'ps1', 'psm1', 'psd1', 'bat', 'cmd', 'vbs', 'lua', &
          'pl', 'pm', 'php', 'c', 'cc', 'cpp', 'cxx', 'h', 'hpp', 'hxx', &
          'go', 'rs', 'java', 'kt', 'swift', 'cs')
      class = CLASS_HERESY
    case default
      class = CLASS_OTHER
    end select
  end function classify

  !> Every file in the Cathedral, relative paths, excluding the yard
  !> (build\), the vault (.git\), and the porch (dist\).
  subroutine list_repo_files(files, ok)
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(out) :: ok
    type(run_result) :: rr_cd, rr
    character(:), allocatable :: root, line, rel
    integer :: i
    allocate (files(0))
    ok = .false.
    rr_cd = run_cmd('cd')
    if (.not. rr_cd%launched .or. rr_cd%exit_code /= 0 .or. size(rr_cd%out) == 0) return
    root = rr_cd%out(1)%s // '\'
    rr = run_cmd('dir /b /s /a:-d')
    if (.not. rr%launched .or. rr%exit_code /= 0) return
    do i = 1, size(rr%out)
      line = rr%out(i)%s
      if (len(line) <= len(root)) cycle
      if (.not. starts_with(line, root)) cycle
      rel = line(len(root) + 1:)
      if (starts_with(rel, 'build\') .or. starts_with(rel, '.git\') .or. &
          starts_with(rel, 'dist\')) cycle
      call push_string(files, rel)
    end do
    ok = .true.
  end subroutine list_repo_files

  !> Read the file-or-component names recorded in the Ledger's table.
  subroutine ledger_entries(lines, entries)
    type(string_t), intent(in) :: lines(:)
    type(string_t), allocatable, intent(out) :: entries(:)
    integer :: i, p1, p2
    character(:), allocatable :: line, cell
    allocate (entries(0))
    do i = 1, size(lines)
      line = trim(adjustl(lines(i)%s))
      if (len(line) < 2) cycle
      if (line(1:1) /= '|') cycle
      p1 = 2
      p2 = index(line(2:), '|')
      if (p2 == 0) cycle
      cell = trim(adjustl(line(p1:p2)))
      if (len(cell) == 0) cycle
      if (cell(1:1) == '`') cell = cell(2:)
      if (len(cell) > 0) then
        if (cell(len(cell):len(cell)) == '`') cell = cell(:len(cell) - 1)
      end if
      if (len(cell) == 0) cycle
      if (cell == 'File or component' .or. cell == 'None') cycle
      if (verify(cell, '-: ') == 0) cycle   ! separator rows
      call push_string(entries, cell)
    end do
  end subroutine ledger_entries

  !> A quiet census for the status report.
  subroutine heresy_summary(n_files, n_lines)
    integer, intent(out) :: n_files, n_lines
    type(string_t), allocatable :: files(:), body(:)
    logical :: ok
    integer :: i
    n_files = 0
    n_lines = 0
    call list_repo_files(files, ok)
    if (.not. ok) return
    do i = 1, size(files)
      if (classify(files(i)%s) == CLASS_HERESY) then
        n_files = n_files + 1
        call read_all_lines(files(i)%s, body)
        n_lines = n_lines + count_nonblank(body)
      end if
    end do
  end subroutine heresy_summary

  subroutine run_confess(exit_code)
    integer, intent(out) :: exit_code
    type(string_t), allocatable :: files(:), body(:), ledger(:), entries(:)
    type(string_t), allocatable :: heresy(:), unrecorded(:), phantom(:)
    integer, allocatable :: heresy_lines(:)
    integer :: i, j, n_fortran, n_decl, n_other, total
    logical :: ok, found
    character(:), allocatable :: a, b

    exit_code = EXIT_FAIL
    call say('THE CONFESSIONAL IS OPEN.')
    call rule()

    call list_repo_files(files, ok)
    if (.not. ok) then
      call lament('THE GROUNDS COULD NOT BE WALKED. dir HAS FAILED US.')
      return
    end if

    n_fortran = 0; n_decl = 0; n_other = 0
    allocate (heresy(0))
    do i = 1, size(files)
      select case (classify(files(i)%s))
      case (CLASS_FORTRAN);     n_fortran = n_fortran + 1
      case (CLASS_DECLARATIVE); n_decl = n_decl + 1
      case (CLASS_HERESY);      call push_string(heresy, files(i)%s)
      case default;             n_other = n_other + 1
      end select
    end do

    call verdict('SCRIPTURE.', int_to_str(n_fortran) // ' FORTRAN FILES.')
    call verdict('DECLARATIVE WORKS.', int_to_str(n_decl) // ' FILES.')
    if (n_other > 0) then
      call verdict('UNCLASSIFIED.', int_to_str(n_other) // ' FILES (REVIEW THEM).')
      do i = 1, size(files)
        if (classify(files(i)%s) == CLASS_OTHER) call say('    ' // files(i)%s)
      end do
    end if

    allocate (heresy_lines(size(heresy)))
    total = 0
    do i = 1, size(heresy)
      call read_all_lines(heresy(i)%s, body)
      heresy_lines(i) = count_nonblank(body)
      total = total + heresy_lines(i)
    end do

    call blank()
    if (size(heresy) == 0) then
      call say('HERESY MEASURED: 0 EXECUTABLE LINES.')
    else
      call say('HERESY DISCOVERED (EXECUTABLE = NONBLANK LINES):')
      do i = 1, size(heresy)
        call say('    ' // heresy(i)%s // '  --  ' // &
                 int_to_str(heresy_lines(i)) // ' EXECUTABLE LINES')
      end do
      call say('HERESY MEASURED: ' // int_to_str(total) // ' EXECUTABLE LINES.')
    end if

    call read_all_lines('HERESY_LEDGER.md', ledger)
    if (size(ledger) == 0) then
      call lament('THE LEDGER ITSELF IS MISSING. THIS IS WORSE THAN HERESY.')
      return
    end if
    call ledger_entries(ledger, entries)

    ! What is present but unconfessed?
    allocate (unrecorded(0))
    do i = 1, size(heresy)
      a = norm_path(heresy(i)%s)
      found = .false.
      do j = 1, size(entries)
        if (a == norm_path(entries(j)%s)) found = .true.
      end do
      if (.not. found) call push_string(unrecorded, heresy(i)%s)
    end do

    ! What is confessed but no longer present?
    allocate (phantom(0))
    do j = 1, size(entries)
      b = norm_path(entries(j)%s)
      found = .false.
      do i = 1, size(heresy)
        if (b == norm_path(heresy(i)%s)) found = .true.
      end do
      if (.not. found) call push_string(phantom, entries(j)%s)
    end do

    call blank()
    if (size(unrecorded) > 0) then
      call say('UNRECORDED HERESY. THE LEDGER MUST BE AMENDED:')
      do i = 1, size(unrecorded)
        call say('    ' // unrecorded(i)%s)
      end do
      exit_code = EXIT_FAIL
    else if (size(phantom) > 0) then
      call say('THE LEDGER MOURNS WHAT NO LONGER EXISTS:')
      do i = 1, size(phantom)
        call say('    ' // phantom(i)%s)
      end do
      exit_code = EXIT_FAIL
    else if (size(heresy) == 0) then
      call say('THE LEDGER IS TRUE. THE CATHEDRAL IS PURE.')
      exit_code = EXIT_OK
    else
      call say('THE LEDGER IS TRUE. THE IMPURITY IS CONFESSED.')
      exit_code = EXIT_OK
    end if
  end subroutine run_confess

  !> Slashes bow to one direction; case bows to none.
  pure function norm_path(p) result(r)
    character(*), intent(in) :: p
    character(:), allocatable :: r
    integer :: i
    r = to_lower(p)
    do i = 1, len(r)
      if (r(i:i) == '\') r(i:i) = '/'
    end do
  end function norm_path

end module forty_confess
