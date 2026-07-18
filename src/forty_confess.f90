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
  public :: transgression_t, ledger_transgressions, split_cells

  integer, parameter :: CLASS_FORTRAN = 1, CLASS_DECLARATIVE = 2
  integer, parameter :: CLASS_HERESY = 3, CLASS_OTHER = 4

  !> One recorded operational or architectural transgression: an event,
  !> not code. Permanent, disclosed, and displayed in the Confessional.
  type :: transgression_t
    character(:), allocatable :: date
    character(:), allocatable :: event
    character(:), allocatable :: commit
    character(:), allocatable :: exec_lines
    character(:), allocatable :: why
    character(:), allocatable :: remediation
    character(:), allocatable :: status
  end type transgression_t

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

  !> Read the file-or-component names recorded in the Ledger's
  !> executable table. Scoped to the '## Current ledger' chapter so the
  !> operational chapter's rows are never mistaken for executable sins.
  subroutine ledger_entries(lines, entries)
    type(string_t), intent(in) :: lines(:)
    type(string_t), allocatable, intent(out) :: entries(:)
    type(string_t), allocatable :: cells(:)
    integer :: i
    logical :: in_chapter
    character(:), allocatable :: line, cell
    allocate (entries(0))
    in_chapter = .false.
    do i = 1, size(lines)
      line = trim(adjustl(lines(i)%s))
      if (starts_with(line, '## ')) then
        in_chapter = (line == '## Current ledger')
        cycle
      end if
      if (.not. in_chapter) cycle
      if (len(line) < 2) cycle
      if (line(1:1) /= '|') cycle
      call split_cells(line, cells)
      if (size(cells) == 0) cycle
      cell = cells(1)%s
      if (len(cell) == 0) cycle
      if (cell == 'File or component' .or. cell == 'None') cycle
      if (verify(cell, '-: ') == 0) cycle   ! separator rows
      call push_string(entries, cell)
    end do
  end subroutine ledger_entries

  !> Read the operational chapter. wellformed reports whether the
  !> chapter exists and every row carries its full seven cells.
  subroutine ledger_transgressions(lines, entries, wellformed)
    type(string_t), intent(in) :: lines(:)
    type(transgression_t), allocatable, intent(out) :: entries(:)
    logical, intent(out) :: wellformed
    type(string_t), allocatable :: cells(:)
    type(transgression_t), allocatable :: tmp(:)
    integer :: i, n
    logical :: in_chapter, chapter_seen
    character(:), allocatable :: line, cell
    allocate (entries(0))
    in_chapter = .false.
    chapter_seen = .false.
    wellformed = .true.
    do i = 1, size(lines)
      line = trim(adjustl(lines(i)%s))
      if (starts_with(line, '## ')) then
        in_chapter = (line == '## Operational transgressions')
        if (in_chapter) chapter_seen = .true.
        cycle
      end if
      if (.not. in_chapter) cycle
      if (len(line) < 2) cycle
      if (line(1:1) /= '|') cycle
      call split_cells(line, cells)
      if (size(cells) == 0) cycle
      cell = cells(1)%s
      if (len(cell) == 0) cycle
      if (cell == 'Date' .or. cell == 'None') cycle
      if (verify(cell, '-: ') == 0) cycle
      if (size(cells) < 7) then
        wellformed = .false.
        cycle
      end if
      n = size(entries)
      allocate (tmp(n + 1))
      tmp(1:n) = entries
      tmp(n + 1)%date = cells(1)%s
      tmp(n + 1)%event = cells(2)%s
      tmp(n + 1)%commit = cells(3)%s
      tmp(n + 1)%exec_lines = cells(4)%s
      tmp(n + 1)%why = cells(5)%s
      tmp(n + 1)%remediation = cells(6)%s
      tmp(n + 1)%status = cells(7)%s
      call move_alloc(tmp, entries)
      if (len(cells(4)%s) == 0) then
        wellformed = .false.
      else if (verify(cells(4)%s, '0123456789') /= 0) then
        wellformed = .false.
      end if
    end do
    if (.not. chapter_seen) wellformed = .false.
  end subroutine ledger_transgressions

  !> Split one '|'-fenced table row into trimmed cells, backticks shed.
  subroutine split_cells(line, cells)
    character(*), intent(in) :: line
    type(string_t), allocatable, intent(out) :: cells(:)
    integer :: i, j
    allocate (cells(0))
    if (len(line) < 2) return
    if (line(1:1) /= '|') return
    i = 1
    do
      j = index(line(i + 1:), '|')
      if (j == 0) exit
      call push_string(cells, clean_cell(line(i + 1:i + j - 1)))
      i = i + j
    end do
  end subroutine split_cells

  function clean_cell(raw) result(cell)
    character(*), intent(in) :: raw
    character(:), allocatable :: cell
    cell = trim(adjustl(raw))
    if (len(cell) >= 2) then
      if (cell(1:1) == '`' .and. cell(len(cell):len(cell)) == '`') then
        cell = cell(2:len(cell) - 1)
      end if
    end if
  end function clean_cell

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

    call report_transgressions(ledger, exit_code)
  end subroutine run_confess

  !> The operational chapter, read aloud and structurally verified.
  !> Recorded transgressions are history, not failure; a missing or
  !> malformed chapter is failure.
  subroutine report_transgressions(ledger, exit_code)
    type(string_t), intent(in) :: ledger(:)
    integer, intent(inout) :: exit_code
    type(transgression_t), allocatable :: trans(:)
    logical :: wellformed
    integer :: i
    character(:), allocatable :: short
    call ledger_transgressions(ledger, trans, wellformed)
    call blank()
    if (.not. wellformed) then
      call lament('THE OPERATIONAL CHAPTER IS MISSING OR MALFORMED.')
      exit_code = EXIT_FAIL
      return
    end if
    call say('OPERATIONAL TRANSGRESSIONS RECORDED: ' // &
             int_to_str(size(trans)) // '.')
    do i = 1, size(trans)
      short = trans(i)%commit
      if (len(short) > 8) short = short(1:8)
      call say('    ' // trans(i)%date // '  ' // short // '  ' // trans(i)%status)
    end do
    call say('THE OPERATIONAL RECORD IS ACKNOWLEDGED.')
  end subroutine report_transgressions

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
