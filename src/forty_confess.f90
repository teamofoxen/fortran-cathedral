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
  public :: committed_t, ledger_committed, committed_counts, extract_hashes
  public :: boundary_t, ledger_boundaries, split_cells
  public :: expiation_t, ledger_expiation

  integer, parameter :: CLASS_FORTRAN = 1, CLASS_DECLARATIVE = 2
  integer, parameter :: CLASS_HERESY = 3, CLASS_OTHER = 4

  !> One committed heresy: an avoidable violation — an operational
  !> mistake, an architectural lapse, a residue admission, or a failure
  !> to grant Fortran ownership it could plausibly have held. An event,
  !> not code. Permanent, disclosed, and displayed in the Confessional.
  type :: committed_t
    character(:), allocatable :: title
    character(:), allocatable :: date
    character(:), allocatable :: offense
    character(:), allocatable :: consequence
    character(:), allocatable :: exec_lines
    character(:), allocatable :: offense_commits
    character(:), allocatable :: remediation
    character(:), allocatable :: evidence_commits
    character(:), allocatable :: status
  end type committed_t

  !> One necessary platform boundary: an external program Forty drives
  !> at a genuine seam, recorded and counted so the accounting of what
  !> is not Fortran stays public and exact.
  type :: boundary_t
    character(:), allocatable :: name
    character(:), allocatable :: role
    character(:), allocatable :: why
  end type boundary_t

  !> The record of a restitution: how a stain was expiated without
  !> erasing, amending, squashing, or rewriting anything.
  type :: expiation_t
    character(:), allocatable :: offender
    character(:), allocatable :: withdrawal
    character(:), allocatable :: reoffering
    character(:), allocatable :: means
    character(:), allocatable :: history
  end type expiation_t

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

  !> Read the committed chapter. Each entry opens with a '### ' heading
  !> and carries its fields as | Key | Value | rows. wellformed reports
  !> whether the chapter exists and every entry bears all eight fields,
  !> with a numeric executable-line count.
  subroutine ledger_committed(lines, entries, wellformed)
    type(string_t), intent(in) :: lines(:)
    type(committed_t), allocatable, intent(out) :: entries(:)
    logical, intent(out) :: wellformed
    type(string_t), allocatable :: cells(:)
    type(committed_t) :: cur
    integer :: i
    logical :: in_chapter, chapter_seen, open_entry
    character(:), allocatable :: line, key
    allocate (entries(0))
    in_chapter = .false.
    chapter_seen = .false.
    open_entry = .false.
    wellformed = .true.
    do i = 1, size(lines)
      line = trim(adjustl(lines(i)%s))
      if (starts_with(line, '## ')) then
        if (in_chapter .and. open_entry) then
          call close_entry(cur, entries, wellformed)
          open_entry = .false.
        end if
        in_chapter = (line == '## Committed heresies')
        if (in_chapter) chapter_seen = .true.
        cycle
      end if
      if (.not. in_chapter) cycle
      if (starts_with(line, '### ')) then
        if (open_entry) call close_entry(cur, entries, wellformed)
        call blank_entry(cur)
        cur%title = trim(adjustl(line(5:)))
        open_entry = .true.
        cycle
      end if
      if (.not. open_entry) cycle
      if (len(line) < 2) cycle
      if (line(1:1) /= '|') cycle
      call split_cells(line, cells)
      if (size(cells) < 2) cycle
      key = cells(1)%s
      if (len(key) == 0) cycle
      if (key == 'Field') cycle
      if (verify(key, '-: ') == 0) cycle   ! separator rows
      select case (key)
      case ('Date');             cur%date = cells(2)%s
      case ('Offense');          cur%offense = cells(2)%s
      case ('Consequence');      cur%consequence = cells(2)%s
      case ('Executable non-Fortran lines introduced'); cur%exec_lines = cells(2)%s
      case ('Offense commits');  cur%offense_commits = cells(2)%s
      case ('Remediation');      cur%remediation = cells(2)%s
      case ('Evidence commits'); cur%evidence_commits = cells(2)%s
      case ('Status');           cur%status = cells(2)%s
      end select
    end do
    if (in_chapter .and. open_entry) call close_entry(cur, entries, wellformed)
    if (.not. chapter_seen) wellformed = .false.
  end subroutine ledger_committed

  subroutine blank_entry(cur)
    type(committed_t), intent(out) :: cur
    cur%title = ''; cur%date = ''; cur%offense = ''; cur%consequence = ''
    cur%exec_lines = ''; cur%offense_commits = ''; cur%remediation = ''
    cur%evidence_commits = ''; cur%status = ''
  end subroutine blank_entry

  !> Append the entry and judge its completeness. An entry missing any
  !> field, or bearing a non-numeric line count, condemns the chapter.
  subroutine close_entry(cur, entries, wellformed)
    type(committed_t), intent(in) :: cur
    type(committed_t), allocatable, intent(inout) :: entries(:)
    logical, intent(inout) :: wellformed
    type(committed_t), allocatable :: tmp(:)
    integer :: n
    n = size(entries)
    allocate (tmp(n + 1))
    tmp(1:n) = entries
    tmp(n + 1) = cur
    call move_alloc(tmp, entries)
    if (len(cur%title) == 0 .or. len(cur%date) == 0 .or. &
        len(cur%offense) == 0 .or. len(cur%consequence) == 0 .or. &
        len(cur%exec_lines) == 0 .or. len(cur%offense_commits) == 0 .or. &
        len(cur%remediation) == 0 .or. len(cur%evidence_commits) == 0 .or. &
        len(cur%status) == 0) then
      wellformed = .false.
    else if (verify(cur%exec_lines, '0123456789') /= 0) then
      wellformed = .false.
    end if
  end subroutine close_entry

  !> The non-overlapping totals: an entry whose status proclaims neither
  !> expiation nor forward correction stands unresolved.
  pure subroutine committed_counts(entries, n_unresolved, n_corrected)
    type(committed_t), intent(in) :: entries(:)
    integer, intent(out) :: n_unresolved, n_corrected
    integer :: i
    n_unresolved = 0
    n_corrected = 0
    do i = 1, size(entries)
      if (index(entries(i)%status, 'EXPIATED') > 0 .or. &
          index(entries(i)%status, 'CORRECTED') > 0) then
        n_corrected = n_corrected + 1
      else
        n_unresolved = n_unresolved + 1
      end if
    end do
  end subroutine committed_counts

  !> Every maximal run of at least seven hex digits in the text: the
  !> commit hashes of a ledger cell, however many it carries.
  subroutine extract_hashes(text, hashes)
    character(*), intent(in) :: text
    type(string_t), allocatable, intent(out) :: hashes(:)
    integer :: i, run_start
    logical :: in_run
    allocate (hashes(0))
    in_run = .false.
    run_start = 0
    do i = 1, len(text) + 1
      if (i <= len(text)) then
        if (is_hex(text(i:i))) then
          if (.not. in_run) then
            in_run = .true.
            run_start = i
          end if
          cycle
        end if
      end if
      if (in_run) then
        if (i - run_start >= 7) call push_string(hashes, text(run_start:i - 1))
        in_run = .false.
      end if
    end do
  end subroutine extract_hashes

  pure function is_hex(c) result(r)
    character(1), intent(in) :: c
    logical :: r
    r = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f')
  end function is_hex

  !> Read the boundary chapter: every external program the Cathedral
  !> stands upon, named, counted, and justified.
  subroutine ledger_boundaries(lines, entries, wellformed)
    type(string_t), intent(in) :: lines(:)
    type(boundary_t), allocatable, intent(out) :: entries(:)
    logical, intent(out) :: wellformed
    type(string_t), allocatable :: cells(:)
    type(boundary_t), allocatable :: tmp(:)
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
        in_chapter = (line == '## Necessary platform boundaries')
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
      if (cell == 'Boundary') cycle
      if (verify(cell, '-: ') == 0) cycle
      if (size(cells) < 3) then
        wellformed = .false.
        cycle
      end if
      n = size(entries)
      allocate (tmp(n + 1))
      tmp(1:n) = entries
      tmp(n + 1)%name = cells(1)%s
      tmp(n + 1)%role = cells(2)%s
      tmp(n + 1)%why = cells(3)%s
      call move_alloc(tmp, entries)
    end do
    if (.not. chapter_seen) wellformed = .false.
    if (size(entries) == 0) wellformed = .false.
  end subroutine ledger_boundaries

  !> Read the expiation record, if one exists. found requires the
  !> chapter plus both commit hashes.
  subroutine ledger_expiation(lines, exp, found)
    type(string_t), intent(in) :: lines(:)
    type(expiation_t), intent(out) :: exp
    logical, intent(out) :: found
    type(string_t), allocatable :: cells(:)
    integer :: i
    logical :: in_chapter
    character(:), allocatable :: line
    exp%offender = ''; exp%withdrawal = ''; exp%reoffering = ''
    exp%means = ''; exp%history = ''
    in_chapter = .false.
    do i = 1, size(lines)
      line = trim(adjustl(lines(i)%s))
      if (starts_with(line, '## ')) then
        in_chapter = (line == '## Expiation record')
        cycle
      end if
      if (.not. in_chapter) cycle
      if (len(line) < 2) cycle
      if (line(1:1) /= '|') cycle
      call split_cells(line, cells)
      if (size(cells) < 2) cycle
      select case (cells(1)%s)
      case ('Expiated transgression');       exp%offender = cells(2)%s
      case ('Withdrawal commit');            exp%withdrawal = cells(2)%s
      case ('Canonical re-offering commit'); exp%reoffering = cells(2)%s
      case ('Means');                        exp%means = cells(2)%s
      case ('History');                      exp%history = cells(2)%s
      end select
    end do
    found = (len(exp%withdrawal) > 0 .and. len(exp%reoffering) > 0)
  end subroutine ledger_expiation

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

    call report_accounting(ledger, size(entries), total, exit_code)
  end subroutine run_confess

  !> The full accounting, in clear and non-overlapping totals: measured
  !> executable lines, recorded necessary impurities, recorded platform
  !> boundaries, and the committed heresies with their resolutions.
  !> Recorded heresy is history, not failure; a missing or malformed
  !> chapter is failure.
  subroutine report_accounting(ledger, n_necessary, exec_total, exit_code)
    type(string_t), intent(in) :: ledger(:)
    integer, intent(in) :: n_necessary, exec_total
    integer, intent(inout) :: exit_code
    type(boundary_t), allocatable :: bounds(:)
    type(committed_t), allocatable :: committed(:)
    logical :: b_ok, c_ok
    integer :: i, n_unres, n_corr
    call blank()
    call ledger_boundaries(ledger, bounds, b_ok)
    if (.not. b_ok) then
      call lament('THE BOUNDARY CHAPTER IS MISSING OR MALFORMED.')
      exit_code = EXIT_FAIL
      return
    end if
    call ledger_committed(ledger, committed, c_ok)
    if (.not. c_ok) then
      call lament('THE COMMITTED CHAPTER IS MISSING OR MALFORMED.')
      exit_code = EXIT_FAIL
      return
    end if
    call committed_counts(committed, n_unres, n_corr)
    call say('THE ACCOUNTING OF HERESY.')
    call verdict('EXECUTABLE HERESY.', int_to_str(exec_total) // &
                 ' LINES, MEASURED FROM FILES ALONE.')
    call verdict('NECESSARY IMPURITIES.', int_to_str(n_necessary) // &
                 ' EXECUTABLE, RECORDED.')
    call verdict('PLATFORM BOUNDARIES.', int_to_str(size(bounds)) // &
                 ' NECESSARY, RECORDED.')
    call verdict('COMMITTED HERESIES.', int_to_str(size(committed)) // &
                 ' RECORDED. ' // int_to_str(n_unres) // ' UNRESOLVED. ' // &
                 int_to_str(n_corr) // ' CORRECTED OR EXPIATED.')
    do i = 1, size(committed)
      call say('    ' // int_to_str(i) // '. ' // committed(i)%title // &
               '  --  ' // committed(i)%status)
    end do
    if (n_unres > 0) then
      call say('AN UNRESOLVED COMMITTED HERESY STANDS. REMEDIATION IS OWED.')
    end if
    call say('THE COMMITTED RECORD IS ACKNOWLEDGED.')
    call report_expiation(ledger, committed, exit_code)
  end subroutine report_accounting

  !> An expiated heresy demands its expiation record, and an expiation
  !> record demands an expiated heresy. Either alone is a ledger fault.
  subroutine report_expiation(ledger, committed, exit_code)
    type(string_t), intent(in) :: ledger(:)
    type(committed_t), intent(in) :: committed(:)
    integer, intent(inout) :: exit_code
    type(expiation_t) :: exp
    logical :: found, any_expiated
    integer :: i
    character(:), allocatable :: w8, r8
    any_expiated = .false.
    do i = 1, size(committed)
      if (index(committed(i)%status, 'EXPIATED') > 0) any_expiated = .true.
    end do
    call ledger_expiation(ledger, exp, found)
    if (any_expiated .and. .not. found) then
      call lament('A HERESY CLAIMS EXPIATION BUT NO EXPIATION RECORD EXISTS.')
      exit_code = EXIT_FAIL
      return
    end if
    if (found .and. .not. any_expiated) then
      call lament('AN EXPIATION RECORD EXISTS FOR NO EXPIATED HERESY.')
      exit_code = EXIT_FAIL
      return
    end if
    if (found) then
      w8 = exp%withdrawal
      r8 = exp%reoffering
      if (len(w8) > 8) w8 = w8(1:8)
      if (len(r8) > 8) r8 = r8(1:8)
      call say('THE EXPIATION RECORD IS ACKNOWLEDGED: WITHDRAWN ' // w8 // &
               ', PRESENTED ANEW ' // r8 // '.')
    end if
  end subroutine report_expiation

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
