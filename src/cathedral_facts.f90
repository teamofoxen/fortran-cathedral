!> cathedral_facts: measurements displayed in the Confessional.
!> Every number here is computed from the repository at generation time.
!> No timestamps enter the pages; the output stays deterministic.
module cathedral_facts
  use forty_util, only: string_t
  use forty_confess, only: heresy_summary, list_repo_files, classify, &
                           CLASS_FORTRAN, transgression_t, ledger_transgressions, &
                           expiation_t, ledger_expiation
  use forty_run, only: read_all_lines
  use forty_canon, only: FORTY_VERSION
  implicit none
  private
  public :: build_facts_t, gather_facts

  type :: build_facts_t
    integer :: fortran_files = 0
    integer :: heresy_files = 0
    integer :: heresy_lines = 0
    integer :: route_count = 0
    character(:), allocatable :: generator
    type(transgression_t), allocatable :: trans(:)
    type(expiation_t) :: expiation
    logical :: expiated = .false.
  end type build_facts_t

contains

  subroutine gather_facts(facts, route_count)
    type(build_facts_t), intent(out) :: facts
    integer, intent(in) :: route_count
    type(string_t), allocatable :: files(:), ledger(:)
    logical :: ok, wellformed
    integer :: i
    facts%generator = 'FORTY ' // FORTY_VERSION
    facts%route_count = route_count
    call heresy_summary(facts%heresy_files, facts%heresy_lines)
    call list_repo_files(files, ok)
    if (ok) then
      do i = 1, size(files)
        if (classify(files(i)%s) == CLASS_FORTRAN) then
          facts%fortran_files = facts%fortran_files + 1
        end if
      end do
    end if
    ! The operational record flows from the Ledger into the public page.
    ! Generation is lenient here; forty confess is the enforcer.
    call read_all_lines('HERESY_LEDGER.md', ledger)
    call ledger_transgressions(ledger, facts%trans, wellformed)
    call ledger_expiation(ledger, facts%expiation, facts%expiated)
  end subroutine gather_facts

end module cathedral_facts
