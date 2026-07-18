!> forty: the Cathedral's verger, keeper of the keys, counter of impurities.
!>
!> A real Fortran program, because this project has standards.
program forty
  use forty_util, only: string_t
  use forty_ui, only: say, lament, banner, blank
  use forty_cli
  use forty_paths, only: in_cathedral_root
  use forty_canon, only: FORTY_VERSION, EXIT_OK, EXIT_FAIL, EXIT_USAGE
  use forty_doctor, only: run_doctor
  use forty_status, only: run_status
  use forty_buildops, only: run_build, run_test, run_clean
  use forty_confess, only: run_confess
  use forty_github, only: github_status, github_connect, github_verify
  use cathedral_generate, only: run_generate, run_open
  use cathedral_validate, only: run_validate
  use forty_offer, only: run_offer
  use forty_atone, only: run_atone
  use forty_audit, only: run_audit
  implicit none

  type(string_t), allocatable :: argv(:)
  type(cli_t) :: cli
  integer :: i, n, code
  character(2048) :: buf

  n = command_argument_count()
  allocate (argv(n))
  do i = 1, n
    call get_command_argument(i, buf)
    argv(i)%s = trim(buf)
  end do

  cli = parse_cli(argv)

  if (len(cli%errmsg) > 0) then
    call lament(cli%errmsg)
    call say('CONSULT: forty help')
    call exit(EXIT_USAGE)
  end if

  ! Forty serves one building. Commands that touch it must be spoken
  ! from its root. The doctor and the help desk see visitors anywhere.
  select case (cli%command)
  case (CMD_STATUS, CMD_BUILD, CMD_TEST, CMD_CONFESS, CMD_CLEAN, CMD_GITHUB, &
        CMD_GENERATE, CMD_VALIDATE, CMD_OPEN, CMD_OFFER, CMD_ATONE, CMD_AUDIT)
    if (.not. in_cathedral_root()) then
      call lament('FORTY SERVES ONE CATHEDRAL. INVOKE HIM FROM ITS ROOT')
      call say('(THE DIRECTORY HOLDING CLAUDE.md, FORTY.md, AND fpm.toml).')
      call exit(EXIT_FAIL)
    end if
  end select

  code = EXIT_OK
  select case (cli%command)
  case (CMD_HELP);    call print_help()
  case (CMD_VERSION); call say('FORTY ' // FORTY_VERSION)
  case (CMD_DOCTOR);  call run_doctor(code)
  case (CMD_STATUS);  call run_status(code)
  case (CMD_BUILD);   call run_build(code)
  case (CMD_TEST);    call run_test(code)
  case (CMD_CONFESS); call run_confess(code)
  case (CMD_CLEAN);   call run_clean(cli%assume_yes, code)
  case (CMD_GENERATE); call run_generate(code)
  case (CMD_VALIDATE); call run_validate(code)
  case (CMD_OPEN);     call run_open(code)
  case (CMD_OFFER);    call run_offer(cli, code)
  case (CMD_ATONE);    call run_atone(cli, code)
  case (CMD_AUDIT);    call run_audit(cli, code)
  case (CMD_GITHUB)
    select case (cli%subcommand)
    case (SUB_STATUS);  call github_status(code)
    case (SUB_CONNECT); call github_connect(cli, code)
    case (SUB_VERIFY);  call github_verify(code)
    end select
  end select

  call exit(code)

contains

  subroutine print_help()
    call banner()
    call say('USAGE: forty <command> [options]')
    call blank()
    call say('COMMANDS:')
    call say('  help                THIS PROCLAMATION.')
    call say('  version             THE VERGER''S VERSION.')
    call say('  doctor              EXAMINE COMPILER, FPM, GIT, AND GH.')
    call say('  status              THE STATE OF THE CATHEDRAL.')
    call say('  build               COMPILE VIA FPM. BLESS A STAMP.')
    call say('  test                RUN THE TRIALS VIA FPM.')
    call say('  confess             MEASURE HERESY. AUDIT THE LEDGER.')
    call say('  generate            RAISE THE SITE INTO dist\.')
    call say('  validate            SURVEY THE RAISED FABRIC.')
    call say('  open                OPEN THE PORCH IN YOUR BROWSER.')
    call say('  offer               COMMIT AND PUSH THROUGH FORTY. ONE CONFIRMATION.')
    call say('  atone <rite>        THE RITE OF RESTITUTION. FORWARD-ONLY. NO ERASURE.')
    call say('  audit provenance    READ THE CATHEDRAL''S OWN HISTORY. REPORT UNDER build\audit\.')
    call say('  clean               SWEEP build\ AND dist\. ASKS FIRST.')
    call say('  github status       REPORT THE GATEHOUSE.')
    call say('  github connect      THE CONSECRATION RITE. ONE CONFIRMATION.')
    call say('  github verify       CONFIRM THE REMOTE IS CANONICAL.')
    call blank()
    call say('OPTIONS:')
    call say('  --dry-run           SHOW THE RITE; PERFORM NOTHING.')
    call say('  --yes, -y           CONSENT BY DECREE (NON-INTERACTIVE USE).')
    call say('  --name=X            REPOSITORY NAME FOR THE CONSECRATION.')
    call say('  --owner=X           REPOSITORY OWNER (DEFAULT: THE AUTHENTICATED SOUL).')
    call say('  --description=X     REPOSITORY DESCRIPTION (MODEST CHARACTERS ONLY).')
    call say('  --visibility=X      public OR private (DEFAULT: public).')
    call say('  --message=X         THE OFFERING''S COMMIT MESSAGE.')
    call blank()
    call say('THE DUMBEST REASONABLE TASK SHOULD BE WRITTEN IN FORTRAN.')
  end subroutine print_help

end program forty
