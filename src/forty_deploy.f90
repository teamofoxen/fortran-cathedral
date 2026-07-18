!> forty_deploy: the opening of the doors. The deployable tree is built
!> from dist\ alone through Git plumbing (a temporary index; main's
!> index and history are never touched), sealed as a commit on the
!> gh-pages line, lifted with an ordinary push, appointed to GitHub
!> Pages through the gh CLI as the platform boundary, and then verified
!> live at the public address. Forty owns the sequence, the capture,
!> the parsing, the confirmation, the interpretation, and the verdict.
!> Credentials remain entirely with the GitHub CLI.
module forty_deploy
  use forty_util, only: string_t, push_string, int_to_str, count_substr
  use forty_ui, only: say, lament, rule, blank, confirm
  use forty_run, only: run_result, run_cmd, run_live, version_line, &
                       read_all_lines, ensure_dir, delete_file
  use forty_paths, only: quote, file_exists
  use forty_git, only: git_initialized, git_branch, git_remote_url, is_hash
  use forty_cli, only: cli_t
  use forty_buildops, only: run_build, run_test
  use forty_confess, only: run_confess
  use cathedral_generate, only: run_generate
  use cathedral_validate, only: run_validate
  use cathedral_routes, only: route_t, routes
  use forty_canon, only: CANON_BASE_URL, CANON_COMMIT_TRAILER, &
                         EXIT_OK, EXIT_FAIL, EXIT_EXTERNAL, EXIT_DECLINED
  implicit none
  private
  public :: run_deploy
  public :: deploy_preflight, build_deploy_tree, deploy_parent, &
            make_deploy_commit, push_deploy, deploy_needed, tree_manifest, &
            gate_production, parse_pages_response, json_str_field

contains

  ! ------------------------------------------------------ testable engine

  !> Fit ground: main, clean, and of one accord with the canonical remote.
  subroutine deploy_preflight(ready, why)
    logical, intent(out) :: ready
    character(:), allocatable, intent(out) :: why
    type(run_result) :: rr
    character(:), allocatable :: branch, head, omain, url
    logical :: found
    integer :: i
    ready = .false.
    why = ''
    if (.not. git_initialized()) then
      why = 'GIT IS UNINITIATED. THERE IS NOTHING TO DEPLOY FROM.'
      return
    end if
    call git_remote_url(found, url)
    if (.not. found) then
      why = 'NO CANONICAL REMOTE EXISTS. THE DOORS NEED A HINGE.'
      return
    end if
    branch = git_branch()
    if (branch /= 'main') then
      why = 'DEPLOYMENT DEPARTS FROM main ALONE. THIS IS: ' // branch
      return
    end if
    rr = run_cmd('git status --porcelain')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      why = 'THE TREE COULD NOT BE READ.'
      return
    end if
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) then
        why = 'THE TREE IS UNCLEAN. OFFER BEFORE OPENING THE DOORS.'
        return
      end if
    end do
    head = version_line('git rev-parse HEAD')
    omain = version_line('git rev-parse origin/main')
    if (.not. is_hash(head) .or. head /= omain) then
      why = 'LOCAL AND REMOTE main ARE NOT OF ONE ACCORD.'
      return
    end if
    why = ''
    ready = .true.
  end subroutine deploy_preflight

  !> The deployable tree, built from dist\ alone through a temporary
  !> index. Nothing outside dist\ can enter; main's index is untouched.
  subroutine build_deploy_tree(tree, ok)
    character(:), allocatable, intent(out) :: tree
    logical, intent(out) :: ok
    type(run_result) :: rr
    character(:), allocatable :: root, idx
    integer :: i
    tree = ''
    ok = .false.
    rr = run_cmd('cd')
    if (size(rr%out) == 0) return
    root = rr%out(1)%s
    call ensure_dir('build')
    call ensure_dir('build\deploy')
    idx = root // '\build\deploy\index'
    call delete_file(idx)
    rr = run_cmd('cd /d ' // quote(root // '\dist') // &
                 ' && set "GIT_INDEX_FILE=' // idx // '"' // &
                 ' && git --git-dir=' // quote(root // '\.git') // ' add -A' // &
                 ' && git --git-dir=' // quote(root // '\.git') // ' write-tree')
    if (.not. rr%launched .or. rr%exit_code /= 0) return
    do i = size(rr%out), 1, -1
      if (len_trim(rr%out(i)%s) > 0) then
        tree = trim(rr%out(i)%s)
        exit
      end if
    end do
    ok = is_hash(tree)
  end subroutine build_deploy_tree

  !> Every path carried by a tree, as git names them.
  subroutine tree_manifest(tree, names, ok)
    character(*), intent(in) :: tree
    type(string_t), allocatable, intent(out) :: names(:)
    logical, intent(out) :: ok
    type(run_result) :: rr
    integer :: i
    allocate (names(0))
    rr = run_cmd('git ls-tree -r --name-only ' // tree)
    ok = rr%launched .and. rr%exit_code == 0
    if (.not. ok) return
    do i = 1, size(rr%out)
      if (len_trim(rr%out(i)%s) > 0) call push_string(names, trim(rr%out(i)%s))
    end do
  end subroutine tree_manifest

  !> The current tip of the public line, refreshed from the remote.
  subroutine deploy_parent(parent, found)
    character(:), allocatable, intent(out) :: parent
    logical, intent(out) :: found
    type(run_result) :: rr
    rr = run_cmd('git fetch origin gh-pages')
    parent = version_line('git rev-parse refs/remotes/origin/gh-pages')
    found = is_hash(parent)
  end subroutine deploy_parent

  !> Does the public line need a new commit at all?
  function deploy_needed(tree, tip) result(r)
    character(*), intent(in) :: tree, tip
    logical :: r
    r = (version_line('git rev-parse ' // tip // ':') /= tree)
  end function deploy_needed

  subroutine make_deploy_commit(tree, parent, has_parent, src8, commit, ok)
    character(*), intent(in) :: tree, parent, src8
    logical, intent(in) :: has_parent
    character(:), allocatable, intent(out) :: commit
    logical, intent(out) :: ok
    character(:), allocatable :: cmd
    cmd = 'git commit-tree ' // tree
    if (has_parent) cmd = cmd // ' -p ' // parent
    cmd = cmd // ' -m "DEPLOY: THE CATHEDRAL AT main ' // src8 // '." -m "' // &
          CANON_COMMIT_TRAILER // '"'
    commit = version_line(cmd)
    ok = is_hash(commit)
  end subroutine make_deploy_commit

  subroutine push_deploy(commit, ok)
    character(*), intent(in) :: commit
    logical, intent(out) :: ok
    type(run_result) :: rr
    rr = run_live('git push origin ' // commit // ':refs/heads/gh-pages')
    ok = rr%launched .and. rr%exit_code == 0
  end subroutine push_deploy

  !> Which stage of the production path refused, if any.
  pure function gate_production(codes) result(stage)
    integer, intent(in) :: codes(5)
    character(:), allocatable :: stage
    character(8), parameter :: NAMES(5) = [character(8) :: &
      'build', 'generate', 'validate', 'test', 'confess']
    integer :: i
    stage = ''
    do i = 1, 5
      if (codes(i) /= 0) then
        stage = trim(NAMES(i))
        return
      end if
    end do
  end function gate_production

  !> One string field from a JSON document, bounded and unescaped —
  !> sufficient for the Pages responses Forty must interpret.
  function json_str_field(doc, key) result(val)
    character(*), intent(in) :: doc, key
    character(:), allocatable :: val
    character(:), allocatable :: pat
    integer :: p, q
    val = ''
    pat = '"' // key // '":'
    p = index(doc, pat)
    if (p == 0) return
    p = p + len(pat)
    do while (p <= len(doc))
      if (doc(p:p) /= ' ') exit
      p = p + 1
    end do
    if (p > len(doc)) return
    if (doc(p:p) /= '"') return
    q = index(doc(p + 1:), '"')
    if (q == 0) return
    if (q > 1) val = doc(p + 1:p + q - 1)
  end function json_str_field

  subroutine parse_pages_response(doc, html_url, branch, okj)
    character(*), intent(in) :: doc
    character(:), allocatable, intent(out) :: html_url, branch
    logical, intent(out) :: okj
    html_url = json_str_field(doc, 'html_url')
    branch = json_str_field(doc, 'branch')
    okj = (len(html_url) > 0 .and. len(branch) > 0)
  end subroutine parse_pages_response

  ! ------------------------------------------------------------ the rite

  subroutine run_deploy(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    type(route_t), allocatable :: rs(:)
    type(string_t), allocatable :: names(:)
    type(run_result) :: rr
    character(:), allocatable :: why, tree, parent, commit, head, src8
    character(:), allocatable :: doc, html_url, pbranch, gpath
    logical :: ready, ok, has_parent, needed, okj, live
    integer :: codes(5), i, j, attempt
    logical :: found

    exit_code = EXIT_FAIL
    call say('THE OPENING OF THE DOORS BEGINS.')
    call rule()

    ! 1. The ground.
    call deploy_preflight(ready, why)
    if (.not. ready) then
      call lament(why)
      return
    end if
    rr = run_cmd('gh auth status')
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call lament('GITHUB DOES NOT KNOW YOU. THE CLI KEEPS THE KEYS: gh auth login')
      return
    end if
    if (.not. curl_present()) then
      call lament('curl IS ABSENT; THE PUBLIC FABRIC COULD NOT BE VERIFIED.')
      return
    end if

    ! 2. The complete production path. Any refusal stops the rite.
    call say('THE PRODUCTION PATH IS WALKED IN FULL.')
    call run_build(codes(1))
    call run_generate(codes(2))
    call run_validate(codes(3))
    call run_test(codes(4))
    call run_confess(codes(5))
    why = gate_production(codes)
    if (len(why) > 0) then
      call lament('THE ' // why // ' STAGE REFUSED. THE DOORS STAY SHUT.')
      return
    end if

    ! 3-4. The deployable tree, and proof it carries the whole porch.
    call build_deploy_tree(tree, ok)
    if (.not. ok) then
      call lament('THE DEPLOYABLE TREE COULD NOT BE FORMED.')
      return
    end if
    rs = routes()
    call tree_manifest(tree, names, ok)
    if (.not. ok) then
      call lament('THE ARK COULD NOT BE READ BACK.')
      return
    end if
    do i = 1, size(rs)
      found = .false.
      do j = 1, size(names)
        if (names(j)%s == rs(i)%file) found = .true.
      end do
      if (.not. found) then
        call lament('THE ARK LACKS ' // rs(i)%file // '. THE OUTPUT IS INCOMPLETE.')
        return
      end if
    end do

    head = version_line('git rev-parse HEAD')
    src8 = head(1:8)
    call deploy_parent(parent, has_parent)
    needed = .true.
    if (has_parent) needed = deploy_needed(tree, parent)

    ! 6. The complete intended transaction.
    call blank()
    call say('THE DEPLOYMENT RITE:')
    call rule()
    call say('  SOURCE COMMIT (main):  ' // src8)
    call say('  DEPLOYABLE TREE:       ' // tree)
    if (has_parent) then
      call say('  PUBLIC LINE TIP:       ' // parent(1:8))
    else
      call say('  PUBLIC LINE TIP:       NONE. THIS IS THE FIRST OPENING.')
    end if
    if (needed) then
      call say('  1. SEAL.    git commit-tree (PARENT AS ABOVE)')
      call say('  2. LIFT.    git push origin (COMMIT):refs/heads/gh-pages')
    else
      call say('  1-2.        THE PUBLIC FABRIC ALREADY CARRIES THIS TREE; NO NEW SEAL.')
    end if
    call say('  3. APPOINT. gh api repos/{owner}/{repo}/pages (CONFIRM OR CREATE)')
    call say('  4. VERIFY.  ' // int_to_str(size(rs)) // ' ROUTES FETCHED FROM THE PUBLIC ADDRESS')
    call rule()
    call say('PUBLIC ADDRESS: ' // CANON_BASE_URL // '/')
    call blank()

    if (cli%dry_run) then
      call say('THE DOORS REMAIN CLOSED (--dry-run). NOTHING WAS MOVED.')
      exit_code = EXIT_OK
      return
    end if
    if (.not. confirm('OPEN THE DOORS TO THE PUBLIC?', cli%assume_yes)) then
      call say('THE DOORS REMAIN CLOSED. THE CATHEDRAL IS PATIENT.')
      exit_code = EXIT_DECLINED
      return
    end if

    ! 5-6. Seal and lift.
    if (needed) then
      call make_deploy_commit(tree, parent, has_parent, src8, commit, ok)
      if (.not. ok) then
        call lament('THE DEPLOYMENT COMMIT COULD NOT BE FORMED. NOTHING MOVED.')
        exit_code = EXIT_EXTERNAL
        return
      end if
      call say('SEALED: ' // commit)
      call push_deploy(commit, ok)
      if (.not. ok) then
        call lament('THE LIFT FAILED. THE PUBLIC LINE DID NOT MOVE.')
        exit_code = EXIT_EXTERNAL
        return
      end if
    else
      commit = parent
      call say('THE PUBLIC FABRIC IS ALREADY CURRENT AT ' // commit(1:8) // '.')
    end if

    ! 7. Appoint the Pages source through the platform's own gate.
    rr = run_cmd('gh api repos/{owner}/{repo}/pages')
    doc = joined_doc(rr)
    if (rr%exit_code /= 0) then
      call say('THE DOORS ARE NOT YET APPOINTED. APPOINTING...')
      rr = run_cmd('gh api repos/{owner}/{repo}/pages -X POST ' // &
                   '-f "source[branch]=gh-pages" -f "source[path]=/"')
      if (.not. rr%launched .or. rr%exit_code /= 0) then
        call lament('GITHUB REFUSED THE APPOINTMENT.')
        exit_code = EXIT_EXTERNAL
        return
      end if
    else
      call parse_pages_response(doc, html_url, pbranch, okj)
      if (okj .and. pbranch /= 'gh-pages') then
        call say('THE DOORS WERE APPOINTED TO ' // pbranch // '. CORRECTING...')
        rr = run_cmd('gh api repos/{owner}/{repo}/pages -X PUT ' // &
                     '-f "source[branch]=gh-pages" -f "source[path]=/"')
        if (.not. rr%launched .or. rr%exit_code /= 0) then
          call lament('GITHUB REFUSED THE CORRECTION.')
          exit_code = EXIT_EXTERNAL
          return
        end if
      end if
    end if

    ! 8. Verify the appointment and the canonical address.
    rr = run_cmd('gh api repos/{owner}/{repo}/pages')
    doc = joined_doc(rr)
    call parse_pages_response(doc, html_url, pbranch, okj)
    if (.not. okj .or. rr%exit_code /= 0) then
      call lament('THE APPOINTMENT COULD NOT BE VERIFIED.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    if (pbranch /= 'gh-pages') then
      call lament('THE PAGES SOURCE IS NOT gh-pages BUT: ' // pbranch)
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('APPOINTED: BRANCH gh-pages AT ' // html_url)
    if (.not. same_address(html_url, CANON_BASE_URL)) then
      call lament('THE PUBLIC ADDRESS DISAGREES WITH THE CANON: ' // html_url)
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('THE CANONICAL ADDRESS AGREES.')

    ! 9. The public fabric itself, fetched and read.
    call blank()
    call say('AWAITING THE PUBLIC FABRIC...')
    live = .false.
    do attempt = 1, 9
      if (fetch_ok(CANON_BASE_URL // '/index.html', doc)) then
        if (count_substr(doc, '<meta name="generator" content="FORTY ') >= 1) then
          live = .true.
          exit
        end if
      end if
      rr = run_cmd('ping -n 11 127.0.0.1')
    end do
    if (.not. live) then
      call say('THE DOORS ARE UNLOCKED AND APPOINTED, BUT THE PAGES FORGE')
      call say('STILL BURNS; THE PUBLIC FABRIC IS NOT YET SERVED. THIS IS')
      call say('PROPAGATION, NOT FAILURE. SUMMON forty deploy AGAIN SHORTLY;')
      call say('AN UNCHANGED CATHEDRAL WILL SKIP STRAIGHT TO VERIFICATION.')
      exit_code = EXIT_OK
      return
    end if
    do i = 1, size(rs)
      if (.not. fetch_ok(CANON_BASE_URL // '/' // rs(i)%file, doc)) then
        call lament('THE PUBLIC ' // rs(i)%file // ' DID NOT ANSWER.')
        exit_code = EXIT_EXTERNAL
        return
      end if
      if (count_substr(doc, '<meta name="generator" content="FORTY ') /= 1) then
        call lament('THE PUBLIC ' // rs(i)%file // ' LACKS THE VERGER''S MARK.')
        exit_code = EXIT_EXTERNAL
        return
      end if
      call say('  LIVE: ' // rs(i)%file)
    end do
    if (.not. fetch_ok(CANON_BASE_URL // '/assets/cathedral.css', doc)) then
      call lament('THE PUBLIC STYLESHEET DID NOT ANSWER.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('  LIVE: assets/cathedral.css')

    call blank()
    call say('THE CATHEDRAL DOORS ARE OPEN.')
    call say('THE PUBLIC FABRIC IS CANONICAL.')
    call say('ADDRESS: ' // html_url)
    call say('DEPLOYED COMMIT: ' // commit)
    exit_code = EXIT_OK
  end subroutine run_deploy

  ! ------------------------------------------------------------- helpers

  function curl_present() result(r)
    logical :: r
    type(run_result) :: rr
    rr = run_cmd('curl --version')
    r = rr%launched .and. rr%exit_code == 0
  end function curl_present

  function fetch_ok(url, doc) result(r)
    character(*), intent(in) :: url
    character(:), allocatable, intent(out) :: doc
    logical :: r
    type(run_result) :: rr
    type(string_t), allocatable :: body(:)
    character(:), allocatable :: cap
    integer :: i
    doc = ''
    cap = 'build\deploy\fetch.tmp'
    call delete_file(cap)
    rr = run_cmd('curl -s -f -o ' // quote(cap) // ' ' // url)
    r = rr%launched .and. rr%exit_code == 0
    if (.not. r) return
    call read_all_lines(cap, body)
    do i = 1, size(body)
      doc = doc // body(i)%s // achar(10)
    end do
    call delete_file(cap)
  end function fetch_ok

  function joined_doc(rr) result(doc)
    type(run_result), intent(in) :: rr
    character(:), allocatable :: doc
    integer :: i
    doc = ''
    do i = 1, size(rr%out)
      doc = doc // rr%out(i)%s // achar(10)
    end do
  end function joined_doc

  !> Two addresses agree when they differ by at most a trailing slash.
  pure function same_address(a, b) result(r)
    character(*), intent(in) :: a, b
    logical :: r
    character(:), allocatable :: x, y
    x = a
    y = b
    if (len(x) > 0) then
      if (x(len(x):len(x)) == '/') x = x(1:len(x) - 1)
    end if
    if (len(y) > 0) then
      if (y(len(y):len(y)) == '/') y = y(1:len(y) - 1)
    end if
    r = (x == y)
  end function same_address

end module forty_deploy
