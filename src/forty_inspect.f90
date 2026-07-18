!> forty_inspect: the browser tape measure. There is no practical need
!> for a Fortran program that measures rendered web pages, which is
!> precisely why the Cathedral owns one.
!>
!> The platform boundary, documented honestly: an installed Chromium
!> browser (Edge or Chrome) is invoked headless with its official
!> debugging flags (--headless=new --dump-dom --virtual-time-budget).
!> Because layout facts only exist inside a rendering engine, Forty
!> GENERATES a temporary measurement harness — an ignored file under
!> build\inspect\ containing the page-evaluation expressions the
!> protocol requires — sends it through the browser, then parses and
!> judges every number itself. The harness is emitted at run time from
!> the strings in this module; it is never tracked application logic.
!> Fortran owns launching, route and viewport selection, the requests,
!> the capture, the parsing, the thresholds, the failures, and the
!> final verdict. The browser owns being a browser.
module forty_inspect
  use forty_util, only: string_t, push_string, int_to_str, count_substr, &
                        starts_with, to_lower
  use forty_ui, only: say, lament, rule, blank
  use forty_run, only: run_result, run_cmd, version_line, write_lines, &
                       ensure_dir, delete_file
  use forty_paths, only: quote, file_exists
  use forty_cli, only: cli_t
  use cathedral_routes, only: route_t, routes
  use forty_canon, only: CANON_BASE_URL, FORTY_VERSION, &
                         EXIT_OK, EXIT_FAIL, EXIT_USAGE, EXIT_ENV
  implicit none
  private
  public :: run_inspect
  public :: measure_t, harness_lines, browser_command, browser_locate, &
            path_to_url, parse_measure, judge_page, render_inspection

  type :: measure_t
    logical :: ok = .false.
    character(:), allocatable :: errmsg
    character(:), allocatable :: title, active, marker, h1, fontsize
    character(:), allocatable :: links, sheetrules
    integer :: scrollw = -1, clientw = -1, sheets = -1, nav = -1
    integer :: svgs = -1, tables = -1, pres = -1
    logical :: overflow = .true.
    logical :: pres_contained = .false.
  end type measure_t

contains

  ! ------------------------------------------------------- the harness

  !> The measurement harness: an iframe of the target at the chosen
  !> width, and the evaluation expressions that read the rendering
  !> engine's own numbers into a parseable block.
  subroutine harness_lines(target_url, width, lines)
    character(*), intent(in) :: target_url
    integer, intent(in) :: width
    type(string_t), allocatable, intent(out) :: lines(:)
    allocate (lines(0))
    call push_string(lines, '<!doctype html>')
    call push_string(lines, '<html><head><meta charset="utf-8"></head><body>')
    call push_string(lines, '<iframe id="t" src="' // target_url // &
                     '" style="width:' // int_to_str(width) // &
                     'px;height:900px;border:0"></iframe>')
    call push_string(lines, '<pre id="out"></pre>')
    call push_string(lines, '<script>')
    call push_string(lines, 'var f=document.getElementById("t");')
    call push_string(lines, 'function emit(s){document.getElementById("out")' // &
                     '.textContent="FORTY-MEASURE-BEGIN\n"+s+"\nFORTY-MEASURE-END";}')
    call push_string(lines, 'function fail(m){emit("OK: NO\nERR: "+m);}')
    call push_string(lines, 'f.addEventListener("load",function(){' // &
                     'setTimeout(function(){try{')
    call push_string(lines, 'var d=f.contentDocument,de=d.documentElement;')
    call push_string(lines, 'var pres=d.querySelectorAll("pre");var cont="YES";')
    call push_string(lines, 'for(var i=0;i<pres.length;i++){var cs=' // &
                     'getComputedStyle(pres[i]).overflowX;' // &
                     'if(!(cs==="auto"||cs==="scroll")){cont="NO";}}')
    call push_string(lines, 'var act=d.querySelector(' // &
                     '''nav a[aria-current="page"]'');')
    call push_string(lines, 'var mk=d.querySelector(''meta[name="generator"]'');')
    call push_string(lines, 'var rules=[];for(var i=0;i<d.styleSheets.length;' // &
                     'i++){try{rules.push(d.styleSheets[i].cssRules.length);}' // &
                     'catch(e){rules.push(-1);}}')
    call push_string(lines, 'var links=[];var as=d.querySelectorAll("a");' // &
                     'for(var i=0;i<as.length;i++){var h=as[i]' // &
                     '.getAttribute("href")||"";' // &
                     'if(h.indexOf("://")<0&&h.charAt(0)!=="#"&&' // &
                     'h.indexOf(".html")>=0){links.push(h.split("#")[0]);}}')
    call push_string(lines, 'var L=[];')
    call push_string(lines, 'L.push("OK: YES");')
    call push_string(lines, 'L.push("TITLE: "+d.title);')
    call push_string(lines, 'L.push("SCROLLW: "+de.scrollWidth);')
    call push_string(lines, 'L.push("CLIENTW: "+de.clientWidth);')
    call push_string(lines, 'L.push("OVERFLOW: "+((de.scrollWidth>' // &
                     'de.clientWidth+1)?"YES":"NO"));')
    call push_string(lines, 'L.push("SHEETS: "+d.styleSheets.length);')
    call push_string(lines, 'L.push("SHEETRULES: "+rules.join(","));')
    call push_string(lines, 'L.push("NAV: "+d.querySelectorAll("nav a").length);')
    call push_string(lines, 'L.push("ACTIVE: "+(act?act.textContent:""));')
    call push_string(lines, 'L.push("MARKER: "+(mk?mk.content:""));')
    call push_string(lines, 'L.push("SVGS: "+d.querySelectorAll("main svg").length);')
    call push_string(lines, 'L.push("TABLES: "+d.querySelectorAll("table").length);')
    call push_string(lines, 'L.push("PRES: "+pres.length);')
    call push_string(lines, 'L.push("PRES-CONTAINED: "+(pres.length?cont:"YES"));')
    call push_string(lines, 'L.push("H1: "+(d.querySelector("h1")||' // &
                     '{textContent:""}).textContent);')
    call push_string(lines, 'L.push("FONTSIZE: "+getComputedStyle(d.body).fontSize);')
    call push_string(lines, 'L.push("LINKS: "+links.join("|"));')
    call push_string(lines, 'emit(L.join("\n"));')
    call push_string(lines, '}catch(e){fail(String(e));}},600);});')
    call push_string(lines, 'setTimeout(function(){if(document.getElementById' // &
                     '("out").textContent===""){fail("TIMEOUT");}},5000);')
    call push_string(lines, '</script>')
    call push_string(lines, '</body></html>')
  end subroutine harness_lines

  !> The browser invocation. The `call` prefix keeps cmd.exe from its
  !> infamous leading-quote stripping, which would otherwise eat the
  !> closing quote of the capture redirect appended downstream.
  function browser_command(exe, harness_url, profile_dir) result(cmd)
    character(*), intent(in) :: exe, harness_url, profile_dir
    character(:), allocatable :: cmd
    cmd = 'call "' // exe // '" --headless=new --disable-gpu' // &
          ' --no-first-run --no-default-browser-check' // &
          ' --allow-file-access-from-files --virtual-time-budget=6000' // &
          ' --user-data-dir="' // profile_dir // '"' // &
          ' --window-size=1366,1000 --dump-dom "' // harness_url // '"'
  end function browser_command

  !> Find an installed Chromium household: Edge first (it ships with
  !> Windows), then Chrome.
  subroutine browser_locate(exe, found)
    character(:), allocatable, intent(out) :: exe
    logical, intent(out) :: found
    character(64), parameter :: CANDIDATES(4) = [character(64) :: &
      'C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe', &
      'C:\Program Files\Microsoft\Edge\Application\msedge.exe', &
      'C:\Program Files\Google\Chrome\Application\chrome.exe', &
      'C:\Program Files (x86)\Google\Chrome\Application\chrome.exe']
    integer :: i
    exe = ''
    found = .false.
    do i = 1, size(CANDIDATES)
      if (file_exists(trim(CANDIDATES(i)))) then
        exe = trim(CANDIDATES(i))
        found = .true.
        return
      end if
    end do
  end subroutine browser_locate

  pure function path_to_url(path) result(url)
    character(*), intent(in) :: path
    character(:), allocatable :: url
    integer :: i
    url = ''
    do i = 1, len(path)
      select case (path(i:i))
      case ('\'); url = url // '/'
      case (' '); url = url // '%20'
      case default; url = url // path(i:i)
      end select
    end do
    url = 'file:///' // url
  end function path_to_url

  ! ------------------------------------------------------- the parsing

  subroutine parse_measure(out_lines, m)
    type(string_t), intent(in) :: out_lines(:)
    type(measure_t), intent(out) :: m
    integer :: i, ib, ie
    character(:), allocatable :: line
    m%errmsg = ''
    m%title = ''; m%active = ''; m%marker = ''; m%h1 = ''
    m%fontsize = ''; m%links = ''; m%sheetrules = ''
    ! The dumped page contains the harness's own script source, whose
    ! emit() line carries BOTH markers at once. The true block is the
    ! first BEGIN that stands alone, closed by the first END after it.
    ib = 0; ie = 0
    do i = 1, size(out_lines)
      if (index(out_lines(i)%s, 'FORTY-MEASURE-BEGIN') > 0 .and. &
          index(out_lines(i)%s, 'FORTY-MEASURE-END') == 0) then
        ib = i
        exit
      end if
    end do
    if (ib > 0) then
      do i = ib + 1, size(out_lines)
        if (index(out_lines(i)%s, 'FORTY-MEASURE-END') > 0) then
          ie = i
          exit
        end if
      end do
    end if
    if (ib == 0 .or. ie == 0 .or. ie <= ib) then
      m%ok = .false.
      m%errmsg = 'NO MEASUREMENT BLOCK WAS RETURNED'
      return
    end if
    m%ok = .true.
    do i = ib + 1, ie - 1
      line = trim(out_lines(i)%s)
      if (starts_with(line, 'OK: ')) then
        if (line(5:) /= 'YES') m%ok = .false.
      else if (starts_with(line, 'ERR: ')) then
        m%errmsg = line(6:)
      else if (starts_with(line, 'TITLE: ')) then
        m%title = line(8:)
      else if (starts_with(line, 'SCROLLW: ')) then
        m%scrollw = to_int(line(10:))
      else if (starts_with(line, 'CLIENTW: ')) then
        m%clientw = to_int(line(10:))
      else if (starts_with(line, 'OVERFLOW: ')) then
        m%overflow = (line(11:) == 'YES')
      else if (starts_with(line, 'SHEETS: ')) then
        m%sheets = to_int(line(9:))
      else if (starts_with(line, 'SHEETRULES: ')) then
        m%sheetrules = line(13:)
      else if (starts_with(line, 'NAV: ')) then
        m%nav = to_int(line(6:))
      else if (starts_with(line, 'ACTIVE: ')) then
        m%active = line(9:)
      else if (starts_with(line, 'MARKER: ')) then
        m%marker = line(9:)
      else if (starts_with(line, 'SVGS: ')) then
        m%svgs = to_int(line(7:))
      else if (starts_with(line, 'TABLES: ')) then
        m%tables = to_int(line(9:))
      else if (starts_with(line, 'PRES: ')) then
        m%pres = to_int(line(7:))
      else if (starts_with(line, 'PRES-CONTAINED: ')) then
        m%pres_contained = (line(17:) == 'YES')
      else if (starts_with(line, 'H1: ')) then
        m%h1 = line(5:)
      else if (starts_with(line, 'FONTSIZE: ')) then
        m%fontsize = line(11:)
      else if (starts_with(line, 'LINKS: ')) then
        m%links = line(8:)
      end if
    end do
  end subroutine parse_measure

  function to_int(s) result(n)
    character(*), intent(in) :: s
    integer :: n, ios
    read (s, *, iostat=ios) n
    if (ios /= 0) n = -1
  end function to_int

  ! ------------------------------------------------------ the judgment

  !> Judge one page at one width. Failures are appended; checks counted.
  subroutine judge_page(m, rt, width, route_count, min_svgs, min_tables, &
                        n_checks, failures)
    type(measure_t), intent(in) :: m
    type(route_t), intent(in) :: rt
    integer, intent(in) :: width, route_count, min_svgs, min_tables
    integer, intent(inout) :: n_checks
    type(string_t), allocatable, intent(inout) :: failures(:)
    character(:), allocatable :: tag, expect_size, rest
    integer :: p
    logical :: links_ok

    tag = rt%file // ' @ ' // int_to_str(width) // 'px: '
    call verdict_on(m%ok, tag // 'THE MEASUREMENT RETURNED (' // &
                    trim(m%errmsg) // ')', n_checks, failures)
    if (.not. m%ok) return
    call verdict_on(.not. m%overflow, tag // 'NO PAGE-LEVEL HORIZONTAL ' // &
                    'OVERFLOW (SCROLLW ' // int_to_str(m%scrollw) // ')', &
                    n_checks, failures)
    call verdict_on(m%clientw <= width .and. m%clientw >= width - 30, &
                    tag // 'THE VIEWPORT WIDTH IS HONORED (' // &
                    int_to_str(m%clientw) // ')', n_checks, failures)
    call verdict_on(m%sheets == 2 .and. index(m%sheetrules, '-1') == 0 .and. &
                    len(m%sheetrules) > 0, &
                    tag // 'BOTH STYLESHEETS LOADED WITH RULES (' // &
                    m%sheetrules // ')', n_checks, failures)
    call verdict_on(m%nav == route_count, tag // 'THE NAV CARRIES ALL ' // &
                    int_to_str(route_count) // ' DOORS', n_checks, failures)
    call verdict_on(m%active == rt%nav, tag // 'THE ACTIVE DOOR IS ' // &
                    rt%nav, n_checks, failures)
    call verdict_on(starts_with(m%marker, 'FORTY '), tag // &
                    'THE VERGER''S MARK IS PRESENT (' // m%marker // ')', &
                    n_checks, failures)
    call verdict_on(m%h1 == rt%title, tag // 'THE HEADING IS THE TITLE', &
                    n_checks, failures)
    call verdict_on(m%svgs >= min_svgs, tag // 'REQUIRED SVG RENDERED (' // &
                    int_to_str(m%svgs) // ' OF ' // int_to_str(min_svgs) // &
                    ')', n_checks, failures)
    call verdict_on(m%tables >= min_tables, tag // 'REQUIRED TABLES ' // &
                    'RENDERED (' // int_to_str(m%tables) // ' OF ' // &
                    int_to_str(min_tables) // ')', n_checks, failures)
    call verdict_on(m%pres_contained, tag // 'CODE BLOCKS SCROLL WITHIN ' // &
                    'THEIR OWN WALLS', n_checks, failures)
    if (width <= 600) then
      expect_size = '16px'
    else
      expect_size = '17px'
    end if
    call verdict_on(m%fontsize == expect_size, tag // 'THE RESPONSIVE ' // &
                    'TYPE SIZE IS ' // expect_size, n_checks, failures)
    ! Broken internal links are judged here, by Fortran, not in the page.
    links_ok = .true.
    rest = m%links
    do
      if (len(rest) == 0) exit
      p = index(rest, '|')
      if (p == 0) then
        if (.not. is_route_file(rest)) links_ok = .false.
        exit
      end if
      if (p > 1) then
        if (.not. is_route_file(rest(1:p - 1))) links_ok = .false.
      end if
      rest = rest(p + 1:)
    end do
    call verdict_on(links_ok, tag // 'EVERY INTERNAL DOOR LEADS TO A ' // &
                    'REGISTERED ROUTE', n_checks, failures)
  end subroutine judge_page

  function is_route_file(name) result(r)
    character(*), intent(in) :: name
    logical :: r
    type(route_t), allocatable :: rs(:)
    integer :: i
    rs = routes()
    r = .false.
    do i = 1, size(rs)
      if (name == rs(i)%file) r = .true.
    end do
  end function is_route_file

  subroutine verdict_on(cond, label, n_checks, failures)
    logical, intent(in) :: cond
    character(*), intent(in) :: label
    integer, intent(inout) :: n_checks
    type(string_t), allocatable, intent(inout) :: failures(:)
    n_checks = n_checks + 1
    if (.not. cond) call push_string(failures, label)
  end subroutine verdict_on

  subroutine render_inspection(target, n_pages, n_checks, failures, lines)
    character(*), intent(in) :: target
    integer, intent(in) :: n_pages, n_checks
    type(string_t), intent(in) :: failures(:)
    type(string_t), allocatable, intent(out) :: lines(:)
    integer :: i
    allocate (lines(0))
    call push_string(lines, repeat('=', 70))
    call push_string(lines, 'THE MEASUREMENT OF THE FABRIC — ' // target)
    call push_string(lines, repeat('=', 70))
    call push_string(lines, 'PAGE-VIEWPORT MEASUREMENTS: ' // int_to_str(n_pages))
    call push_string(lines, 'CHECKS APPLIED: ' // int_to_str(n_checks))
    call push_string(lines, 'FAULTS: ' // int_to_str(size(failures)))
    do i = 1, size(failures)
      call push_string(lines, '  FAULT: ' // failures(i)%s)
    end do
    if (size(failures) == 0) then
      call push_string(lines, 'THE FABRIC HAS BEEN MEASURED.')
      call push_string(lines, 'NO STONE PROJECTS BEYOND THE NAVE.')
    end if
  end subroutine render_inspection

  ! -------------------------------------------------------- the rite

  subroutine run_inspect(cli, exit_code)
    type(cli_t), intent(in) :: cli
    integer, intent(out) :: exit_code
    type(route_t), allocatable :: rs(:)
    type(string_t), allocatable :: hl(:), failures(:), report(:)
    type(run_result) :: rr
    type(measure_t) :: m
    character(:), allocatable :: target, exe, root, base_dir, harness, url
    logical :: found, ok
    integer :: widths(2), i, w, n_checks, n_pages
    integer :: min_svgs, min_tables

    target = cli%rite
    if (len(target) == 0) target = 'local'
    if (target /= 'local' .and. target /= 'public') then
      call lament('UNKNOWN INSPECTION TARGET: ' // target)
      call say('KNOWN TARGETS: local | public')
      exit_code = EXIT_USAGE
      return
    end if

    call say('THE TAPE MEASURE IS UNROLLED (' // target // ').')
    call rule()
    call ensure_dir('build')
    call ensure_dir('build\inspect')
    call ensure_dir('build\inspect\profile')
    rr = run_cmd('cd')
    root = rr%out(1)%s

    call browser_locate(exe, found)
    if (.not. found) then
      call lament('NO CHROMIUM HOUSEHOLD WAS FOUND. THE TAPE CANNOT UNROLL.')
      exit_code = EXIT_ENV
      return
    end if
    call say('THE BROWSER SERVES: ' // exe)

    rs = routes()
    if (target == 'local') then
      if (.not. file_exists('dist\index.html')) then
        call lament('THE PORCH IS NOT BUILT. RAISE IT FIRST: forty generate')
        exit_code = EXIT_FAIL
        return
      end if
      base_dir = root // '\dist'
    else
      call ensure_dir('build\inspect\mirror')
      call ensure_dir('build\inspect\mirror\assets')
      do i = 1, size(rs)
        rr = run_cmd('curl -s -f -o ' // &
                     quote(root // '\build\inspect\mirror\' // rs(i)%file) // &
                     ' ' // CANON_BASE_URL // '/' // rs(i)%file)
        if (rr%exit_code /= 0) then
          call lament('THE PUBLIC ' // rs(i)%file // ' COULD NOT BE FETCHED.')
          exit_code = EXIT_FAIL
          return
        end if
      end do
      rr = run_cmd('curl -s -f -o ' // &
                   quote(root // '\build\inspect\mirror\assets\tokens.css') // &
                   ' ' // CANON_BASE_URL // '/assets/tokens.css')
      rr = run_cmd('curl -s -f -o ' // &
                   quote(root // '\build\inspect\mirror\assets\cathedral.css') // &
                   ' ' // CANON_BASE_URL // '/assets/cathedral.css')
      rr = run_cmd('curl -s -f -o ' // &
                   quote(root // '\build\inspect\mirror\assets\ornament.svg') // &
                   ' ' // CANON_BASE_URL // '/assets/ornament.svg')
      call say('THE PUBLIC FABRIC IS FETCHED AND LAID OUT FOR MEASUREMENT.')
      base_dir = root // '\build\inspect\mirror'
    end if

    widths(1) = 1280
    widths(2) = 375
    allocate (failures(0))
    n_checks = 0
    n_pages = 0
    harness = root // '\build\inspect\harness.html'
    do i = 1, size(rs)
      select case (rs(i)%slug)
      case ('blas');         min_svgs = 3; min_tables = 9
      case ('confessional'); min_svgs = 0; min_tables = 2
      case ('why');          min_svgs = 0; min_tables = 1
      case default;          min_svgs = 0; min_tables = 0
      end select
      do w = 1, size(widths)
        url = path_to_url(base_dir // '\' // rs(i)%file)
        call harness_lines(url, widths(w), hl)
        call write_lines(harness, hl, ok)
        if (.not. ok) then
          call lament('THE HARNESS COULD NOT BE LAID.')
          exit_code = EXIT_FAIL
          return
        end if
        ! Each summons receives a fresh profile chamber; Chromium holds
        ! locks on a shared one and rapid successive launches collide.
        call ensure_dir('build\inspect\profile\p' // int_to_str(n_pages + 1))
        rr = run_cmd(browser_command(exe, path_to_url(harness), &
                                     root // '\build\inspect\profile\p' // &
                                     int_to_str(n_pages + 1)))
        if (.not. rr%launched) then
          call lament('THE BROWSER COULD NOT BE SUMMONED.')
          exit_code = EXIT_ENV
          return
        end if
        call parse_measure(rr%out, m)
        n_pages = n_pages + 1
        call judge_page(m, rs(i), widths(w), size(rs), min_svgs, min_tables, &
                        n_checks, failures)
        call say('  MEASURED: ' // rs(i)%file // ' @ ' // &
                 int_to_str(widths(w)) // 'px')
      end do
    end do

    call render_inspection(target, n_pages, n_checks, failures, report)
    call write_lines('build\inspect\inspection.txt', report, ok)
    call rule()
    call say('MEASUREMENTS: ' // int_to_str(n_pages) // '.  CHECKS: ' // &
             int_to_str(n_checks) // '.  FAULTS: ' // &
             int_to_str(size(failures)) // '.')
    call say('THE FULL TAPE RESTS AT build\inspect\inspection.txt')
    if (size(failures) > 0) then
      do i = 1, min(size(failures), 12)
        call lament('FAULT: ' // failures(i)%s)
      end do
      exit_code = EXIT_FAIL
      return
    end if
    call blank()
    call say('THE FABRIC HAS BEEN MEASURED.')
    call say('NO STONE PROJECTS BEYOND THE NAVE.')
    exit_code = EXIT_OK
  end subroutine run_inspect

end module forty_inspect
