!> cathedral_generate: the raising of the Cathedral. Orchestrates the
!> route registry, the content, the assembly, and the emission of every
!> asset into dist\ — which is ignored ground; the working tree stays
!> clean. Also keeps the porch door: forty open.
module cathedral_generate
  use forty_util, only: string_t, int_to_str
  use forty_ui, only: say, lament
  use forty_paths, only: file_exists, quote
  use forty_run, only: run_result, run_cmd, ensure_dir, write_lines
  use forty_canon, only: EXIT_OK, EXIT_FAIL, EXIT_EXTERNAL
  use cathedral_routes, only: route_t, routes
  use cathedral_facts, only: build_facts_t, gather_facts
  use cathedral_content, only: page_body
  use cathedral_pages, only: assemble_page
  use cathedral_style, only: tokens_css_lines, cathedral_css_lines
  use cathedral_assets, only: ornament_svg_lines, robots_lines, sitemap_lines, &
                              routes_json_lines
  implicit none
  private
  public :: run_generate, run_open

contains

  subroutine run_generate(exit_code)
    integer, intent(out) :: exit_code
    type(route_t), allocatable :: rs(:)
    type(build_facts_t) :: facts
    type(string_t), allocatable :: body(:), page(:), work(:)
    logical :: ok, known
    integer :: i

    exit_code = EXIT_FAIL
    rs = routes()
    call gather_facts(facts, size(rs))

    call say('RAISING THE CATHEDRAL...')
    call ensure_dir('dist')
    call ensure_dir('dist\assets')

    do i = 1, size(rs)
      call page_body(rs(i)%slug, facts, body, known)
      if (.not. known) then
        call lament('NO CONTENT IS CARVED FOR ROUTE: ' // rs(i)%slug)
        return
      end if
      call assemble_page(rs(i), body, facts, page)
      call write_lines('dist\' // rs(i)%file, page, ok)
      if (.not. ok) then
        call lament('THE PAGE COULD NOT BE LAID: dist\' // rs(i)%file)
        return
      end if
      call say('  INSCRIBED dist\' // rs(i)%file // ' (' // &
               int_to_str(size(page)) // ' LINES).')
    end do

    call tokens_css_lines(work)
    if (.not. emit('dist\assets\tokens.css', work)) return
    call cathedral_css_lines(work)
    if (.not. emit('dist\assets\cathedral.css', work)) return
    call ornament_svg_lines(work)
    if (.not. emit('dist\assets\ornament.svg', work)) return
    call robots_lines(work)
    if (.not. emit('dist\robots.txt', work)) return
    call sitemap_lines(work)
    if (.not. emit('dist\sitemap.xml', work)) return
    call routes_json_lines(work, facts%generator)
    if (.not. emit('dist\routes.json', work)) return
    ! An empty .nojekyll asks GitHub Pages to serve the porch as laid,
    ! with no Jekyll pass. The emptiest file Forty has ever inscribed.
    if (allocated(work)) deallocate (work)
    allocate (work(0))
    if (.not. emit('dist\.nojekyll', work)) return

    call say('THE CATHEDRAL IS RAISED: ' // int_to_str(size(rs)) // &
             ' PAGES, 7 WORKS, 0 LINES OF JAVASCRIPT.')
    exit_code = EXIT_OK
  end subroutine run_generate

  !> Print the porch's true address, then open the doors with the
  !> visitor's own browser. The site is plain files; no server attends.
  subroutine run_open(exit_code)
    integer, intent(out) :: exit_code
    type(run_result) :: rr
    character(:), allocatable :: porch

    if (.not. file_exists('dist\index.html')) then
      call lament('THE PORCH IS NOT BUILT. RAISE IT FIRST: forty generate')
      exit_code = EXIT_FAIL
      return
    end if
    rr = run_cmd('cd')
    if (.not. rr%launched .or. rr%exit_code /= 0 .or. size(rr%out) == 0) then
      call lament('THE GROUND COULD NOT BE NAMED.')
      exit_code = EXIT_FAIL
      return
    end if
    porch = rr%out(1)%s // '\dist\index.html'
    call say('THE PORCH: ' // porch)
    rr = run_cmd('start "" ' // quote(porch))
    if (.not. rr%launched .or. rr%exit_code /= 0) then
      call say('THE DOORS DID NOT SWING; ENTER BY THE PATH ABOVE.')
      exit_code = EXIT_EXTERNAL
      return
    end if
    call say('THE DOORS ARE OPEN.')
    exit_code = EXIT_OK
  end subroutine run_open

  function emit(path, lines) result(ok)
    character(*), intent(in) :: path
    type(string_t), intent(in) :: lines(:)
    logical :: ok
    call write_lines(path, lines, ok)
    if (ok) then
      call say('  INSCRIBED ' // path // ' (' // int_to_str(size(lines)) // ' LINES).')
    else
      call lament('THE WORK COULD NOT BE LAID: ' // path)
    end if
  end function emit

end module cathedral_generate
