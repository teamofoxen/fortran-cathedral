!> cathedral_assets: the small works. A rose window computed with real
!> trigonometry, a robots.txt, a sitemap, and a route manifest — each
!> emitted by Fortran because the Mandate is clear about the dumb jobs.
module cathedral_assets
  use, intrinsic :: iso_fortran_env, only: real64
  use forty_util, only: string_t, push_string, int_to_str
  use forty_canon, only: CANON_BASE_URL
  use cathedral_routes, only: route_t, routes
  use cathedral_style, only: ACCENT_HEX
  use cathedral_html, only: escape_json
  implicit none
  private
  public :: ornament_svg_lines, robots_lines, sitemap_lines, routes_json_lines

contains

  !> A rose window in the punched-card manner. Twelve slots stand for
  !> the twelve columns a card could spare; eight bores are placed by
  !> sine and cosine, because this site does not fake its numerics.
  subroutine ornament_svg_lines(lines)
    type(string_t), allocatable, intent(out) :: lines(:)
    real(real64), parameter :: PI = 3.14159265358979324_real64
    real(real64) :: a, x, y
    integer :: k
    allocate (lines(0))
    call push_string(lines, '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 128 128">')
    call push_string(lines, '  <title>Rose window of the Fortran Cathedral</title>')
    call push_string(lines, '  <circle cx="64" cy="64" r="58" fill="none" stroke="' // &
                     ACCENT_HEX // '" stroke-width="4"/>')
    call push_string(lines, '  <circle cx="64" cy="64" r="42" fill="none" stroke="' // &
                     ACCENT_HEX // '" stroke-width="2"/>')
    do k = 0, 11
      call push_string(lines, '  <rect x="61" y="12" width="6" height="15" rx="2" fill="' // &
                       ACCENT_HEX // '" transform="rotate(' // int_to_str(k * 30) // &
                       ' 64 64)"/>')
    end do
    do k = 0, 7
      a = real(k, real64) * (PI / 4.0_real64)
      x = 64.0_real64 + 28.0_real64 * sin(a)
      y = 64.0_real64 - 28.0_real64 * cos(a)
      call push_string(lines, '  <circle cx="' // fmt_real(x) // '" cy="' // &
                       fmt_real(y) // '" r="3.5" fill="' // ACCENT_HEX // '"/>')
    end do
    call push_string(lines, '  <circle cx="64" cy="64" r="8" fill="' // ACCENT_HEX // '"/>')
    call push_string(lines, '</svg>')
  end subroutine ornament_svg_lines

  subroutine robots_lines(lines)
    type(string_t), allocatable, intent(out) :: lines(:)
    allocate (lines(0))
    call push_string(lines, '# EMITTED BY FORTY. ALL CRAWLERS ARE WELCOME IN THE NAVE.')
    call push_string(lines, 'User-agent: *')
    call push_string(lines, 'Allow: /')
    call push_string(lines, 'Sitemap: ' // CANON_BASE_URL // '/sitemap.xml')
  end subroutine robots_lines

  subroutine sitemap_lines(lines)
    type(string_t), allocatable, intent(out) :: lines(:)
    type(route_t), allocatable :: rs(:)
    integer :: i
    rs = routes()
    allocate (lines(0))
    call push_string(lines, '<?xml version="1.0" encoding="UTF-8"?>')
    call push_string(lines, '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">')
    do i = 1, size(rs)
      call push_string(lines, '  <url>')
      call push_string(lines, '    <loc>' // CANON_BASE_URL // '/' // rs(i)%file // '</loc>')
      call push_string(lines, '  </url>')
    end do
    call push_string(lines, '</urlset>')
  end subroutine sitemap_lines

  subroutine routes_json_lines(lines, generator)
    type(string_t), allocatable, intent(out) :: lines(:)
    character(*), intent(in) :: generator
    type(route_t), allocatable :: rs(:)
    integer :: i
    character(:), allocatable :: closer
    rs = routes()
    allocate (lines(0))
    call push_string(lines, '{')
    call push_string(lines, '  "generator": "' // escape_json(generator) // '",')
    call push_string(lines, '  "base": "' // escape_json(CANON_BASE_URL) // '",')
    call push_string(lines, '  "routes": [')
    do i = 1, size(rs)
      if (i < size(rs)) then
        closer = '    },'
      else
        closer = '    }'
      end if
      call push_string(lines, '    {')
      call push_string(lines, '      "slug": "' // escape_json(rs(i)%slug) // '",')
      call push_string(lines, '      "file": "' // escape_json(rs(i)%file) // '",')
      call push_string(lines, '      "title": "' // escape_json(rs(i)%title) // '"')
      call push_string(lines, closer)
    end do
    call push_string(lines, '  ]')
    call push_string(lines, '}')
  end subroutine routes_json_lines

  function fmt_real(v) result(r)
    real(real64), intent(in) :: v
    character(:), allocatable :: r
    character(32) :: buf
    write (buf, '(f0.2)') v
    r = trim(buf)
  end function fmt_real

end module cathedral_assets
