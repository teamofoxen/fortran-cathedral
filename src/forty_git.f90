!> forty_git: small readings of the local repository's soul.
module forty_git
  use forty_util, only: string_t
  use forty_paths, only: file_exists
  use forty_run, only: run_result, run_cmd, version_line
  implicit none
  private
  public :: git_initialized, git_branch, git_remote_url, slug_from_url, valid_slug

contains

  function git_initialized() result(r)
    logical :: r
    r = file_exists('.git\HEAD')
  end function git_initialized

  function git_branch() result(branch)
    character(:), allocatable :: branch
    branch = version_line('git rev-parse --abbrev-ref HEAD')
  end function git_branch

  subroutine git_remote_url(found, url)
    logical, intent(out) :: found
    character(:), allocatable, intent(out) :: url
    type(run_result) :: rr
    found = .false.
    url = ''
    rr = run_cmd('git remote get-url origin')
    if (rr%launched .and. rr%exit_code == 0 .and. size(rr%out) > 0) then
      url = trim(rr%out(1)%s)
      found = (len(url) > 0)
    end if
  end subroutine git_remote_url

  !> owner/name from an https or ssh GitHub URL; empty if not GitHub.
  pure function slug_from_url(url) result(slug)
    character(*), intent(in) :: url
    character(:), allocatable :: slug
    integer :: p
    character(:), allocatable :: rest
    slug = ''
    p = index(url, 'github.com')
    if (p == 0) return
    rest = url(p + len('github.com'):)
    if (len(rest) < 2) return
    if (rest(1:1) /= '/' .and. rest(1:1) /= ':') return
    rest = rest(2:)
    if (len(rest) >= 4) then
      if (rest(len(rest) - 3:) == '.git') rest = rest(:len(rest) - 4)
    end if
    do while (len(rest) > 0)
      if (rest(len(rest):len(rest)) == '/') then
        rest = rest(:len(rest) - 1)
      else
        exit
      end if
    end do
    slug = rest
  end function slug_from_url

  !> A slug fit to pass to gh: owner/name in modest characters only.
  pure function valid_slug(s) result(ok)
    character(*), intent(in) :: s
    logical :: ok
    integer :: i, c, slashes
    ok = .false.
    if (len(s) < 3 .or. len(s) > 141) return
    slashes = 0
    do i = 1, len(s)
      c = iachar(s(i:i))
      if (s(i:i) == '/') then
        slashes = slashes + 1
      else if (.not. ((c >= iachar('0') .and. c <= iachar('9')) .or. &
                      (c >= iachar('a') .and. c <= iachar('z')) .or. &
                      (c >= iachar('A') .and. c <= iachar('Z')) .or. &
                      s(i:i) == '.' .or. s(i:i) == '_' .or. s(i:i) == '-')) then
        return
      end if
    end do
    ok = (slashes == 1 .and. s(1:1) /= '/' .and. s(len(s):len(s)) /= '/')
  end function valid_slug

end module forty_git
