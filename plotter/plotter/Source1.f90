module m_mandel
  implicit none  
  integer, private, parameter :: kd = SELECTED_REAL_KIND(15)
contains
  pure elemental integer function imandel(z)
    complex(kd), intent(in) :: z
    complex(kd) :: c
    c = (0.0_kd, 0.0_kd)
    do imandel = 150, 1, -1 
      if (abs(c) > 2.0_kd) exit
      c = c * c - z 
    end do
    return
  end function imandel
end module m_mandel

module m_jacobi
  implicit none
contains
  subroutine laplace(v) 
    real, intent(out) :: v(-50:50, -50:50)
    logical :: mask(lbound(v, 1):ubound(v, 1), lbound(v, 2):ubound(v, 2))
    integer :: i, j, iter
    mask = .true.
    mask(-10, -5:5) = .false. ! static voltage
    mask( 10, -5:5) = .false. 
    v = 0.0
    v(-10, -5:5) =  10.0 
    v( 10, -5:5) = -10.0
  !
    do iter = 1, 200 
      forall (i = lbound(v, 1) + 1:ubound(v, 1) - 1, j = lbound(v, 2) + 1:ubound(v, 2) - 1, mask(i, j)) 
        v(i, j) = 0.25 * ( v(i - 1, j) + v(i + 1, j) + v(i, j - 1) + v(i, j + 1) )
      end forall
    end do   
    return
  end subroutine laplace
  
end module m_jacobi
!==================================================================
 
program Mandel
  use m_plot
  use m_mandel
  use m_jacobi
  implicit none
  integer, parameter :: kd = SELECTED_REAL_KIND(15)
  integer, parameter :: m = 1000
  integer :: nwinx = 800, nwiny = 600
  integer :: i, j, k, imax, jmax, maxiter, icount, ix, iy
  real (kd) :: xmin, xmax, ymin, ymax 
  real (kd) :: xmin1, xmax1, ymin1, ymax1 
  real (kd) :: x, y, z, dx, dy, dz, a, b, c, d, p
  real (kd) :: t0, t1 
  real :: v(-50:50, -50:50)
  integer, allocatable :: ic(:, :)
  integer :: icol(0:m), it0, it1

  class(t_device), allocatable :: fig1, fig2, fig3, fig4, fig5 
  type(t_rgb), parameter :: rgb_black = t_rgb(0, 0, 0)
!
  xmin = -2.0d0 
  xmax =  2.0d0 
  ymin = -2.0d0 
  ymax =  2.0d0  
  maxiter = 150
!
  dx = xmax - xmin
  dy = ymax - ymin
  if (dx <= 0.0_kd .OR. dy <= 0.0_kd .OR. maxiter <= 0 .OR. maxiter > M) stop 'input error'
  if (dx * nwinx > dy * nwiny) then
    imax = nwinx
    jmax = nint(nwinx * dy / dx)
  else
    imax = nint(nwiny * dx / dy)
    jmax = int(nwiny)
  end if
!
  dx = dx / real(imax, kd)
  dy = dy / real(jmax, kd)
  icol(0) = 0 ! black
  j = irgb(255, 255, 255)
  do i = maxiter, 1, -1
   icol(i) = j 
   if (j > 1) j = j - irgb(255, 255, 255) / maxiter
  end do
!
  allocate( ic(0:imax, 0:jmax) )  
  call system_clock(it0)
  call cpu_time(t0)
  do concurrent (i = 0:imax, j = 0:jmax) 
    x = xmin + i * dx
    y = ymax - j * dy
    ic(i, j) = imandel(cmplx(x, y, kd))
  end do
  call cpu_time(t1)
  call system_clock(it1)
  print *, ' do concurrent time =', t1 - t0, it1 - it0
!
  allocate(fig1, source = t_win32('Mandelbrot 1', imax, jmax, 1, rgb_black))
  call fig1%on()
  do i = 0, imax
    do j = 0, jmax
      call fig1%dot(i, j, icol(ic(i, j)))  
    end do
    call fig1%show()
  end do
  deallocate( ic )
!
  xmin = 1.10950d0
  xmax = 1.10951d0
  ymin = 0.24758d0 
  ymax = 0.24759d0 
!
  dx = xmax - xmin
  dy = ymax - ymin
  if (dx <= 0.0_kd .OR. dy <= 0.0_kd .OR. maxiter <= 0 .OR. maxiter > M) stop 'input error'
  if (dx * nwinx > dy * nwiny) then
    imax = nwinx
    jmax = nint(nwinx * dy / dx)
  else
    imax = nint(nwiny * dx / dy)
    jmax = int(nwiny)
  end if
!
  dx = dx / real(imax, kd)
  dy = dy / real(jmax, kd)
  allocate( ic(0:imax, 0:jmax) )  
  call system_clock(it0)
  call cpu_time(t0)
  do concurrent (i = 0:imax, j = 0:jmax) 
    x = xmin + i * dx
    y = ymax - j * dy
    ic(i, j) = imandel(cmplx(x, y, kd))
  end do
  call cpu_time(t1)
  call system_clock(it1)
  print *, ' do concurrent time =', t1 - t0, it1 - it0
!
  allocate(fig2, source = t_win32('Mandelbrot 2', imax, jmax, 1, rgb_black))
  call fig2%on()
  do i = 0, imax
    do j = 0, jmax
      call fig2%dot(i, j, icol(ic(i, j)))  
    end do
    call fig2%show()
  end do
!
  allocate(fig3, source = t_win32('Lorentz attractor', 800, 600, 1, rgb_black))
  call fig3%on()
  call fig3%pen(2, t_rgb(255, 125, 0))
  a = 10.0d0
  b = 28.0d0
  c = 8.0d0 / 3.0d0
  d = 0.01d0
  x = 1.0d0
  y = 1.0d0
  z = 1.0d0
  do k = 1, 3000
    dx = a * (y - x)
    dy = x * (b - z) - y
    dz = x * y - c * z
    x = x + d * dx
    y = y + d * dy
    z = z + d * dz
    if (k < 100) call fig3%moveTo( INT(12 * (x + 30)), INT(12 * z) )
    call fig3%lineTo( INT(12 * (x + 30)), INT(12 * z) )
    call fig3%show()
  end do
 ! 
  allocate(fig4, source = t_win32('Chaos', 640, 480, 1, rgb_black))
  call fig4%on()
  do ix = 1, 640
    p = 0.3
    x = ix * (3.0 - 1.5) / 640.0 + 1.5 
    do i = 1, 50
      p = p + x * p * (1.0 - p)
    end do
    do i = 51, 100
      y = p / 1.5 * 480.0
      call fig4%dot(ix, 480 - INT(y), irgb(0, 255, 122))
       p = p + x * p * (1 - p)
    end do
    call fig4%show()
  end do
  !
  allocate(fig5, source = t_win32('Laplace', 1200, 600, 1, rgb_black))
  call fig5%on()
  call laplace(v)
! x-direction  
  do i =  lbound(v, 2), ubound(v, 2)
    ix = 600 + 10 * lbound(v, 2) + 2 * i
    iy = 300 - 3 * i 
    call fig5%moveto(ix, iy)
    do j =  lbound(v, 1), ubound(v, 1)
      ix = 600 + 10 * j + 2 * i 
      iy = 300 - 3 * i - v(j, i) * 20 
      call fig5%lineto(ix, iy)
    end do
  end do
! y-direction
  do j =  lbound(v, 1), ubound(v, 1)
    ix = 600 + 10 * j + 2 * lbound(v, 2) 
    iy = 300 - 3 * lbound(v, 2)
    call fig5%moveto(ix, iy)
    do i = lbound(v, 2), ubound(v, 2)
      ix = 600 + 10 * j + 2 * i 
      iy = 300 - 3 * i - v(j, i) * 20
      call fig5%lineto(ix, iy)
    end do
  end do
  call fig5%show()
 
 
  call fig1%off()
  deallocate(fig1)
  
  call fig2%off()
  deallocate(fig2)
  
  call fig3%off()
  deallocate(fig3)
  
  call fig4%off()
  deallocate(fig4)
  
  call fig5%off()
  deallocate(fig5)
 
  stop
contains 
  integer function irgb(ir, ig, ib)
    implicit none
    integer, intent(in) :: ir, ig, ib
    irgb = ir + (ig + (ib * 256)) * 256
    return
  end function irgb
end program Mandel