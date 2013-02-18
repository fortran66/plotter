module m_oop
  implicit none
        
  type :: t_rgb
    integer :: ir, ig, ib
  end type t_rgb
  
  type, abstract :: t_device
    character(len = 80) :: title = 'Plotter'
    integer :: nsize_x = 640, nsize_y = 480
    integer :: line_width = 1
    type (t_rgb) :: rgb = t_rgb(0, 0, 0)
  contains
    procedure (device_on), deferred, pass :: on
    procedure (device_off), deferred, pass :: off
    procedure (device_show), deferred, pass :: show
    procedure (device_pen), deferred, pass :: pen
    procedure (device_lineTo), deferred, pass :: lineTo
    procedure (device_moveTo), deferred, pass :: moveTo
    procedure (device_dot), deferred, pass :: dot
    procedure (device_text), deferred, pass :: text
  end type t_device 

  abstract interface 
    subroutine device_on(self)
      import :: t_device
      class(t_device), intent(in out) :: self
    end subroutine device_on
  
    subroutine device_off(self)
      import :: t_device
      class(t_device), intent(in) :: self
    end subroutine device_off

    subroutine device_show(self)
      import :: t_device
      class(t_device), intent(in) :: self
    end subroutine device_show
    
    subroutine device_pen(self, line_width, rgb)
      import :: t_device, t_rgb
      class(t_device), intent(in out) :: self
      integer, intent(in), optional :: line_width
      type (t_rgb), intent(in), optional :: rgb
    end subroutine device_pen

    subroutine device_lineTo(self, ix, iy)
      import :: t_device
      class(t_device), intent(in) :: self
      integer, intent(in) :: ix, iy
    end subroutine device_lineTo
  
    subroutine device_moveTo(self, ix, iy)
      import :: t_device
      class(t_device), intent(in) :: self
      integer, intent(in) :: ix, iy
    end subroutine device_moveTo

    subroutine device_dot(self, ix, iy, icol)
      import :: t_device, t_rgb
      class(t_device), intent(in) :: self
      integer, intent(in) :: ix, iy
      integer, intent(in) :: icol
!      type (t_rgb), intent(in) :: rgb
    end subroutine device_dot
    
    subroutine device_text(self, ix, iy, txt, rgb, ifontsize, ifontdirection)
      import :: t_device, t_rgb
      class(t_device), intent(in) :: self
      integer, intent(in) :: ix, iy
      character (LEN = *), intent(in) :: txt
      type (t_rgb), intent(in), optional :: rgb
      integer, optional, intent(in) :: ifontsize, ifontdirection
    end subroutine device_text 
  end interface
  
end module m_oop


module m_win32
  use, intrinsic :: iso_c_binding
  use ifwina
  use ifwinty
  use ifmt, only : RTL_CRITICAL_SECTION
  use m_oop
  implicit none

  type :: t_wnd
    integer (HANDLE) :: hWnd      = 0
    integer (HANDLE) :: hDC       = 0
    integer (LPINT)  :: hThread   = 0
    integer (LPDWORD):: id        = 0
    integer (HANDLE) :: hPen      = 0
    type (RTL_CRITICAL_SECTION) :: CriticalSection = RTL_CRITICAL_SECTION(0,0,0,0,0,0)
  end type t_wnd      

  type, extends(t_device) :: t_win32
    type (t_wnd) :: wnd 
  contains 
    procedure, pass :: on => gr_on
    procedure, pass :: off => gr_off
    procedure, pass :: show => gr_show
    procedure, pass :: pen => gr_pen
    procedure, pass :: lineTo => gr_lineTo
    procedure, pass :: moveTo => gr_moveTo
    procedure, pass :: dot => gr_dot
    procedure, pass :: text => gr_text
  end type t_win32

  integer (DWORD), save :: iTls = 0
  integer, save :: nwin = 0 
  type (  RTL_CRITICAL_SECTION), save    ::   gCriticalSection = RTL_CRITICAL_SECTION(0,0,0,0,0,0)
  type (T_RTL_CRITICAL_SECTION), pointer :: lpgCriticalSection
contains
  !--------------------------------------------------------------------------------
  integer(4) function WinMain( hInstance, nCmdShow, win32 )
    implicit none
    integer (HANDLE), intent(in) :: hInstance 
    integer (SINT)  , intent(in) :: nCmdShow
    type (t_win32)  , intent(in) :: win32
    type (T_WNDCLASS) :: wc
    type (T_MSG)      :: mesg
    integer (HANDLE)  :: hWndMain
    integer (BOOL)    :: iretb
    character (LEN = 256) :: ClassName = 'Fortran'//char(0)
    integer :: iwindow_frame_x, iwindow_frame_y
    logical, save :: first = .true. 
    ! Init Main window
    iwindow_frame_x = 2 * GetSystemMetrics(SM_CXFIXEDFRAME) !side line = 6, title bar = 25
    iwindow_frame_y = 2 * GetSystemMetrics(SM_CYFIXEDFRAME) + GetSystemMetrics(SM_CYCAPTION)
    !
    if (first) then
      WinMain = -1 ! Error code 
      wc%lpszClassName = transfer(c_loc(ClassName)     , int(0)) 
      wc%lpfnWndProc   = transfer(c_funloc(MainWndProc), int(0)) ! CALLBACK procedure name
      wc%style        = ior(CS_VREDRAW , CS_HREDRAW)
      wc%hInstance     = hInstance
      wc%hIcon        = NULL   
      wc%hCursor      = LoadCursor( NULL, IDC_ARROW )
      wc%hbrBackground = ( COLOR_WINDOW + 1 )
      if ( RegisterClass(wc) == 0 ) return    ! initialize window
      first = .false.
      call c_f_pointer(c_loc(gCriticalSection), lpgCriticalSection)
      call InitializeCriticalSection( lpgCriticalSection ) 
    end if
    ! Init instance
    WinMain = -2 ! Error code 
    hWndMain = CreateWindowEx(  0, ClassName,                        &
                                trim(win32%title)//char(0),          &
                                int(ior(WS_OVERLAPPED, WS_SYSMENU)), &
                                CW_USEDEFAULT, CW_USEDEFAULT,        &
                                win32%nsize_x + iwindow_frame_x,     &
                                win32%nsize_y + iwindow_frame_y,     &
                                0, 0,                                &
                                hInstance,                           &
                                transfer(c_loc(win32%wnd), int(0))    )  
    if (hWndMain == 0) return
    iretb = ShowWindow( hWndMain, nCmdShow )
    iretb = UpdateWindow( hWndMain )
    ! Message Loop
    do while ( GetMessage (mesg, NULL, 0, 0) ) 
      iretb = TranslateMessage( mesg ) 
      iretb = DispatchMessage(  mesg )
    end do
    WinMain = mesg%wParam
    call InitializeCriticalSection( lpgCriticalSection ) 
    return
  end function WinMain
  !-------------------------------------------------------------------------------------
  integer (LRESULT) function MainWndProc( hWnd, mesg, wParam, lParam ) 
  !DEC$ ATTRIBUTES STDcall, DECORATE, ALIAS : 'MainWndProc' :: MainWndProc
    integer (HANDLE) , intent(in) :: hWnd
    integer (UINT)   , intent(in) :: mesg
    integer (fwParam), intent(in) :: wParam
    integer (flParam), intent(in) :: lParam
    !
    integer (HANDLE) :: hDC, hBmp
    integer (BOOL)   :: iretb
    type (T_PAINTSTRUCT) :: ps
    type (T_RECT)        :: rc
    !
    type (T_CREATESTRUCT), pointer :: cs
    type (t_wnd)         , pointer :: wnd
    type (c_ptr) :: c_p ! mold for transfer function
    !
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    !
    call c_f_pointer(c_loc(gCriticalSection), lpgCriticalSection)
    call EnterCriticalSection( lpgCriticalSection )
    MainWndProc = 0
    select case ( mesg )
      case (WM_CREATE)
        call c_f_pointer(transfer(lParam           , c_p), cs ) ! LOC(cs ) = lParam
        call c_f_pointer(transfer(cs%lpCreateParams, c_p), wnd) ! LOC(wnd) = cs%lpCreateParams 
        iretb    = TlsSetValue(iTls, cs%lpCreateParams)
        wnd%hWnd = hWnd
        hDC      = GetDC(hWnd)
        wnd%hDC  = CreateCompatibleDC(hDC)
        iretb    = GetClientRect(hWnd, rc)
        hBmp     = CreateCompatibleBitmap(hDC, rc%right - rc%left, rc%bottom - rc%top)
        iretb    = SelectObject(wnd%hDC, hBmp)
        iretb    = PatBlt(wnd%hDC, 0, 0, rc%right - rc%left, rc%bottom - rc%top, WHITENESS)
        iretb    = ReleaseDC(hWnd, hDC)
        iretb    = DeleteObject(hBmp)
      case (WM_DESTROY)
        call c_f_pointer(transfer(TlsGetValue(iTls), c_p), wnd) ! LOC(wnd) = TlsGetValue(iTls)
        call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
        call EnterCriticalSection( lpCriticalSection )          ! EnterCriticalSection( LOC(wnd%CriticalSection) )
        iretb = DeleteObject( wnd%hDC )
        call PostQuitMessage( 0 )
        call LeaveCriticalSection( lpCriticalSection )
      case (WM_PAINT)
        call c_f_pointer(transfer(TlsGetValue(iTls), c_p), wnd)  
        call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
        call EnterCriticalSection( lpCriticalSection )
        hDC    = BeginPaint(    wnd%hWnd, ps )
        iretb  = GetClientRect( wnd%hWnd, rc )
        iretb  = BitBlt(hDC, 0, 0, rc%right - rc%left, rc%bottom - rc%top, wnd%hDC, 0, 0, SRCCOPY)
        iretb  = endPaint( wnd%hWnd, ps )
        call LeaveCriticalSection( lpCriticalSection )
      case (WM_RBUTTONUP)
        call c_f_pointer(transfer(TlsGetValue(iTls), c_p), wnd)  
        call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
        call EnterCriticalSection( lpCriticalSection )
        iretb = DeleteObject( wnd%hDC )
        call PostQuitMessage( 0 )
        call LeaveCriticalSection( lpCriticalSection )
      case default
        MainWndProc = DefWindowProc( hWnd, mesg, wParam, lParam )
    end select 
    call LeaveCriticalSection( lpgCriticalSection )
    return
  end function MainWndProc
  !-------------------------------------------------------------------------------------
  subroutine gr_on(self)
    use IFMT, only : CreateThread ! multithread module
    class(t_win32), intent(in out) :: self
    integer (BOOL)    :: iretb
    integer (HANDLE)  :: hBmp
    type (T_RECT)    :: rc
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection

    associate(wnd => self%wnd)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call InitializeCriticalSection( lpCriticalSection ) 
      if (nwin == 0) iTls  = TlsAlloc()
      nwin = nwin + 1
      wnd%hThread = CreateThread(NULL, 0, Thread_Proc, NULL, CREATE_SUSPENDED, wnd%id) 
      iretb       = SetThreadPriority(wnd%hThread, THREAD_PRIORITY_BELOW_NORMAL)
      iretb       = ResumeThread(wnd%hThread)
      call sleep(100) ! wait for Window initialization 
      iretb = GetClientRect(wnd%hWnd, rc)
      hBmp  = CreateCompatibleBitmap(wnd%hDC, rc%right - rc%left, rc%bottom - rc%top)
      iretb = SelectObject(wnd%hDC, hBmp)
      iretb = DeleteObject(hBmp)
      iretb = PatBlt(wnd%hDC, 0, 0, rc%right - rc%left, rc%bottom - rc%top, WHITENESS)
      wnd%hPen = CreatePen(PS_SOLID, 1, 0)
    end associate
    return
  contains 

    integer (LONG) function Thread_Proc(lp_ThreadParameter)
    !DEC$ ATTRIBUTES STDcall, ALIAS:"_thread_proc" :: Thread_Proc
      integer (LPINT), intent(in) :: lp_ThreadParameter
      integer :: hInst
      hInst       = GetModuleHandle(NULL)
      Thread_Proc = WinMain(hInst, SW_SHOWNORMAL, self)
      return
    end function Thread_Proc
    
  end subroutine gr_on
  !-------------------------------------------------------------------------------------
  subroutine gr_off(self)
    use, intrinsic :: iso_c_binding
    class(t_win32), intent(in) :: self
    integer (BOOL)  :: iretb
    integer (DWORD) :: iwait
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    associate(wnd => self%wnd)
      iwait = INFINITE
      iretb = InvalidateRect(wnd%hWnd, NULL, FALSE)
      iretb = DeleteObject(wnd%hPen) 
      iretb = WaitForSingleObject(wnd%hThread, iwait)
      iretb = CloseHandle(wnd%hThread)
      iretb = PostMessage(wnd%hWnd, WM_DESTROY, NULL, NULL)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call DeleteCriticalSection( lpCriticalSection ) 
    end associate
    nwin = nwin - 1
    if (nwin == 0) iretb = TlsFree(iTls)
    return
  end subroutine gr_off
  !-------------------------------------------------------------------------------------
  subroutine gr_show(self)
    class(t_win32), intent(in) :: self
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    integer (BOOL):: iretb
    associate(wnd => self%wnd)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection ) 
      iretb = InvalidateRect(wnd%hWnd, NULL, FALSE)
      call LeaveCriticalSection( lpCriticalSection )  
    end associate
    return
  end subroutine gr_show
  !-------------------------------------------------------------------------------------
  subroutine gr_pen(self, line_width, rgb)
    class(t_win32), intent(in out) :: self
    integer, intent(in), optional :: line_width
    type (t_rgb), intent(in), optional :: rgb
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    integer (BOOL) :: iretb
    associate( rgb_ => self%rgb, line_width_ => self%line_width, wnd => self%wnd )
      if ( present(rgb) ) rgb_ = rgb
      if ( present(line_width) ) line_width_ = line_width
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection ) 
      iretb    = DeleteObject(wnd%hPen) 
      wnd%hPen = CreatePen(PS_SOLID, line_width_, irgb(rgb_))
      iretb    = SelectObject(wnd%hDC, wnd%hPen)
      iretb    = MoveToEx(wnd%hDC, 0, 0, NULL)
      call LeaveCriticalSection( lpCriticalSection )  
    end associate
    return
  end subroutine gr_pen
  !----------------------------------------------------------------
  integer function irgb(rgb)
    type(t_rgb), intent(in) :: rgb
    irgb = rgb%ir + (rgb%ig + (rgb%ib * 256)) * 256
    return
  end function irgb
  !----------------------------------------------------------------
  subroutine gr_moveTo(self, ix, iy)
    class(t_win32), intent(in) :: self
    integer, intent(in) :: ix, iy
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    integer (BOOL):: iretb
    associate(wnd => self%wnd)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection )  
      iretb = MoveToEx(wnd%hDC, ix, iy, NULL)
      call LeaveCriticalSection( lpCriticalSection )  
    end associate
    return
  end subroutine gr_moveTo
  !----------------------------------------------------------------
  subroutine gr_lineTo(self, ix, iy)
    class(t_win32), intent(in) :: self
    integer, intent(in) :: ix, iy
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    integer (BOOL):: iretb
    associate(wnd => self%wnd)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection ) 
      iretb = LineTo(wnd%hDC, ix, iy)
      call LeaveCriticalSection( lpCriticalSection ) 
    end associate
    return
  end subroutine gr_lineTo
  !-------------------------------------------------------------------------------------
  subroutine gr_dot(self, ix, iy, icol)
    class(t_win32), intent(in) :: self
    integer, intent(in) :: ix, iy, icol
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection
    integer (BOOL):: iretb
    associate(wnd => self%wnd)
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection ) 
      iretb = SetPixel(wnd%hDC, ix, iy, icol)
      call LeaveCriticalSection( lpCriticalSection ) 
    end associate
    return
  end subroutine gr_dot
  !----------------------------------------------------------------
  subroutine gr_text(self, ix, iy, txt, rgb, ifontsize, ifontdirection)
    class(t_win32), intent(in) :: self
    integer, intent(in) :: ix, iy
    character (LEN = *), intent(in) :: txt
    type (t_rgb), intent(in), optional :: rgb
    integer, optional, intent(in) :: ifontsize, ifontdirection
    integer (BOOL)   :: iretb
    integer (HANDLE) :: hFont
    integer :: kfontsize, kfontdirection
    type (T_RTL_CRITICAL_SECTION), pointer :: lpCriticalSection

    associate(wnd => self%wnd)
      if ( present(rgb) ) iretb = SetTextColor(wnd%hDC, irgb(rgb))
      if ( present(ifontsize) ) then 
        kfontsize = ifontsize
      else
        kfontsize = 10
      end if
      if ( present(ifontdirection) ) then
        kfontdirection = ifontdirection
      else  
        kfontdirection = 0
      end if
      call c_f_pointer(c_loc(wnd%CriticalSection), lpCriticalSection)
      call EnterCriticalSection( lpCriticalSection ) 
      iretb = SetBkMode(wnd%hDC, TRANSPARENT)
      hFont = CreateFont( kfontsize , 10 , kfontdirection , 0 ,FW_DONTCARE , FALSE , FALSE , FALSE ,  &
		        	      ANSI_CHARSET , OUT_DEFAULT_PRECIS ,                   &
	  		              CLIP_DEFAULT_PRECIS , PROOF_QUALITY ,                 &
			              ior(FIXED_PITCH,  FF_ROMAN) , NULL       		)
      iretb = SelectObject(wnd%hdc , hFont)
      iretb = TextOut(wnd%hDC, ix, iy, txt, len_trim(txt))
      iretb = SelectObject(wnd%hdc , GetStockObject(SYSTEM_FONT))
      iretb = DeleteObject(hFont)
      call LeaveCriticalSection( lpCriticalSection ) 
    end associate
    return
  end subroutine gr_text
 end module m_win32

module m_plot
  use m_oop
  use m_win32
  implicit none
  private
  public :: t_rgb, t_device, t_win32, t_wnd
end module m_plot