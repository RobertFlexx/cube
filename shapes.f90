module shape_lab_runtime
  use, intrinsic :: iso_c_binding, only: c_int, c_funptr
  implicit none
  integer(c_int), volatile, save :: stop_requested = 0_c_int
contains
  subroutine handle_signal(sig) bind(C)
    integer(c_int), value :: sig
    stop_requested = sig
  end subroutine handle_signal
end module shape_lab_runtime

program fortran_shape_lab
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, output_unit
  use, intrinsic :: iso_c_binding, only: c_int, c_short, c_long, c_ptr, c_loc, c_funptr, c_funloc, &
                                           c_size_t, c_char
  use shape_lab_runtime, only: stop_requested, handle_signal
  implicit none

  interface
    function c_ioctl(fd, request, argp) bind(C, name="ioctl") result(res)
      import :: c_int, c_long, c_ptr
      integer(c_int), value :: fd
      integer(c_long), value :: request
      type(c_ptr), value :: argp
      integer(c_int) :: res
    end function c_ioctl

    subroutine c_usleep(usec) bind(C, name="usleep")
      import :: c_int
      integer(c_int), value :: usec
    end subroutine c_usleep

    function c_signal(sig, handler) bind(C, name="signal") result(previous)
      import :: c_int, c_funptr
      integer(c_int), value :: sig
      type(c_funptr), value :: handler
      type(c_funptr) :: previous
    end function c_signal

    function c_read(fd, buf, count) bind(C, name="read") result(res)
      import :: c_int, c_ptr, c_size_t, c_long
      integer(c_int), value :: fd
      type(c_ptr), value :: buf
      integer(c_size_t), value :: count
      integer(c_long) :: res
    end function c_read
  end interface

  type, bind(C) :: winsize
    integer(c_short) :: ws_row
    integer(c_short) :: ws_col
    integer(c_short) :: ws_xpixel
    integer(c_short) :: ws_ypixel
  end type winsize

  type :: mesh_type
    character(len=20) :: name = 'Cube'
    integer :: nv = 0, nt = 0, ne = 0
    logical :: smooth = .false.
    real(real32), allocatable :: v(:,:), n(:,:)
    integer, allocatable :: tri(:,:), edge(:,:)
  end type mesh_type

  type :: theme_type
    character(len=24) :: name = 'Neon Ice'
    integer :: sky_top(3), sky_horizon(3), floor_rgb(3), grid_rgb(3), star_rgb(3)
    integer :: base_a(3), base_b(3), edge_rgb(3)
    real(real32) :: key_pos(3), fill_pos(3), key_rgb(3), fill_rgb(3)
    real(real32) :: ambient, key_power, fill_power, rim_power
    real(real32) :: metallic, roughness, exposure, fog_strength
    logical :: stars = .true., grid = .true.
  end type theme_type

  integer, parameter :: target_fps = 30
  integer, parameter :: resize_check_frames = 10
  integer, parameter :: max_shape = 5
  integer, parameter :: max_theme = 5
  integer, parameter :: cell_bytes = 64
  integer(c_int), parameter :: SIGINT = 2_c_int
  integer(c_int), parameter :: SIGTERM = 15_c_int
  integer(c_long), parameter :: TIOCGWINSZ = int(z'5413', c_long)
  real(real64), parameter :: target_frame_time = 1.0_real64 / real(target_fps, real64)
  real(real32), parameter :: PI = 3.1415927_real32
  character(len=1), parameter :: ESC = achar(27)
  character(len=3), parameter :: UPPER_HALF = '▀'

  type(mesh_type) :: mesh
  type(theme_type) :: theme
  integer(int32), allocatable :: pixels(:,:), background(:,:), previous_pixels(:,:)
  real(real32), allocatable :: zbuffer(:,:)
  character(len=:), allocatable :: frame_text
  character(len=3), save :: decimal3(0:255)
  character(kind=c_char), target :: input_bytes(1024)

  integer :: cols, rows, ph, new_cols, new_rows
  integer :: frame_number, used_bytes, shape_id, theme_id
  integer(int64) :: clock_rate, frame_start, frame_end, previous_start
  real(real64) :: dt, elapsed, sleep_time, t
  real(real32) :: orientation(3,3), angular_velocity(3), drag_vector(3)
  real(real32) :: camera_distance, auto_speed, interaction_quiet, inertia_speed
  logical :: auto_rotate, show_hud, force_redraw
  type(c_funptr) :: previous_handler

  integer :: input_state, mouse_len
  character(len=64) :: mouse_text
  logical :: dragging, mouse_present, mouse_over_object
  integer :: mouse_x, mouse_y

  call init_decimal_table()
  shape_id = 1
  theme_id = 1
  call load_shape(shape_id, mesh)
  call load_theme(theme_id, theme)

  previous_handler = c_signal(SIGINT, c_funloc(handle_signal))
  previous_handler = c_signal(SIGTERM, c_funloc(handle_signal))

  call rotation_matrix(-0.34_real32, 0.62_real32, 0.08_real32, orientation)
  camera_distance = 5.0_real32
  angular_velocity = 0.0_real32
  drag_vector = [0.0_real32,0.0_real32,-1.0_real32]
  auto_speed = 1.0_real32
  interaction_quiet = 0.0_real32
  auto_rotate = .true.
  show_hud = .true.
  force_redraw = .true.
  dragging = .false.
  mouse_present = .false.
  mouse_over_object = .false.
  input_state = 0
  mouse_len = 0
  mouse_text = ''
  mouse_x = 0
  mouse_y = 0
  t = 0.0_real64
  frame_number = 0

  call terminal_size(cols, rows)
  call ensure_buffers(cols, rows)

  call execute_command_line('stty -echo -icanon min 0 time 0', wait=.true.)
  write(output_unit,'(A)',advance='no') ESC//'[?1049h'//ESC//'[?25l'//ESC//'[?7l'// &
       ESC//'[?1003h'//ESC//'[?1006h'//ESC//'[2J'
  flush(output_unit)

  call system_clock(previous_start, clock_rate)

  do while (stop_requested == 0_c_int)
    call system_clock(frame_start)

    if (frame_number > 0) then
      dt = real(frame_start - previous_start, real64) / real(clock_rate, real64)
      dt = min(max(dt, 0.001_real64), 0.080_real64)
    else
      dt = target_frame_time
    end if
    previous_start = frame_start

    if (mod(frame_number, resize_check_frames) == 0) then
      call terminal_size(new_cols, new_rows)
      if (new_cols /= cols .or. new_rows /= rows) then
        cols = new_cols
        rows = new_rows
        call ensure_buffers(cols, rows)
        force_redraw = .true.
        write(output_unit,'(A)',advance='no') ESC//'[2J'
      end if
    end if

    call poll_input(real(dt,real32))
    if (stop_requested /= 0_c_int) exit

    ph = rows * 2
    t = t + dt
    interaction_quiet = max(0.0_real32, interaction_quiet - real(dt,real32))

    if (.not. dragging) then
      inertia_speed=sqrt(dot_product(angular_velocity,angular_velocity))
      if(inertia_speed>1.0e-4_real32) &
        call apply_orientation_delta(angular_velocity/inertia_speed,inertia_speed*real(dt,real32))
      angular_velocity = angular_velocity * exp(-3.35_real32 * real(dt,real32))
    end if

    if (auto_rotate .and. .not. dragging .and. interaction_quiet <= 0.0_real32) then
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32], &
        0.58_real32*auto_speed*real(dt,real32))
      call apply_orientation_delta([0.0_real32,0.0_real32,1.0_real32], &
        0.075_real32*auto_speed*real(dt,real32))
    end if

    pixels = background
    zbuffer = huge(1.0_real32)

    call render_frame(cols, ph, real(t,real32), orientation)
    call draw_shape_outline(cols, ph)
    if (mouse_present) then
      call cursor_hits_shape(mouse_x, mouse_y, mouse_over_object)
      call draw_interaction_cursor(cols, ph, mouse_x, mouse_y, mouse_over_object, dragging)
    end if
    call build_frame_delta(cols, rows, used_bytes, force_redraw)

    write(output_unit,'(A)',advance='no') ESC//'[?2026h'
    if (used_bytes > 0) write(output_unit,'(A)',advance='no') frame_text(1:used_bytes)
    if (show_hud) call write_hud()
    write(output_unit,'(A)',advance='no') ESC//'[0m'//ESC//'[?2026l'
    flush(output_unit)
    force_redraw = .false.

    frame_number = frame_number + 1
    call system_clock(frame_end)
    elapsed = real(frame_end - frame_start, real64) / real(clock_rate, real64)
    sleep_time = target_frame_time - elapsed
    if (sleep_time > 0.0005_real64) call c_usleep(int(sleep_time*1000000.0_real64,c_int))
  end do

  write(output_unit,'(A)',advance='no') ESC//'[?2026l'//ESC//'[?1006l'//ESC//'[?1003l'//ESC//'[?1002l'// &
       ESC//'[0m'//ESC//'[?7h'//ESC//'[?25h'//ESC//'[?1049l'
  flush(output_unit)
  call execute_command_line('stty echo icanon', wait=.true.)

contains

  subroutine init_decimal_table()
    integer :: i
    do i = 0, 255
      write(decimal3(i),'(I3.3)') i
    end do
  end subroutine init_decimal_table

  subroutine load_shape(id, m)
    integer, intent(in) :: id
    type(mesh_type), intent(inout) :: m

    if (allocated(m%v)) deallocate(m%v)
    if (allocated(m%n)) deallocate(m%n)
    if (allocated(m%tri)) deallocate(m%tri)
    if (allocated(m%edge)) deallocate(m%edge)

    select case(id)
    case(1)
      call init_cube(m)
    case(2)
      call init_icosahedron(m)
    case(3)
      call init_sphere(m, 16, 32)
    case(4)
      call init_torus(m, 36, 18)
    case default
      call init_tetrahedron(m)
    end select
  end subroutine load_shape

  subroutine init_cube(m)
    type(mesh_type), intent(out) :: m
    integer :: i
    m%name = 'Cube'
    m%nv = 8; m%nt = 12; m%ne = 12; m%smooth = .false.
    allocate(m%v(3,m%nv), m%n(3,m%nv), m%tri(3,m%nt), m%edge(2,m%ne))
    m%v = reshape([ &
      -1.0_real32,-1.0_real32,-1.0_real32,  1.0_real32,-1.0_real32,-1.0_real32, &
       1.0_real32, 1.0_real32,-1.0_real32, -1.0_real32, 1.0_real32,-1.0_real32, &
      -1.0_real32,-1.0_real32, 1.0_real32,  1.0_real32,-1.0_real32, 1.0_real32, &
       1.0_real32, 1.0_real32, 1.0_real32, -1.0_real32, 1.0_real32, 1.0_real32 ], [3,8])
    do i=1,m%nv
      m%n(:,i) = normalize3(m%v(:,i))
    end do
    m%tri = reshape([ &
      1,3,2, 1,4,3, 5,6,7, 5,7,8, 1,5,8, 1,8,4, &
      2,3,7, 2,7,6, 1,2,6, 1,6,5, 4,8,7, 4,7,3 ], [3,12])
    m%edge = reshape([1,2,2,3,3,4,4,1, 5,6,6,7,7,8,8,5, 1,5,2,6,3,7,4,8],[2,12])
  end subroutine init_cube

  subroutine init_tetrahedron(m)
    type(mesh_type), intent(out) :: m
    integer :: i
    real(real32) :: s
    m%name='Tetrahedron'; m%nv=4; m%nt=4; m%ne=6; m%smooth=.false.
    allocate(m%v(3,4),m%n(3,4),m%tri(3,4),m%edge(2,6))
    s = 1.15_real32
    m%v = s*reshape([1.0_real32,1.0_real32,1.0_real32, -1.0_real32,-1.0_real32,1.0_real32, &
                     -1.0_real32,1.0_real32,-1.0_real32, 1.0_real32,-1.0_real32,-1.0_real32],[3,4])
    do i=1,4; m%n(:,i)=normalize3(m%v(:,i)); end do
    m%tri=reshape([1,2,3, 1,4,2, 1,3,4, 2,4,3],[3,4])
    m%edge=reshape([1,2,1,3,1,4,2,3,2,4,3,4],[2,6])
  end subroutine init_tetrahedron

  subroutine init_icosahedron(m)
    type(mesh_type), intent(out) :: m
    real(real32) :: phi
    integer :: i
    phi = (1.0_real32 + sqrt(5.0_real32))*0.5_real32
    m%name='Icosahedron'; m%nv=12; m%nt=20; m%smooth=.false.
    allocate(m%v(3,12),m%n(3,12),m%tri(3,20))
    m%v=reshape([ &
      -1.0_real32, phi,0.0_real32,  1.0_real32, phi,0.0_real32, -1.0_real32,-phi,0.0_real32, &
       1.0_real32,-phi,0.0_real32, 0.0_real32,-1.0_real32, phi, 0.0_real32,1.0_real32, phi, &
       0.0_real32,-1.0_real32,-phi, 0.0_real32,1.0_real32,-phi, phi,0.0_real32,-1.0_real32, &
       phi,0.0_real32,1.0_real32, -phi,0.0_real32,-1.0_real32, -phi,0.0_real32,1.0_real32],[3,12])
    do i=1,12
      m%v(:,i)=normalize3(m%v(:,i))*1.24_real32
      m%n(:,i)=normalize3(m%v(:,i))
    end do
    m%tri=reshape([ &
      1,12,6, 1,6,2, 1,2,8, 1,8,11, 1,11,12, &
      2,6,10, 6,12,5, 12,11,3, 11,8,7, 8,2,9, &
      4,10,5, 4,5,3, 4,3,7, 4,7,9, 4,9,10, &
      5,10,6, 3,5,12, 7,3,11, 9,7,8, 10,9,2 ],[3,20])
    call build_edges_from_triangles(m)
  end subroutine init_icosahedron

  subroutine init_sphere(m, nlat, nlon)
    type(mesh_type), intent(out) :: m
    integer, intent(in) :: nlat, nlon
    integer :: j,k,idx,ti,r0,r1,k1
    real(real32) :: th,ph,rr
    m%name='Sphere'; m%nv=2+(nlat-1)*nlon; m%nt=2*nlon*(nlat-1); m%ne=0; m%smooth=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    m%v(:,1)=[0.0_real32,1.22_real32,0.0_real32]
    m%n(:,1)=[0.0_real32,1.0_real32,0.0_real32]
    idx=2
    do j=1,nlat-1
      th=PI*real(j,real32)/real(nlat,real32)
      rr=sin(th)
      do k=0,nlon-1
        ph=2.0_real32*PI*real(k,real32)/real(nlon,real32)
        m%n(:,idx)=[rr*cos(ph),cos(th),rr*sin(ph)]
        m%v(:,idx)=1.22_real32*m%n(:,idx)
        idx=idx+1
      end do
    end do
    m%v(:,m%nv)=[0.0_real32,-1.22_real32,0.0_real32]
    m%n(:,m%nv)=[0.0_real32,-1.0_real32,0.0_real32]
    ti=1
    do k=0,nlon-1
      k1=mod(k+1,nlon)
      m%tri(:,ti)=[1,2+k,2+k1]; ti=ti+1
    end do
    do j=1,nlat-2
      r0=2+(j-1)*nlon
      r1=2+j*nlon
      do k=0,nlon-1
        k1=mod(k+1,nlon)
        m%tri(:,ti)=[r0+k,r1+k,r1+k1]; ti=ti+1
        m%tri(:,ti)=[r0+k,r1+k1,r0+k1]; ti=ti+1
      end do
    end do
    r0=2+(nlat-2)*nlon
    do k=0,nlon-1
      k1=mod(k+1,nlon)
      m%tri(:,ti)=[r0+k,m%nv,r0+k1]; ti=ti+1
    end do
  end subroutine init_sphere

  subroutine init_torus(m, nmajor, nminor)
    type(mesh_type), intent(out) :: m
    integer, intent(in) :: nmajor,nminor
    integer :: a,b,idx,ti,a1,b1,i00,i10,i11,i01
    real(real32) :: u,vv,cu,su,cv,sv,major_r,minor_r
    major_r=0.82_real32; minor_r=0.36_real32
    m%name='Torus'; m%nv=nmajor*nminor; m%nt=2*nmajor*nminor; m%ne=0; m%smooth=.true.
    allocate(m%v(3,m%nv),m%n(3,m%nv),m%tri(3,m%nt),m%edge(2,0))
    idx=1
    do a=0,nmajor-1
      u=2.0_real32*PI*real(a,real32)/real(nmajor,real32); cu=cos(u); su=sin(u)
      do b=0,nminor-1
        vv=2.0_real32*PI*real(b,real32)/real(nminor,real32); cv=cos(vv); sv=sin(vv)
        m%v(:,idx)=[(major_r+minor_r*cv)*cu, minor_r*sv, (major_r+minor_r*cv)*su]
        m%n(:,idx)=normalize3([cv*cu,sv,cv*su])
        idx=idx+1
      end do
    end do
    ti=1
    do a=0,nmajor-1
      a1=mod(a+1,nmajor)
      do b=0,nminor-1
        b1=mod(b+1,nminor)
        i00=1+a*nminor+b; i10=1+a1*nminor+b; i11=1+a1*nminor+b1; i01=1+a*nminor+b1
        m%tri(:,ti)=[i00,i10,i11]; ti=ti+1
        m%tri(:,ti)=[i00,i11,i01]; ti=ti+1
      end do
    end do
  end subroutine init_torus

  subroutine build_edges_from_triangles(m)
    type(mesh_type), intent(inout) :: m
    integer, allocatable :: tmp(:,:)
    integer :: i,j,a,b,count,k
    logical :: found
    allocate(tmp(2,3*m%nt)); count=0
    do i=1,m%nt
      do j=1,3
        a=m%tri(j,i); b=m%tri(1+mod(j,3),i)
        if (a>b) then; k=a; a=b; b=k; end if
        found=.false.
        do k=1,count
          if (tmp(1,k)==a .and. tmp(2,k)==b) then; found=.true.; exit; end if
        end do
        if (.not.found) then; count=count+1; tmp(:,count)=[a,b]; end if
      end do
    end do
    m%ne=count
    allocate(m%edge(2,count))
    if (count>0) m%edge=tmp(:,1:count)
    deallocate(tmp)
  end subroutine build_edges_from_triangles

  subroutine load_theme(id, th)
    integer,intent(in)::id
    type(theme_type),intent(out)::th
    select case(id)
    case(1)
      th%name='Neon Ice'
      th%sky_top=[3,5,14]; th%sky_horizon=[17,31,65]; th%floor_rgb=[5,9,20]
      th%grid_rgb=[21,70,115]; th%star_rgb=[150,205,255]
      th%base_a=[45,125,255]; th%base_b=[95,238,245]; th%edge_rgb=[216,246,255]
      th%key_pos=[-3.3_real32,3.6_real32,-3.8_real32]; th%fill_pos=[3.8_real32,-0.4_real32,-1.8_real32]
      th%key_rgb=[1.00_real32,0.93_real32,0.82_real32]; th%fill_rgb=[0.22_real32,0.58_real32,1.00_real32]
      th%ambient=.115_real32; th%key_power=8.2_real32; th%fill_power=2.6_real32; th%rim_power=.45_real32
      th%metallic=.38_real32; th%roughness=.24_real32; th%exposure=1.18_real32; th%fog_strength=.28_real32
      th%stars=.true.; th%grid=.true.
    case(2)
      th%name='Sunset Chrome'
      th%sky_top=[15,5,15]; th%sky_horizon=[78,24,46]; th%floor_rgb=[17,7,14]
      th%grid_rgb=[119,46,68]; th%star_rgb=[255,203,150]
      th%base_a=[255,93,51]; th%base_b=[255,190,75]; th%edge_rgb=[255,232,197]
      th%key_pos=[-2.8_real32,3.2_real32,-3.4_real32]; th%fill_pos=[3.6_real32,0.2_real32,-2.0_real32]
      th%key_rgb=[1.00_real32,0.63_real32,0.36_real32]; th%fill_rgb=[0.62_real32,0.22_real32,0.70_real32]
      th%ambient=.13_real32; th%key_power=8.8_real32; th%fill_power=2.1_real32; th%rim_power=.36_real32
      th%metallic=.62_real32; th%roughness=.18_real32; th%exposure=1.14_real32; th%fog_strength=.24_real32
      th%stars=.true.; th%grid=.true.
    case(3)
      th%name='Emerald Lab'
      th%sky_top=[2,10,8]; th%sky_horizon=[8,43,34]; th%floor_rgb=[3,14,11]
      th%grid_rgb=[15,79,59]; th%star_rgb=[155,255,216]
      th%base_a=[24,190,122]; th%base_b=[94,255,193]; th%edge_rgb=[210,255,235]
      th%key_pos=[-3.6_real32,3.5_real32,-3.0_real32]; th%fill_pos=[3.2_real32,-.3_real32,-2.6_real32]
      th%key_rgb=[0.74_real32,1.00_real32,0.88_real32]; th%fill_rgb=[0.10_real32,0.63_real32,0.46_real32]
      th%ambient=.12_real32; th%key_power=7.6_real32; th%fill_power=2.4_real32; th%rim_power=.32_real32
      th%metallic=.18_real32; th%roughness=.30_real32; th%exposure=1.15_real32; th%fog_strength=.25_real32
      th%stars=.false.; th%grid=.true.
    case(4)
      th%name='Studio Neutral'
      th%sky_top=[18,19,22]; th%sky_horizon=[47,49,53]; th%floor_rgb=[20,21,23]
      th%grid_rgb=[54,56,60]; th%star_rgb=[180,180,180]
      th%base_a=[176,181,190]; th%base_b=[112,121,137]; th%edge_rgb=[245,245,245]
      th%key_pos=[-3.8_real32,4.4_real32,-4.0_real32]; th%fill_pos=[4.0_real32,1.0_real32,-1.0_real32]
      th%key_rgb=[1.00_real32,0.96_real32,0.90_real32]; th%fill_rgb=[0.46_real32,0.55_real32,0.72_real32]
      th%ambient=.17_real32; th%key_power=9.5_real32; th%fill_power=2.0_real32; th%rim_power=.18_real32
      th%metallic=.72_real32; th%roughness=.21_real32; th%exposure=1.03_real32; th%fog_strength=.18_real32
      th%stars=.false.; th%grid=.false.
    case default
      th%name='Vaporwave'
      th%sky_top=[11,3,24]; th%sky_horizon=[57,13,77]; th%floor_rgb=[11,4,25]
      th%grid_rgb=[80,26,122]; th%star_rgb=[218,183,255]
      th%base_a=[255,61,183]; th%base_b=[45,219,255]; th%edge_rgb=[255,229,252]
      th%key_pos=[-3.0_real32,3.3_real32,-3.7_real32]; th%fill_pos=[3.3_real32,-.1_real32,-2.1_real32]
      th%key_rgb=[1.0_real32,.32_real32,.72_real32]; th%fill_rgb=[.20_real32,.71_real32,1.0_real32]
      th%ambient=.12_real32; th%key_power=8.1_real32; th%fill_power=2.7_real32; th%rim_power=.50_real32
      th%metallic=.46_real32; th%roughness=.20_real32; th%exposure=1.16_real32; th%fog_strength=.29_real32
      th%stars=.true.; th%grid=.true.
    end select
  end subroutine load_theme

  subroutine terminal_size(out_cols, out_rows)
    integer,intent(out)::out_cols,out_rows
    type(winsize),target::ws
    integer(c_int)::rc
    character(len=64)::env
    integer::stat,ios
    out_cols=100; out_rows=40
    ws=winsize(0_c_short,0_c_short,0_c_short,0_c_short)
    rc=c_ioctl(1_c_int,TIOCGWINSZ,c_loc(ws))
    if (rc==0 .and. int(ws%ws_col)>0 .and. int(ws%ws_row)>0) then
      out_cols=int(ws%ws_col); out_rows=int(ws%ws_row)
    else
      call get_environment_variable('COLUMNS',env,status=stat)
      if (stat==0) then; read(env,*,iostat=ios) out_cols; if(ios/=0)out_cols=100; end if
      call get_environment_variable('LINES',env,status=stat)
      if (stat==0) then; read(env,*,iostat=ios) out_rows; if(ios/=0)out_rows=40; end if
    end if
    out_cols=max(out_cols,40); out_rows=max(out_rows,16)
  end subroutine terminal_size

  subroutine ensure_buffers(w,r)
    integer,intent(in)::w,r
    integer::height,needed
    height=r*2; needed=r*(w*cell_bytes+40)+256
    if (allocated(pixels)) then
      if(size(pixels,1)==w .and. size(pixels,2)==height) return
      deallocate(pixels,background,previous_pixels,zbuffer)
      if(allocated(frame_text))deallocate(frame_text)
    end if
    allocate(pixels(w,height),background(w,height),previous_pixels(w,height),zbuffer(w,height))
    allocate(character(len=needed)::frame_text)
    call make_background(w,height)
    previous_pixels=-1_int32
    zbuffer=huge(1.0_real32)
  end subroutine ensure_buffers

  subroutine rebuild_background()
    call make_background(cols,rows*2)
    previous_pixels=-1_int32
    force_redraw=.true.
  end subroutine rebuild_background

  subroutine render_frame(w,height,time,rot)
    integer,intent(in)::w,height
    real(real32),intent(in)::time,rot(3,3)
    real(real32),allocatable::world(:,:),wn(:,:),sx(:),sy(:),depth(:)
    logical,allocatable::ok(:)
    real(real32)::bob,face_n(3),centroid(3),camera_pos(3)
    real(real32)::shadow_x,shadow_y,shadow_depth,object_scale
    logical::shadow_ok
    integer::i,ti,eidx,ids(3),base_col(3)
    real(real32)::edge_pulse

    allocate(world(3,mesh%nv),wn(3,mesh%nv),sx(mesh%nv),sy(mesh%nv),depth(mesh%nv),ok(mesh%nv))
    bob=0.075_real32*sin(time*1.15_real32)
    object_scale=merge(1.03_real32,1.0_real32,mesh%smooth)
    do i=1,mesh%nv
      world(:,i)=matmul(rot,mesh%v(:,i))*object_scale
      world(2,i)=world(2,i)+0.27_real32+bob
      wn(:,i)=normalize3(matmul(rot,mesh%n(:,i)))
      call project_point(world(:,i),w,height,sx(i),sy(i),depth(i),ok(i))
    end do

    call project_point([0.0_real32,-1.30_real32,0.18_real32],w,height,shadow_x,shadow_y,shadow_depth,shadow_ok)
    if(shadow_ok)call draw_soft_shadow(w,height,shadow_x,shadow_y, &
      min(real(w,real32),real(height,real32))*.235_real32, &
      min(real(w,real32),real(height,real32))*.055_real32,time)

    camera_pos=[0.0_real32,0.0_real32,-camera_distance]
    do ti=1,mesh%nt
      ids=mesh%tri(:,ti)
      if(.not.all(ok(ids)))cycle
      if(mesh%smooth)then
        face_n=normalize3(wn(:,ids(1))+wn(:,ids(2))+wn(:,ids(3)))
      else
        face_n=normalize3(cross3(world(:,ids(2))-world(:,ids(1)),world(:,ids(3))-world(:,ids(1))))
        centroid=(world(:,ids(1))+world(:,ids(2))+world(:,ids(3)))/3.0_real32
        if(dot_product(face_n,centroid)<0.0_real32)face_n=-face_n
      end if
      centroid=(world(:,ids(1))+world(:,ids(2))+world(:,ids(3)))/3.0_real32
      if(dot_product(face_n,camera_pos-centroid)<=0.015_real32)cycle
      call material_color(ti,face_n,base_col)
      if(mesh%smooth)then
        call fill_lit_triangle(w,height, &
          sx(ids(1)),sy(ids(1)),depth(ids(1)),world(:,ids(1)),wn(:,ids(1)), &
          sx(ids(2)),sy(ids(2)),depth(ids(2)),world(:,ids(2)),wn(:,ids(2)), &
          sx(ids(3)),sy(ids(3)),depth(ids(3)),world(:,ids(3)),wn(:,ids(3)),base_col)
      else
        call fill_lit_triangle(w,height, &
          sx(ids(1)),sy(ids(1)),depth(ids(1)),world(:,ids(1)),face_n, &
          sx(ids(2)),sy(ids(2)),depth(ids(2)),world(:,ids(2)),face_n, &
          sx(ids(3)),sy(ids(3)),depth(ids(3)),world(:,ids(3)),face_n,base_col)
      end if
    end do

    if(mesh%ne>0)then
      edge_pulse=.92_real32+.08_real32*sin(time*1.55_real32)
      do eidx=1,mesh%ne
        if(ok(mesh%edge(1,eidx)).and.ok(mesh%edge(2,eidx)))then
          call draw_glowing_line(w,height, &
            sx(mesh%edge(1,eidx)),sy(mesh%edge(1,eidx)),depth(mesh%edge(1,eidx)), &
            sx(mesh%edge(2,eidx)),sy(mesh%edge(2,eidx)),depth(mesh%edge(2,eidx)),edge_pulse)
        end if
      end do
    end if
    deallocate(world,wn,sx,sy,depth,ok)
  end subroutine render_frame

  subroutine material_color(tri_idx,normal,col)
    integer,intent(in)::tri_idx
    real(real32),intent(in)::normal(3)
    integer,intent(out)::col(3)
    real(real32)::mixv
    mixv=.17_real32+.22_real32*(normal(2)*.5_real32+.5_real32)+.08_real32*sin(real(tri_idx,real32)*1.71_real32)
    mixv=clamp01(mixv)
    col=nint(real(theme%base_a,real32)*(1.0_real32-mixv)+real(theme%base_b,real32)*mixv)
  end subroutine material_color

  subroutine make_background(w,height)
    integer,intent(in)::w,height
    integer::x,y,hashv,irgb(3)
    real(real32)::fx,fy,q,dx,dy,vignette,glow,star,line_x,line_z,gridx,gridz
    real(real32)::c(3),a(3),b(3),floorc(3),gridc(3),starc(3),horizon
    horizon=.65_real32
    a=real(theme%sky_top,real32); b=real(theme%sky_horizon,real32)
    floorc=real(theme%floor_rgb,real32); gridc=real(theme%grid_rgb,real32); starc=real(theme%star_rgb,real32)
    do y=1,height
      fy=real(y-1,real32)/real(max(1,height-1),real32)
      do x=1,w
        fx=real(x-1,real32)/real(max(1,w-1),real32)
        dx=(fx-.50_real32)/.68_real32; dy=(fy-.42_real32)/.78_real32
        vignette=clamp01(1.08_real32-.35_real32*(dx*dx+dy*dy))
        glow=exp(-2.5_real32*(dx*dx+dy*dy))
        if(fy<horizon)then
          q=fy/horizon
          c=a*(1.0_real32-q)+b*q
          c=c+(6.0_real32+14.0_real32*glow)*[.35_real32,.50_real32,.80_real32]
          if(theme%stars)then
            hashv=modulo(x*1973+y*9277+x*y*17,7919)
            if(hashv<10)then
              star=20.0_real32+real(modulo(hashv*37,75),real32)
              c=c+starc*(star/255.0_real32)
            end if
          end if
        else
          q=(fy-horizon)/max(1.0_real32-horizon,.001_real32)
          c=floorc*(.78_real32+.35_real32*q)+real(theme%sky_horizon,real32)*(.07_real32*glow)
          if(theme%grid)then
            gridx=(fx-.5_real32)/max(.045_real32+q*.72_real32,.01_real32)
            gridz=1.0_real32/max(q+.055_real32,.055_real32)
            line_x=abs(gridx*10.0_real32-real(nint(gridx*10.0_real32),real32))
            line_z=abs(gridz*.85_real32-real(nint(gridz*.85_real32),real32))
            if(line_x<.040_real32 .or. line_z<.032_real32) c=c+gridc*(.20_real32*(1.0_real32-q))
          end if
        end if
        c=c*vignette
        irgb=[clamp255(c(1)),clamp255(c(2)),clamp255(c(3))]
        background(x,y)=pack_rgb(irgb(1),irgb(2),irgb(3))
      end do
    end do
  end subroutine make_background

  subroutine draw_soft_shadow(w,height,cx,cy,rx,ry,time)
    integer,intent(in)::w,height
    real(real32),intent(in)::cx,cy,rx,ry,time
    integer::x,y,minx,maxx,miny,maxy
    real(real32)::dx,dy,d2,alpha,glow_alpha,pulse
    minx=max(1,int(floor(cx-rx*1.55_real32))); maxx=min(w,int(ceiling(cx+rx*1.55_real32)))
    miny=max(1,int(floor(cy-ry*2.60_real32))); maxy=min(height,int(ceiling(cy+ry*2.60_real32)))
    pulse=.98_real32+.02_real32*sin(time*1.2_real32)
    do y=miny,maxy
      do x=minx,maxx
        dx=(real(x,real32)-cx)/max(rx*pulse,.001_real32)
        dy=(real(y,real32)-cy)/max(ry,.001_real32)
        d2=dx*dx+dy*dy
        if(d2>5.5_real32)cycle
        alpha=.54_real32*exp(-1.35_real32*d2)
        call blend_pixel(x,y,[0,0,2],alpha)
        glow_alpha=.030_real32*exp(-.72_real32*d2)
        call add_pixel(x,y,theme%edge_rgb,glow_alpha)
      end do
    end do
  end subroutine draw_soft_shadow

  subroutine fill_lit_triangle(w,height,x0,y0,z0,p0,n0,x1,y1,z1,p1,n1,x2,y2,z2,p2,n2,base_col)
    integer,intent(in)::w,height,base_col(3)
    real(real32),intent(in)::x0,y0,z0,p0(3),n0(3),x1,y1,z1,p1(3),n1(3),x2,y2,z2,p2(3),n2(3)
    integer::minx,maxx,miny,maxy,x,y,out_col(3)
    real(real32)::area,sgn,area_abs,px,py,e0,e1,e2,b0,b1,b2,q0,q1,q2,denom,depth
    real(real32)::pos(3),normal(3),view(3),ldir(3),camera_pos(3),radiance(3),linear(3),base(3)
    real(real32)::dist2,ndotv,fresnel,fog,noise,coat,coat_ndh,mixv
    real(real32)::mapped(3),fogc(3),hdir(3),base_a_linear(3),base_b_linear(3)

    minx=max(1,int(floor(min(x0,min(x1,x2))))); maxx=min(w,int(ceiling(max(x0,max(x1,x2)))))
    miny=max(1,int(floor(min(y0,min(y1,y2))))); maxy=min(height,int(ceiling(max(y0,max(y1,y2)))))
    if(minx>maxx .or. miny>maxy)return
    area=edge_function(x0,y0,x1,y1,x2,y2)
    if(abs(area)<1.0e-7_real32)return
    sgn=merge(-1.0_real32,1.0_real32,area<0.0_real32); area_abs=area*sgn
    base=(real(base_col,real32)/255.0_real32)**2.2_real32
    base_a_linear=(real(theme%base_a,real32)/255.0_real32)**2.2_real32
    base_b_linear=(real(theme%base_b,real32)/255.0_real32)**2.2_real32
    camera_pos=[0.0_real32,0.0_real32,-camera_distance]
    fogc=(real(theme%sky_horizon,real32)/255.0_real32)**2.2_real32

    do y=miny,maxy
      do x=minx,maxx
        px=real(x,real32)-.5_real32; py=real(y,real32)-.5_real32
        e0=edge_function(x1,y1,x2,y2,px,py)*sgn
        e1=edge_function(x2,y2,x0,y0,px,py)*sgn
        e2=edge_function(x0,y0,x1,y1,px,py)*sgn
        if(e0<-.001_real32 .or. e1<-.001_real32 .or. e2<-.001_real32)cycle
        b0=e0/area_abs; b1=e1/area_abs; b2=e2/area_abs
        q0=b0/z0; q1=b1/z1; q2=b2/z2; denom=q0+q1+q2
        if(denom<=1.0e-8_real32)cycle
        depth=1.0_real32/denom
        if(depth>=zbuffer(x,y))cycle
        pos=(q0*p0+q1*p1+q2*p2)*depth
        normal=normalize3((q0*n0+q1*n1+q2*n2)*depth)
        if(mesh%smooth)then
          mixv=clamp01(.24_real32+.30_real32*(normal(2)*.5_real32+.5_real32)+ &
                       .10_real32*(normal(1)*.5_real32+.5_real32))
          base=base_a_linear*(1.0_real32-mixv)+base_b_linear*mixv
        end if
        view=normalize3(camera_pos-pos); ndotv=max(.001_real32,dot_product(normal,view))

        linear=base*theme%ambient*(.62_real32+.38_real32*(normal(2)*.5_real32+.5_real32))
        ldir=normalize3(theme%key_pos-pos); dist2=dot_product(theme%key_pos-pos,theme%key_pos-pos)
        radiance=theme%key_rgb*(theme%key_power/(1.0_real32+.11_real32*dist2))
        linear=linear+brdf_light(normal,view,ldir,radiance,base,theme%metallic,theme%roughness)
        ldir=normalize3(theme%fill_pos-pos); dist2=dot_product(theme%fill_pos-pos,theme%fill_pos-pos)
        radiance=theme%fill_rgb*(theme%fill_power/(1.0_real32+.09_real32*dist2))
        linear=linear+brdf_light(normal,view,ldir,radiance,base,theme%metallic,min(.72_real32,theme%roughness+.10_real32))

        fresnel=(1.0_real32-ndotv)**3.0_real32
        linear=linear+theme%rim_power*fresnel*(real(theme%edge_rgb,real32)/255.0_real32)**2.0_real32
        hdir=normalize3(normalize3(theme%key_pos-pos)+view)
        coat_ndh=max(0.0_real32,dot_product(normal,hdir))
        coat=(coat_ndh**max(18.0_real32,150.0_real32*(1.0_real32-theme%roughness)))*.16_real32
        linear=linear+coat*theme%key_rgb

        fog=clamp01((depth-4.4_real32)/4.2_real32)*theme%fog_strength
        linear=linear*(1.0_real32-fog)+fogc*fog
        noise=(real(modulo(x*17+y*29,37),real32)/36.0_real32-.5_real32)*.0035_real32
        linear=max(linear+noise,0.0_real32)
        mapped=aces_tonemap(linear*theme%exposure)
        mapped=clamp_vec(mapped,0.0_real32,1.0_real32)**(1.0_real32/2.2_real32)
        out_col=[clamp255(mapped(1)*255.0_real32),clamp255(mapped(2)*255.0_real32),clamp255(mapped(3)*255.0_real32)]
        zbuffer(x,y)=depth; pixels(x,y)=pack_rgb(out_col(1),out_col(2),out_col(3))
      end do
    end do
  end subroutine fill_lit_triangle

  pure function brdf_light(n,v,l,radiance,base,metallic,roughness) result(outc)
    real(real32),intent(in)::n(3),v(3),l(3),radiance(3),base(3),metallic,roughness
    real(real32)::outc(3),h(3),f0(3),f(3),kd(3),spec(3),diff(3)
    real(real32)::ndotl,ndotv,ndoth,vdoth,a,a2,denom,d,k,g1,g2,g
    ndotl=max(0.0_real32,dot_product(n,l)); ndotv=max(.001_real32,dot_product(n,v))
    if(ndotl<=0.0_real32)then; outc=0.0_real32; return; end if
    h=normalize3(v+l); ndoth=max(.001_real32,dot_product(n,h)); vdoth=max(.001_real32,dot_product(v,h))
    a=max(.045_real32,roughness*roughness); a2=a*a
    denom=ndoth*ndoth*(a2-1.0_real32)+1.0_real32
    d=a2/(PI*denom*denom+.00001_real32)
    k=(roughness+1.0_real32)**2/8.0_real32
    g1=ndotv/(ndotv*(1.0_real32-k)+k); g2=ndotl/(ndotl*(1.0_real32-k)+k); g=g1*g2
    f0=.04_real32*(1.0_real32-metallic)+base*metallic
    f=f0+(1.0_real32-f0)*(1.0_real32-vdoth)**5
    spec=(d*g*f)/max(.001_real32,4.0_real32*ndotv*ndotl)
    kd=(1.0_real32-f)*(1.0_real32-metallic)
    diff=kd*base/PI
    outc=(diff+spec)*radiance*ndotl
  end function brdf_light

  pure function aces_tonemap(x) result(y)
    real(real32),intent(in)::x(3)
    real(real32)::y(3)
    y=(x*(2.51_real32*x+.03_real32))/(x*(2.43_real32*x+.59_real32)+.14_real32)
  end function aces_tonemap

  subroutine draw_glowing_line(w,height,x0,y0,z0,x1,y1,z1,pulse)
    integer,intent(in)::w,height
    real(real32),intent(in)::x0,y0,z0,x1,y1,z1,pulse
    integer::steps,i,x,y,ox,oy
    real(real32)::a,depth,r2,alpha
    steps=max(abs(nint(x1-x0)),abs(nint(y1-y0)))+1; steps=max(steps,1)
    do i=0,steps
      a=real(i,real32)/real(steps,real32); x=nint(x0+(x1-x0)*a); y=nint(y0+(y1-y0)*a); depth=z0+(z1-z0)*a
      do oy=-1,1
        do ox=-1,1
          r2=real(ox*ox+oy*oy,real32); alpha=pulse*.13_real32*exp(-.95_real32*r2)
          call blend_visible(x+ox,y+oy,depth,theme%edge_rgb,alpha,w,height)
        end do
      end do
      call plot_depth(x,y,depth-.025_real32,theme%edge_rgb,w,height)
    end do
  end subroutine draw_glowing_line

  subroutine draw_shape_outline(w,height)
    integer,intent(in)::w,height
    integer::x,y
    logical::boundary,near_surface
    real(real32)::empty_depth

    empty_depth=huge(1.0_real32)*.5_real32

    ! A restrained halo outside the silhouette softens the one-cell terminal edge.
    do y=2,height-1
      do x=2,w-1
        if(zbuffer(x,y)<empty_depth)cycle
        near_surface=zbuffer(x-1,y)<empty_depth .or. zbuffer(x+1,y)<empty_depth .or. &
                     zbuffer(x,y-1)<empty_depth .or. zbuffer(x,y+1)<empty_depth
        if(near_surface)call add_pixel(x,y,theme%edge_rgb,.075_real32)
      end do
    end do

    ! Keep every mesh readable against both the bright horizon and dark floor.
    do y=1,height
      do x=1,w
        if(zbuffer(x,y)>=empty_depth)cycle
        boundary=x==1 .or. x==w .or. y==1 .or. y==height
        if(.not.boundary)then
          boundary=zbuffer(x-1,y)>=empty_depth .or. zbuffer(x+1,y)>=empty_depth .or. &
                   zbuffer(x,y-1)>=empty_depth .or. zbuffer(x,y+1)>=empty_depth
        end if
        if(boundary)call blend_pixel(x,y,theme%edge_rgb,.42_real32)
      end do
    end do
  end subroutine draw_shape_outline

  subroutine cursor_hits_shape(cell_x,cell_y,hit)
    integer,intent(in)::cell_x,cell_y
    logical,intent(out)::hit
    integer::py
    real(real32)::empty_depth

    hit=.false.
    if(cell_x<1 .or. cell_x>cols .or. cell_y<1 .or. cell_y>rows)return
    py=max(1,min(rows*2,(cell_y-1)*2+1))
    empty_depth=huge(1.0_real32)*.5_real32
    hit=zbuffer(cell_x,py)<empty_depth .or. zbuffer(cell_x,min(py+1,rows*2))<empty_depth
  end subroutine cursor_hits_shape

  subroutine draw_interaction_cursor(w,height,cell_x,cell_y,over_shape,active)
    integer,intent(in)::w,height,cell_x,cell_y
    logical,intent(in)::over_shape,active
    integer::x,y,cx,cy,dx,dy,color(3),shadow(3)
    real(real32)::distance,radius

    cx=cell_x
    cy=(cell_y-1)*2+1
    if(cx<1 .or. cx>w .or. cy<1 .or. cy>height)return

    shadow=[1,3,8]
    if(active)then
      color=[255,255,255]
      radius=4.1_real32
    else if(over_shape)then
      color=theme%edge_rgb
      radius=3.25_real32
    else
      color=nint(.62_real32*real(theme%edge_rgb,real32)+.38_real32*real(theme%grid_rgb,real32))
      radius=3.25_real32
    end if

    do y=max(1,cy-6),min(height,cy+6)
      do x=max(1,cx-6),min(w,cx+6)
        dx=x-cx;dy=y-cy
        distance=sqrt(real(dx*dx+dy*dy,real32))
        if(abs(distance-radius)<1.05_real32)call blend_pixel(x,y,shadow,.76_real32)
        if(abs(distance-radius)<.48_real32)call blend_pixel(x,y,color,.92_real32)
      end do
    end do

    ! The open center keeps the target visible; short ticks make drag direction obvious.
    do dx=-1,1
      x=cx+dx
      if(x>=1 .and. x<=w)then
        call blend_pixel(x,cy,shadow,.72_real32)
        call blend_pixel(x,cy,color,.88_real32)
      end if
    end do
    do dy=-1,1
      y=cy+dy
      if(y>=1 .and. y<=height)then
        call blend_pixel(cx,y,shadow,.72_real32)
        call blend_pixel(cx,y,color,.88_real32)
      end if
    end do
  end subroutine draw_interaction_cursor

  subroutine project_point(point,w,height,sx,sy,depth,ok)
    real(real32),intent(in)::point(3)
    integer,intent(in)::w,height
    real(real32),intent(out)::sx,sy,depth
    logical,intent(out)::ok
    real(real32)::scale
    depth=point(3)+camera_distance
    if(depth<=.20_real32)then; sx=0.0; sy=0.0; ok=.false.; return; end if
    scale=min(real(w,real32)*1.70_real32,real(height,real32))*.80_real32
    sx=real(w,real32)*.5_real32+point(1)*scale/depth
    sy=real(height,real32)*.47_real32-point(2)*scale/depth
    ok=.true.
  end subroutine project_point

  subroutine apply_orientation_delta(axis,angle)
    real(real32),intent(in)::axis(3),angle
    real(real32)::delta(3,3),axis_length

    axis_length=sqrt(dot_product(axis,axis))
    if(axis_length<1.0e-6_real32 .or. abs(angle)<1.0e-6_real32)return
    call axis_angle_matrix(axis/axis_length,angle,delta)
    orientation=matmul(delta,orientation)
    call orthonormalize_orientation()
  end subroutine apply_orientation_delta

  subroutine axis_angle_matrix(axis,angle,matrix)
    real(real32),intent(in)::axis(3),angle
    real(real32),intent(out)::matrix(3,3)
    real(real32)::x,y,z,c,s,t

    x=axis(1);y=axis(2);z=axis(3)
    c=cos(angle);s=sin(angle);t=1.0_real32-c
    matrix(1,1)=t*x*x+c
    matrix(1,2)=t*x*y-s*z
    matrix(1,3)=t*x*z+s*y
    matrix(2,1)=t*x*y+s*z
    matrix(2,2)=t*y*y+c
    matrix(2,3)=t*y*z-s*x
    matrix(3,1)=t*x*z-s*y
    matrix(3,2)=t*y*z+s*x
    matrix(3,3)=t*z*z+c
  end subroutine axis_angle_matrix

  subroutine orthonormalize_orientation()
    real(real32)::x_axis(3),y_axis(3),z_axis(3)

    x_axis=normalize3(orientation(:,1))
    y_axis=orientation(:,2)-dot_product(orientation(:,2),x_axis)*x_axis
    y_axis=normalize3(y_axis)
    z_axis=normalize3(cross3(x_axis,y_axis))
    y_axis=normalize3(cross3(z_axis,x_axis))
    orientation(:,1)=x_axis
    orientation(:,2)=y_axis
    orientation(:,3)=z_axis
  end subroutine orthonormalize_orientation

  subroutine trackball_vector(cell_x,cell_y,vector)
    integer,intent(in)::cell_x,cell_y
    real(real32),intent(out)::vector(3)
    real(real32)::screen_x,screen_y,radius,distance2

    radius=max(4.0_real32,min(real(cols,real32),real(rows*2,real32))*.42_real32)
    screen_x=(real(cell_x,real32)-real(cols,real32)*.5_real32)/radius
    screen_y=(real(rows*2,real32)*.47_real32-real((cell_y-1)*2+1,real32))/radius
    distance2=screen_x*screen_x+screen_y*screen_y
    if(distance2<=1.0_real32)then
      vector=[screen_x,screen_y,-sqrt(max(0.0_real32,1.0_real32-distance2))]
    else
      vector=[screen_x/sqrt(distance2),screen_y/sqrt(distance2),0.0_real32]
    end if
    vector=normalize3(vector)
  end subroutine trackball_vector

  subroutine rotation_matrix(ax,ay,az,matrix)
    real(real32),intent(in)::ax,ay,az
    real(real32),intent(out)::matrix(3,3)
    real(real32)::cx,sx,cy,sy,cz,sz
    cx=cos(ax);sx=sin(ax);cy=cos(ay);sy=sin(ay);cz=cos(az);sz=sin(az)
    matrix(1,1)=cz*cy; matrix(1,2)=cz*sy*sx-sz*cx; matrix(1,3)=cz*sy*cx+sz*sx
    matrix(2,1)=sz*cy; matrix(2,2)=sz*sy*sx+cz*cx; matrix(2,3)=sz*sy*cx-cz*sx
    matrix(3,1)=-sy; matrix(3,2)=cy*sx; matrix(3,3)=cy*cx
  end subroutine rotation_matrix

  subroutine plot_depth(x,y,depth,color,w,height)
    integer,intent(in)::x,y,color(3),w,height
    real(real32),intent(in)::depth
    if(x<1.or.x>w.or.y<1.or.y>height)return
    if(depth<zbuffer(x,y))then; zbuffer(x,y)=depth; pixels(x,y)=pack_rgb(color(1),color(2),color(3)); end if
  end subroutine plot_depth

  subroutine blend_visible(x,y,depth,color,alpha,w,height)
    integer,intent(in)::x,y,color(3),w,height
    real(real32),intent(in)::depth,alpha
    if(x<1.or.x>w.or.y<1.or.y>height)return
    if(depth<=zbuffer(x,y)+.08_real32)call blend_pixel(x,y,color,alpha)
  end subroutine blend_visible

  subroutine blend_pixel(x,y,color,alpha)
    integer,intent(in)::x,y,color(3)
    real(real32),intent(in)::alpha
    integer::old(3),mixed(3); real(real32)::a
    a=clamp01(alpha); call unpack_rgb(pixels(x,y),old)
    mixed=nint(real(old,real32)*(1.0_real32-a)+real(color,real32)*a)
    pixels(x,y)=pack_rgb(mixed(1),mixed(2),mixed(3))
  end subroutine blend_pixel

  subroutine add_pixel(x,y,color,amount)
    integer,intent(in)::x,y,color(3)
    real(real32),intent(in)::amount
    integer::old(3),mixed(3)
    call unpack_rgb(pixels(x,y),old); mixed=old+nint(real(color,real32)*max(amount,0.0_real32))
    pixels(x,y)=pack_rgb(clamp255(real(mixed(1),real32)),clamp255(real(mixed(2),real32)),clamp255(real(mixed(3),real32)))
  end subroutine add_pixel

  subroutine build_frame_delta(w,terminal_rows,used,full)
    integer,intent(in)::w,terminal_rows
    integer,intent(out)::used
    logical,intent(in)::full
    integer::row,x,ty,by,pos,last_fg,last_bg,fg,bg
    logical::changed,in_run
    pos=1; last_fg=-2; last_bg=-2; in_run=.false.
    do row=1,terminal_rows
      ty=(row-1)*2+1; by=ty+1; in_run=.false.
      do x=1,w
        changed=full .or. pixels(x,ty)/=previous_pixels(x,ty) .or. pixels(x,by)/=previous_pixels(x,by)
        if(.not.changed)then
          in_run=.false.; cycle
        end if
        if(.not.in_run)then
          call append_cursor(pos,row,x)
          last_fg=-2; last_bg=-2; in_run=.true.
        end if
        fg=pixels(x,ty); bg=pixels(x,by)
        if(fg==last_fg .and. bg==last_bg)then
          call append_glyph(pos)
        else if(fg==last_fg)then
          call append_bg(pos,bg); call append_glyph(pos); last_bg=bg
        else if(bg==last_bg)then
          call append_fg(pos,fg); call append_glyph(pos); last_fg=fg
        else
          call append_pair(pos,fg,bg); call append_glyph(pos); last_fg=fg; last_bg=bg
        end if
      end do
    end do
    previous_pixels=pixels
    used=pos-1
  end subroutine build_frame_delta

  subroutine append_cursor(pos,row,col)
    integer,intent(inout)::pos
    integer,intent(in)::row,col
    character(len=24)::tmp
    integer::n
    write(tmp,'(A,I0,A,I0,A)')ESC//'[',row,';',col,'H'
    n=len_trim(tmp); frame_text(pos:pos+n-1)=tmp(1:n); pos=pos+n
  end subroutine append_cursor

  subroutine append_pair(pos,fg,bg)
    integer,intent(inout)::pos
    integer,intent(in)::fg,bg
    integer::a(3),b(3)
    call unpack_rgb(fg,a);call unpack_rgb(bg,b)
    frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[38;2;';pos=pos+6
    call append_rgb_triplet(pos,a)
    frame_text(pos:pos+5)=';48;2;';pos=pos+6
    call append_rgb_triplet(pos,b)
    frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_pair

  subroutine append_fg(pos,fg)
    integer,intent(inout)::pos
    integer,intent(in)::fg
    integer::a(3)
    call unpack_rgb(fg,a);frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[38;2;';pos=pos+6
    call append_rgb_triplet(pos,a);frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_fg

  subroutine append_bg(pos,bg)
    integer,intent(inout)::pos
    integer,intent(in)::bg
    integer::a(3)
    call unpack_rgb(bg,a);frame_text(pos:pos)=ESC;pos=pos+1;frame_text(pos:pos+5)='[48;2;';pos=pos+6
    call append_rgb_triplet(pos,a);frame_text(pos:pos)='m';pos=pos+1
  end subroutine append_bg

  subroutine append_rgb_triplet(pos,a)
    integer,intent(inout)::pos
    integer,intent(in)::a(3)
    frame_text(pos:pos+2)=decimal3(a(1));pos=pos+3;frame_text(pos:pos)=';';pos=pos+1
    frame_text(pos:pos+2)=decimal3(a(2));pos=pos+3;frame_text(pos:pos)=';';pos=pos+1
    frame_text(pos:pos+2)=decimal3(a(3));pos=pos+3
  end subroutine append_rgb_triplet

  subroutine append_glyph(pos)
    integer,intent(inout)::pos
    frame_text(pos:pos+2)=UPPER_HALF;pos=pos+3
  end subroutine append_glyph

  subroutine write_hud()
    character(len=160)::identity,status,help_text
    character(len=12)::motion
    integer::bg(3),accent_bg(3),fg(3),muted(3),status_col,identity_col

    bg=min(theme%floor_rgb+[7,8,12],[32,34,42])
    accent_bg=min(nint(.28_real32*real(theme%base_a,real32)+.72_real32*real(bg,real32)),[86,86,94])
    fg=theme%edge_rgb
    muted=nint(.62_real32*real(fg,real32)+.38_real32*real(bg,real32))
    motion=merge('AUTO        ','PAUSED      ',auto_rotate)
    write(identity,'(A,A,A)')trim(mesh%name),'  /  ',trim(theme%name)
    write(status,'(A,A,F3.1,A,F3.1)')trim(motion),'  ',auto_speed,'x  |  zoom ',camera_distance

    call paint_hud_row(1,bg)
    call write_hud_text(1,2,' SHAPE LAB ',fg,accent_bg,.true.)
    identity_col=15
    call write_hud_text(1,identity_col,trim(identity),fg,bg,.true.)
    status_col=cols-len_trim(status)
    if(status_col>identity_col+len_trim(identity)+2) &
      call write_hud_text(1,status_col,trim(status),muted,bg,.false.)

    if(cols>=117)then
      help_text=' DRAG free rotate   WHEEL zoom   ARROWS / WASD rotate   1-5 shape   T theme'// &
                '   SPACE pause   R reset   H hide   Q quit'
    else if(cols>=78)then
      help_text=' DRAG free rotate  WHEEL zoom  WASD rotate  1-5 shape  T theme  Q quit  H hide'
    else if(cols>=47)then
      help_text=' DRAG free  WHEEL zoom  1-5 shape  Q quit  H UI'
    else
      help_text=' DRAG free  WHEEL zoom  Q quit  H UI'
    end if
    call paint_hud_row(rows,bg)
    call write_hud_text(rows,1,trim(help_text),muted,bg,.false.)
    write(output_unit,'(A)',advance='no')ESC//'[0m'
  end subroutine write_hud

  subroutine paint_hud_row(row,bg)
    integer,intent(in)::row,bg(3)
    character(len=:),allocatable::blank
    allocate(character(len=cols)::blank)
    blank(:)=' '
    call move_terminal_cursor(row,1)
    call set_hud_style([232,238,247],bg,.false.)
    write(output_unit,'(A)',advance='no')blank
  end subroutine paint_hud_row

  subroutine write_hud_text(row,col,text_value,fg,bg,bold)
    integer,intent(in)::row,col,fg(3),bg(3)
    character(len=*),intent(in)::text_value
    logical,intent(in)::bold
    integer::n
    if(col<1 .or. col>cols)return
    n=min(len_trim(text_value),cols-col+1)
    if(n<=0)return
    call move_terminal_cursor(row,col)
    call set_hud_style(fg,bg,bold)
    write(output_unit,'(A)',advance='no')text_value(1:n)
  end subroutine write_hud_text

  subroutine move_terminal_cursor(row,col)
    integer,intent(in)::row,col
    write(output_unit,'(A,I0,A,I0,A)',advance='no')ESC//'[',row,';',col,'H'
  end subroutine move_terminal_cursor

  subroutine set_hud_style(fg,bg,bold)
    integer,intent(in)::fg(3),bg(3)
    logical,intent(in)::bold
    write(output_unit,'(A)',advance='no')ESC//'[0m'
    if(bold)write(output_unit,'(A)',advance='no')ESC//'[1m'
    write(output_unit,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)',advance='no') &
      ESC//'[38;2;',fg(1),';',fg(2),';',fg(3),';48;2;',bg(1),';',bg(2),';',bg(3),'m'
  end subroutine set_hud_style

  subroutine poll_input(frame_dt)
    real(real32),intent(in)::frame_dt
    integer(c_long)::nread
    integer::i
    character(len=1)::ch
    do
      nread=c_read(0_c_int,c_loc(input_bytes(1)),int(size(input_bytes),c_size_t))
      if(nread<=0_c_long)exit
      do i=1,int(nread)
        ch=achar(iachar(input_bytes(i)))
        call feed_input_char(ch,frame_dt)
      end do
      if(nread<int(size(input_bytes),c_long))exit
    end do
  end subroutine poll_input

  subroutine feed_input_char(ch,frame_dt)
    character(len=1),intent(in)::ch
    real(real32),intent(in)::frame_dt
    select case(input_state)
    case(0)
      if(iachar(ch)==27)then
        input_state=1
      else
        call handle_key(ch)
      end if
    case(1)
      if(ch=='[')then;input_state=2;else;input_state=0;end if
    case(2)
      if(ch=='<')then
        input_state=3;mouse_len=0;mouse_text=''
      else
        call handle_arrow_key(ch)
        input_state=0
      end if
    case(3)
      if(ch=='M'.or.ch=='m')then
        call handle_mouse(mouse_text(1:max(1,mouse_len)),ch=='m',frame_dt)
        input_state=0;mouse_len=0
      else if(mouse_len<len(mouse_text))then
        mouse_len=mouse_len+1;mouse_text(mouse_len:mouse_len)=ch
      else
        input_state=0;mouse_len=0
      end if
    end select
  end subroutine feed_input_char

  subroutine handle_key(ch)
    character(len=1),intent(in)::ch
    integer::v
    select case(ch)
    case('q','Q')
      stop_requested=1_c_int
    case(' ')
      auto_rotate=.not.auto_rotate; force_redraw=.true.
    case('t','T')
      theme_id=1+mod(theme_id,max_theme); call load_theme(theme_id,theme); call rebuild_background()
    case('r','R')
      call rotation_matrix(-.34_real32,.62_real32,.08_real32,orientation)
      camera_distance=5.0_real32;angular_velocity=0.0_real32
      interaction_quiet=.8_real32;force_redraw=.true.
    case('h','H')
      show_hud=.not.show_hud; force_redraw=.true.
    case('[','-')
      auto_speed=max(.15_real32,auto_speed-.15_real32)
    case(']','+')
      auto_speed=min(3.0_real32,auto_speed+.15_real32)
    case('a','A')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],.12_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('d','D')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],-.12_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('w','W')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],.10_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('s','S')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],-.10_real32)
      angular_velocity=0.0_real32;interaction_quiet=.5_real32
    case('z','Z')
      camera_distance=max(3.15_real32,camera_distance-.25_real32); interaction_quiet=.5_real32
    case('x','X')
      camera_distance=min(9.0_real32,camera_distance+.25_real32); interaction_quiet=.5_real32
    case('1','2','3','4','5')
      read(ch,*)v; shape_id=v; call load_shape(shape_id,mesh); interaction_quiet=.45_real32; force_redraw=.true.
    case('n','N')
      shape_id=1+mod(shape_id,max_shape);call load_shape(shape_id,mesh)
      interaction_quiet=.45_real32;force_redraw=.true.
    end select
  end subroutine handle_key

  subroutine handle_arrow_key(ch)
    character(len=1),intent(in)::ch
    select case(ch)
    case('A')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],.10_real32)
    case('B')
      call apply_orientation_delta([1.0_real32,0.0_real32,0.0_real32],-.10_real32)
    case('C')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],-.12_real32)
    case('D')
      call apply_orientation_delta([0.0_real32,1.0_real32,0.0_real32],.12_real32)
    case default
      return
    end select
    angular_velocity=0.0_real32;interaction_quiet=.5_real32
  end subroutine handle_arrow_key

  subroutine handle_mouse(text,released,frame_dt)
    character(len=*),intent(in)::text
    logical,intent(in)::released
    real(real32),intent(in)::frame_dt
    character(len=64)::tmp
    integer::i,ios,b,x,y,base,dx,dy
    real(real32)::current_vector(3),axis(3),axis_length,angle,dot_value,speed
    tmp='';tmp(1:min(len_trim(text),len(tmp)))=text(1:min(len_trim(text),len(tmp)))
    do i=1,len_trim(tmp);if(tmp(i:i)==';')tmp(i:i)=' ';end do
    read(tmp,*,iostat=ios)b,x,y
    if(ios/=0)return
    if(x>=1 .and. x<=cols .and. y>=1 .and. y<=rows)then
      mouse_present=.true.
    end if
    base=iand(b,3)
    if(iand(b,64)/=0)then
      if(iand(b,1)==0)then
        camera_distance=max(3.15_real32,camera_distance-.28_real32)
      else
        camera_distance=min(9.0_real32,camera_distance+.28_real32)
      end if
      mouse_x=x;mouse_y=y
      interaction_quiet=.7_real32;force_redraw=.true.;return
    end if
    if(released)then
      mouse_x=x;mouse_y=y
      dragging=.false.;interaction_quiet=.85_real32;return
    end if
    if(iand(b,32)/=0 .and. dragging)then
      dx=x-mouse_x;dy=y-mouse_y
      if(abs(dx)<=40.and.abs(dy)<=25)then
        call trackball_vector(x,y,current_vector)
        axis=cross3(drag_vector,current_vector)
        axis_length=sqrt(dot_product(axis,axis))
        dot_value=clamp_real(dot_product(drag_vector,current_vector),-1.0_real32,1.0_real32)
        if(axis_length>1.0e-5_real32)then
          angle=atan2(axis_length,dot_value)
          axis=axis/axis_length
          call apply_orientation_delta(axis,angle)
          speed=min(8.0_real32,angle/max(frame_dt,.008_real32))
          angular_velocity=axis*speed
        end if
        drag_vector=current_vector
      end if
      mouse_x=x;mouse_y=y;interaction_quiet=.85_real32;return
    end if
    if(iand(b,32)/=0)then
      mouse_x=x;mouse_y=y;return
    end if
    if(base==0)then
      if(x>=1.and.x<=cols.and.y>1.and.y<rows)then
        dragging=.true.;mouse_x=x;mouse_y=y;angular_velocity=0.0_real32
        call trackball_vector(x,y,drag_vector)
        interaction_quiet=.85_real32
      end if
    end if
  end subroutine handle_mouse

  pure function cross3(a,b) result(c)
    real(real32),intent(in)::a(3),b(3)
    real(real32)::c(3)
    c=[a(2)*b(3)-a(3)*b(2),a(3)*b(1)-a(1)*b(3),a(1)*b(2)-a(2)*b(1)]
  end function cross3

  pure function normalize3(v) result(n)
    real(real32),intent(in)::v(3)
    real(real32)::n(3),ls
    ls=dot_product(v,v);if(ls<=1.0e-12_real32)then;n=0.0_real32;else;n=v/sqrt(ls);end if
  end function normalize3

  pure function clamp_vec(v,lo,hi) result(r)
    real(real32),intent(in)::v(3),lo,hi
    real(real32)::r(3);r=min(max(v,lo),hi)
  end function clamp_vec

  pure real(real32) function clamp01(v)
    real(real32),intent(in)::v;clamp01=min(max(v,0.0_real32),1.0_real32)
  end function clamp01

  pure real(real32) function clamp_real(v,lo,hi)
    real(real32),intent(in)::v,lo,hi;clamp_real=min(max(v,lo),hi)
  end function clamp_real

  pure integer function clamp255(v)
    real(real32),intent(in)::v;clamp255=min(max(nint(v),0),255)
  end function clamp255

  pure real(real32) function edge_function(ax,ay,bx,by,cx,cy)
    real(real32),intent(in)::ax,ay,bx,by,cx,cy
    edge_function=(cx-ax)*(by-ay)-(cy-ay)*(bx-ax)
  end function edge_function

  pure integer(int32) function pack_rgb(r,g,b)
    integer,intent(in)::r,g,b
    pack_rgb=ishft(int(iand(r,255),int32),16)+ishft(int(iand(g,255),int32),8)+int(iand(b,255),int32)
  end function pack_rgb

  pure subroutine unpack_rgb(packed,rgb)
    integer(int32),intent(in)::packed
    integer,intent(out)::rgb(3)
    rgb(1)=int(ibits(packed,16,8));rgb(2)=int(ibits(packed,8,8));rgb(3)=int(ibits(packed,0,8))
  end subroutine unpack_rgb

end program fortran_shape_lab
