program fortran_spincube
  use, intrinsic :: iso_fortran_env, only: real64, output_unit
  use, intrinsic :: iso_c_binding, only: c_int, c_short, c_long, c_ptr, c_loc
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
  end interface

  type, bind(C) :: winsize
    integer(c_short) :: ws_row
    integer(c_short) :: ws_col
    integer(c_short) :: ws_xpixel
    integer(c_short) :: ws_ypixel
  end type winsize

  integer, parameter :: fps = 24
  integer(c_long), parameter :: TIOCGWINSZ = int(z'5413', c_long)
  real(real64), parameter :: distance = 5.0_real64
  character(len=1), parameter :: ESC = achar(27)
  character(len=*), parameter :: UPPER_HALF = '▀'

  real(real64), dimension(3,8) :: verts
  integer, dimension(2,12) :: edges
  integer, dimension(4,6) :: faces
  real(real64), dimension(3,6) :: normals
  integer, dimension(3,6) :: face_colors

  integer :: cols, rows, ph
  real(real64) :: yaw, pitch, roll, t

  call init_geometry(verts, edges, faces, normals, face_colors)

  yaw = 0.55_real64
  pitch = -0.28_real64
  roll = 0.08_real64
  t = 0.0_real64

  write(output_unit,'(A)',advance='no') ESC//'[2J'//ESC//'[?25l'
  flush(output_unit)

  do
    call terminal_size(cols, rows)
    ph = max(2, rows * 2)

    write(output_unit,'(A)',advance='no') ESC//'[H'
    call render_frame(cols, rows, ph, t, yaw, pitch, roll, verts, edges, faces, normals, face_colors)
    write(output_unit,'(A)',advance='no') ESC//'[0m'//ESC//'[J'
    flush(output_unit)

    yaw = yaw + 0.026_real64
    pitch = pitch + 0.006_real64
    roll = roll + 0.004_real64
    t = t + 1.0_real64 / real(fps, real64)

    call c_usleep(int(1000000.0_real64 / real(fps, real64), c_int))
  end do

contains

  subroutine init_geometry(verts, edges, faces, normals, face_colors)
    real(real64), intent(out) :: verts(3,8)
    integer, intent(out) :: edges(2,12)
    integer, intent(out) :: faces(4,6)
    real(real64), intent(out) :: normals(3,6)
    integer, intent(out) :: face_colors(3,6)

    verts = reshape([ &
      -1.0_real64, -1.0_real64, -1.0_real64, &
       1.0_real64, -1.0_real64, -1.0_real64, &
       1.0_real64,  1.0_real64, -1.0_real64, &
      -1.0_real64,  1.0_real64, -1.0_real64, &
      -1.0_real64, -1.0_real64,  1.0_real64, &
       1.0_real64, -1.0_real64,  1.0_real64, &
       1.0_real64,  1.0_real64,  1.0_real64, &
      -1.0_real64,  1.0_real64,  1.0_real64  &
    ], [3,8])

    edges = reshape([ &
      1,2, 2,3, 3,4, 4,1, &
      5,6, 6,7, 7,8, 8,5, &
      1,5, 2,6, 3,7, 4,8  &
    ], [2,12])

    faces = reshape([ &
      1,2,3,4, &
      5,6,7,8, &
      1,4,8,5, &
      2,6,7,3, &
      1,5,6,2, &
      4,3,7,8  &
    ], [4,6])

    normals = reshape([ &
       0.0_real64,  0.0_real64, -1.0_real64, &
       0.0_real64,  0.0_real64,  1.0_real64, &
      -1.0_real64,  0.0_real64,  0.0_real64, &
       1.0_real64,  0.0_real64,  0.0_real64, &
       0.0_real64, -1.0_real64,  0.0_real64, &
       0.0_real64,  1.0_real64,  0.0_real64  &
    ], [3,6])

    face_colors = reshape([ &
       82, 150, 255, &
       46,  95, 215, &
       52, 215, 230, &
      255, 125,  92, &
      128, 140, 185, &
      225, 235, 255  &
    ], [3,6])
  end subroutine init_geometry

  subroutine terminal_size(cols, rows)
    integer, intent(out) :: cols, rows
    type(winsize), target :: ws
    integer(c_int) :: rc
    character(len=64) :: env
    integer :: stat, ios

    cols = 100
    rows = 40

    ws%ws_row = 0_c_short
    ws%ws_col = 0_c_short
    ws%ws_xpixel = 0_c_short
    ws%ws_ypixel = 0_c_short

    rc = c_ioctl(1_c_int, TIOCGWINSZ, c_loc(ws))
    if (rc == 0 .and. int(ws%ws_col) > 0 .and. int(ws%ws_row) > 0) then
      cols = int(ws%ws_col)
      rows = int(ws%ws_row)
    else
      call get_environment_variable('COLUMNS', env, status=stat)
      if (stat == 0) then
        read(env,*,iostat=ios) cols
        if (ios /= 0) cols = 100
      end if

      call get_environment_variable('LINES', env, status=stat)
      if (stat == 0) then
        read(env,*,iostat=ios) rows
        if (ios /= 0) rows = 40
      end if
    end if

    cols = max(cols, 40)
    rows = max(rows, 16)
  end subroutine terminal_size

  subroutine render_frame(w, rows, ph, t, yaw, pitch, roll, verts, edges, faces, normals, face_colors)
    integer, intent(in) :: w, rows, ph
    real(real64), intent(in) :: t, yaw, pitch, roll
    real(real64), intent(in) :: verts(3,8), normals(3,6)
    integer, intent(in) :: edges(2,12), faces(4,6), face_colors(3,6)

    integer, allocatable :: rr(:,:), gg(:,:), bb(:,:)
    real(real64), allocatable :: zz(:,:)
    real(real64) :: sx(8), sy(8), sz(8)
    logical :: ok(8)
    real(real64) :: r(3), rn(3)
    real(real64) :: light_dir(3), view_dir(3), half_dir(3)
    real(real64) :: diffuse, rim, specular, light
    integer :: i, f

    allocate(rr(w,ph), gg(w,ph), bb(w,ph), zz(w,ph))
    call make_background(w, ph, rr, gg, bb, zz)

    light_dir = normalize_vec([-0.48_real64, -0.62_real64, -1.0_real64])
    view_dir  = normalize_vec([0.0_real64, 0.0_real64, -1.0_real64])
    half_dir  = normalize_vec(light_dir + view_dir)

    do i = 1, 8
      r = rotate_vec(verts(:,i), pitch, yaw, roll)
      call project_point(r, w, ph, sx(i), sy(i), sz(i), ok(i))
    end do

    do f = 1, 6
      rn = rotate_vec(normals(:,f), pitch, yaw, roll)

      if (rn(3) > 0.08_real64) cycle
      if (.not. all(ok(faces(:,f)))) cycle

      diffuse = max(0.0_real64, dot_product(rn, light_dir))
      rim = (1.0_real64 - abs(rn(3))) * 0.20_real64
      specular = max(0.0_real64, dot_product(rn, half_dir)) ** 36.0_real64
      light = clamp(0.15_real64 + diffuse * 0.83_real64 + rim + specular * 0.70_real64, 0.0_real64, 1.0_real64)

      call fill_tri(w, ph, rr, gg, bb, zz, &
        sx(faces(1,f)), sy(faces(1,f)), sz(faces(1,f)), &
        sx(faces(2,f)), sy(faces(2,f)), sz(faces(2,f)), &
        sx(faces(3,f)), sy(faces(3,f)), sz(faces(3,f)), &
        face_colors(:,f), light, specular, t)

      call fill_tri(w, ph, rr, gg, bb, zz, &
        sx(faces(1,f)), sy(faces(1,f)), sz(faces(1,f)), &
        sx(faces(3,f)), sy(faces(3,f)), sz(faces(3,f)), &
        sx(faces(4,f)), sy(faces(4,f)), sz(faces(4,f)), &
        face_colors(:,f), light, specular, t)
    end do

    do i = 1, 12
      if (ok(edges(1,i)) .and. ok(edges(2,i))) then
        call draw_line(w, ph, rr, gg, bb, zz, &
          sx(edges(1,i)), sy(edges(1,i)), sz(edges(1,i)), &
          sx(edges(2,i)), sy(edges(2,i)), sz(edges(2,i)), &
          [242, 246, 255])
      end if
    end do

    call blit(w, rows, rr, gg, bb)

    deallocate(rr, gg, bb, zz)
  end subroutine render_frame

  subroutine make_background(w, ph, rr, gg, bb, zz)
    integer, intent(in) :: w, ph
    integer, intent(out) :: rr(w,ph), gg(w,ph), bb(w,ph)
    real(real64), intent(out) :: zz(w,ph)
    integer :: x, y
    real(real64) :: fx, fy, dx, dy, glow, vignette, base

    do y = 1, ph
      fy = real(y - 1, real64) / real(max(ph - 1, 1), real64)
      do x = 1, w
        fx = real(x - 1, real64) / real(max(w - 1, 1), real64)
        dx = (fx - 0.50_real64) / 0.62_real64
        dy = (fy - 0.48_real64) / 0.78_real64
        glow = clamp(1.0_real64 - (dx*dx + dy*dy), 0.0_real64, 1.0_real64)
        vignette = clamp(1.0_real64 - 0.55_real64*(dx*dx + dy*dy), 0.0_real64, 1.0_real64)
        base = (8.0_real64 + 17.0_real64*glow + 9.0_real64*fy) * vignette
        rr(x,y) = int(base * 0.62_real64)
        gg(x,y) = int(base * 0.72_real64)
        bb(x,y) = int(base * 1.05_real64 + 4.0_real64)
        zz(x,y) = 1.0e30_real64
      end do
    end do
  end subroutine make_background

  function normalize_vec(v) result(o)
    real(real64), intent(in) :: v(3)
    real(real64) :: o(3), len
    len = sqrt(dot_product(v, v))
    if (len <= 1.0e-10_real64) then
      o = [0.0_real64, 0.0_real64, 0.0_real64]
    else
      o = v / len
    end if
  end function normalize_vec

  function rotate_vec(p, ax, ay, az) result(o)
    real(real64), intent(in) :: p(3), ax, ay, az
    real(real64) :: o(3)
    real(real64) :: x, y, z, nx, ny, nz
    real(real64) :: cx, sx, cy, sy, cz, sz

    x = p(1)
    y = p(2)
    z = p(3)

    cx = cos(ax)
    sx = sin(ax)
    cy = cos(ay)
    sy = sin(ay)
    cz = cos(az)
    sz = sin(az)

    ny = y * cx - z * sx
    nz = y * sx + z * cx
    y = ny
    z = nz

    nx = x * cy + z * sy
    nz = -x * sy + z * cy
    x = nx
    z = nz

    nx = x * cz - y * sz
    ny = x * sz + y * cz

    o = [nx, ny, z]
  end function rotate_vec

  subroutine project_point(p, w, ph, sx, sy, depth, ok)
    real(real64), intent(in) :: p(3)
    integer, intent(in) :: w, ph
    real(real64), intent(out) :: sx, sy, depth
    logical, intent(out) :: ok
    real(real64) :: scale

    depth = p(3) + distance
    if (depth <= 0.20_real64) then
      sx = 0.0_real64
      sy = 0.0_real64
      ok = .false.
      return
    end if

    scale = min(real(w, real64), real(ph, real64)) * 0.72_real64
    sx = real(w, real64) * 0.5_real64 + p(1) * scale / depth
    sy = real(ph, real64) * 0.5_real64 - p(2) * scale / depth
    ok = .true.
  end subroutine project_point

  real(real64) function clamp(x, lo, hi)
    real(real64), intent(in) :: x, lo, hi
    clamp = min(max(x, lo), hi)
  end function clamp

  integer function clamp255(x)
    real(real64), intent(in) :: x
    clamp255 = int(clamp(real(nint(x), real64), 0.0_real64, 255.0_real64))
  end function clamp255

  real(real64) function edge_fn(ax, ay, bx, by, cx, cy)
    real(real64), intent(in) :: ax, ay, bx, by, cx, cy
    edge_fn = (cx - ax) * (by - ay) - (cy - ay) * (bx - ax)
  end function edge_fn

  subroutine plot(w, ph, rr, gg, bb, zz, x, y, z, col)
    integer, intent(in) :: w, ph, x, y, col(3)
    integer, intent(inout) :: rr(w,ph), gg(w,ph), bb(w,ph)
    real(real64), intent(inout) :: zz(w,ph)
    real(real64), intent(in) :: z

    if (x < 1 .or. y < 1 .or. x > w .or. y > ph) return

    if (z < zz(x,y)) then
      zz(x,y) = z
      rr(x,y) = col(1)
      gg(x,y) = col(2)
      bb(x,y) = col(3)
    end if
  end subroutine plot

  subroutine fill_tri(w, ph, rr, gg, bb, zz, x0,y0,z0, x1,y1,z1, x2,y2,z2, base_col, light, specular, t)
    integer, intent(in) :: w, ph, base_col(3)
    integer, intent(inout) :: rr(w,ph), gg(w,ph), bb(w,ph)
    real(real64), intent(inout) :: zz(w,ph)
    real(real64), intent(in) :: x0,y0,z0, x1,y1,z1, x2,y2,z2, light, specular, t

    integer :: minx, maxx, miny, maxy, x, y
    real(real64) :: area, sgn, area_abs, px, py
    real(real64) :: e0, e1, e2, b0, b1, b2, z
    real(real64) :: grain, fog, bright
    integer :: col(3)

    minx = max(1, int(floor(min(x0, min(x1, x2)))))
    maxx = min(w, int(ceiling(max(x0, max(x1, x2)))))
    miny = max(1, int(floor(min(y0, min(y1, y2)))))
    maxy = min(ph, int(ceiling(max(y0, max(y1, y2)))))

    if (minx > maxx .or. miny > maxy) return

    area = edge_fn(x0,y0,x1,y1,x2,y2)
    if (abs(area) < 1.0e-8_real64) return

    if (area < 0.0_real64) then
      sgn = -1.0_real64
    else
      sgn = 1.0_real64
    end if
    area_abs = area * sgn

    do y = miny, maxy
      do x = minx, maxx
        px = real(x, real64) - 0.5_real64
        py = real(y, real64) - 0.5_real64

        e0 = edge_fn(x1,y1,x2,y2,px,py) * sgn
        e1 = edge_fn(x2,y2,x0,y0,px,py) * sgn
        e2 = edge_fn(x0,y0,x1,y1,px,py) * sgn

        if (e0 < -0.001_real64 .or. e1 < -0.001_real64 .or. e2 < -0.001_real64) cycle

        b0 = e0 / area_abs
        b1 = e1 / area_abs
        b2 = e2 / area_abs
        z = z0*b0 + z1*b1 + z2*b2

        grain = (real(mod(x*17 + y*29 + int(t*39.0_real64), 41), real64) / 40.0_real64 - 0.5_real64) * 0.017_real64
        fog = clamp(1.35_real64 - z * 0.105_real64, 0.42_real64, 1.0_real64)
        bright = clamp((light + grain) * fog, 0.0_real64, 1.0_real64)

        col(1) = clamp255(real(base_col(1), real64) * bright + 255.0_real64 * specular * 0.55_real64)
        col(2) = clamp255(real(base_col(2), real64) * bright + 255.0_real64 * specular * 0.55_real64)
        col(3) = clamp255(real(base_col(3), real64) * bright + 255.0_real64 * specular * 0.55_real64)

        call plot(w, ph, rr, gg, bb, zz, x, y, z, col)
      end do
    end do
  end subroutine fill_tri

  subroutine draw_line(w, ph, rr, gg, bb, zz, x0,y0,z0, x1,y1,z1, col)
    integer, intent(in) :: w, ph, col(3)
    integer, intent(inout) :: rr(w,ph), gg(w,ph), bb(w,ph)
    real(real64), intent(inout) :: zz(w,ph)
    real(real64), intent(in) :: x0,y0,z0, x1,y1,z1
    integer :: steps, i, x, y
    real(real64) :: a, z

    steps = max(abs(nint(x1 - x0)), abs(nint(y1 - y0))) + 1
    steps = max(steps, 1)

    do i = 0, steps
      a = real(i, real64) / real(steps, real64)
      x = nint(x0 + (x1 - x0) * a)
      y = nint(y0 + (y1 - y0) * a)
      z = z0 + (z1 - z0) * a - 0.025_real64
      call plot(w, ph, rr, gg, bb, zz, x, y, z, col)
    end do
  end subroutine draw_line

  subroutine blit(w, rows, rr, gg, bb)
    integer, intent(in) :: w, rows
    integer, intent(in) :: rr(w,rows*2), gg(w,rows*2), bb(w,rows*2)
    integer :: row, x, y0, y1

    do row = 1, rows
      y0 = (row - 1) * 2 + 1
      y1 = y0 + 1

      do x = 1, w
        write(output_unit,'(A)',advance='no') &
          ESC//'[38;2;'//trim(itoa(rr(x,y0)))//';'//trim(itoa(gg(x,y0)))//';'//trim(itoa(bb(x,y0)))//'m'// &
          ESC//'[48;2;'//trim(itoa(rr(x,y1)))//';'//trim(itoa(gg(x,y1)))//';'//trim(itoa(bb(x,y1)))//'m'// &
          UPPER_HALF
      end do

      if (row < rows) write(output_unit,'(A)',advance='no') ESC//'[0m'//new_line('a')
    end do
  end subroutine blit

  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=16) :: s
    write(s,'(I0)') i
  end function itoa

end program fortran_spincube
