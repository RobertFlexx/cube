module spincube_runtime
  use, intrinsic :: iso_c_binding, only: c_int, c_funptr
  implicit none

  integer(c_int), volatile, save :: stop_requested = 0_c_int

contains

  subroutine handle_signal(sig) bind(C)
    integer(c_int), value :: sig
    stop_requested = sig
  end subroutine handle_signal

end module spincube_runtime

program fortran_spincube_hq
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, output_unit
  use, intrinsic :: iso_c_binding, only: c_int, c_short, c_long, c_ptr, c_loc, c_funptr, c_funloc
  use spincube_runtime, only: stop_requested, handle_signal
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
  end interface

  type, bind(C) :: winsize
    integer(c_short) :: ws_row
    integer(c_short) :: ws_col
    integer(c_short) :: ws_xpixel
    integer(c_short) :: ws_ypixel
  end type winsize

  integer, parameter :: target_fps = 30
  integer, parameter :: resize_check_frames = 12
  integer, parameter :: cell_bytes = 39
  integer(c_int), parameter :: SIGINT = 2_c_int
  integer(c_int), parameter :: SIGTERM = 15_c_int
  integer(c_long), parameter :: TIOCGWINSZ = int(z'5413', c_long)
  real(real32), parameter :: camera_distance = 5.2_real32
  real(real64), parameter :: target_frame_time = 1.0_real64 / real(target_fps, real64)
  character(len=1), parameter :: ESC = achar(27)
  character(len=3), parameter :: UPPER_HALF = achar(226)//achar(150)//achar(128)

  real(real32), dimension(3,8) :: verts
  integer, dimension(2,12) :: edges
  integer, dimension(4,6) :: faces
  real(real32), dimension(3,6) :: normals
  integer, dimension(3,6) :: face_colors

  integer(int32), allocatable :: pixels(:,:), background(:,:)
  real(real32), allocatable :: zbuffer(:,:)
  character(len=:), allocatable :: frame_text
  character(len=3), save :: decimal3(0:255)

  integer :: cols, rows, ph, new_cols, new_rows
  integer :: frame_number, used_bytes
  integer(int64) :: clock_rate, frame_start, frame_end, previous_start
  real(real64) :: dt, elapsed, sleep_time, t
  real(real32) :: yaw, pitch, roll
  type(c_funptr) :: previous_handler

  call init_decimal_table()
  call init_geometry(verts, edges, faces, normals, face_colors)

  previous_handler = c_signal(SIGINT, c_funloc(handle_signal))
  previous_handler = c_signal(SIGTERM, c_funloc(handle_signal))

  yaw = 0.55_real32
  pitch = -0.28_real32
  roll = 0.08_real32
  t = 0.0_real64
  frame_number = 0

  call terminal_size(cols, rows)
  call ensure_buffers(cols, rows)

  write(output_unit,'(A)',advance='no') &
    ESC//'[?1049h'//ESC//'[?25l'//ESC//'[?7l'//ESC//'[2J'//ESC//'[H'
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
        write(output_unit,'(A)',advance='no') ESC//'[2J'//ESC//'[H'
      end if
    end if

    ph = rows * 2
    t = t + dt
    yaw = yaw + real(dt, real32) * 0.78_real32
    pitch = pitch + real(dt, real32) * 0.17_real32
    roll = roll + real(dt, real32) * 0.11_real32

    pixels = background
    zbuffer = huge(1.0_real32)

    call render_frame(cols, rows, ph, real(t, real32), yaw, pitch, roll, &
      verts, edges, faces, normals, face_colors)
    call build_frame(cols, rows, used_bytes)

    write(output_unit,'(A)',advance='no') ESC//'[?2026h'//ESC//'[H'
    write(output_unit,'(A)',advance='no') frame_text(1:used_bytes)
    write(output_unit,'(A)',advance='no') ESC//'[0m'//ESC//'[?2026l'
    flush(output_unit)

    frame_number = frame_number + 1

    call system_clock(frame_end)
    elapsed = real(frame_end - frame_start, real64) / real(clock_rate, real64)
    sleep_time = target_frame_time - elapsed
    if (sleep_time > 0.0005_real64) then
      call c_usleep(int(sleep_time * 1000000.0_real64, c_int))
    end if
  end do

  write(output_unit,'(A)',advance='no') &
    ESC//'[?2026l'//ESC//'[0m'//ESC//'[?7h'//ESC//'[?25h'//ESC//'[?1049l'
  flush(output_unit)

contains

  subroutine init_decimal_table()
    integer :: i
    do i = 0, 255
      write(decimal3(i),'(I3.3)') i
    end do
  end subroutine init_decimal_table

  subroutine init_geometry(v, e, f, n, colors)
    real(real32), intent(out) :: v(3,8), n(3,6)
    integer, intent(out) :: e(2,12), f(4,6), colors(3,6)

    v = reshape([ &
      -1.0_real32, -1.0_real32, -1.0_real32, &
       1.0_real32, -1.0_real32, -1.0_real32, &
       1.0_real32,  1.0_real32, -1.0_real32, &
      -1.0_real32,  1.0_real32, -1.0_real32, &
      -1.0_real32, -1.0_real32,  1.0_real32, &
       1.0_real32, -1.0_real32,  1.0_real32, &
       1.0_real32,  1.0_real32,  1.0_real32, &
      -1.0_real32,  1.0_real32,  1.0_real32  &
    ], [3,8])

    e = reshape([ &
      1,2, 2,3, 3,4, 4,1, &
      5,6, 6,7, 7,8, 8,5, &
      1,5, 2,6, 3,7, 4,8  &
    ], [2,12])

    f = reshape([ &
      1,2,3,4, &
      5,6,7,8, &
      1,4,8,5, &
      2,6,7,3, &
      1,5,6,2, &
      4,3,7,8  &
    ], [4,6])

    n = reshape([ &
       0.0_real32,  0.0_real32, -1.0_real32, &
       0.0_real32,  0.0_real32,  1.0_real32, &
      -1.0_real32,  0.0_real32,  0.0_real32, &
       1.0_real32,  0.0_real32,  0.0_real32, &
       0.0_real32, -1.0_real32,  0.0_real32, &
       0.0_real32,  1.0_real32,  0.0_real32  &
    ], [3,6])

    colors = reshape([ &
       70, 145, 255, &
       38,  82, 210, &
       32, 220, 224, &
      255, 104,  78, &
      132, 118, 205, &
      225, 239, 255  &
    ], [3,6])
  end subroutine init_geometry

  subroutine terminal_size(out_cols, out_rows)
    integer, intent(out) :: out_cols, out_rows
    type(winsize), target :: ws
    integer(c_int) :: rc
    character(len=64) :: env
    integer :: stat, ios

    out_cols = 100
    out_rows = 40
    ws = winsize(0_c_short, 0_c_short, 0_c_short, 0_c_short)

    rc = c_ioctl(1_c_int, TIOCGWINSZ, c_loc(ws))
    if (rc == 0 .and. int(ws%ws_col) > 0 .and. int(ws%ws_row) > 0) then
      out_cols = int(ws%ws_col)
      out_rows = int(ws%ws_row)
    else
      call get_environment_variable('COLUMNS', env, status=stat)
      if (stat == 0) then
        read(env,*,iostat=ios) out_cols
        if (ios /= 0) out_cols = 100
      end if

      call get_environment_variable('LINES', env, status=stat)
      if (stat == 0) then
        read(env,*,iostat=ios) out_rows
        if (ios /= 0) out_rows = 40
      end if
    end if

    out_cols = max(out_cols, 40)
    out_rows = max(out_rows, 16)
  end subroutine terminal_size

  subroutine ensure_buffers(w, r)
    integer, intent(in) :: w, r
    integer :: height, needed

    height = r * 2
    needed = r * (w * cell_bytes + 5) + 32

    if (allocated(pixels)) then
      if (size(pixels,1) == w .and. size(pixels,2) == height) return
      deallocate(pixels, background, zbuffer)
      if (allocated(frame_text)) deallocate(frame_text)
    end if

    allocate(pixels(w,height), background(w,height), zbuffer(w,height))
    allocate(character(len=needed) :: frame_text)
    call make_background(w, height)
  end subroutine ensure_buffers

  subroutine render_frame(w, terminal_rows, height, time, ax, ay, az, v, e, f, n, colors)
    integer, intent(in) :: w, terminal_rows, height
    real(real32), intent(in) :: time, ax, ay, az
    real(real32), intent(in) :: v(3,8), n(3,6)
    integer, intent(in) :: e(2,12), f(4,6), colors(3,6)

    real(real32) :: rotation(3,3), world(3,8), rotated_normal(3,6)
    real(real32) :: sx(8), sy(8), depth(8), p(3), floor_point(3)
    real(real32) :: shadow_x, shadow_y, shadow_depth
    real(real32) :: bob, edge_pulse
    logical :: ok(8), shadow_ok
    integer :: i, face

    call rotation_matrix(ax, ay, az, rotation)
    bob = 0.10_real32 * sin(time * 1.20_real32)

    do i = 1, 8
      world(:,i) = matmul(rotation, v(:,i))
      world(2,i) = world(2,i) + 0.28_real32 + bob
      call project_point(world(:,i), w, height, sx(i), sy(i), depth(i), ok(i))
    end do

    do face = 1, 6
      rotated_normal(:,face) = normalize3(matmul(rotation, n(:,face)))
    end do

    floor_point = [0.0_real32, -1.40_real32, 0.25_real32]
    call project_point(floor_point, w, height, shadow_x, shadow_y, shadow_depth, shadow_ok)
    if (shadow_ok) then
      call draw_soft_shadow(w, height, shadow_x, shadow_y, &
        min(real(w,real32),real(height,real32))*0.22_real32, &
        min(real(w,real32),real(height,real32))*0.052_real32, time)
    end if

    do face = 1, 6
      if (rotated_normal(3,face) > 0.035_real32) cycle
      if (.not. all(ok(f(:,face)))) cycle

      call fill_lit_triangle(w, height, &
        sx(f(1,face)), sy(f(1,face)), depth(f(1,face)), world(:,f(1,face)), &
        sx(f(2,face)), sy(f(2,face)), depth(f(2,face)), world(:,f(2,face)), &
        sx(f(3,face)), sy(f(3,face)), depth(f(3,face)), world(:,f(3,face)), &
        rotated_normal(:,face), colors(:,face), time)

      call fill_lit_triangle(w, height, &
        sx(f(1,face)), sy(f(1,face)), depth(f(1,face)), world(:,f(1,face)), &
        sx(f(3,face)), sy(f(3,face)), depth(f(3,face)), world(:,f(3,face)), &
        sx(f(4,face)), sy(f(4,face)), depth(f(4,face)), world(:,f(4,face)), &
        rotated_normal(:,face), colors(:,face), time)
    end do

    edge_pulse = 0.82_real32 + 0.18_real32 * sin(time * 1.65_real32)
    do i = 1, 12
      if (ok(e(1,i)) .and. ok(e(2,i))) then
        call draw_glowing_line(w, height, &
          sx(e(1,i)), sy(e(1,i)), depth(e(1,i)), &
          sx(e(2,i)), sy(e(2,i)), depth(e(2,i)), edge_pulse)
      end if
    end do
  end subroutine render_frame

  subroutine make_background(w, height)
    integer, intent(in) :: w, height
    integer :: x, y, r, g, b, hash_value
    real(real32) :: fx, fy, dx, dy, glow, vignette, horizon
    real(real32) :: q, floor_depth, grid_x, grid_z, line_x, line_z
    real(real32) :: star, base, redf, greenf, bluef

    horizon = 0.655_real32

    do y = 1, height
      fy = real(y - 1, real32) / real(max(height - 1,1), real32)
      do x = 1, w
        fx = real(x - 1, real32) / real(max(w - 1,1), real32)
        dx = (fx - 0.50_real32) / 0.60_real32
        dy = (fy - 0.43_real32) / 0.72_real32
        glow = exp(-2.40_real32 * (dx*dx + dy*dy))
        vignette = clamp01(1.08_real32 - 0.42_real32 * (dx*dx + dy*dy))

        redf = (5.0_real32 + 15.0_real32*glow + 5.0_real32*fy) * vignette
        greenf = (7.0_real32 + 20.0_real32*glow + 7.0_real32*fy) * vignette
        bluef = (15.0_real32 + 38.0_real32*glow + 11.0_real32*fy) * vignette

        if (fy < horizon) then
          hash_value = modulo(x*1973 + y*9277 + x*y*17, 7919)
          star = 0.0_real32
          if (hash_value < 12) star = 28.0_real32 + real(modulo(hash_value*29,45),real32)
          redf = redf + star*0.55_real32
          greenf = greenf + star*0.72_real32
          bluef = bluef + star
        else
          q = (fy - horizon) / max(1.0_real32 - horizon, 0.001_real32)
          floor_depth = q*q
          base = 8.0_real32 + 13.0_real32*q + 6.0_real32*glow
          redf = base * 0.54_real32
          greenf = base * 0.68_real32
          bluef = base * 1.08_real32 + 4.0_real32

          grid_x = (fx - 0.5_real32) / max(0.045_real32 + q*0.72_real32, 0.01_real32)
          grid_z = 1.0_real32 / max(q + 0.055_real32, 0.055_real32)
          line_x = abs(grid_x*10.0_real32 - real(nint(grid_x*10.0_real32),real32))
          line_z = abs(grid_z*0.85_real32 - real(nint(grid_z*0.85_real32),real32))

          if (line_x < 0.045_real32 .or. line_z < 0.035_real32) then
            redf = redf + 4.0_real32*(1.0_real32-floor_depth)
            greenf = greenf + 11.0_real32*(1.0_real32-floor_depth)
            bluef = bluef + 23.0_real32*(1.0_real32-floor_depth)
          end if
        end if

        r = clamp255(redf)
        g = clamp255(greenf)
        b = clamp255(bluef)
        background(x,y) = pack_rgb(r,g,b)
      end do
    end do
  end subroutine make_background

  subroutine draw_soft_shadow(w, height, cx, cy, rx, ry, time)
    integer, intent(in) :: w, height
    real(real32), intent(in) :: cx, cy, rx, ry, time
    integer :: x, y, minx, maxx, miny, maxy
    real(real32) :: dx, dy, d2, alpha, glow_alpha, pulse

    minx = max(1, int(floor(cx - rx*1.55_real32)))
    maxx = min(w, int(ceiling(cx + rx*1.55_real32)))
    miny = max(1, int(floor(cy - ry*2.60_real32)))
    maxy = min(height, int(ceiling(cy + ry*2.60_real32)))
    pulse = 0.96_real32 + 0.04_real32*sin(time*1.3_real32)

    do y = miny, maxy
      do x = minx, maxx
        dx = (real(x,real32)-cx) / max(rx*pulse,0.001_real32)
        dy = (real(y,real32)-cy) / max(ry,0.001_real32)
        d2 = dx*dx + dy*dy
        if (d2 > 5.5_real32) cycle

        alpha = 0.48_real32 * exp(-1.40_real32*d2)
        call blend_pixel(x, y, [0, 1, 5], alpha)

        glow_alpha = 0.085_real32 * exp(-0.72_real32*d2)
        call add_pixel(x, y, [12, 48, 78], glow_alpha)
      end do
    end do
  end subroutine draw_soft_shadow

  subroutine fill_lit_triangle(w, height, x0,y0,z0,p0, x1,y1,z1,p1, x2,y2,z2,p2, normal, base_col, time)
    integer, intent(in) :: w, height, base_col(3)
    real(real32), intent(in) :: x0,y0,z0,p0(3), x1,y1,z1,p1(3), x2,y2,z2,p2(3)
    real(real32), intent(in) :: normal(3), time

    integer :: minx, maxx, miny, maxy, x, y
    integer :: out_col(3)
    real(real32) :: area, sign_area, area_abs, px, py
    real(real32) :: e0, e1, e2, b0, b1, b2, q0, q1, q2, denom, depth
    real(real32) :: position(3), view_dir(3), key_dir(3), fill_dir(3), half_dir(3)
    real(real32) :: key_pos(3), fill_pos(3), camera_pos(3)
    real(real32) :: key_dist2, fill_dist2, key_att, fill_att
    real(real32) :: ndotl, ndotf, ndotv, specular, clearcoat, fresnel
    real(real32) :: ambient, hemi, edge_light, noise, fog, exposure
    real(real32) :: base_linear(3), linear_color(3), fog_color(3), mapped(3)
    real(real32) :: phase, face_gradient

    minx = max(1, int(floor(min(x0,min(x1,x2)))))
    maxx = min(w, int(ceiling(max(x0,max(x1,x2)))))
    miny = max(1, int(floor(min(y0,min(y1,y2)))))
    maxy = min(height, int(ceiling(max(y0,max(y1,y2)))))
    if (minx > maxx .or. miny > maxy) return

    area = edge_function(x0,y0,x1,y1,x2,y2)
    if (abs(area) < 1.0e-7_real32) return
    sign_area = merge(-1.0_real32, 1.0_real32, area < 0.0_real32)
    area_abs = area * sign_area

    base_linear = (real(base_col,real32) / 255.0_real32) ** 2.0_real32
    camera_pos = [0.0_real32, 0.0_real32, -camera_distance]
    key_pos = [-3.4_real32, 3.2_real32, -3.6_real32]
    fill_pos = [3.8_real32, -0.8_real32, -2.2_real32]
    fog_color = [0.020_real32, 0.032_real32, 0.075_real32]
    exposure = 1.42_real32

    do y = miny, maxy
      do x = minx, maxx
        px = real(x,real32) - 0.5_real32
        py = real(y,real32) - 0.5_real32

        e0 = edge_function(x1,y1,x2,y2,px,py) * sign_area
        e1 = edge_function(x2,y2,x0,y0,px,py) * sign_area
        e2 = edge_function(x0,y0,x1,y1,px,py) * sign_area
        if (e0 < -0.001_real32 .or. e1 < -0.001_real32 .or. e2 < -0.001_real32) cycle

        b0 = e0 / area_abs
        b1 = e1 / area_abs
        b2 = e2 / area_abs

        q0 = b0 / z0
        q1 = b1 / z1
        q2 = b2 / z2
        denom = q0 + q1 + q2
        if (denom <= 1.0e-8_real32) cycle

        depth = 1.0_real32 / denom
        if (depth >= zbuffer(x,y)) cycle
        position = (q0*p0 + q1*p1 + q2*p2) * depth

        view_dir = normalize3(camera_pos - position)
        key_dir = normalize3(key_pos - position)
        fill_dir = normalize3(fill_pos - position)
        half_dir = normalize3(key_dir + view_dir)

        key_dist2 = dot_product(key_pos-position, key_pos-position)
        fill_dist2 = dot_product(fill_pos-position, fill_pos-position)
        key_att = 1.0_real32 / (1.0_real32 + 0.050_real32*key_dist2)
        fill_att = 1.0_real32 / (1.0_real32 + 0.075_real32*fill_dist2)

        ndotl = max(0.0_real32, dot_product(normal,key_dir))
        ndotf = max(0.0_real32, dot_product(normal,fill_dir))
        ndotv = max(0.0_real32, dot_product(normal,view_dir))
        specular = max(0.0_real32, dot_product(normal,half_dir)) ** 52.0_real32
        clearcoat = max(0.0_real32, dot_product(normal,half_dir)) ** 120.0_real32
        fresnel = (1.0_real32 - ndotv) ** 3.0_real32

        hemi = 0.14_real32 + 0.13_real32*(normal(2)*0.5_real32 + 0.5_real32)
        ambient = hemi + 0.035_real32
        edge_light = 0.28_real32*fresnel

        phase = position(1)*1.7_real32 + position(2)*1.1_real32 + time*0.48_real32
        face_gradient = 0.94_real32 + 0.06_real32*sin(phase)

        linear_color = base_linear * face_gradient * &
          (ambient + 1.28_real32*ndotl*key_att + 0.42_real32*ndotf*fill_att)
        linear_color = linear_color + specular*key_att*[1.15_real32,1.09_real32,1.02_real32]
        linear_color = linear_color + clearcoat*key_att*[1.25_real32,1.30_real32,1.42_real32]
        linear_color = linear_color + edge_light*[0.22_real32,0.58_real32,1.15_real32]

        noise = (real(modulo(x*17 + y*29 + int(time*23.0_real32),37),real32)/36.0_real32 - 0.5_real32)*0.006_real32
        linear_color = max(linear_color + noise, 0.0_real32)

        fog = clamp01((depth - 4.4_real32) / 3.4_real32)
        linear_color = linear_color*(1.0_real32-0.38_real32*fog) + fog_color*(0.38_real32*fog)

        mapped = (linear_color*exposure) / (1.0_real32 + linear_color*exposure)
        mapped = sqrt(clamp_vec(mapped,0.0_real32,1.0_real32))

        out_col = [clamp255(mapped(1)*255.0_real32), &
                   clamp255(mapped(2)*255.0_real32), &
                   clamp255(mapped(3)*255.0_real32)]

        zbuffer(x,y) = depth
        pixels(x,y) = pack_rgb(out_col(1),out_col(2),out_col(3))
      end do
    end do
  end subroutine fill_lit_triangle

  subroutine draw_glowing_line(w, height, x0,y0,z0, x1,y1,z1, pulse)
    integer, intent(in) :: w, height
    real(real32), intent(in) :: x0,y0,z0,x1,y1,z1,pulse
    integer :: steps, i, x, y, ox, oy
    real(real32) :: a, depth, radius2, alpha

    steps = max(abs(nint(x1-x0)),abs(nint(y1-y0))) + 1
    steps = max(steps,1)

    do i = 0, steps
      a = real(i,real32) / real(steps,real32)
      x = nint(x0 + (x1-x0)*a)
      y = nint(y0 + (y1-y0)*a)
      depth = z0 + (z1-z0)*a

      do oy = -2, 2
        do ox = -2, 2
          radius2 = real(ox*ox + oy*oy,real32)
          if (radius2 > 5.0_real32) cycle
          alpha = pulse * 0.17_real32 * exp(-0.78_real32*radius2)
          call blend_visible(x+ox,y+oy,depth,[60,150,255],alpha,w,height)
        end do
      end do

      call plot_depth(x,y,depth-0.040_real32,[238,248,255],w,height)
    end do
  end subroutine draw_glowing_line

  subroutine project_point(point, w, height, sx, sy, depth, ok)
    real(real32), intent(in) :: point(3)
    integer, intent(in) :: w, height
    real(real32), intent(out) :: sx, sy, depth
    logical, intent(out) :: ok
    real(real32) :: scale

    depth = point(3) + camera_distance
    if (depth <= 0.20_real32) then
      sx = 0.0_real32
      sy = 0.0_real32
      ok = .false.
      return
    end if

    scale = min(real(w,real32)*1.70_real32,real(height,real32))*0.78_real32
    sx = real(w,real32)*0.5_real32 + point(1)*scale/depth
    sy = real(height,real32)*0.48_real32 - point(2)*scale/depth
    ok = .true.
  end subroutine project_point

  subroutine rotation_matrix(ax, ay, az, matrix)
    real(real32), intent(in) :: ax, ay, az
    real(real32), intent(out) :: matrix(3,3)
    real(real32) :: cx, sx, cy, sy, cz, sz

    cx = cos(ax); sx = sin(ax)
    cy = cos(ay); sy = sin(ay)
    cz = cos(az); sz = sin(az)

    matrix(1,1) = cz*cy
    matrix(1,2) = cz*sy*sx - sz*cx
    matrix(1,3) = cz*sy*cx + sz*sx
    matrix(2,1) = sz*cy
    matrix(2,2) = sz*sy*sx + cz*cx
    matrix(2,3) = sz*sy*cx - cz*sx
    matrix(3,1) = -sy
    matrix(3,2) = cy*sx
    matrix(3,3) = cy*cx
  end subroutine rotation_matrix

  subroutine plot_depth(x, y, depth, color, w, height)
    integer, intent(in) :: x, y, color(3), w, height
    real(real32), intent(in) :: depth
    if (x < 1 .or. x > w .or. y < 1 .or. y > height) return
    if (depth < zbuffer(x,y)) then
      zbuffer(x,y) = depth
      pixels(x,y) = pack_rgb(color(1),color(2),color(3))
    end if
  end subroutine plot_depth

  subroutine blend_visible(x, y, depth, color, alpha, w, height)
    integer, intent(in) :: x, y, color(3), w, height
    real(real32), intent(in) :: depth, alpha
    if (x < 1 .or. x > w .or. y < 1 .or. y > height) return
    if (depth <= zbuffer(x,y) + 0.10_real32) call blend_pixel(x,y,color,alpha)
  end subroutine blend_visible

  subroutine blend_pixel(x, y, color, alpha)
    integer, intent(in) :: x, y, color(3)
    real(real32), intent(in) :: alpha
    integer :: old_color(3), mixed(3)
    real(real32) :: a

    a = clamp01(alpha)
    call unpack_rgb(pixels(x,y),old_color)
    mixed = nint(real(old_color,real32)*(1.0_real32-a) + real(color,real32)*a)
    pixels(x,y) = pack_rgb(mixed(1),mixed(2),mixed(3))
  end subroutine blend_pixel

  subroutine add_pixel(x, y, color, amount)
    integer, intent(in) :: x, y, color(3)
    real(real32), intent(in) :: amount
    integer :: old_color(3), mixed(3)

    call unpack_rgb(pixels(x,y),old_color)
    mixed = old_color + nint(real(color,real32)*max(amount,0.0_real32))
    pixels(x,y) = pack_rgb(clamp255(real(mixed(1),real32)), &
                           clamp255(real(mixed(2),real32)), &
                           clamp255(real(mixed(3),real32)))
  end subroutine add_pixel

  subroutine build_frame(w, terminal_rows, used)
    integer, intent(in) :: w, terminal_rows
    integer, intent(out) :: used
    integer :: row, x, top_y, bottom_y, pos
    integer :: top_rgb(3), bottom_rgb(3)

    pos = 1
    do row = 1, terminal_rows
      top_y = (row-1)*2 + 1
      bottom_y = top_y + 1

      do x = 1, w
        call unpack_rgb(pixels(x,top_y),top_rgb)
        call unpack_rgb(pixels(x,bottom_y),bottom_rgb)

        frame_text(pos:pos) = ESC
        pos = pos + 1
        frame_text(pos:pos+5) = '[38;2;'
        pos = pos + 6
        frame_text(pos:pos+2) = decimal3(top_rgb(1))
        pos = pos + 3
        frame_text(pos:pos) = ';'
        pos = pos + 1
        frame_text(pos:pos+2) = decimal3(top_rgb(2))
        pos = pos + 3
        frame_text(pos:pos) = ';'
        pos = pos + 1
        frame_text(pos:pos+2) = decimal3(top_rgb(3))
        pos = pos + 3
        frame_text(pos:pos+5) = ';48;2;'
        pos = pos + 6
        frame_text(pos:pos+2) = decimal3(bottom_rgb(1))
        pos = pos + 3
        frame_text(pos:pos) = ';'
        pos = pos + 1
        frame_text(pos:pos+2) = decimal3(bottom_rgb(2))
        pos = pos + 3
        frame_text(pos:pos) = ';'
        pos = pos + 1
        frame_text(pos:pos+2) = decimal3(bottom_rgb(3))
        pos = pos + 3
        frame_text(pos:pos) = 'm'
        pos = pos + 1
        frame_text(pos:pos+2) = UPPER_HALF
        pos = pos + 3
      end do

      if (row < terminal_rows) then
        frame_text(pos:pos+3) = ESC//'[0m'
        pos = pos + 4
        frame_text(pos:pos) = achar(10)
        pos = pos + 1
      end if
    end do
    used = pos - 1
  end subroutine build_frame

  pure function normalize3(vector) result(normalized)
    real(real32), intent(in) :: vector(3)
    real(real32) :: normalized(3), length_squared

    length_squared = dot_product(vector,vector)
    if (length_squared <= 1.0e-12_real32) then
      normalized = 0.0_real32
    else
      normalized = vector / sqrt(length_squared)
    end if
  end function normalize3

  pure function clamp_vec(vector, low, high) result(result_vector)
    real(real32), intent(in) :: vector(3), low, high
    real(real32) :: result_vector(3)
    result_vector = min(max(vector,low),high)
  end function clamp_vec

  pure real(real32) function clamp01(value)
    real(real32), intent(in) :: value
    clamp01 = min(max(value,0.0_real32),1.0_real32)
  end function clamp01

  pure integer function clamp255(value)
    real(real32), intent(in) :: value
    clamp255 = min(max(nint(value),0),255)
  end function clamp255

  pure real(real32) function edge_function(ax, ay, bx, by, cx, cy)
    real(real32), intent(in) :: ax, ay, bx, by, cx, cy
    edge_function = (cx-ax)*(by-ay) - (cy-ay)*(bx-ax)
  end function edge_function

  pure integer(int32) function pack_rgb(r, g, b)
    integer, intent(in) :: r, g, b
    pack_rgb = ishft(int(iand(r,255),int32),16) + &
               ishft(int(iand(g,255),int32),8) + int(iand(b,255),int32)
  end function pack_rgb

  pure subroutine unpack_rgb(packed, rgb)
    integer(int32), intent(in) :: packed
    integer, intent(out) :: rgb(3)
    rgb(1) = int(ibits(packed,16,8))
    rgb(2) = int(ibits(packed,8,8))
    rgb(3) = int(ibits(packed,0,8))
  end subroutine unpack_rgb

end program fortran_spincube_hq
