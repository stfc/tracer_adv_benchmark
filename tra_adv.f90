program tra_adv
  use omp_lib, only : omp_get_wtime
  real*8, allocatable, dimension(:,:,:), save :: tsn
  real*8, allocatable, dimension(:,:,:), save :: pun
  real*8, allocatable, dimension(:,:,:), save :: pvn
  real*8, allocatable, dimension(:,:,:), save :: pwn
  real*8, allocatable, dimension(:,:,:), save :: mydomain
  real*8, allocatable, dimension(:,:,:), save :: zslpx
  real*8, allocatable, dimension(:,:,:), save :: zslpy
  real*8, allocatable, dimension(:,:,:), save :: zwx
  real*8, allocatable, dimension(:,:,:), save :: zwy
  real*8, allocatable, dimension(:,:,:), save :: umask
  real*8, allocatable, dimension(:,:,:), save :: vmask
  real*8, allocatable, dimension(:,:,:), save :: tmask
  real*8, allocatable, dimension(:,:,:), save :: zind
  real*8, allocatable, dimension(:,:), save :: ztfreez
  real*8, allocatable, dimension(:,:), save :: rnfmsk
  real*8, allocatable, dimension(:,:), save :: upsmsk
  real*8, allocatable, dimension(:), save :: rnfmsk_z
  real*8 :: zice
  real*8 :: zu
  real*8 :: z0u
  real*8 :: zzwx
  real*8 :: zv
  real*8 :: z0v
  real*8 :: zzwy
  real*8 :: ztra
  real*8 :: zbtr
  real*8 :: zdt
  real*8 :: zalpha
  real*8 :: r
  real*8 :: checksum
  real*8 :: zw
  real*8 :: z0w
  integer :: jpi
  integer :: jpj
  integer :: jpk
  integer :: ji
  integer :: jj
  integer :: jk
  integer :: jt
  integer*8 :: itn_count
  CHARACTER(LEN = 10) :: env
  double precision :: step_timer_f
  double precision :: first_step
  integer :: idx
  integer :: idx_1
  integer :: idx_2
  integer :: idx_3
  integer :: idx_4
  integer :: idx_5
  integer :: idx_6
  integer :: idx_7
  integer :: idx_8
  integer :: idx_9
  integer :: idx_10
  integer :: idx_11
  integer :: idx_12
  integer :: idx_13
  integer :: idx_14
  integer :: idx_15
  integer :: idx_16
  integer :: idx_17
  integer :: loop_start
  integer :: loop_stop
  integer :: loop_start_1
  integer :: loop_stop_1
  integer :: loop_start_2
  integer :: loop_stop_2
  integer :: loop_start_3
  integer :: loop_stop_3
  integer :: loop_stop_4
  integer :: loop_stop_5
  integer :: loop_stop_6
  integer :: loop_start_4
  integer :: loop_stop_7
  integer :: loop_start_5
  integer :: loop_stop_8
  integer :: loop_start_6
  integer :: loop_stop_9
  integer :: loop_start_7
  integer :: loop_stop_10
  integer :: loop_stop_11
  integer :: loop_stop_12
  integer :: loop_stop_13
  integer :: loop_stop_14
  integer :: loop_stop_15
  integer :: loop_stop_16
  integer :: loop_stop_17
  integer :: loop_stop_18
  integer :: loop_start_8
  integer :: loop_stop_19
  integer :: loop_start_9
  integer :: loop_stop_20
  integer :: loop_start_10
  integer :: loop_stop_21
  integer :: loop_start_11
  integer :: loop_stop_22
  integer :: loop_stop_23
  integer :: loop_start_12
  integer :: loop_stop_24
  integer :: loop_start_13
  integer :: loop_stop_25
  integer :: loop_start_14
  integer :: loop_stop_26
  integer :: loop_start_15
  integer :: loop_stop_27
  integer :: loop_stop_28
  integer :: loop_stop_29
  integer :: loop_start_16
  integer :: loop_stop_30
  integer :: loop_start_17
  integer :: loop_stop_31
  integer :: loop_stop_32
  integer :: loop_stop_33
  integer :: loop_stop_34
  integer :: loop_stop_35
  integer :: loop_stop_36
  integer :: loop_stop_37
  integer :: loop_stop_38
  integer :: loop_stop_39
  integer :: loop_stop_40

  call get_environment_variable('JPI', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpi
  call get_environment_variable('JPJ', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpj
  call get_environment_variable('JPK', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') jpk
  call get_environment_variable('IT', env)

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Read_Stmt
  READ(env, '(i10)') itn_count
  ALLOCATE(mydomain(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zwx(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zwy(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zslpx(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zslpy(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pun(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pvn(1:jpi,1:jpj,1:jpk))
  ALLOCATE(pwn(1:jpi,1:jpj,1:jpk))
  ALLOCATE(umask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(vmask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(tmask(1:jpi,1:jpj,1:jpk))
  ALLOCATE(zind(1:jpi,1:jpj,1:jpk))
  ALLOCATE(ztfreez(1:jpi,1:jpj))
  ALLOCATE(rnfmsk(1:jpi,1:jpj))
  ALLOCATE(upsmsk(1:jpi,1:jpj))
  ALLOCATE(rnfmsk_z(1:jpk))
  ALLOCATE(tsn(1:jpi,1:jpj,1:jpk))
  r = jpi * jpj * jpk
  !$omp target
  !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
  do jk = 1, jpk, 1
    do jj = 1, jpj, 1
      do ji = 1, jpi, 1
        umask(ji,jj,jk) = ji * jj * jk / r
        mydomain(ji,jj,jk) = ji * jj * jk / r
        pun(ji,jj,jk) = ji * jj * jk / r
        pvn(ji,jj,jk) = ji * jj * jk / r
        pwn(ji,jj,jk) = ji * jj * jk / r
        vmask(ji,jj,jk) = ji * jj * jk / r
        tsn(ji,jj,jk) = ji * jj * jk / r
        tmask(ji,jj,jk) = ji * jj * jk / r
      enddo
    enddo
  enddo
  !$omp end teams distribute parallel do
  !$omp end target
  r = jpi * jpj
  !$omp target
  !$omp teams distribute parallel do collapse(2) default(shared) private(ji,jj)
  do jj = 1, jpj, 1
    do ji = 1, jpi, 1
      ztfreez(ji,jj) = ji * jj / r
      upsmsk(ji,jj) = ji * jj / r
      rnfmsk(ji,jj) = ji * jj / r
    enddo
  enddo
  !$omp end teams distribute parallel do
  !$omp end target
  !$omp target
  !$omp teams distribute parallel do collapse(1) default(shared) private(jk)
  do jk = 1, jpk, 1
    rnfmsk_z(jk) = jk / jpk
  enddo
  !$omp end teams distribute parallel do
  !$omp end target
  step_timer_f = omp_get_wtime()
  loop_stop = UBOUND(zwx, dim=2)
  loop_start = LBOUND(zwx, dim=2)
  loop_stop_1 = UBOUND(zwx, dim=1)
  loop_start_1 = LBOUND(zwx, dim=1)
  loop_stop_2 = UBOUND(zwy, dim=2)
  loop_start_2 = LBOUND(zwy, dim=2)
  loop_stop_3 = UBOUND(zwy, dim=1)
  loop_start_3 = LBOUND(zwy, dim=1)
  loop_stop_4 = jpk - 1
  loop_stop_5 = jpj - 1
  loop_stop_6 = jpi - 1
  loop_stop_7 = UBOUND(zslpx, dim=2)
  loop_start_4 = LBOUND(zslpx, dim=2)
  loop_stop_8 = UBOUND(zslpx, dim=1)
  loop_start_5 = LBOUND(zslpx, dim=1)
  loop_stop_9 = UBOUND(zslpy, dim=2)
  loop_start_6 = LBOUND(zslpy, dim=2)
  loop_stop_10 = UBOUND(zslpy, dim=1)
  loop_start_7 = LBOUND(zslpy, dim=1)
  loop_stop_11 = jpk - 1
  loop_stop_12 = jpk - 1
  loop_stop_13 = jpk - 1
  loop_stop_14 = jpj - 1
  loop_stop_15 = jpi - 1
  loop_stop_16 = jpk - 1
  loop_stop_17 = jpj - 1
  loop_stop_18 = jpi - 1
  loop_stop_19 = UBOUND(zwx, dim=2)
  loop_start_8 = LBOUND(zwx, dim=2)
  loop_stop_20 = UBOUND(zwx, dim=1)
  loop_start_9 = LBOUND(zwx, dim=1)
  loop_stop_21 = UBOUND(zwx, dim=2)
  loop_start_10 = LBOUND(zwx, dim=2)
  loop_stop_22 = UBOUND(zwx, dim=1)
  loop_start_11 = LBOUND(zwx, dim=1)
  loop_stop_23 = jpk - 1
  loop_stop_24 = UBOUND(zwx, dim=2)
  loop_start_12 = LBOUND(zwx, dim=2)
  loop_stop_25 = UBOUND(zwx, dim=1)
  loop_start_13 = LBOUND(zwx, dim=1)
  loop_stop_26 = UBOUND(zslpx, dim=2)
  loop_start_14 = LBOUND(zslpx, dim=2)
  loop_stop_27 = UBOUND(zslpx, dim=1)
  loop_start_15 = LBOUND(zslpx, dim=1)
  loop_stop_28 = jpk - 1
  loop_stop_29 = jpk - 1
  loop_stop_30 = UBOUND(zwx, dim=2)
  loop_start_16 = LBOUND(zwx, dim=2)
  loop_stop_31 = UBOUND(zwx, dim=1)
  loop_start_17 = LBOUND(zwx, dim=1)
  loop_stop_32 = jpk - 1
  loop_stop_33 = jpj - 1
  loop_stop_34 = jpi - 1
  loop_stop_35 = jpk - 1
  loop_stop_36 = jpj - 1
  loop_stop_37 = jpi - 1

  ! PSyclone: Loop cannot be parallelised because:
  ! Error: The write access to 'zind(ji,jj,jk)' in 'zind(ji,jj,jk) = MAX(rnfmsk(ji,jj) * rnfmsk_z(jk), upsmsk(ji,jj), zice) * 
!& tmask(ji,jj,jk)' causes a write-write race condition. Variable: 'zind'.
  ! Error: The write access to 'zwx(idx_1,idx,jpk)' in 'zwx(idx_1,idx,jpk) = 0.e0' causes a write-write race condition. Variable: 
!& 'zwx'.
  ! Error: The write access to 'zwy(idx_3,idx_2,jpk)' in 'zwy(idx_3,idx_2,jpk) = 0.e0' causes a write-write race condition. 
!& Variable: 'zwy'.
  ! Error: The write access to 'mydomain(ji,jj,jk)' in 'mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra' and the read access to 
!& 'mydomain(ji + 1,jj,jk)' in 'zwx(ji,jj,jk) = umask(ji,jj,jk) * (mydomain(ji + 1,jj,jk) - mydomain(ji,jj,jk))' are dependent and 
!& cannot be parallelised. Variable: 'mydomain'.
  ! Error: The write access to 'zslpx(idx_5,idx_4,jpk)' in 'zslpx(idx_5,idx_4,jpk) = 0.e0' causes a write-write race condition. 
!& Variable: 'zslpx'.
  ! Error: The write access to 'zslpy(idx_7,idx_6,jpk)' in 'zslpy(idx_7,idx_6,jpk) = 0.e0' causes a write-write race condition. 
!& Variable: 'zslpy'.
  ! Consider using the "ignore_dependencies_for" transformation option if this is a false dependency
  ! Consider using the "array_privatisation" transformation option if this is a write-write dependency
  do jt = 1, itn_count, 1
    if(jt == 1) then
        first_step = omp_get_wtime()
    end if
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,zice)
    do jk = 1, jpk, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          if (tsn(ji,jj,jk) <= ztfreez(ji,jj) + 0.1d0) then
            zice = 1.d0
          else
            zice = 0.d0
          end if
          zind(ji,jj,jk) = MAX(rnfmsk(ji,jj) * rnfmsk_z(jk), upsmsk(ji,jj), zice) * tmask(ji,jj,jk)
          zind(ji,jj,jk) = 1 - zind(ji,jj,jk)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx,idx_1)
    do idx = loop_start, loop_stop, 1
      do idx_1 = loop_start_1, loop_stop_1, 1
        zwx(idx_1,idx,jpk) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_2,idx_3)
    do idx_2 = loop_start_2, loop_stop_2, 1
      do idx_3 = loop_start_3, loop_stop_3, 1
        zwy(idx_3,idx_2,jpk) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
    do jk = 1, loop_stop_4, 1
      do jj = 1, loop_stop_5, 1
        do ji = 1, loop_stop_6, 1
          zwx(ji,jj,jk) = umask(ji,jj,jk) * (mydomain(ji + 1,jj,jk) - mydomain(ji,jj,jk))
          zwy(ji,jj,jk) = vmask(ji,jj,jk) * (mydomain(ji,jj + 1,jk) - mydomain(ji,jj,jk))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_4,idx_5)
    do idx_4 = loop_start_4, loop_stop_7, 1
      do idx_5 = loop_start_5, loop_stop_8, 1
        zslpx(idx_5,idx_4,jpk) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_6,idx_7)
    do idx_6 = loop_start_6, loop_stop_9, 1
      do idx_7 = loop_start_7, loop_stop_10, 1
        zslpy(idx_7,idx_6,jpk) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
    do jk = 1, loop_stop_11, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          zslpx(ji,jj,jk) = (zwx(ji,jj,jk) + zwx(ji - 1,jj,jk)) * (0.25d0 + SIGN(0.25d0, zwx(ji,jj,jk) * zwx(ji - 1,jj,jk)))
          zslpy(ji,jj,jk) = (zwy(ji,jj,jk) + zwy(ji,jj - 1,jk)) * (0.25d0 + SIGN(0.25d0, zwy(ji,jj,jk) * zwy(ji,jj - 1,jk)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
    do jk = 1, loop_stop_12, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          zslpx(ji,jj,jk) = SIGN(1.d0, zslpx(ji,jj,jk)) * MIN(ABS(zslpx(ji,jj,jk)), 2.d0 * ABS(zwx(ji - 1,jj,jk)), 2.d0 * &
&ABS(zwx(ji,jj,jk)))
          zslpy(ji,jj,jk) = SIGN(1.d0, zslpy(ji,jj,jk)) * MIN(ABS(zslpy(ji,jj,jk)), 2.d0 * ABS(zwy(ji,jj - 1,jk)), 2.d0 * &
&ABS(zwy(ji,jj,jk)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zdt = 1
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,z0u,z0v,zalpha,zu,zv,zzwx,zzwy)
    do jk = 1, loop_stop_13, 1
      do jj = 2, loop_stop_14, 1
        do ji = 2, loop_stop_15, 1
          z0u = SIGN(0.5d0, pun(ji,jj,jk))
          zalpha = 0.5d0 - z0u
          zu = z0u - 0.5d0 * pun(ji,jj,jk) * zdt
          zzwx = mydomain(ji + 1,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji + 1,jj,jk))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zu * zslpx(ji,jj,jk))
          zwx(ji,jj,jk) = pun(ji,jj,jk) * (zalpha * zzwx + (1. - zalpha) * zzwy)
          z0v = SIGN(0.5d0, pvn(ji,jj,jk))
          zalpha = 0.5d0 - z0v
          zv = z0v - 0.5d0 * pvn(ji,jj,jk) * zdt
          zzwx = mydomain(ji,jj + 1,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj + 1,jk))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zv * zslpy(ji,jj,jk))
          zwy(ji,jj,jk) = pvn(ji,jj,jk) * (zalpha * zzwx + (1.d0 - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,ztra)
    do jk = 1, loop_stop_16, 1
      do jj = 2, loop_stop_17, 1
        do ji = 2, loop_stop_18, 1
          ztra = -zbtr * (zwx(ji,jj,jk) - zwx(ji - 1,jj,jk) + zwy(ji,jj,jk) - zwy(ji,jj - 1,jk))
          mydomain(ji,jj,jk) = mydomain(ji,jj,jk) + ztra
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_8,idx_9)
    do idx_8 = loop_start_8, loop_stop_19, 1
      do idx_9 = loop_start_9, loop_stop_20, 1
        zwx(idx_9,idx_8,1) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_10,idx_11)
    do idx_10 = loop_start_10, loop_stop_21, 1
      do idx_11 = loop_start_11, loop_stop_22, 1
        zwx(idx_11,idx_10,jpk) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(idx_12,idx_13,jk)
    do jk = 2, loop_stop_23, 1
      do idx_12 = loop_start_12, loop_stop_24, 1
        do idx_13 = loop_start_13, loop_stop_25, 1
          zwx(idx_13,idx_12,jk) = tmask(idx_13 + (LBOUND(tmask, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(tmask, dim=2) - &
&LBOUND(zwx, dim=2)),jk) * (mydomain(idx_13 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(mydomain, dim=2) - &
&LBOUND(zwx, dim=2)),jk - 1) - mydomain(idx_13 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_12 + (LBOUND(mydomain, dim=2) &
&- LBOUND(zwx, dim=2)),jk))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_14,idx_15)
    do idx_14 = loop_start_14, loop_stop_26, 1
      do idx_15 = loop_start_15, loop_stop_27, 1
        zslpx(idx_15,idx_14,1) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
    do jk = 2, loop_stop_28, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          zslpx(ji,jj,jk) = (zwx(ji,jj,jk) + zwx(ji,jj,jk + 1)) * (0.25d0 + SIGN(0.25d0, zwx(ji,jj,jk) * zwx(ji,jj,jk + 1)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk)
    do jk = 2, loop_stop_29, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          zslpx(ji,jj,jk) = SIGN(1.d0, zslpx(ji,jj,jk)) * MIN(ABS(zslpx(ji,jj,jk)), 2.d0 * ABS(zwx(ji,jj,jk + 1)), 2.d0 * &
&ABS(zwx(ji,jj,jk)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_16,idx_17)
    do idx_16 = loop_start_16, loop_stop_30, 1
      do idx_17 = loop_start_17, loop_stop_31, 1
        zwx(idx_17,idx_16,1) = pwn(idx_17 + (LBOUND(pwn, dim=1) - LBOUND(zwx, dim=1)),idx_16 + (LBOUND(pwn, dim=2) - LBOUND(zwx, &
&dim=2)),1) * mydomain(idx_17 + (LBOUND(mydomain, dim=1) - LBOUND(zwx, dim=1)),idx_16 + (LBOUND(mydomain, dim=2) - LBOUND(zwx, &
&dim=2)),1)
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zdt = 1
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,z0w,zalpha,zw,zzwx,zzwy)
    do jk = 1, loop_stop_32, 1
      do jj = 2, loop_stop_33, 1
        do ji = 2, loop_stop_34, 1
          z0w = SIGN(0.5d0, pwn(ji,jj,jk + 1))
          zalpha = 0.5d0 + z0w
          zw = z0w - 0.5d0 * pwn(ji,jj,jk + 1) * zdt * zbtr
          zzwx = mydomain(ji,jj,jk + 1) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk + 1))
          zzwy = mydomain(ji,jj,jk) + zind(ji,jj,jk) * (zw * zslpx(ji,jj,jk))
          zwx(ji,jj,jk + 1) = pwn(ji,jj,jk + 1) * (zalpha * zzwx + (1. - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,ztra)
    do jk = 1, loop_stop_35, 1
      do jj = 2, loop_stop_36, 1
        do ji = 2, loop_stop_37, 1
          ztra = -zbtr * (zwx(ji,jj,jk) - zwx(ji,jj,jk + 1))
          mydomain(ji,jj,jk) = ztra
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    if(jt == 1) then
        first_step = omp_get_wtime() - first_step
    end if
  enddo
  step_timer_f = (omp_get_wtime() - step_timer_f) - first_step

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Open_Stmt
  PRINT *, "step timer total", step_timer_f
  PRINT *, "Time per step", step_timer_f/REAL(itn_count)
  OPEN(UNIT = 24, FILE = 'output.dat', FORM = 'formatted')
  checksum = 0.0d0
  loop_stop_38 = jpk - 1
  loop_stop_39 = jpj - 1
  loop_stop_40 = jpi - 1
  do jk = 1, loop_stop_38, 1
    do jj = 2, loop_stop_39, 1
      do ji = 2, loop_stop_40, 1
        checksum = checksum + mydomain(ji,jj,jk)

        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Write_Stmt
        WRITE(24, *) mydomain(ji, jj, jk)
      enddo
    enddo
  enddo

  ! PSyclone CodeBlock (unsupported code) reason:
  !  - Unsupported statement: Write_Stmt
  !  - Unsupported statement: Close_Stmt
  WRITE(*, "('Checksum for domain ', 2(I4, ' x'), I4, ' (',I4,' iterations) = ',E23.16)") jpi, jpj, jpk, itn_count, checksum
  CLOSE(UNIT = 24)
  DEALLOCATE(mydomain)
  DEALLOCATE(zwx)
  DEALLOCATE(zwy)
  DEALLOCATE(zslpx)
  DEALLOCATE(zslpy)
  DEALLOCATE(pun)
  DEALLOCATE(pvn)
  DEALLOCATE(pwn)
  DEALLOCATE(umask)
  DEALLOCATE(vmask)
  DEALLOCATE(tmask)
  DEALLOCATE(zind)
  DEALLOCATE(ztfreez)
  DEALLOCATE(rnfmsk)
  DEALLOCATE(upsmsk)
  DEALLOCATE(rnfmsk_z)
  DEALLOCATE(tsn)

end program tra_adv
