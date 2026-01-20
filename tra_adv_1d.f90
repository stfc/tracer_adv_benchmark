module index_mod

    contains

pure function get_index_3d(i,j,k,ii,jj) result(id)
    integer, intent(in):: i, j, k, ii, jj
    integer :: id

    id = (k-1)*ii*jj+(j-1)*ii+i
end function get_index_3d

pure function get_index_2d(i,j,ii) result(id)
    integer, intent(in) :: i, j, ii
    integer :: id
    id = (j-1)*ii+i
end function get_index_2d

end module

program tra_adv
  use omp_lib, only : omp_get_wtime
  use index_mod, only: get_index_3d, get_index_2d
  real*8, allocatable, dimension(:), save :: tsn
  real*8, allocatable, dimension(:), save :: pun
  real*8, allocatable, dimension(:), save :: pvn
  real*8, allocatable, dimension(:), save :: pwn
  real*8, allocatable, dimension(:), save :: mydomain
  real*8, allocatable, dimension(:), save :: zslpx
  real*8, allocatable, dimension(:), save :: zslpy
  real*8, allocatable, dimension(:), save :: zwx
  real*8, allocatable, dimension(:), save :: zwy
  real*8, allocatable, dimension(:), save :: umask
  real*8, allocatable, dimension(:), save :: vmask
  real*8, allocatable, dimension(:), save :: tmask
  real*8, allocatable, dimension(:), save :: zind
  real*8, allocatable, dimension(:), save :: ztfreez
  real*8, allocatable, dimension(:), save :: rnfmsk
  real*8, allocatable, dimension(:), save :: upsmsk
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
  integer :: id3, id2, id3_b, id3_c

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
  !ALLOCATE(mydomain(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(zwx(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(zwy(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(zslpx(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(zslpy(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(pun(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(pvn(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(pwn(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(umask(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(vmask(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(tmask(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(zind(1:jpi,1:jpj,1:jpk))
  !ALLOCATE(ztfreez(1:jpi,1:jpj))
  !ALLOCATE(rnfmsk(1:jpi,1:jpj))
  !ALLOCATE(upsmsk(1:jpi,1:jpj))
  !ALLOCATE(rnfmsk_z(1:jpk))
  !ALLOCATE(tsn(1:jpi,1:jpj,1:jpk))
  ALLOCATE(mydomain(1:jpi*jpj*jpk))
  ALLOCATE(zwx(1:jpi*jpj*jpk))
  ALLOCATE(zwy(1:jpi*jpj*jpk))
  ALLOCATE(zslpx(1:jpi*jpj*jpk))
  ALLOCATE(zslpy(1:jpi*jpj*jpk))
  ALLOCATE(pun(1:jpi*jpj*jpk))
  ALLOCATE(pvn(1:jpi*jpj*jpk))
  ALLOCATE(pwn(1:jpi*jpj*jpk))
  ALLOCATE(umask(1:jpi*jpj*jpk))
  ALLOCATE(vmask(1:jpi*jpj*jpk))
  ALLOCATE(tmask(1:jpi*jpj*jpk))
  ALLOCATE(zind(1:jpi*jpj*jpk))
  ALLOCATE(ztfreez(1:jpi*jpj))
  ALLOCATE(rnfmsk(1:jpi*jpj))
  ALLOCATE(upsmsk(1:jpi*jpj))
  ALLOCATE(rnfmsk_z(1:jpk))
  ALLOCATE(tsn(1:jpi*jpj*jpk))
  r = jpi * jpj * jpk
  !$omp target
  !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3)
  do jk = 1, jpk, 1
    do jj = 1, jpj, 1
      do ji = 1, jpi, 1
        id3 = get_index_3d(ji,jj,jk,jpi,jpj)
        umask(id3) = ji * jj * jk / r
        mydomain(id3) = ji * jj * jk / r
        pun(id3) = ji * jj * jk / r
        pvn(id3) = ji * jj * jk / r
        pwn(id3) = ji * jj * jk / r
        vmask(id3) = ji * jj * jk / r
        tsn(id3) = ji * jj * jk / r
        tmask(id3) = ji * jj * jk / r
      enddo
    enddo
  enddo
  !$omp end teams distribute parallel do
  !$omp end target
  r = jpi * jpj
  !$omp target
  !$omp teams distribute parallel do collapse(2) default(shared) private(ji,jj,id2)
  do jj = 1, jpj, 1
    do ji = 1, jpi, 1
      id2 = get_index_2d(ji,jj,jpi)
      ztfreez(id2) = ji * jj / r
      upsmsk(id2) = ji * jj / r
      rnfmsk(id2) = ji * jj / r
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
  !dim 2 = jpj
  !dim 1 = jpi
  !dim 3 = jpk
  loop_stop = jpj
  loop_start = 1
  loop_stop_1 = jpi
  loop_start_1 = 1
  loop_stop_2 = jpj
  loop_start_2 = 1
  loop_stop_3 = jpi
  loop_start_3 = 1
  loop_stop_4 = jpk - 1
  loop_stop_5 = jpj - 1
  loop_stop_6 = jpi - 1
  loop_stop_7 = jpj
  loop_start_4 = 1
  loop_stop_8 = jpi
  loop_start_5 = 1
  loop_stop_9 = jpj
  loop_start_6 = 1
  loop_stop_10 = jpi
  loop_start_7 = 1
  loop_stop_11 = jpk - 1
  loop_stop_12 = jpk - 1
  loop_stop_13 = jpk - 1
  loop_stop_14 = jpj - 1
  loop_stop_15 = jpi - 1
  loop_stop_16 = jpk - 1
  loop_stop_17 = jpj - 1
  loop_stop_18 = jpi - 1
  loop_stop_19 = jpj
  loop_start_8 = 1
  loop_stop_20 = jpi
  loop_start_9 = 1
  loop_stop_21 = jpj
  loop_start_10 = 1
  loop_stop_22 = jpi
  loop_start_11 = 1
  loop_stop_23 = jpk - 1
  loop_stop_24 = jpj
  loop_start_12 = 1
  loop_stop_25 = jpi
  loop_start_13 = 1
  loop_stop_26 = jpj
  loop_start_14 = 1
  loop_stop_27 = jpi
  loop_start_15 = 1
  loop_stop_28 = jpk - 1
  loop_stop_29 = jpk - 1
  loop_stop_30 = jpj
  loop_start_16 = 1
  loop_stop_31 = jpi
  loop_start_17 = 1
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
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,zice,id3,id2)
    do jk = 1, jpk, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id2 = get_index_2d(ji,jj,jpi)
          if (tsn(id3) <= ztfreez(id2) + 0.1d0) then
            zice = 1.d0
          else
            zice = 0.d0
          end if
          zind(id3) = MAX(rnfmsk(id2) * rnfmsk_z(jk), upsmsk(id2), zice) * tmask(id3)
          zind(id3) = 1 - zind(id3)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx,idx_1,id3)
    do idx = loop_start, loop_stop, 1
      do idx_1 = loop_start_1, loop_stop_1, 1
        id3 = get_index_3d(idx_1,idx,jpk,jpi,jpj)
        zwx(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_2,idx_3,id3)
    do idx_2 = loop_start_2, loop_stop_2, 1
      do idx_3 = loop_start_3, loop_stop_3, 1
        id3 = get_index_3d(idx_3,idx_2,jpk,jpi,jpj)
        zwy(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3)
    do jk = 1, loop_stop_4, 1
      do jj = 1, loop_stop_5, 1
        do ji = 1, loop_stop_6, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          zwx(id3) = umask(id3) * (mydomain(get_index_3d(ji + 1,jj,jk,jpi,jpj)) - mydomain(id3))
          zwy(id3) = vmask(id3) * (mydomain(get_index_3d(ji,jj + 1,jk,jpi,jpj)) - mydomain(id3))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_4,idx_5,id3)
    do idx_4 = loop_start_4, loop_stop_7, 1
      do idx_5 = loop_start_5, loop_stop_8, 1
        id3 = get_index_3d(idx_5,idx_4,jpk,jpi,jpj)
        zslpx(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_6,idx_7,id3)
    do idx_6 = loop_start_6, loop_stop_9, 1
      do idx_7 = loop_start_7, loop_stop_10, 1
        id3 = get_index_3d(idx_7,idx_6,jpk,jpi,jpj)
        zslpy(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3,id3_b,id3_c)
    do jk = 1, loop_stop_11, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji - 1,jj,jk,jpi,jpj)
          id3_c = get_index_3d(ji,jj-1,jk,jpi,jpj)
          zslpx(id3) = (zwx(id3) + zwx(id3_b)) * (0.25d0 + SIGN(0.25d0, zwx(id3) * zwx(id3_b)))
          zslpy(id3) = (zwy(id3) + zwy(id3_c)) * (0.25d0 + SIGN(0.25d0, zwy(id3) * zwy(id3_c)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3,id3_b,id3_c)
    do jk = 1, loop_stop_12, 1
      do jj = 2, jpj, 1
        do ji = 2, jpi, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji - 1,jj,jk,jpi,jpj)
          id3_c = get_index_3d(ji,jj - 1,jk,jpi,jpj)
          zslpx(id3) = SIGN(1.d0, zslpx(id3)) * MIN(ABS(zslpx(id3)), 2.d0 * ABS(zwx(id3_b)), 2.d0 * &
&ABS(zwx(id3)))
          zslpy(id3) = SIGN(1.d0, zslpy(id3)) * MIN(ABS(zslpy(id3)), 2.d0 * ABS(zwy(id3_c)), 2.d0 * &
&ABS(zwy(id3)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zdt = 1
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) &
    !$omp& private(ji,jj,jk,z0u,z0v,zalpha,zu,zv,zzwx,zzwy,id3,id3_b,id3_c)
    do jk = 1, loop_stop_13, 1
      do jj = 2, loop_stop_14, 1
        do ji = 2, loop_stop_15, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji + 1,jj,jk,jpi,jpj)
          id3_c = get_index_3d(ji,jj + 1,jk,jpi,jpj)
          z0u = SIGN(0.5d0, pun(id3))
          zalpha = 0.5d0 - z0u
          zu = z0u - 0.5d0 * pun(id3) * zdt
          zzwx = mydomain(id3_b) + zind(id3) * (zu * zslpx(id3_b))
          zzwy = mydomain(id3) + zind(id3) * (zu * zslpx(id3))
          zwx(id3) = pun(id3) * (zalpha * zzwx + (1. - zalpha) * zzwy)
          z0v = SIGN(0.5d0, pvn(id3))
          zalpha = 0.5d0 - z0v
          zv = z0v - 0.5d0 * pvn(id3) * zdt
          zzwx = mydomain(id3_c) + zind(id3) * (zv * zslpy(id3_c))
          zzwy = mydomain(id3) + zind(id3) * (zv * zslpy(id3))
          zwy(id3) = pvn(id3) * (zalpha * zzwx + (1.d0 - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,ztra,id3,id3_b,id3_c)
    do jk = 1, loop_stop_16, 1
      do jj = 2, loop_stop_17, 1
        do ji = 2, loop_stop_18, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji - 1,jj,jk,jpi,jpj)
          id3_c = get_index_3d(ji,jj - 1,jk,jpi,jpj)
          ztra = -zbtr * (zwx(id3) - zwx(id3_b) + zwy(id3) - zwy(id3_c))
          mydomain(id3) = mydomain(id3) + ztra
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_8,idx_9,id3)
    do idx_8 = loop_start_8, loop_stop_19, 1
      do idx_9 = loop_start_9, loop_stop_20, 1
        id3 = get_index_3d(idx_9,idx_8,1,jpi,jpj)
        zwx(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_10,idx_11,id3)
    do idx_10 = loop_start_10, loop_stop_21, 1
      do idx_11 = loop_start_11, loop_stop_22, 1
        id3 = get_index_3d(idx_11,idx_10,jpk,jpi,jpj)
        zwx(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(idx_12,idx_13,jk,id3,id3_b)
    do jk = 2, loop_stop_23, 1
      do idx_12 = loop_start_12, loop_stop_24, 1
        do idx_13 = loop_start_13, loop_stop_25, 1
          id3 = get_index_3d(idx_13,idx_12,jk,jpi,jpj)
          id3_b = get_index_3d(idx_13,idx_12,jk-1,jpi,jpj)
          zwx(id3) = tmask(id3) * (mydomain(id3_b) - mydomain(id3))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_14,idx_15,id3)
    do idx_14 = loop_start_14, loop_stop_26, 1
      do idx_15 = loop_start_15, loop_stop_27, 1
        id3 = get_index_3d(idx_15,idx_14,1,jpi,jpj)
        zslpx(id3) = 0.e0
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3,id3_b)
    do jk = 2, loop_stop_28, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji,jj,jk+1,jpi,jpj)
          zslpx(id3) = (zwx(id3) + zwx(id3_b)) * (0.25d0 + SIGN(0.25d0, zwx(id3) * zwx(id3_b)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,id3,id3_b)
    do jk = 2, loop_stop_29, 1
      do jj = 1, jpj, 1
        do ji = 1, jpi, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji,jj,jk+1,jpi,jpj)
          zslpx(id3) = SIGN(1.d0, zslpx(id3)) * MIN(ABS(zslpx(id3)), 2.d0 * ABS(zwx(id3_b)), 2.d0 * &
&ABS(zwx(id3)))
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    !$omp target
    !$omp teams distribute parallel do collapse(2) default(shared) private(idx_16,idx_17,id3)
    do idx_16 = loop_start_16, loop_stop_30, 1
      do idx_17 = loop_start_17, loop_stop_31, 1
        id3 = get_index_3d(idx_17,idx_16,1,jpi,jpj)
        zwx(id3) = pwn(id3) * mydomain(id3)
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zdt = 1
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,z0w,zalpha,zw,zzwx,zzwy,id3,id3_b)
    do jk = 1, loop_stop_32, 1
      do jj = 2, loop_stop_33, 1
        do ji = 2, loop_stop_34, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji,jj,jk+1,jpi,jpj)
          z0w = SIGN(0.5d0, pwn(id3_b))
          zalpha = 0.5d0 + z0w
          zw = z0w - 0.5d0 * pwn(id3_b) * zdt * zbtr
          zzwx = mydomain(id3_b) + zind(id3) * (zw * zslpx(id3_b))
          zzwy = mydomain(id3) + zind(id3) * (zw * zslpx(id3))
          zwx(id3_b) = pwn(id3_b) * (zalpha * zzwx + (1. - zalpha) * zzwy)
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
    zbtr = 1.
    !$omp target
    !$omp teams distribute parallel do collapse(3) default(shared) private(ji,jj,jk,ztra,id3,id3_b)
    do jk = 1, loop_stop_35, 1
      do jj = 2, loop_stop_36, 1
        do ji = 2, loop_stop_37, 1
          id3 = get_index_3d(ji,jj,jk,jpi,jpj)
          id3_b = get_index_3d(ji,jj,jk+1,jpi,jpj)
          ztra = -zbtr * (zwx(id3) - zwx(id3_b))
          mydomain(id3) = ztra
        enddo
      enddo
    enddo
    !$omp end teams distribute parallel do
    !$omp end target
  enddo
  step_timer_f = omp_get_wtime() - step_timer_f

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
        checksum = checksum + mydomain(get_index_3d(ji,jj,jk,jpi,jpj))

        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Write_Stmt
        WRITE(24, *) mydomain(get_index_3d(ji, jj, jk,jpi,jpj))
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
