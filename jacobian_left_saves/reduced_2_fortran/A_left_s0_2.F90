   subroutine jacobian_transpose_s0_2_qed_ccsd_2(wf, sigma, bs, bs2_vovo, bs_2, bs_vo, bs_vovo, d_oo, d_ov, d_vo, d_vv, s2, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), intent(inout) :: sigma
!
      real(dp), intent(in) :: bs, bs_2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo, d_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, bs_vovo, s2, t_vovo
!
      real(dp) :: X1
      real(dp), dimension(:,:), allocatable :: X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12
!
      integer :: i1
!
      real(dp), external :: ddot
!
      sigma = sigma + four*bs_2 * wf%qed%frequencies(wf%mode)
      X1 = zero
!
      do i1 = 1, wf%n_o
         X1 = X1 + d_oo(i1,i1)
      end do
!
      sigma = sigma + four*bs * X1
      sigma = sigma + four * ddot(wf%n_v*wf%n_o, bs_vo, 1, d_vo, 1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X2, wf%n_o, wf%n_v)
      sigma = sigma + eight*bs_2 * ddot(wf%n_v*wf%n_o, X2, 1, wf%s1, 1)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X3, wf%n_o, wf%n_v)
      call mem%alloc(X4, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         X3, 1, &
         zero, &
         X4, 1)
!
      call mem%dealloc(X3)
      sigma = sigma + ddot(wf%n_v*wf%n_o, bs_vo, 1, X4, 1)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         bs_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X5, &
         wf%n_v)
!
      sigma = sigma + ddot(wf%n_v**2, X5, 1, d_vv, 1)
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         bs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X6, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X6, 1, d_oo, 1)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         four, &
         bs2_vovo, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_v)
!
      sigma = sigma + ddot(wf%n_v**2, X7, 1, d_vv, 1)
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -four, &
         s2, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X8, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X8, 1, d_oo, 1)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -four, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X9, &
         wf%n_v)
!
      call mem%alloc(X10, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X9, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X10, &
         wf%n_v)
!
      call mem%dealloc(X9)
      sigma = sigma + ddot(wf%n_v*wf%n_o, X10, 1, wf%s1, 1)
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -four, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X11, &
         wf%n_o)
!
      call mem%alloc(X12, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X11, 1, X12, 1)
      call mem%dealloc(X11)
      call mem%dealloc(X12)
!
   end subroutine jacobian_transpose_s0_2_qed_ccsd_2
