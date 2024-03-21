   subroutine jacobian_transpose_s0_qed_ccsd_2(wf, sigma, bs2_vovo, bs_vo, bγ2, d_oo, d_ov, d_vv, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), intent(inout) :: sigma
!
      real(dp), intent(in) :: bγ2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15
!
      real(dp), external :: ddot
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X1, wf%n_o, wf%n_v)
      sigma = sigma + four*bγ2 * ddot(wf%n_v*wf%n_o, X1, 1, wf%s1_2, 1)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_v)
!
      sigma = sigma + ddot(wf%n_v*wf%n_o, X2, 1, wf%s1, 1)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         wf%s1, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X3, 1, d_oo, 1)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X4, wf%n_o, wf%n_v)
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         X4, 1, &
         zero, &
         X5, 1)
!
      call mem%dealloc(X4)
      sigma = sigma + ddot(wf%n_v*wf%n_o, bs_vo, 1, X5, 1)
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X6, &
         wf%n_v)
!
      sigma = sigma + ddot(wf%n_v**2, X6, 1, d_vv, 1)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X7, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X7, 1, d_oo, 1)
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X8, &
         wf%n_v)
!
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X8, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X9, &
         wf%n_v)
!
      call mem%dealloc(X8)
      sigma = sigma + ddot(wf%n_v*wf%n_o, X9, 1, wf%s1, 1)
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X10, &
         wf%n_o)
!
      call mem%alloc(X11, wf%n_o, wf%n_o)
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
         X11, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X10, 1, X11, 1)
      call mem%dealloc(X10)
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_v)
!
      call mem%alloc(X13, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X12, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X13, &
         wf%n_v)
!
      call mem%dealloc(X12)
      sigma = sigma + ddot(wf%n_v*wf%n_o, X13, 1, wf%s1_2, 1)
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X14, &
         wf%n_o)
!
      call mem%alloc(X15, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X15, &
         wf%n_o)
!
      sigma = sigma + ddot(wf%n_o**2, X14, 1, X15, 1)
      call mem%dealloc(X14)
      call mem%dealloc(X15)
!
   end subroutine jacobian_transpose_s0_qed_ccsd_2
