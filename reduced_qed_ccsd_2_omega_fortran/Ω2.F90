   subroutine omega_2_qed_ccsd_2(wf, omega, F_ov, L_ovov, d_ov, g_ovov, v₂_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), intent(inout) :: omega
!
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: v₂_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X6, X7, X8
      real(dp), dimension(:,:,:,:), allocatable :: X5
!
      real(dp), external :: ddot
!
      omega = omega + four*wf%s0_1 * wf%qed%frequencies(wf%mode)
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(F_ov, X1, wf%n_o, wf%n_v)
      omega = omega + four * ddot(wf%n_v*wf%n_o, X1, 1, wf%s1_2, 1)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X2, wf%n_o, wf%n_v)
      omega = omega + four * ddot(wf%n_v*wf%n_o, X2, 1, wf%s1, 1)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X3, wf%n_o, wf%n_v)
      omega = omega + eight*wf%s0_1 * ddot(wf%n_v*wf%n_o, X3, 1, wf%s1, 1)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X4, wf%n_o, wf%n_v)
      omega = omega + four*wf%s0 * ddot(wf%n_v*wf%n_o, X4, 1, wf%s1_2, 1)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_2143(g_ovov, X5, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      omega = omega + two * ddot(wf%n_v**2*wf%n_o**2, X5, 1, v₂_vovo, 1)
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X6, wf%n_v, wf%n_o)
      call mem%alloc(X7, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X6, 1, &
         zero, &
         X7, 1)
!
      call mem%dealloc(X6)
      call mem%alloc(X8, wf%n_v, wf%n_o)
      call sort_to_21(X7, X8, wf%n_o, wf%n_v)
      call mem%dealloc(X7)
      omega = omega + ddot(wf%n_v*wf%n_o, X8, 1, wf%s1, 1)
      call mem%dealloc(X8)
!
   end subroutine omega_2_qed_ccsd_2
