   subroutine omega_1_qed_ccsd_2(wf, omega, d_oo, d_ov)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), intent(inout) :: omega
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
!
      real(dp) :: X1
      real(dp), dimension(:,:), allocatable :: X2
!
      integer :: i1
!
      real(dp), external :: ddot
!
      X1 = zero
!
      do i1 = 1, wf%n_o
         X1 = X1 + d_oo(i1,i1)
      end do
!
      omega = omega + four*X1 * wf%s0_2
      call mem%alloc(X2, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X2, wf%n_o, wf%n_v)
      omega = omega + four * ddot(wf%n_v*wf%n_o, X2, 1, wf%s1_2, 1)
      call mem%dealloc(X2)
!
   end subroutine omega_1_qed_ccsd_2
