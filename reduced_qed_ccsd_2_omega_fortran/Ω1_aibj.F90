   subroutine omega_1_aibj_qed_ccsd_2(wf, omega_vovo, d_oo, d_ov, d_vo, d_vv, s₂_vovo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: omega_vovo
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: d_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: s₂_vovo, t_vovo
!
      real(dp) :: X1
      real(dp), dimension(:,:), allocatable :: X2, X3, X4, X5
!
      integer :: i1
!
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         d_vo, 1, &
         wf%s1_2, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         d_vv, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      X1 = zero
!
      do i1 = 1, wf%n_o
         X1 = X1 + d_oo(i1,i1)
      end do
!
      call daxpy(wf%n_v**2*wf%n_o**2, two*X1, s₂_vovo, 1, omega_vovo, 1)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two*wf%s0_1, &
         d_vv, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two*wf%s0_1, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X2, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X2, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X3, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X4, wf%n_o, wf%n_v)
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         X4, 1, &
         zero, &
         X5, 1)
!
      call mem%dealloc(X4)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%s1_2, 1, &
         X5, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X5)
!
   end subroutine omega_1_aibj_qed_ccsd_2
