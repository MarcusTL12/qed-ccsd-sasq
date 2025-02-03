   subroutine density_1e_1b_qed_ccsd_mu_nu_oo_diagonal_terms_qed_ccsd_qed_ccsd(wf, D, Ls1, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, s0, s_vo, s_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), intent(inout) :: D
!
      real(dp), intent(in) :: Ls1, Rs, s0
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo, Rs_vovo, Rt_vovo, s_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2
!
      real(dp), external :: ddot
!
      D = D + two*Ls1 * s0
      D = D + two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      D = D + two * ddot(wf%n_v*wf%n_o, Lt_vo, 1, Rs_vo, 1)
      D = D + two*Rs * ddot(wf%n_v*wf%n_o, Ls_vo, 1, s_vo, 1)
      D = D + two*s0 * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      D = D + two*s0 * ddot(wf%n_v*wf%n_o, Lt_vo, 1, Rt_vo, 1)
      D = D + ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      D = D + ddot(wf%n_v**2*wf%n_o**2, Lt_vovo, 1, Rs_vovo, 1)
      D = D + Rs * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, s_vovo, 1)
      call mem%alloc(X1, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X1, 1)
!
      D = D + ddot(wf%n_v*wf%n_o, X1, 1, s_vo, 1)
      call mem%dealloc(X1)
      D = D + s0 * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X2, 1)
!
      D = D + ddot(wf%n_v*wf%n_o, X2, 1, Rt_vo, 1)
      call mem%dealloc(X2)
      D = D + s0 * ddot(wf%n_v**2*wf%n_o**2, Lt_vovo, 1, Rt_vovo, 1)
!
   end subroutine density_1e_1b_qed_ccsd_mu_nu_oo_diagonal_terms_qed_ccsd_qed_ccsd

