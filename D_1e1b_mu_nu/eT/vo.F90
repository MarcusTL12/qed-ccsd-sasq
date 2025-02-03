   subroutine density_1e_1b_qed_ccsd_mu_nu_vo_qed_ccsd_qed_ccsd(wf, D_vo, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rt_vo, s0, s_vo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: D_vo
!
      real(dp), intent(in) :: Rs, s0
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo
!
      call daxpy(wf%n_v*wf%n_o, Rs, Lt_vo, 1, D_vo, 1)
      call daxpy(wf%n_v*wf%n_o, Rs*s0, Ls_vo, 1, D_vo, 1)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         one, &
         D_vo, 1)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         one, &
         D_vo, 1)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         one, &
         D_vo, 1)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         one, &
         D_vo, 1)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         one, &
         D_vo, 1)
!
!
   end subroutine density_1e_1b_qed_ccsd_mu_nu_vo_qed_ccsd_qed_ccsd

