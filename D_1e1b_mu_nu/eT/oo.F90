   subroutine density_1e_1b_qed_ccsd_mu_nu_oo_qed_ccsd_qed_ccsd(wf, D_oo, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, s0, s_vo, s_vovo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(inout) :: D_oo
!
      real(dp), intent(in) :: Rs, s0
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo, Rs_vovo, Rt_vovo, s_vovo, t_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         Lt_vo, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two*Rs, &
         s_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Rs_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Rt_vo, &
         wf%n_v, &
         Lt_vo, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two*Rs, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X1, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X1, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X2, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X2, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%dealloc(X2)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%alloc(X3, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X3, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X3, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X4, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X4, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%dealloc(X4)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs*s0, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -s0, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X5, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X5, &
         wf%n_v, &
         one, &
         D_oo, &
         wf%n_o)
!
      call mem%dealloc(X5)
!
   end subroutine density_1e_1b_qed_ccsd_mu_nu_oo_qed_ccsd_qed_ccsd

