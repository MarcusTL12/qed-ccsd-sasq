   subroutine density_1e_1b_qed_ccsd_mu_nu_vv_qed_ccsd_qed_ccsd(wf, D_vv, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, s0, s_vo, s_vovo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_v), intent(inout) :: D_vv
!
      real(dp), intent(in) :: Rs, s0
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo, Rs_vovo, Rt_vovo, s_vovo, t_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Lt_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         two*Rs, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         s0, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         s0, &
         Lt_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Lt_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two*Rs, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X1, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X1, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X2, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X2, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%dealloc(X2)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         s0, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         Rs, &
         Lt_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%alloc(X3, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X3, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X3, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X4, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X4, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%dealloc(X4)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         s0, &
         Lt_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         Rs*s0, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X5, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X5, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         one, &
         D_vv, &
         wf%n_v)
!
      call mem%dealloc(X5)
!
   end subroutine density_1e_1b_qed_ccsd_mu_nu_vv_qed_ccsd_qed_ccsd

