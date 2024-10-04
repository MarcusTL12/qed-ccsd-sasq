   subroutine F_matrix_transformation_bilinear_singles_photon_qed_ccsd(wf, rho, Ls1, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Rv_vovo, d_oo, d_ov, d_vv, s0, s_vo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: rho
!
      real(dp), intent(in) :: Ls1, Rs, s0
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Rv_vovo, t_vovo
!
      real(dp) :: X2, X5, X14
      real(dp), dimension(:,:), allocatable :: X1, X3, X4, X6, X7, X8, X9, X10, X11, X12, X13, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28
!
      real(dp), external :: ddot
!
      call add_21_to_12(two*Ls1*Rs, d_ov, rho, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -Rs, &
         Ls_vo, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         Rs, &
         d_vv, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%alloc(X1, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X1, wf%n_v, wf%n_o)
      X2 = two * ddot(wf%n_v*wf%n_o, X1, 1, d_ov, 1)
      call mem%dealloc(X1)
      call daxpy(wf%n_v*wf%n_o, X2, Ls_vo, 1, rho, 1)
      call mem%alloc(X3, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X3, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X3, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X4, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X4, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X4)
      X5 = two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      call add_21_to_12(X5, d_ov, rho, wf%n_v, wf%n_o)
      call mem%alloc(X6, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X6, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X6, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X7, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         zero, &
         X8, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X8, 1, &
         one, &
         rho, 1)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X9, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X9, 1, &
         one, &
         rho, 1)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X10, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X10, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X11, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         X11, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_v)
!
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
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X13, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X13, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X13)
      X14 = ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call add_21_to_12(X14, d_ov, rho, wf%n_v, wf%n_o)
      call mem%alloc(X15, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X15, wf%n_o, wf%n_v)
      call mem%alloc(X16, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         X15, 1, &
         zero, &
         X16, 1)
!
      call mem%dealloc(X15)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X16, 1, &
         one, &
         rho, 1)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X17, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         Rs, &
         X17, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X18, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X18, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         d_ov, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X19, &
         wf%n_o)
!
      call mem%alloc(X20, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X19, &
         wf%n_o, &
         zero, &
         X20, &
         wf%n_v)
!
      call mem%dealloc(X19)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X20, 1, &
         one, &
         rho, 1)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X21, &
         wf%n_o)
!
      call mem%alloc(X22, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s_vo, &
         wf%n_v, &
         X21, &
         wf%n_o, &
         zero, &
         X22, &
         wf%n_v)
!
      call mem%dealloc(X21)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X22, 1, &
         one, &
         rho, 1)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X23, &
         wf%n_o)
!
      call mem%alloc(X24, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X24, 1)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X24, &
         wf%n_v, &
         X23, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X23)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X25, 1)
!
      call mem%alloc(X26, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X25, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_o)
!
      call mem%dealloc(X25)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X26, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -s0, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X27, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X27, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X28, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X28, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X28)
!
   end subroutine F_matrix_transformation_bilinear_singles_photon_qed_ccsd

