   subroutine F_matrix_transformation_bilinear_singles_qed_ccsd(wf, rho, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, d_oo, d_ov, d_vv, s0, s_vo, s_vovo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: rho
!
      real(dp), intent(in) :: Rs, s0
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo, Rs_vovo, Rt_vovo, s_vovo, t_vovo
!
      real(dp) :: X5, X18
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38
!
      real(dp), external :: ddot
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -Rs, &
         Lt_vo, &
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
         Lt_vo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%alloc(X1, wf%n_o, wf%n_o)
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
         X1, &
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
         X1, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X2, &
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
         X2, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
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
         Lt_vo, &
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
         -one, &
         Lt_vo, &
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
      X5 = two * ddot(wf%n_v*wf%n_o, Lt_vo, 1, Rs_vo, 1)
      call add_21_to_12(X5, d_ov, rho, wf%n_v, wf%n_o)
      call mem%alloc(X6, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -Rs, &
         d_ov, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X6, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
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
         -Rs, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
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
      call mem%alloc(X8, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X8, &
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
         X8, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X9, &
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
         X9, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_o, wf%n_o)
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
         X10, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Lt_vo, &
         wf%n_v, &
         X10, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Lt_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X11, &
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
         X11, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X12, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X12, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X13, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         X13, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X14, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X14, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X15, &
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
         X15, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Lt_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X16, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X16, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X17, &
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
         X17, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X17)
      X18 = ddot(wf%n_v**2*wf%n_o**2, Lt_vovo, 1, Rs_vovo, 1)
      call add_21_to_12(X18, d_ov, rho, wf%n_v, wf%n_o)
      call mem%alloc(X19, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X19, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X19, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_o)
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
         X20, &
         wf%n_o)
!
      call mem%alloc(X21, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X21, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X21, &
         wf%n_v, &
         X20, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X20)
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X22, &
         wf%n_o)
!
      call mem%alloc(X23, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X23, 1)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X23, &
         wf%n_v, &
         X22, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X22)
      call mem%dealloc(X23)
      call mem%alloc(X24, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X24, &
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
         X24, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
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
         Rs_vo, &
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
      call mem%alloc(X27, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X27, 1)
!
      call mem%alloc(X28, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X27, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X28, &
         wf%n_o)
!
      call mem%dealloc(X27)
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
      call mem%alloc(X29, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -s0, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X29, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X30, &
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
         X30, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Lt_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X31, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X31, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -Rs, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X32, &
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
         X32, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_o, wf%n_o)
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
         X33, &
         wf%n_o)
!
      call mem%alloc(X34, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X34, 1)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X34, &
         wf%n_v, &
         X33, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X33)
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X35, 1)
!
      call mem%alloc(X36, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X35, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X36, &
         wf%n_o)
!
      call mem%dealloc(X35)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X36, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -s0, &
         Lt_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X37, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X38, &
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
         X38, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X38)
!
   end subroutine F_matrix_transformation_bilinear_singles_qed_ccsd

