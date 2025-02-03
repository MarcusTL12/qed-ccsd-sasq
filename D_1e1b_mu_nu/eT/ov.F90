   subroutine density_1e_1b_qed_ccsd_mu_nu_ov_qed_ccsd_qed_ccsd(wf, D_ov, Ls1, Ls_vo, Ls_vovo, Lt_vo, Lt_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Ru_vovo, Rv_vovo, s0, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_o,wf%n_v), intent(inout) :: D_ov
!
      real(dp), intent(in) :: Ls1, Rs, s0
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Lt_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Lt_vovo, Rs_vovo, Rt_vovo, Ru_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
!
      real(dp) :: X3, X6, X10, X13, X18, X19, X24, X37, X42, X60, X67
      real(dp), dimension(:,:), allocatable :: X1, X2, X4, X5, X7, X8, X9, X11, X12, X14, X15, X16, X17, X20, X21, X22, X23, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X38, X39, X40, X41, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X61, X62, X63, X64, X65, X66, X68, X69, X70, X71, X72, X73
!
      real(dp), external :: ddot
!
      call add_21_to_12(two*Ls1, Rt_vo, D_ov, wf%n_o, wf%n_v)
      call add_21_to_12(four*Ls1*Rs, s_vo, D_ov, wf%n_o, wf%n_v)
      call add_21_to_12(two*Ls1*s0, Rs_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X1, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X1, 1)
!
      call add_21_to_12(one, X1, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Lt_vo, 1, &
         zero, &
         X2, 1)
!
      call add_21_to_12(one, X2, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X2)
      X3 = two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, s_vo, 1)
      call add_21_to_12(X3, Rs_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X4, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X4, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X4, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_o, wf%n_o)
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
         X5, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X5, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X5)
      X6 = four * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      call add_21_to_12(X6, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X7, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X7, 1)
!
      call add_21_to_12(one, X7, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X8, 1)
!
      call add_21_to_12(one, X8, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Lt_vo, 1, &
         zero, &
         X9, 1)
!
      call add_21_to_12(one, X9, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X9)
      X10 = two * ddot(wf%n_v*wf%n_o, Lt_vo, 1, s_vo, 1)
      call add_21_to_12(X10, Rt_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X11, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Lt_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X11, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X11, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Lt_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X12, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X12)
      X13 = two * ddot(wf%n_v*wf%n_o, Lt_vo, 1, Rt_vo, 1)
      call add_21_to_12(X13, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X14, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Lt_vo, 1, &
         zero, &
         X14, 1)
!
      call add_21_to_12(one, X14, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs*s0, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X15, 1)
!
      call add_21_to_12(one, X15, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X16, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X16, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_o, wf%n_o)
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
         X17, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X17, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X17)
      X18 = two*s0 * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call add_21_to_12(X18, s_vo, D_ov, wf%n_o, wf%n_v)
      X19 = ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, s_vovo, 1)
      call add_21_to_12(X19, Rs_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X20, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X20, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X20, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X21, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_v, wf%n_v)
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
         X22, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X22, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
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
         X23, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X23, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X23)
      X24 = two * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call add_21_to_12(X24, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X25, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X25, 1)
!
      call mem%alloc(X26, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X25, 1, &
         zero, &
         X26, 1)
!
      call mem%dealloc(X25)
      call add_21_to_12(one, X26, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X27, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X27, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_v, wf%n_v)
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
         X28, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X28, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X29, 1)
!
      call mem%alloc(X30, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X29, 1, &
         zero, &
         X30, 1)
!
      call mem%dealloc(X29)
      call add_21_to_12(one, X30, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X31, 1)
!
      call mem%alloc(X32, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         X31, 1, &
         zero, &
         X32, 1)
!
      call mem%dealloc(X31)
      call add_21_to_12(one, X32, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X33, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X33, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X33)
      call mem%alloc(X34, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Lt_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X34, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X34, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X35, 1)
!
      call mem%alloc(X36, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X35, 1, &
         zero, &
         X36, 1)
!
      call mem%dealloc(X35)
      call add_21_to_12(one, X36, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X36)
      X37 = ddot(wf%n_v**2*wf%n_o**2, Lt_vovo, 1, s_vovo, 1)
      call add_21_to_12(X37, Rt_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X38, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X38, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X38, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X38)
      call mem%alloc(X39, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Lt_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X39, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X39, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X39)
      call mem%alloc(X40, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Lt_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X40, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X40, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X41, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X41, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X41)
      X42 = ddot(wf%n_v**2*wf%n_o**2, Lt_vovo, 1, Rt_vovo, 1)
      call add_21_to_12(X42, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X43, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X43, 1)
!
      call mem%alloc(X44, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X43, 1, &
         zero, &
         X44, 1)
!
      call mem%dealloc(X43)
      call add_21_to_12(one, X44, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X44)
      call mem%alloc(X45, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X45, 1)
!
      call mem%alloc(X46, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         X45, 1, &
         zero, &
         X46, 1)
!
      call mem%dealloc(X45)
      call add_21_to_12(one, X46, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two*Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X47, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X47, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X48, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         Rs, &
         s_vo, &
         wf%n_v, &
         X48, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X49, 1)
!
      call mem%alloc(X50, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X49, 1, &
         zero, &
         X50, 1)
!
      call mem%dealloc(X49)
      call add_21_to_12(one, X50, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X51, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X51, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -s0, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X52, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X52, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X53, 1)
!
      call mem%alloc(X54, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X53, 1, &
         zero, &
         X54, 1)
!
      call mem%dealloc(X53)
      call add_21_to_12(one, X54, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X54)
      call mem%alloc(X55, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X55, 1)
!
      call mem%alloc(X56, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X55, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X56, &
         wf%n_o)
!
      call mem%dealloc(X55)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X56, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X57, 1)
!
      call mem%alloc(X58, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X57, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X58, &
         wf%n_o)
!
      call mem%dealloc(X57)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X58, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X58)
      call mem%alloc(X59, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vo, 1, &
         zero, &
         X59, 1)
!
      X60 = ddot(wf%n_v*wf%n_o, X59, 1, Rt_vo, 1)
      call mem%dealloc(X59)
      call add_21_to_12(X60, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X61, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X61, 1)
!
      call mem%alloc(X62, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X61, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X62, &
         wf%n_o)
!
      call mem%dealloc(X61)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X62, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X62)
      call mem%alloc(X63, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X63, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X63, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X63)
      call mem%alloc(X64, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -s0, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X64, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X64, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_v, wf%n_v)
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
         X65, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X65, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X65)
      call mem%alloc(X66, wf%n_o, wf%n_o)
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
         X66, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X66, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X66)
      X67 = s0 * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call add_21_to_12(X67, s_vo, D_ov, wf%n_o, wf%n_v)
      call mem%alloc(X68, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X68, 1)
!
      call mem%alloc(X69, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X68, 1, &
         zero, &
         X69, 1)
!
      call mem%dealloc(X68)
      call add_21_to_12(one, X69, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X69)
      call mem%alloc(X70, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -s0, &
         Lt_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X70, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X70, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Lt_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X71, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         s0, &
         Rt_vo, &
         wf%n_v, &
         X71, &
         wf%n_v, &
         one, &
         D_ov, &
         wf%n_o)
!
      call mem%dealloc(X71)
      call mem%alloc(X72, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         s0, &
         Lt_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X72, 1)
!
      call mem%alloc(X73, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X72, 1, &
         zero, &
         X73, 1)
!
      call mem%dealloc(X72)
      call add_21_to_12(one, X73, D_ov, wf%n_o, wf%n_v)
      call mem%dealloc(X73)
!
   end subroutine density_1e_1b_qed_ccsd_mu_nu_ov_qed_ccsd_qed_ccsd

