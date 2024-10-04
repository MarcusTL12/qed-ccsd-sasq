   subroutine W_J_oo_diagonal_terms_qed_ccsd(wf, W_J_oo, LJ_oo, LJ_ov, LJ_tr, LJ_vo, LJ_vv, Ls1, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Rv_vovo, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%eri_t1%n_J), intent(inout) :: W_J_oo
!
      real(dp), intent(in) :: Ls1, Rs
      real(dp), dimension(wf%eri_t1%n_J), intent(in) :: LJ_tr
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(in) :: LJ_oo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(in) :: LJ_ov
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(in) :: LJ_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: LJ_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
!
      real(dp) :: X2, X8, X15, X40
      real(dp), dimension(:,:), allocatable :: X1, X3, X4, X5, X6, X7, X9, X10, X11, X12, X13, X14, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X41, X42, X43, X44
!
      real(dp), external :: ddot
!
      call daxpy(wf%eri_t1%n_J, four*Ls1*Rs, LJ_tr, 1, W_J_oo, 1)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vo, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%alloc(X1, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X1, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         four*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X1, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X1)
      X2 = four * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      call daxpy(wf%eri_t1%n_J, X2, LJ_tr, 1, W_J_oo, 1)
      call mem%alloc(X3, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         two, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X3, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Rs_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X4, &
         wf%n_o)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X4, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X5, 1)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         X5, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X6, 1)
!
      call mem%alloc(X7, wf%n_o, wf%n_v)
      call sort_to_21(X6, X7, wf%n_v, wf%n_o)
      call mem%dealloc(X6)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X7, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X7)
      X8 = two * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call daxpy(wf%eri_t1%n_J, X8, LJ_tr, 1, W_J_oo, 1)
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X9, 1)
!
      call mem%alloc(X10, wf%n_o, wf%n_v)
      call sort_to_21(X9, X10, wf%n_v, wf%n_o)
      call mem%dealloc(X9)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X10, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_o)
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
         X11, &
         wf%n_o)
!
      call mem%alloc(X12, wf%n_o, wf%n_v)
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
         zero, &
         X12, &
         wf%n_o)
!
      call mem%dealloc(X11)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X12, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X13, &
         wf%n_o)
!
      call mem%alloc(X14, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X13, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X14, &
         wf%n_o)
!
      call mem%dealloc(X13)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X14, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X14)
      X15 = four * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call mem%alloc(X16, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X16, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         X15, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X16, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X17, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X17, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X18, &
         wf%n_o)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X18, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two*Rs, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X19, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X19, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two*Rs, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X20, &
         wf%n_o)
!
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X20, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X21, 1)
!
      call mem%alloc(X22, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X21, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X22, &
         wf%n_v)
!
      call mem%dealloc(X21)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X22, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X23, 1)
!
      call mem%alloc(X24, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X23, &
         wf%n_v, &
         zero, &
         X24, &
         wf%n_o)
!
      call mem%dealloc(X23)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X24, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X25, &
         wf%n_o)
!
      call mem%alloc(X26, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X25, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_o)
!
      call mem%dealloc(X25)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X26, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_v, wf%n_v)
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
         X27, &
         wf%n_v)
!
      call mem%alloc(X28, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X27, &
         wf%n_v, &
         zero, &
         X28, &
         wf%n_o)
!
      call mem%dealloc(X27)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X28, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
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
      call mem%alloc(X31, wf%n_o, wf%n_v)
      call sort_to_21(X30, X31, wf%n_v, wf%n_o)
      call mem%dealloc(X30)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X31, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_o, wf%n_o)
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
         X32, &
         wf%n_o)
!
      call mem%alloc(X33, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X32, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X33, &
         wf%n_o)
!
      call mem%dealloc(X32)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X33, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X33)
      call mem%alloc(X34, wf%n_v, wf%n_v)
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
         X34, &
         wf%n_v)
!
      call mem%alloc(X35, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X34, &
         wf%n_v, &
         zero, &
         X35, &
         wf%n_o)
!
      call mem%dealloc(X34)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X35, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X35)
      call mem%alloc(X36, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X36, &
         wf%n_v)
!
      call mem%alloc(X37, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X36, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_o)
!
      call mem%dealloc(X36)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X37, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X38, &
         wf%n_o)
!
      call mem%alloc(X39, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X38, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X39, &
         wf%n_o)
!
      call mem%dealloc(X38)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X39, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X39)
      X40 = two * ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call mem%alloc(X41, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X41, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         X40, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X41, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X41)
      call mem%alloc(X42, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X42, 1)
!
      call mem%alloc(X43, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X42, 1, &
         zero, &
         X43, 1)
!
      call mem%dealloc(X42)
      call mem%alloc(X44, wf%n_o, wf%n_v)
      call sort_to_21(X43, X44, wf%n_v, wf%n_o)
      call mem%dealloc(X43)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X44, 1, &
         one, &
         W_J_oo, 1)
!
      call mem%dealloc(X44)
!
   end subroutine W_J_oo_diagonal_terms_qed_ccsd

