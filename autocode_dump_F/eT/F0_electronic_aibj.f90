   subroutine F_matrix_transformation_doubles_qed_ccsd(wf, rho, F_ov, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, Ls_vo, Ls_vovo, Rs_vo, Rs_vovo, Rt_vo, Rv_vovo, g_ooov, g_ovoo, g_ovov, g_ovvv, g_vvov, s_vo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: rho
!
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rv_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X5, X7, X8, X9, X10, X11, X17, X18, X19, X20, X41, X43, X49, X51, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90
      real(dp), dimension(:,:,:,:), allocatable :: X4, X6, X12, X13, X14, X15, X16, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X42, X44, X45, X46, X47, X48, X50, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112
!
      call mem%alloc(X1, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         F_ov, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X1, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X1, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         F_ov, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X2, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_v)
!
      call mem%alloc(X4, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X3, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X4, &
         wf%n_v)
!
      call mem%dealloc(X3)
      call add_1432_to_1234(one, X4, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X5, &
         wf%n_o)
!
      call mem%alloc(X6, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X6, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X6, &
         wf%n_o, &
         X5, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X5)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X7, wf%n_v, wf%n_o)
      call mem%alloc(X8, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X7, 1, &
         zero, &
         X8, 1)
!
      call mem%dealloc(X7)
      call mem%alloc(X9, wf%n_v, wf%n_o)
      call sort_to_21(X8, X9, wf%n_o, wf%n_v)
      call mem%dealloc(X8)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X9, 1, &
         Ls_vo, 1, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X10, wf%n_v, wf%n_o)
      call mem%alloc(X11, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X10, 1, &
         zero, &
         X11, 1)
!
      call mem%dealloc(X10)
      call mem%alloc(X12, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vo, 1, &
         X11, 1, &
         X12, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X11)
      call add_1423_to_1234(one, X12, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         L_ovvv, &
         wf%n_v**2*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X13, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X14, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X13, &
         wf%n_v*wf%n_o, &
         zero, &
         X14, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X13)
      call add_1243_to_1234(one, X14, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X15, &
         wf%n_o)
!
      call mem%alloc(X16, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(L_ovoo, X16, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X16, &
         wf%n_v*wf%n_o, &
         X15, &
         wf%n_o**2, &
         one, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X16)
      call mem%dealloc(X15)
      call mem%alloc(X17, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X17, wf%n_v, wf%n_o)
      call mem%alloc(X18, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ooov, &
         wf%n_o**2, &
         X17, 1, &
         zero, &
         X18, 1)
!
      call mem%dealloc(X17)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X18, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X19, wf%n_v, wf%n_o)
      call mem%alloc(X20, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         L_vvov, &
         wf%n_v**2, &
         X19, 1, &
         zero, &
         X20, 1)
!
      call mem%dealloc(X19)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X20, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_o)
!
      call mem%alloc(X22, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X21, &
         wf%n_o**2, &
         g_ovoo, &
         wf%n_v*wf%n_o, &
         zero, &
         X22, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X21)
      call add_1423_to_1234(one, X22, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         g_ovvv, &
         wf%n_v**2*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X23, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X24, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X23, &
         wf%n_v*wf%n_o, &
         zero, &
         X24, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X23)
      call add_1423_to_1234(one, X24, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X25, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X26, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X25, &
         wf%n_o**3, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_o**3)
!
      call mem%dealloc(X25)
      call mem%alloc(X27, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X27, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X28, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X27, &
         wf%n_v**2, &
         X26, &
         wf%n_o**2, &
         zero, &
         X28, &
         wf%n_v**2)
!
      call mem%dealloc(X26)
      call mem%dealloc(X27)
      call add_1342_to_1234(one, X28, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_o)
!
      call mem%alloc(X30, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(g_ooov, X30, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X31, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X29, X31, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X29)
      call mem%alloc(X32, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X31, &
         wf%n_o**2, &
         X30, &
         wf%n_v*wf%n_o, &
         zero, &
         X32, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X30)
      call mem%dealloc(X31)
      call add_1423_to_1234(one, X32, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1432(g_ovvv, X33, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X34, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         X33, &
         wf%n_v**2*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X34, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X33)
      call mem%alloc(X35, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X35, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X36, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X35, &
         wf%n_v*wf%n_o, &
         X34, &
         wf%n_v*wf%n_o, &
         zero, &
         X36, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X34)
      call mem%dealloc(X35)
      call add_1423_to_1234(one, X36, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X38, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X37, X38, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X37)
      call mem%alloc(X39, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X39, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X40, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X39, &
         wf%n_v*wf%n_o, &
         X38, &
         wf%n_o**2, &
         zero, &
         X40, &
         wf%n_v**2)
!
      call mem%dealloc(X38)
      call mem%dealloc(X39)
      call add_1324_to_1234(one, X40, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X41, &
         wf%n_v)
!
      call mem%alloc(X42, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X41, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X42, &
         wf%n_v)
!
      call mem%dealloc(X41)
      call add_1432_to_1234(one, X42, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X42)
      call mem%alloc(X43, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X43, &
         wf%n_o)
!
      call mem%alloc(X44, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X44, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X44, &
         wf%n_o, &
         X43, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X43)
      call mem%dealloc(X44)
      call mem%alloc(X45, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1243(L_ovov, X45, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X46, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X45, &
         wf%n_v*wf%n_o, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X46, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X45)
      call mem%alloc(X47, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X46, &
         wf%n_v*wf%n_o, &
         zero, &
         X47, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X46)
      call add_1423_to_1234(one, X47, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rs_vovo, X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X49, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_o, &
         X48, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X49, &
         wf%n_o)
!
      call mem%dealloc(X48)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X49, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X50, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X51, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         X50, &
         wf%n_v*wf%n_o**2, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X51, &
         wf%n_v)
!
      call mem%dealloc(X50)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X51, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_2143(L_ovov, X52, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X53, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X52, &
         wf%n_v*wf%n_o, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X53, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X52)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X53, &
         wf%n_v*wf%n_o, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X53)
      call mem%alloc(X54, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X54, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X55, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X54, &
         wf%n_v*wf%n_o, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X55, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X54)
      call mem%alloc(X56, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X55, &
         wf%n_v*wf%n_o, &
         g_ovov, &
         wf%n_v*wf%n_o, &
         zero, &
         X56, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X55)
      call add_1423_to_1234(one, X56, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X58, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X58, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X59, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         half, &
         X57, &
         wf%n_v**2, &
         X58, &
         wf%n_o**2, &
         zero, &
         X59, &
         wf%n_o**2)
!
      call mem%dealloc(X57)
      call mem%dealloc(X58)
      call mem%alloc(X60, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X60, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X61, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X60, &
         wf%n_v**2, &
         X59, &
         wf%n_o**2, &
         zero, &
         X61, &
         wf%n_v**2)
!
      call mem%dealloc(X59)
      call mem%dealloc(X60)
      call add_1324_to_1234(one, X61, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X61)
      call mem%alloc(X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Ls_vovo, X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X63, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X63, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X64, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X62, &
         wf%n_v*wf%n_o, &
         X63, &
         wf%n_v*wf%n_o, &
         zero, &
         X64, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X62)
      call mem%dealloc(X63)
      call mem%alloc(X65, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X65, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X66, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X64, &
         wf%n_v*wf%n_o, &
         X65, &
         wf%n_v*wf%n_o, &
         zero, &
         X66, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X65)
      call mem%dealloc(X64)
      call add_1423_to_1234(one, X66, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X66)
      call mem%alloc(X67, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X68, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         half, &
         X67, &
         wf%n_v**2, &
         X68, &
         wf%n_v**2, &
         zero, &
         X69, &
         wf%n_o**2)
!
      call mem%dealloc(X67)
      call mem%dealloc(X68)
      call mem%alloc(X70, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X70, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X71, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X70, &
         wf%n_o**2, &
         X69, &
         wf%n_o**2, &
         zero, &
         X71, &
         wf%n_v**2)
!
      call mem%dealloc(X69)
      call mem%dealloc(X70)
      call add_1324_to_1234(one, X71, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X71)
      call mem%alloc(X72, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X72, &
         wf%n_o)
!
      call mem%alloc(X73, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X73, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X74, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X72, X74, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X72)
      call mem%alloc(X75, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X73, X75, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X73)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X74, &
         wf%n_o**2, &
         X75, &
         wf%n_o**2, &
         one, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X74)
      call mem%dealloc(X75)
      call mem%alloc(X76, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X76, &
         wf%n_o)
!
      call mem%alloc(X77, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X77, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X78, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X77, X78, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X77)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X78, &
         wf%n_v*wf%n_o, &
         X76, &
         wf%n_o**2, &
         one, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X76)
      call mem%dealloc(X78)
      call mem%alloc(X79, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X79, wf%n_v, wf%n_o)
      call mem%alloc(X80, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X79, 1, &
         zero, &
         X80, 1)
!
      call mem%dealloc(X79)
      call mem%alloc(X81, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X80, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X81, &
         wf%n_o)
!
      call mem%dealloc(X80)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X81, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X81)
      call mem%alloc(X82, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X82, wf%n_v, wf%n_o)
      call mem%alloc(X83, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X82, 1, &
         zero, &
         X83, 1)
!
      call mem%dealloc(X82)
      call mem%alloc(X84, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X83, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X84, &
         wf%n_o)
!
      call mem%dealloc(X83)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X84, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X84)
      call mem%alloc(X85, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X85, wf%n_v, wf%n_o)
      call mem%alloc(X86, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X85, 1, &
         zero, &
         X86, 1)
!
      call mem%dealloc(X85)
      call mem%alloc(X87, wf%n_v, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         s_vo, &
         wf%n_v, &
         X86, &
         wf%n_o, &
         zero, &
         X87, &
         wf%n_v)
!
      call mem%dealloc(X86)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X87, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X87)
      call mem%alloc(X88, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X88, wf%n_v, wf%n_o)
      call mem%alloc(X89, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X88, 1, &
         zero, &
         X89, 1)
!
      call mem%dealloc(X88)
      call mem%alloc(X90, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X89, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X90, &
         wf%n_v)
!
      call mem%dealloc(X89)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X90, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X90)
      call mem%alloc(X91, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X91, &
         wf%n_o)
!
      call mem%alloc(X92, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X92, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X93, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X91, &
         wf%n_o**2, &
         X92, &
         wf%n_v*wf%n_o, &
         zero, &
         X93, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X91)
      call mem%dealloc(X92)
      call add_1423_to_1234(one, X93, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X93)
      call mem%alloc(X94, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X94, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X95, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X95, &
         wf%n_o)
!
      call mem%alloc(X96, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X95, &
         wf%n_o**2, &
         X94, &
         wf%n_v*wf%n_o, &
         zero, &
         X96, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X94)
      call mem%dealloc(X95)
      call add_1423_to_1234(one, X96, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X96)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X97, &
         wf%n_o)
!
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X97, X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X98, &
         wf%n_o**3, &
         zero, &
         X99, &
         wf%n_o)
!
      call mem%dealloc(X98)
      call mem%alloc(X100, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X100, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X101, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X100, &
         wf%n_v**2, &
         X99, &
         wf%n_o**2, &
         zero, &
         X101, &
         wf%n_v**2)
!
      call mem%dealloc(X99)
      call mem%dealloc(X100)
      call add_1324_to_1234(one, X101, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X101)
      call mem%alloc(X102, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X102, &
         wf%n_o)
!
      call mem%alloc(X103, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X103, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X104, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X102, &
         wf%n_o**2, &
         X103, &
         wf%n_v*wf%n_o, &
         zero, &
         X104, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X102)
      call mem%dealloc(X103)
      call add_1423_to_1234(one, X104, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X104)
      call mem%alloc(X105, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X105, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X106, &
         wf%n_o)
!
      call mem%alloc(X107, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X106, &
         wf%n_o**2, &
         X105, &
         wf%n_v*wf%n_o, &
         zero, &
         X107, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X105)
      call mem%dealloc(X106)
      call add_1423_to_1234(one, X107, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X107)
      call mem%alloc(X108, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X108, &
         wf%n_o)
!
      call mem%alloc(X109, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X108, X109, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X108)
      call mem%alloc(X110, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X109, &
         wf%n_o**3, &
         zero, &
         X110, &
         wf%n_o)
!
      call mem%dealloc(X109)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X111, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X112, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X111, &
         wf%n_o**2, &
         X110, &
         wf%n_o**2, &
         zero, &
         X112, &
         wf%n_v**2)
!
      call mem%dealloc(X110)
      call mem%dealloc(X111)
      call add_1342_to_1234(one, X112, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X112)
!
   end subroutine F_matrix_transformation_doubles_qed_ccsd

