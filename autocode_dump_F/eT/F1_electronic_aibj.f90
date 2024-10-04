   subroutine F_matrix_transformation_electronic_doubles_photon_qed_ccsd(wf, rho, F_ov, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, Ls_vo, Ls_vovo, Rt_vo, Rt_vovo, Ru_vovo, g_ooov, g_ovoo, g_ovov, g_ovvv, g_vvov)
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
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rt_vo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rt_vovo, Ru_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X5, X7, X8, X9, X10, X11, X17, X18, X19, X20, X41, X43, X49, X51
      real(dp), dimension(:,:,:,:), allocatable :: X4, X6, X12, X13, X14, X15, X16, X21, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X42, X44, X45, X46, X47, X48, X50, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71
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
         Rt_vo, &
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
         Rt_vo, &
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
         Rt_vo, &
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
         Rt_vo, &
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
      call sort_to_21(Rt_vo, X7, wf%n_v, wf%n_o)
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
      call sort_to_21(Rt_vo, X10, wf%n_v, wf%n_o)
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
         Rt_vo, &
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
         Rt_vo, &
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
      call mem%dealloc(X15)
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X17, wf%n_v, wf%n_o)
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
      call sort_to_21(Rt_vo, X19, wf%n_v, wf%n_o)
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
         Rt_vo, &
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
         Rt_vo, &
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
         Rt_vo, &
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
         Rt_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_o)
!
      call mem%alloc(X30, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X29, X30, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X29)
      call mem%alloc(X31, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(g_ooov, X31, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X32, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X30, &
         wf%n_o**2, &
         X31, &
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
         Rt_vo, &
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
         Rt_vo, &
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
         Rt_vovo, &
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
         Rt_vovo, &
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
      call mem%alloc(X45, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X45, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X46, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X45, X46, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X45)
      call mem%alloc(X47, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X46, &
         wf%n_v*wf%n_o, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         zero, &
         X47, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X46)
      call add_1423_to_1234(one, X47, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rt_vovo, X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         Rt_vovo, &
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
         Ru_vovo, &
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
      call sort_to_1423(Rt_vovo, X54, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X55, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X54, &
         wf%n_v*wf%n_o, &
         g_ovov, &
         wf%n_v*wf%n_o, &
         zero, &
         X55, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X54)
      call mem%alloc(X56, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X55, &
         wf%n_v*wf%n_o, &
         zero, &
         X56, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X55)
      call add_1423_to_1234(one, X56, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
      call mem%alloc(X63, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rt_vovo, X63, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X64, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
      call mem%alloc(X65, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X65, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
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
      call mem%dealloc(X64)
      call mem%dealloc(X65)
      call add_1423_to_1234(one, X66, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X66)
      call mem%alloc(X67, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X68, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
!
   end subroutine F_matrix_transformation_electronic_doubles_photon_qed_ccsd

