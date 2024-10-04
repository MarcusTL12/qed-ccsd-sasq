   subroutine F_matrix_transformation_electronic_singles_photon_qed_ccsd(wf, rho, F_ov, L_J_vv, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, Ls1, Ls_vo, Ls_vovo, Rt_vo, Rt_vovo, Ru_vovo, g_oooo, g_ooov, g_oovv, g_ovoo, g_ovov, g_ovvo, g_ovvv, g_voov, g_vvoo, g_vvov, t_vovo, u_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: rho
!
      real(dp), intent(in) :: Ls1
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rt_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: L_J_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_oovv
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_o), intent(in) :: g_ovvo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rt_vovo, Ru_vovo, t_vovo, u_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_o), intent(in) :: g_vvoo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X16, X18, X19, X20, X21, X35, X36, X37, X38, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76
      real(dp), dimension(:,:,:), allocatable :: X33, X34
      real(dp), dimension(:,:,:,:), allocatable :: X15, X17, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106
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
         F_ov, &
         wf%n_o, &
         X2, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X3, wf%n_v, wf%n_o)
      call mem%alloc(X4, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Ls1, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X3, 1, &
         zero, &
         X4, 1)
!
      call mem%dealloc(X3)
      call add_21_to_12(one, X4, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_v)
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
         zero, &
         X5, &
         wf%n_v)
!
      call mem%alloc(X6, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X5, 1, &
         zero, &
         X6, 1)
!
      call mem%dealloc(X5)
      call add_21_to_12(one, X6, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_o)
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
         zero, &
         X7, &
         wf%n_o)
!
      call mem%alloc(X8, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X7, 1, &
         zero, &
         X8, 1)
!
      call mem%dealloc(X7)
      call add_21_to_12(one, X8, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X9, wf%n_v, wf%n_o)
      call mem%alloc(X10, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ooov, &
         wf%n_o**2, &
         X9, 1, &
         zero, &
         X10, 1)
!
      call mem%dealloc(X9)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X10, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X11, wf%n_v, wf%n_o)
      call mem%alloc(X12, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         L_vvov, &
         wf%n_v**2, &
         X11, 1, &
         zero, &
         X12, 1)
!
      call mem%dealloc(X11)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X12, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_v, wf%n_v)
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
         X13, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X13, &
         wf%n_v, &
         F_ov, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_o)
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
         X14, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         F_ov, &
         wf%n_o, &
         X14, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rt_vovo, X15, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X16, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_o, &
         X15, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X16, &
         wf%n_o)
!
      call mem%dealloc(X15)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X16, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X17, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X18, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         X17, &
         wf%n_v*wf%n_o**2, &
         Rt_vovo, &
         wf%n_v, &
         zero, &
         X18, &
         wf%n_v)
!
      call mem%dealloc(X17)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X18, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X19, 1)
!
      call mem%alloc(X20, wf%n_o, wf%n_v)
      call sort_to_21(X19, X20, wf%n_v, wf%n_o)
      call mem%dealloc(X19)
      call mem%alloc(X21, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X20, 1, &
         zero, &
         X21, 1)
!
      call mem%dealloc(X20)
      call add_21_to_12(one, X21, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X22, &
         wf%n_o)
!
      call mem%alloc(X23, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X22, X23, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X22)
      call mem%alloc(X24, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1342(g_oooo, X24, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X23, &
         wf%n_o**3, &
         X24, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X23)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovvo, X25, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%alloc(X26, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         X25, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X26, &
         wf%n_o)
!
      call mem%dealloc(X25)
      call mem%alloc(X27, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X26, X27, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X26)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X27, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         g_oovv, &
         wf%n_v*wf%n_o**2, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X28, &
         wf%n_v*wf%n_o**2)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X28, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X29, &
         wf%n_v*wf%n_o**2)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_voov, &
         wf%n_v*wf%n_o**2, &
         X29, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X30, &
         wf%n_o)
!
      call mem%alloc(X31, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_3124(X30, X31, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X30)
      call mem%alloc(X32, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_vvoo, X32, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X32, &
         wf%n_v*wf%n_o**2, &
         X31, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X31)
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X33, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X34, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X33, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X34, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X33)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         X34, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_v, wf%n_v)
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
         zero, &
         X35, &
         wf%n_v)
!
      call mem%alloc(X36, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X35, 1, &
         zero, &
         X36, 1)
!
      call mem%dealloc(X35)
      call add_21_to_12(one, X36, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_o, wf%n_o)
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
         zero, &
         X37, &
         wf%n_o)
!
      call mem%alloc(X38, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X37, 1, &
         zero, &
         X38, 1)
!
      call mem%dealloc(X37)
      call add_21_to_12(one, X38, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X38)
      call mem%alloc(X39, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(L_ooov, X39, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X40, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X39, &
         wf%n_o**2, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X40, &
         wf%n_o**2)
!
      call mem%dealloc(X39)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X40, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X41, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X42, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_vvov, X42, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X43, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X41, X43, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X41)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X42, &
         wf%n_v**2*wf%n_o, &
         X43, &
         wf%n_v**2*wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X42)
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X44, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X45, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X45, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X46, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X44, &
         wf%n_o**2, &
         X45, &
         wf%n_v*wf%n_o, &
         zero, &
         X46, &
         wf%n_o**2)
!
      call mem%dealloc(X44)
      call mem%dealloc(X45)
      call mem%alloc(X47, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X46, X47, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X46)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X47, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X49, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         g_ooov, &
         wf%n_o**2, &
         X48, &
         wf%n_v*wf%n_o, &
         zero, &
         X49, &
         wf%n_o**2)
!
      call mem%dealloc(X48)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X49, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X50, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X51, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovvv, X51, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X52, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         -one, &
         X50, &
         wf%n_v**2, &
         X51, &
         wf%n_v*wf%n_o, &
         zero, &
         X52, &
         wf%n_o**2)
!
      call mem%dealloc(X50)
      call mem%dealloc(X51)
      call mem%alloc(X53, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1423(X52, X53, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X52)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X53, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X53)
      call mem%alloc(X54, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(Ls_vovo, X54, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X55, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X55, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X56, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X55, &
         wf%n_v**2, &
         X54, &
         wf%n_v**2, &
         zero, &
         X56, &
         wf%n_o**2)
!
      call mem%dealloc(X54)
      call mem%dealloc(X55)
      call mem%alloc(X57, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X57, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X57, &
         wf%n_o**3, &
         X56, &
         wf%n_o**3, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X56)
      call mem%dealloc(X57)
      call mem%alloc(X58, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X58, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X59, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rt_vovo, X59, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X60, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X58, &
         wf%n_v*wf%n_o, &
         X59, &
         wf%n_v*wf%n_o, &
         zero, &
         X60, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X58)
      call mem%dealloc(X59)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X60, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X60)
      call mem%alloc(X61, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X61, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X62, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X61, &
         wf%n_v*wf%n_o, &
         zero, &
         X62, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X61)
      call mem%alloc(X63, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X62, X63, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X62)
      call mem%alloc(X64, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X64, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X64, &
         wf%n_v**2*wf%n_o, &
         X63, &
         wf%n_v**2*wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X63)
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_o, wf%n_o)
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
         X65, &
         wf%n_o)
!
      call mem%alloc(X66, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X65, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X66, &
         wf%n_o)
!
      call mem%dealloc(X65)
      call mem%alloc(X67, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X66, 1, &
         zero, &
         X67, 1)
!
      call mem%dealloc(X66)
      call add_21_to_12(one, X67, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X67)
      call mem%alloc(X68, wf%n_v, wf%n_v)
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
         X68, &
         wf%n_v)
!
      call mem%alloc(X69, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X68, &
         wf%n_v, &
         zero, &
         X69, &
         wf%n_o)
!
      call mem%dealloc(X68)
      call mem%alloc(X70, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X69, 1, &
         zero, &
         X70, 1)
!
      call mem%dealloc(X69)
      call add_21_to_12(one, X70, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X71, wf%n_v, wf%n_o)
      call mem%alloc(X72, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X71, 1, &
         zero, &
         X72, 1)
!
      call mem%dealloc(X71)
      call mem%alloc(X73, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X73, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X73, &
         wf%n_v, &
         X72, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X72)
      call mem%dealloc(X73)
      call mem%alloc(X74, wf%n_o, wf%n_o)
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
         X74, &
         wf%n_o)
!
      call mem%alloc(X75, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X75, wf%n_v, wf%n_o)
      call mem%alloc(X76, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X75, 1, &
         zero, &
         X76, 1)
!
      call mem%dealloc(X75)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X76, &
         wf%n_o, &
         X74, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X74)
      call mem%dealloc(X76)
      call mem%alloc(X77, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X77, &
         wf%n_o)
!
      call mem%alloc(X78, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X78, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X79, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X79, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X80, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X78, &
         wf%n_o**2, &
         X79, &
         wf%n_v**2, &
         zero, &
         X80, &
         wf%n_o**2)
!
      call mem%dealloc(X78)
      call mem%dealloc(X79)
      call mem%alloc(X81, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X77, X81, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X77)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X81, &
         wf%n_o**3, &
         X80, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X81)
      call mem%dealloc(X80)
      call mem%alloc(X82, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X82, &
         wf%n_o)
!
      call mem%alloc(X83, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X82, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X83, &
         wf%n_o**2)
!
      call mem%dealloc(X82)
      call mem%alloc(X84, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X83, X84, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X83)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X84, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X84)
      call mem%alloc(X85, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X85, &
         wf%n_o)
!
      call mem%alloc(X86, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X86, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X87, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X86, &
         wf%n_v*wf%n_o, &
         X85, &
         wf%n_o**2, &
         zero, &
         X87, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X85)
      call mem%dealloc(X86)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X87, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X87)
      call mem%alloc(X88, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X88, &
         wf%n_o)
!
      call mem%alloc(X89, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X88, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X89, &
         wf%n_o**2)
!
      call mem%dealloc(X88)
      call mem%alloc(X90, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X89, X90, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X89)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X90, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X90)
      call mem%alloc(X91, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X91, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X92, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X92, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X93, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X91, &
         wf%n_v*wf%n_o, &
         X92, &
         wf%n_v*wf%n_o, &
         zero, &
         X93, &
         wf%n_o**2)
!
      call mem%dealloc(X91)
      call mem%dealloc(X92)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X93, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X93)
      call mem%alloc(X94, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X94, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X95, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X95, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X96, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X94, &
         wf%n_v**2, &
         X95, &
         wf%n_v**2, &
         zero, &
         X96, &
         wf%n_o**2)
!
      call mem%dealloc(X94)
      call mem%dealloc(X95)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X97, &
         wf%n_o)
!
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X97, X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X97)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X98, &
         wf%n_o**3, &
         X96, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X96)
      call mem%dealloc(X98)
      call mem%alloc(X99, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X99, &
         wf%n_o)
!
      call mem%alloc(X100, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X99, X100, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X99)
      call mem%alloc(X101, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X100, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X101, &
         wf%n_o**2)
!
      call mem%dealloc(X100)
      call mem%alloc(X102, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X101, X102, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X101)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X102, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X103, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X104, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X103, X104, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X103)
      call mem%alloc(X105, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X104, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X105, &
         wf%n_o**2)
!
      call mem%dealloc(X104)
      call mem%alloc(X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X105, X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X105)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X106, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X106)
!
   end subroutine F_matrix_transformation_electronic_singles_photon_qed_ccsd

