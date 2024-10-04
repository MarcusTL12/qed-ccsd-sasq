   subroutine F_matrix_transformation_singles_qed_ccsd(wf, rho, F_ov, L_J_vv, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, Ls1, Ls_vo, Ls_vovo, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Rv_vovo, g_oooo, g_ooov, g_oovv, g_ovoo, g_ovov, g_ovvo, g_ovvv, g_voov, g_vvoo, g_vvov, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
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
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: L_J_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_oovv
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_o), intent(in) :: g_ovvo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_o), intent(in) :: g_vvoo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X16, X18, X19, X20, X21, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X131, X132, X133, X134, X135, X136, X137, X138, X139, X140, X141, X142, X143, X144, X145, X146, X147, X148, X149, X150, X151, X152, X153, X154, X155, X156, X157, X158, X159, X160, X164, X165, X166, X170, X171, X172
      real(dp), dimension(:,:,:), allocatable :: X33, X34
      real(dp), dimension(:,:,:,:), allocatable :: X15, X17, X22, X23, X24, X25, X26, X27, X28, X29, X30, X31, X32, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112, X113, X114, X115, X116, X117, X118, X119, X120, X121, X122, X123, X124, X125, X126, X127, X128, X129, X130, X161, X162, X163, X167, X168, X169, X173, X174, X175, X176, X177, X178, X179, X180, X181, X182, X183, X184, X185, X186, X187, X188, X189, X190, X191, X192, X193, X194, X195, X196, X197, X198, X199, X200, X201, X202, X203, X204, X205, X206, X207, X208, X209, X210, X211, X212, X213, X214, X215, X216, X217, X218, X219, X220, X221, X222, X223, X224, X225, X226, X227, X228, X229, X230, X231, X232, X233, X234, X235, X236, X237, X238, X239, X240, X241, X242, X243, X244, X245, X246, X247, X248, X249, X250, X251, X252
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
         Rs_vo, &
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
      call sort_to_21(Rs_vo, X3, wf%n_v, wf%n_o)
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
         Rs_vo, &
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
         Rs_vo, &
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
      call sort_to_21(Rs_vo, X9, wf%n_v, wf%n_o)
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
      call sort_to_21(Rs_vo, X11, wf%n_v, wf%n_o)
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
         Rs_vovo, &
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
         Rs_vovo, &
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
      call sort_to_1432(Rs_vovo, X15, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         Rs_vovo, &
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
         Rv_vovo, &
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
      call mem%alloc(X22, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1243(g_oooo, X22, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%alloc(X23, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X22, &
         wf%n_o**3, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X23, &
         wf%n_o**3)
!
      call mem%dealloc(X22)
      call mem%alloc(X24, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X23, X24, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X23)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X24, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
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
         Rs_vo, &
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
         Rs_vo, &
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
         Rs_vo, &
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
      call mem%alloc(X30, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X30, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X31, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2413(X30, X31, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         Rs_vo, &
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
      call mem%alloc(X35, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X35, &
         wf%n_o)
!
      call mem%alloc(X36, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X35, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X36, &
         wf%n_o)
!
      call mem%dealloc(X35)
      call mem%alloc(X37, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X36, 1, &
         zero, &
         X37, 1)
!
      call mem%dealloc(X36)
      call add_21_to_12(one, X37, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_o, wf%n_o)
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
      call mem%alloc(X40, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X39, 1, &
         zero, &
         X40, 1)
!
      call mem%dealloc(X39)
      call add_21_to_12(one, X40, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X41, wf%n_v, wf%n_o)
      call mem%alloc(X42, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X41, 1, &
         zero, &
         X42, 1)
!
      call mem%dealloc(X41)
      call mem%alloc(X43, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X42, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X43, &
         wf%n_o)
!
      call mem%dealloc(X42)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X43, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X44, wf%n_v, wf%n_o)
      call mem%alloc(X45, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X44, 1, &
         zero, &
         X45, 1)
!
      call mem%dealloc(X44)
      call mem%alloc(X46, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X45, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X46, &
         wf%n_o)
!
      call mem%dealloc(X45)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X46, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X47, &
         wf%n_o)
!
      call mem%alloc(X48, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X48, wf%n_v, wf%n_o)
      call mem%alloc(X49, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X48, 1, &
         zero, &
         X49, 1)
!
      call mem%dealloc(X48)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X49, &
         wf%n_o, &
         X47, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%n_o, wf%n_o)
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
         X50, &
         wf%n_o)
!
      call mem%alloc(X51, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X51, wf%n_v, wf%n_o)
      call mem%alloc(X52, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X51, 1, &
         zero, &
         X52, 1)
!
      call mem%dealloc(X51)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X52, &
         wf%n_o, &
         X50, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X50)
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X53, &
         wf%n_v)
!
      call mem%alloc(X54, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X53, 1, &
         zero, &
         X54, 1)
!
      call mem%dealloc(X53)
      call add_21_to_12(one, X54, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X54)
      call mem%alloc(X55, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X55, &
         wf%n_o)
!
      call mem%alloc(X56, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X55, 1, &
         zero, &
         X56, 1)
!
      call mem%dealloc(X55)
      call add_21_to_12(one, X56, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(L_ooov, X57, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X58, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X57, &
         wf%n_o**2, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X58, &
         wf%n_o**2)
!
      call mem%dealloc(X57)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X58, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X58)
      call mem%alloc(X59, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X59, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X60, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_vvov, X60, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X61, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X59, X61, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X59)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X60, &
         wf%n_v**2*wf%n_o, &
         X61, &
         wf%n_v**2*wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X60)
      call mem%dealloc(X61)
      call mem%alloc(X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rs_vovo, X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X63, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovoo, X63, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X64, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X63, &
         wf%n_o**2, &
         X62, &
         wf%n_v*wf%n_o, &
         zero, &
         X64, &
         wf%n_o**2)
!
      call mem%dealloc(X62)
      call mem%dealloc(X63)
      call mem%alloc(X65, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X64, X65, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X64)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X65, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X65)
      call mem%alloc(X66, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X67, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         g_ooov, &
         wf%n_o**2, &
         X66, &
         wf%n_v*wf%n_o, &
         zero, &
         X67, &
         wf%n_o**2)
!
      call mem%dealloc(X66)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X67, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X67)
      call mem%alloc(X68, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovvv, X69, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X70, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         -one, &
         X68, &
         wf%n_v**2, &
         X69, &
         wf%n_v*wf%n_o, &
         zero, &
         X70, &
         wf%n_o**2)
!
      call mem%dealloc(X68)
      call mem%dealloc(X69)
      call mem%alloc(X71, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1423(X70, X71, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X70)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X71, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X71)
      call mem%alloc(X72, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(Ls_vovo, X72, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X73, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X73, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X74, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X73, &
         wf%n_v**2, &
         X72, &
         wf%n_v**2, &
         zero, &
         X74, &
         wf%n_o**2)
!
      call mem%dealloc(X72)
      call mem%dealloc(X73)
      call mem%alloc(X75, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X75, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X75, &
         wf%n_o**3, &
         X74, &
         wf%n_o**3, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X74)
      call mem%dealloc(X75)
      call mem%alloc(X76, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X76, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X77, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(Rs_vovo, X77, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X78, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X76, &
         wf%n_v*wf%n_o, &
         X77, &
         wf%n_v*wf%n_o, &
         zero, &
         X78, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X76)
      call mem%dealloc(X77)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X78, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X78)
      call mem%alloc(X79, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X79, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X80, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         X79, &
         wf%n_v*wf%n_o, &
         zero, &
         X80, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X79)
      call mem%alloc(X81, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X80, X81, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X80)
      call mem%alloc(X82, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X82, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X82, &
         wf%n_v**2*wf%n_o, &
         X81, &
         wf%n_v**2*wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X81)
      call mem%dealloc(X82)
      call mem%alloc(X83, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         g_ooov, &
         wf%n_o**3, &
         s_vo, &
         wf%n_v, &
         zero, &
         X83, &
         wf%n_o**3)
!
      call mem%alloc(X84, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1243(X83, X84, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X83)
      call mem%alloc(X85, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X84, &
         wf%n_o**3, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X85, &
         wf%n_o**3)
!
      call mem%dealloc(X84)
      call mem%alloc(X86, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X85, X86, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X85)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X86, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X86)
      call mem%alloc(X87, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X87, &
         wf%n_o)
!
      call mem%alloc(X88, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X88, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X89, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X88, &
         wf%n_o**3, &
         s_vo, &
         wf%n_v, &
         zero, &
         X89, &
         wf%n_o**3)
!
      call mem%dealloc(X88)
      call mem%alloc(X90, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X87, X90, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X87)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X90, &
         wf%n_o**3, &
         X89, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X90)
      call mem%dealloc(X89)
      call mem%alloc(X91, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1342(g_ovvv, X91, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X92, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         X91, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X92, &
         wf%n_o)
!
      call mem%dealloc(X91)
      call mem%alloc(X93, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X92, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X93, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X92)
      call mem%alloc(X94, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X93, X94, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X93)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X94, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X94)
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
      call mem%alloc(X96, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X96, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X96, &
         wf%n_o**3, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X97, &
         wf%n_o**3)
!
      call mem%dealloc(X96)
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X95, X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X95)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X98, &
         wf%n_o**3, &
         X97, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X98)
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X99, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X100, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         g_ooov, &
         wf%n_o**3, &
         zero, &
         X100, &
         wf%n_o)
!
      call mem%alloc(X101, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X99, X101, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X99)
      call mem%alloc(X102, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1342(X100, X102, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X100)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X101, &
         wf%n_o**3, &
         X102, &
         wf%n_o**3, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X101)
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
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
         X103, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X104, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X103, X104, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X103)
      call mem%alloc(X105, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X104, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X105, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X104)
      call mem%alloc(X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X105, X106, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X105)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X106, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X106)
      call mem%alloc(X107, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X107, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X108, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1243(X107, X108, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X107)
      call mem%alloc(X109, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X108, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X109, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X108)
      call mem%alloc(X110, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1243(X109, X110, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%dealloc(X109)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X110, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X110)
      call mem%alloc(X111, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X111, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X112, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X111, X112, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X111)
      call mem%alloc(X113, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X112, &
         wf%n_o**3, &
         s_vo, &
         wf%n_v, &
         zero, &
         X113, &
         wf%n_o**3)
!
      call mem%dealloc(X112)
      call mem%alloc(X114, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X114, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X114, &
         wf%n_o**3, &
         X113, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X113)
      call mem%dealloc(X114)
      call mem%alloc(X115, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X115, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X116, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X116, &
         wf%n_o)
!
      call mem%alloc(X117, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1243(X115, X117, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X115)
      call mem%alloc(X118, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1243(X116, X118, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X116)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X118, &
         wf%n_v*wf%n_o**2, &
         X117, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X117)
      call mem%dealloc(X118)
      call mem%alloc(X119, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X119, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X120, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X119, X120, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X119)
      call mem%alloc(X121, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X120, &
         wf%n_o**3, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X121, &
         wf%n_o**3)
!
      call mem%dealloc(X120)
      call mem%alloc(X122, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1423(X121, X122, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X121)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         g_ooov, &
         wf%n_o**3, &
         X122, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X122)
      call mem%alloc(X123, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X123, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X124, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X123, X124, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X123)
      call mem%alloc(X125, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X124, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X125, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X124)
      call mem%alloc(X126, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X126, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X126, &
         wf%n_v**2*wf%n_o, &
         X125, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X125)
      call mem%dealloc(X126)
      call mem%alloc(X127, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X127, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X128, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         X127, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X128, &
         wf%n_o)
!
      call mem%dealloc(X127)
      call mem%alloc(X129, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X129, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X130, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X129, X130, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X129)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X128, &
         wf%n_v*wf%n_o**2, &
         X130, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X128)
      call mem%dealloc(X130)
      call mem%alloc(X131, wf%n_o, wf%n_o)
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
         X131, &
         wf%n_o)
!
      call mem%alloc(X132, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X131, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X132, &
         wf%n_o)
!
      call mem%dealloc(X131)
      call mem%alloc(X133, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X132, 1, &
         zero, &
         X133, 1)
!
      call mem%dealloc(X132)
      call add_21_to_12(one, X133, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X133)
      call mem%alloc(X134, wf%n_v, wf%n_v)
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
         X134, &
         wf%n_v)
!
      call mem%alloc(X135, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         X134, &
         wf%n_v, &
         zero, &
         X135, &
         wf%n_o)
!
      call mem%dealloc(X134)
      call mem%alloc(X136, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X135, 1, &
         zero, &
         X136, 1)
!
      call mem%dealloc(X135)
      call add_21_to_12(one, X136, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X136)
      call mem%alloc(X137, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X137, wf%n_v, wf%n_o)
      call mem%alloc(X138, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X137, 1, &
         zero, &
         X138, 1)
!
      call mem%dealloc(X137)
      call mem%alloc(X139, wf%n_v, wf%n_v)
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
         X139, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X139, &
         wf%n_v, &
         X138, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X138)
      call mem%dealloc(X139)
      call mem%alloc(X140, wf%n_o, wf%n_o)
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
         X140, &
         wf%n_o)
!
      call mem%alloc(X141, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X141, wf%n_v, wf%n_o)
      call mem%alloc(X142, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X141, 1, &
         zero, &
         X142, 1)
!
      call mem%dealloc(X141)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X142, &
         wf%n_o, &
         X140, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X140)
      call mem%dealloc(X142)
      call mem%alloc(X143, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X143, &
         wf%n_o)
!
      call mem%alloc(X144, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X143, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X144, &
         wf%n_o)
!
      call mem%dealloc(X143)
      call mem%alloc(X145, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X144, 1, &
         zero, &
         X145, 1)
!
      call mem%dealloc(X144)
      call add_21_to_12(one, X145, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X145)
      call mem%alloc(X146, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X146, &
         wf%n_v)
!
      call mem%alloc(X147, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X146, &
         wf%n_v, &
         zero, &
         X147, &
         wf%n_o)
!
      call mem%dealloc(X146)
      call mem%alloc(X148, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X147, 1, &
         zero, &
         X148, 1)
!
      call mem%dealloc(X147)
      call add_21_to_12(one, X148, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X148)
      call mem%alloc(X149, wf%n_v, wf%n_v)
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
         X149, &
         wf%n_v)
!
      call mem%alloc(X150, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X149, &
         wf%n_v, &
         zero, &
         X150, &
         wf%n_o)
!
      call mem%dealloc(X149)
      call mem%alloc(X151, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X150, 1, &
         zero, &
         X151, 1)
!
      call mem%dealloc(X150)
      call add_21_to_12(one, X151, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X151)
      call mem%alloc(X152, wf%n_o, wf%n_o)
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
         X152, &
         wf%n_o)
!
      call mem%alloc(X153, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X152, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X153, &
         wf%n_o)
!
      call mem%dealloc(X152)
      call mem%alloc(X154, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X153, 1, &
         zero, &
         X154, 1)
!
      call mem%dealloc(X153)
      call add_21_to_12(one, X154, rho, wf%n_v, wf%n_o)
      call mem%dealloc(X154)
      call mem%alloc(X155, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X155, wf%n_v, wf%n_o)
      call mem%alloc(X156, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X155, 1, &
         zero, &
         X156, 1)
!
      call mem%dealloc(X155)
      call mem%alloc(X157, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         s_vovo, &
         wf%n_v, &
         zero, &
         X157, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X157, &
         wf%n_v, &
         X156, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X156)
      call mem%dealloc(X157)
      call mem%alloc(X158, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X158, wf%n_v, wf%n_o)
      call mem%alloc(X159, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X158, 1, &
         zero, &
         X159, 1)
!
      call mem%dealloc(X158)
      call mem%alloc(X160, wf%n_v, wf%n_v)
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
         X160, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X160, &
         wf%n_v, &
         X159, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X159)
      call mem%dealloc(X160)
      call mem%alloc(X161, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         s_vo, &
         wf%n_v, &
         zero, &
         X161, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X162, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X161, X162, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X161)
      call mem%alloc(X163, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X162, &
         wf%n_v*wf%n_o, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X163, &
         wf%n_o**2)
!
      call mem%dealloc(X162)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X163, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X163)
      call mem%alloc(X164, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X164, &
         wf%n_o)
!
      call mem%alloc(X165, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X165, wf%n_v, wf%n_o)
      call mem%alloc(X166, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X165, 1, &
         zero, &
         X166, 1)
!
      call mem%dealloc(X165)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X166, &
         wf%n_o, &
         X164, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X164)
      call mem%dealloc(X166)
      call mem%alloc(X167, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         s_vo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X167, &
         wf%n_o)
!
      call mem%alloc(X168, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X167, &
         wf%n_o**2, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X168, &
         wf%n_o**2)
!
      call mem%dealloc(X167)
      call mem%alloc(X169, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_4312(X168, X169, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X168)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         X169, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X169)
      call mem%alloc(X170, wf%n_o, wf%n_o)
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
         X170, &
         wf%n_o)
!
      call mem%alloc(X171, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X171, wf%n_v, wf%n_o)
      call mem%alloc(X172, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X171, 1, &
         zero, &
         X172, 1)
!
      call mem%dealloc(X171)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X172, &
         wf%n_o, &
         X170, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X170)
      call mem%dealloc(X172)
      call mem%alloc(X173, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X173, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X174, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X174, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X175, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X174, &
         wf%n_v**2, &
         X173, &
         wf%n_o**2, &
         zero, &
         X175, &
         wf%n_o**2)
!
      call mem%dealloc(X173)
      call mem%dealloc(X174)
      call mem%alloc(X176, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X175, &
         wf%n_o**3, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X176, &
         wf%n_o**3)
!
      call mem%dealloc(X175)
      call mem%alloc(X177, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1423(X176, X177, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X176)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X177, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X177)
      call mem%alloc(X178, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X178, &
         wf%n_o)
!
      call mem%alloc(X179, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X178, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X179, &
         wf%n_o**2)
!
      call mem%dealloc(X178)
      call mem%alloc(X180, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X179, X180, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X179)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X180, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X180)
      call mem%alloc(X181, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X181, &
         wf%n_o)
!
      call mem%alloc(X182, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X182, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X183, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X182, &
         wf%n_v*wf%n_o, &
         X181, &
         wf%n_o**2, &
         zero, &
         X183, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X181)
      call mem%dealloc(X182)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X183, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X183)
      call mem%alloc(X184, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X184, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X185, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X184, X185, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X184)
      call mem%alloc(X186, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X185, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X186, &
         wf%n_o**2)
!
      call mem%dealloc(X185)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X186, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X186)
      call mem%alloc(X187, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X187, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X188, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X188, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X189, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X187, &
         wf%n_v*wf%n_o, &
         X188, &
         wf%n_v*wf%n_o, &
         zero, &
         X189, &
         wf%n_o**2)
!
      call mem%dealloc(X187)
      call mem%dealloc(X188)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X189, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X189)
      call mem%alloc(X190, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(Ls_vovo, X190, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X191, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X191, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X192, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X190, &
         wf%n_v**2, &
         X191, &
         wf%n_v**2, &
         zero, &
         X192, &
         wf%n_o**2)
!
      call mem%dealloc(X190)
      call mem%dealloc(X191)
      call mem%alloc(X193, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X192, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X193, &
         wf%n_o**3)
!
      call mem%dealloc(X192)
      call mem%alloc(X194, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X193, X194, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X193)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X194, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X194)
      call mem%alloc(X195, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X195, &
         wf%n_o)
!
      call mem%alloc(X196, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X195, X196, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X195)
      call mem%alloc(X197, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X196, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X197, &
         wf%n_o**2)
!
      call mem%dealloc(X196)
      call mem%alloc(X198, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X197, X198, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X197)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X198, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X198)
      call mem%alloc(X199, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X199, &
         wf%n_o)
!
      call mem%alloc(X200, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X199, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X200, &
         wf%n_o**2)
!
      call mem%dealloc(X199)
      call mem%alloc(X201, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_4312(X200, X201, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X200)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X201, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X201)
      call mem%alloc(X202, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X202, &
         wf%n_o)
!
      call mem%alloc(X203, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X203, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X204, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X204, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X205, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X203, &
         wf%n_o**2, &
         X204, &
         wf%n_v**2, &
         zero, &
         X205, &
         wf%n_o**2)
!
      call mem%dealloc(X203)
      call mem%dealloc(X204)
      call mem%alloc(X206, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X202, X206, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X202)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X206, &
         wf%n_o**3, &
         X205, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X206)
      call mem%dealloc(X205)
      call mem%alloc(X207, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X207, &
         wf%n_o)
!
      call mem%alloc(X208, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X207, &
         wf%n_o**2, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X208, &
         wf%n_o**2)
!
      call mem%dealloc(X207)
      call mem%alloc(X209, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X208, X209, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X208)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X209, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X209)
      call mem%alloc(X210, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X210, &
         wf%n_o)
!
      call mem%alloc(X211, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X211, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X212, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X211, &
         wf%n_v*wf%n_o, &
         X210, &
         wf%n_o**2, &
         zero, &
         X212, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X210)
      call mem%dealloc(X211)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X212, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X212)
      call mem%alloc(X213, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X213, &
         wf%n_o)
!
      call mem%alloc(X214, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X214, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X215, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X214, &
         wf%n_v*wf%n_o, &
         X213, &
         wf%n_o**2, &
         zero, &
         X215, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X214)
      call mem%dealloc(X213)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X215, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X215)
      call mem%alloc(X216, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X216, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X217, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X217, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X218, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X216, &
         wf%n_v*wf%n_o, &
         X217, &
         wf%n_v*wf%n_o, &
         zero, &
         X218, &
         wf%n_o**2)
!
      call mem%dealloc(X216)
      call mem%dealloc(X217)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X218, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X218)
      call mem%alloc(X219, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X219, &
         wf%n_o)
!
      call mem%alloc(X220, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X220, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X221, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X221, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X222, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X221, &
         wf%n_o**2, &
         X220, &
         wf%n_v**2, &
         zero, &
         X222, &
         wf%n_o**2)
!
      call mem%dealloc(X220)
      call mem%dealloc(X221)
      call mem%alloc(X223, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X219, X223, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X219)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X223, &
         wf%n_o**3, &
         X222, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X223)
      call mem%dealloc(X222)
      call mem%alloc(X224, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X224, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X225, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X224, X225, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X224)
      call mem%alloc(X226, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X225, &
         wf%n_o**2, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X226, &
         wf%n_o**2)
!
      call mem%dealloc(X225)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X226, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X226)
      call mem%alloc(X227, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X227, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X228, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X228, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X229, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X227, &
         wf%n_v*wf%n_o, &
         X228, &
         wf%n_v*wf%n_o, &
         zero, &
         X229, &
         wf%n_o**2)
!
      call mem%dealloc(X227)
      call mem%dealloc(X228)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X229, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X229)
      call mem%alloc(X230, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(Ls_vovo, X230, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X231, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X231, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X232, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X230, &
         wf%n_v**2, &
         X231, &
         wf%n_v**2, &
         zero, &
         X232, &
         wf%n_o**2)
!
      call mem%dealloc(X230)
      call mem%dealloc(X231)
      call mem%alloc(X233, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X232, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X233, &
         wf%n_o**3)
!
      call mem%dealloc(X232)
      call mem%alloc(X234, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X233, X234, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X233)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X234, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X234)
      call mem%alloc(X235, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(Ls_vovo, X235, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X236, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X236, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X237, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X235, &
         wf%n_v**2, &
         X236, &
         wf%n_v**2, &
         zero, &
         X237, &
         wf%n_o**2)
!
      call mem%dealloc(X235)
      call mem%dealloc(X236)
      call mem%alloc(X238, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X237, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X238, &
         wf%n_o**3)
!
      call mem%dealloc(X237)
      call mem%alloc(X239, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X238, X239, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X238)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X239, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X239)
      call mem%alloc(X240, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X240, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X241, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X241, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X242, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X240, &
         wf%n_v*wf%n_o, &
         X241, &
         wf%n_v*wf%n_o, &
         zero, &
         X242, &
         wf%n_o**2)
!
      call mem%dealloc(X240)
      call mem%dealloc(X241)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X242, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X242)
      call mem%alloc(X243, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X243, &
         wf%n_o)
!
      call mem%alloc(X244, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X244, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X245, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X244, &
         wf%n_v*wf%n_o, &
         X243, &
         wf%n_o**2, &
         zero, &
         X245, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X243)
      call mem%dealloc(X244)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X245, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X245)
      call mem%alloc(X246, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X246, &
         wf%n_o)
!
      call mem%alloc(X247, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X246, X247, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X246)
      call mem%alloc(X248, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X247, &
         wf%n_o**2, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X248, &
         wf%n_o**2)
!
      call mem%dealloc(X247)
      call mem%alloc(X249, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X248, X249, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X248)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         X249, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X249)
      call mem%alloc(X250, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X250, &
         wf%n_o)
!
      call mem%alloc(X251, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X250, &
         wf%n_o**2, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X251, &
         wf%n_o**2)
!
      call mem%dealloc(X250)
      call mem%alloc(X252, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_4312(X251, X252, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X251)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X252, &
         wf%n_v*wf%n_o**2, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X252)
!
   end subroutine F_matrix_transformation_singles_qed_ccsd

