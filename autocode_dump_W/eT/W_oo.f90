   subroutine W_J_oo_terms_qed_ccsd(wf, W_J_oo, LJ_oo, LJ_ov, LJ_tr, LJ_vo, LJ_vv, Ls1, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Ru_vovo, Rv_vovo, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(inout) :: W_J_oo
!
      real(dp), intent(in) :: Ls1, Rs
      real(dp), dimension(wf%eri_t1%n_J), intent(in) :: LJ_tr
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(in) :: LJ_oo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(in) :: LJ_ov
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(in) :: LJ_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: LJ_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Ru_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
!
      real(dp) :: X4, X29, X46, X143
      real(dp), dimension(:), allocatable :: X36, X39, X108, X150, X179
      real(dp), dimension(:,:), allocatable :: X3, X5, X7, X11, X14, X18, X19, X23, X24, X26, X27, X31, X34, X35, X37, X38, X40, X43, X44, X45, X47, X49, X61, X63, X75, X76, X78, X79, X81, X97, X98, X100, X101, X103, X106, X107, X122, X123, X128, X131, X132, X134, X135, X137, X138, X140, X141, X145, X148, X149, X177, X178, X180, X183, X184, X189
      real(dp), dimension(:,:,:), allocatable :: X1, X2, X6, X8, X9, X10, X12, X15, X20, X25, X28, X30, X32, X33, X41, X42, X48, X50, X62, X64, X77, X80, X82, X83, X84, X85, X87, X99, X102, X104, X105, X112, X121, X124, X133, X136, X139, X142, X144, X146, X147, X154, X163, X171, X175, X181, X182, X185
      real(dp), dimension(:,:,:,:), allocatable :: X13, X16, X17, X21, X22, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X86, X88, X89, X90, X91, X92, X93, X94, X95, X96, X109, X110, X111, X113, X114, X115, X116, X117, X118, X119, X120, X125, X126, X127, X129, X130, X151, X152, X153, X155, X156, X157, X158, X159, X160, X161, X162, X164, X165, X166, X167, X168, X169, X170, X172, X173, X174, X176, X186, X187, X188, X190, X191, X192, X193, X194
!
      real(dp), external :: ddot
!
      call add_132_to_123(-two*Ls1*Rs, LJ_oo, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%alloc(X1, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X1, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -Rs, &
         X1, &
         wf%eri_t1%n_J*wf%n_o, &
         Ls_vo, &
         wf%n_v, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two*Ls1, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X2, &
         wf%eri_t1%n_J*wf%n_o)
!
      call add_132_to_123(one, X2, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_o)
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
         X3, &
         wf%n_o)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_tr, 1, &
         X3, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X3)
      X4 = -two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rs_vo, 1)
      call add_132_to_123(X4, LJ_oo, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%alloc(X5, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X5, &
         wf%n_o)
!
      call mem%alloc(X6, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X5, &
         wf%n_o, &
         zero, &
         X6, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X5)
      call add_132_to_123(one, X6, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_o)
!
      call mem%alloc(X8, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X8, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X8, &
         wf%eri_t1%n_J*wf%n_o, &
         X7, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X7)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_o, wf%eri_t1%n_J, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         zero, &
         X9, &
         wf%n_o)
!
      call mem%alloc(X10, wf%n_o, wf%n_o, wf%eri_t1%n_J)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         X9, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X10, &
         wf%n_o)
!
      call mem%dealloc(X9)
      call add_321_to_123(one, X10, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X11, 1)
!
      call mem%alloc(X12, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call sort_to_132(LJ_vo, X12, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X12, &
         wf%eri_t1%n_J*wf%n_o, &
         X11, &
         wf%n_v, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X11)
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X13, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         X13, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X14, 1)
!
      call mem%alloc(X15, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X14, &
         wf%n_v, &
         zero, &
         X15, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X14)
      call add_132_to_123(one, X15, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rv_vovo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X16, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X17, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X16, X17, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X16)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X17, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
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
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_tr, 1, &
         X18, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         Ls_vo, 1, &
         zero, &
         X19, 1)
!
      call mem%alloc(X20, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X19, &
         wf%n_v, &
         zero, &
         X20, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X19)
      call add_132_to_123(one, X20, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         u_vovo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X22, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X21, X22, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X21)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         Rs, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X22, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X23, &
         wf%n_o)
!
      call mem%alloc(X24, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X23, &
         wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X24, &
         wf%n_o)
!
      call mem%dealloc(X23)
      call mem%alloc(X25, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X24, &
         wf%n_o, &
         zero, &
         X25, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X24)
      call add_132_to_123(one, X25, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_o)
!
      call mem%alloc(X27, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X26, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X27, &
         wf%n_o)
!
      call mem%dealloc(X26)
      call mem%alloc(X28, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X27, &
         wf%n_o, &
         zero, &
         X28, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X27)
      call add_132_to_123(one, X28, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X28)
      X29 = -two * ddot(wf%n_v*wf%n_o, Ls_vo, 1, Rt_vo, 1)
      call mem%alloc(X30, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         X29, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X30, &
         wf%eri_t1%n_J*wf%n_o)
!
      call add_132_to_123(one, X30, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X31, &
         wf%n_o)
!
      call mem%alloc(X32, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X32, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X33, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X32, X33, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X32)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X33, &
         wf%eri_t1%n_J*wf%n_o, &
         X31, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X31)
      call mem%dealloc(X33)
      call mem%alloc(X34, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         s_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X34, &
         wf%n_o)
!
      call mem%alloc(X35, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X35, wf%n_v, wf%n_o)
      call mem%alloc(X36, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X35, 1, &
         zero, &
         X36, 1)
!
      call mem%dealloc(X35)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         X36, 1, &
         X34, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X34)
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Rt_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_o)
!
      call mem%alloc(X38, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X38, wf%n_v, wf%n_o)
      call mem%alloc(X39, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X38, 1, &
         zero, &
         X39, 1)
!
      call mem%dealloc(X38)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         X39, 1, &
         X37, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X37)
      call mem%dealloc(X39)
      call mem%alloc(X40, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Ls_vo, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X40, &
         wf%n_o)
!
      call mem%alloc(X41, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X41, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X42, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X41, X42, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X41)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X42, &
         wf%eri_t1%n_J*wf%n_o, &
         X40, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X40)
      call mem%dealloc(X42)
      call mem%alloc(X43, wf%n_o, wf%n_o)
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
         X43, &
         wf%n_o)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_tr, 1, &
         X43, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X44, 1)
!
      call mem%alloc(X45, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X44, &
         wf%n_v, &
         zero, &
         X45, &
         wf%n_o)
!
      call mem%dealloc(X44)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         LJ_tr, 1, &
         X45, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X45)
      X46 =  -ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rs_vovo, 1)
      call add_132_to_123(X46, LJ_oo, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%alloc(X47, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X47, &
         wf%n_o)
!
      call mem%alloc(X48, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X47, &
         wf%n_o, &
         zero, &
         X48, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X47)
      call add_132_to_123(one, X48, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X49, &
         wf%n_o)
!
      call mem%alloc(X50, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X50, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X50, &
         wf%eri_t1%n_J*wf%n_o, &
         X49, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X49)
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X51, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X52, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X51, X52, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X51)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X52, &
         wf%n_v**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X53, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X54, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X54, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X55, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X53, &
         wf%n_v*wf%n_o, &
         X54, &
         wf%n_v*wf%n_o, &
         zero, &
         X55, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X53)
      call mem%dealloc(X54)
      call mem%alloc(X56, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X55, X56, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X55)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X56, &
         wf%n_v**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X58, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X58, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X59, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X58, &
         wf%n_v**2, &
         X57, &
         wf%n_v**2, &
         zero, &
         X59, &
         wf%n_o**2)
!
      call mem%dealloc(X57)
      call mem%dealloc(X58)
      call mem%alloc(X60, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X59, X60, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X59)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X60, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X60)
      call mem%alloc(X61, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X61, &
         wf%n_o)
!
      call mem%alloc(X62, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X61, &
         wf%n_o, &
         zero, &
         X62, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X61)
      call add_132_to_123(one, X62, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X62)
      call mem%alloc(X63, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X63, &
         wf%n_o)
!
      call mem%alloc(X64, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X64, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X64, &
         wf%eri_t1%n_J*wf%n_o, &
         X63, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X63)
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X65, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X66, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X65, X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X65)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X66, &
         wf%n_v**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X66)
      call mem%alloc(X67, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X68, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         X67, &
         wf%n_v*wf%n_o, &
         X68, &
         wf%n_v*wf%n_o, &
         zero, &
         X69, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X67)
      call mem%dealloc(X68)
      call mem%alloc(X70, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X69, X70, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X69)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X70, &
         wf%n_v**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X72, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X72, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X73, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         Rs, &
         X72, &
         wf%n_v**2, &
         X71, &
         wf%n_v**2, &
         zero, &
         X73, &
         wf%n_o**2)
!
      call mem%dealloc(X71)
      call mem%dealloc(X72)
      call mem%alloc(X74, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X73, X74, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X73)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X74, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X75, 1)
!
      call mem%alloc(X76, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X75, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X76, &
         wf%n_o)
!
      call mem%dealloc(X75)
      call mem%alloc(X77, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X76, &
         wf%n_o, &
         zero, &
         X77, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X76)
      call add_132_to_123(one, X77, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X77)
      call mem%alloc(X78, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X78, 1)
!
      call mem%alloc(X79, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X78, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X79, &
         wf%n_o)
!
      call mem%dealloc(X78)
      call mem%alloc(X80, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(LJ_oo, X80, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X80, &
         wf%eri_t1%n_J*wf%n_o, &
         X79, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X79)
      call mem%dealloc(X80)
      call mem%alloc(X81, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X81, 1)
!
      call mem%alloc(X82, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call sort_to_132(LJ_vv, X82, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%alloc(X83, wf%n_o, wf%eri_t1%n_J, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         one, &
         X81, &
         wf%n_v, &
         X82, &
         wf%n_v*wf%eri_t1%n_J, &
         zero, &
         X83, &
         wf%n_o)
!
      call mem%dealloc(X82)
      call mem%dealloc(X81)
      call mem%alloc(X84, wf%n_o, wf%n_o, wf%eri_t1%n_J)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X83, &
         wf%eri_t1%n_J*wf%n_o, &
         zero, &
         X84, &
         wf%n_o)
!
      call mem%dealloc(X83)
      call add_231_to_123(one, X84, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X84)
      call mem%alloc(X85, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X85, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X86, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X86, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X85, &
         wf%eri_t1%n_J, &
         X86, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X85)
      call mem%dealloc(X86)
      call mem%alloc(X87, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         zero, &
         X87, &
         wf%n_v*wf%eri_t1%n_J)
!
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
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X87, &
         wf%eri_t1%n_J, &
         X88, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X87)
      call mem%dealloc(X88)
      call mem%alloc(X89, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X89, &
         wf%n_o)
!
      call mem%alloc(X90, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X89, X90, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X89)
      call mem%alloc(X91, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         X90, &
         wf%n_o**3, &
         zero, &
         X91, &
         wf%n_o)
!
      call mem%dealloc(X90)
      call mem%alloc(X92, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1423(X91, X92, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X91)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X92, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X92)
      call mem%alloc(X93, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X93, &
         wf%n_o)
!
      call mem%alloc(X94, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X93, X94, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X93)
      call mem%alloc(X95, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X94, &
         wf%n_o**3, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X95, &
         wf%n_o**3)
!
      call mem%dealloc(X94)
      call mem%alloc(X96, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1243(X95, X96, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X95)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X96, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X96)
      call mem%alloc(X97, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X97, &
         wf%n_o)
!
      call mem%alloc(X98, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X97, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X98, &
         wf%n_o)
!
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X98, &
         wf%n_o, &
         zero, &
         X99, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X98)
      call add_132_to_123(one, X99, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X99)
      call mem%alloc(X100, wf%n_v, wf%n_v)
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
         X100, &
         wf%n_v)
!
      call mem%alloc(X101, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X100, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X101, &
         wf%n_v)
!
      call mem%dealloc(X100)
      call mem%alloc(X102, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X101, &
         wf%n_v, &
         zero, &
         X102, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X101)
      call add_132_to_123(one, X102, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X103, &
         wf%n_o)
!
      call mem%alloc(X104, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X104, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X105, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X104, X105, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X104)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X105, &
         wf%eri_t1%n_J*wf%n_o, &
         X103, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X103)
      call mem%dealloc(X105)
      call mem%alloc(X106, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X106, &
         wf%n_o)
!
      call mem%alloc(X107, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X107, wf%n_v, wf%n_o)
      call mem%alloc(X108, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X107, 1, &
         zero, &
         X108, 1)
!
      call mem%dealloc(X107)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         X108, 1, &
         X106, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X106)
      call mem%dealloc(X108)
      call mem%alloc(X109, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X109, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X110, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X110, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X110, &
         wf%n_v**2, &
         X109, &
         wf%n_v**2, &
         zero, &
         X111, &
         wf%n_o**2)
!
      call mem%dealloc(X109)
      call mem%dealloc(X110)
      call mem%alloc(X112, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X112, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X113, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X111, X113, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X111)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X112, &
         wf%eri_t1%n_J, &
         X113, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X113)
      call mem%dealloc(X112)
      call mem%alloc(X114, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X114, &
         wf%n_o)
!
      call mem%alloc(X115, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X114, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X115, &
         wf%n_o**2)
!
      call mem%dealloc(X114)
      call mem%alloc(X116, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X115, X116, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X115)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X116, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X116)
      call mem%alloc(X117, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X117, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X118, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X118, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X119, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X117, &
         wf%n_v*wf%n_o, &
         X118, &
         wf%n_v*wf%n_o, &
         zero, &
         X119, &
         wf%n_o**2)
!
      call mem%dealloc(X117)
      call mem%dealloc(X118)
      call mem%alloc(X120, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X119, X120, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X119)
      call mem%alloc(X121, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X120, &
         wf%n_o**2, &
         zero, &
         X121, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X120)
      call add_132_to_123(one, X121, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X121)
      call mem%alloc(X122, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X122, 1)
!
      call mem%alloc(X123, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         X122, 1, &
         zero, &
         X123, 1)
!
      call mem%dealloc(X122)
      call mem%alloc(X124, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X123, &
         wf%n_v, &
         zero, &
         X124, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X123)
      call add_132_to_123(one, X124, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X124)
      call mem%alloc(X125, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X125, &
         wf%n_o)
!
      call mem%alloc(X126, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X125, &
         wf%n_o**2, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X126, &
         wf%n_o**2)
!
      call mem%dealloc(X125)
      call mem%alloc(X127, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X126, X127, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X126)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X127, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X127)
      call mem%alloc(X128, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X128, 1)
!
      call mem%alloc(X129, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         u_vovo, &
         wf%n_v, &
         X128, &
         wf%n_v, &
         zero, &
         X129, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X128)
      call mem%alloc(X130, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X129, X130, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X129)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X130, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X130)
      call mem%alloc(X131, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
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
         Rt_vo, &
         wf%n_v, &
         zero, &
         X132, &
         wf%n_o)
!
      call mem%dealloc(X131)
      call mem%alloc(X133, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X132, &
         wf%n_o, &
         zero, &
         X133, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X132)
      call add_132_to_123(one, X133, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X133)
      call mem%alloc(X134, wf%n_v, wf%n_v)
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
         X134, &
         wf%n_v)
!
      call mem%alloc(X135, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X134, &
         wf%n_v, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X135, &
         wf%n_v)
!
      call mem%dealloc(X134)
      call mem%alloc(X136, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X135, &
         wf%n_v, &
         zero, &
         X136, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X135)
      call add_132_to_123(one, X136, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X136)
      call mem%alloc(X137, wf%n_v, wf%n_v)
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
         X137, &
         wf%n_v)
!
      call mem%alloc(X138, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         s_vo, &
         wf%n_v, &
         X137, &
         wf%n_v, &
         zero, &
         X138, &
         wf%n_o)
!
      call mem%dealloc(X137)
      call mem%alloc(X139, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X138, &
         wf%n_o, &
         zero, &
         X139, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X138)
      call add_132_to_123(one, X139, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X139)
      call mem%alloc(X140, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X140, &
         wf%n_o)
!
      call mem%alloc(X141, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X140, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X141, &
         wf%n_o)
!
      call mem%dealloc(X140)
      call mem%alloc(X142, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X141, &
         wf%n_o, &
         zero, &
         X142, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X141)
      call add_132_to_123(one, X142, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X142)
      X143 =  -ddot(wf%n_v**2*wf%n_o**2, Ls_vovo, 1, Rt_vovo, 1)
      call mem%alloc(X144, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         X143, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X144, &
         wf%eri_t1%n_J*wf%n_o)
!
      call add_132_to_123(one, X144, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X144)
      call mem%alloc(X145, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X145, &
         wf%n_o)
!
      call mem%alloc(X146, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X146, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X147, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X146, X147, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X146)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X147, &
         wf%eri_t1%n_J*wf%n_o, &
         X145, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X145)
      call mem%dealloc(X147)
      call mem%alloc(X148, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X148, &
         wf%n_o)
!
      call mem%alloc(X149, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X149, wf%n_v, wf%n_o)
      call mem%alloc(X150, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X149, 1, &
         zero, &
         X150, 1)
!
      call mem%dealloc(X149)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         X150, 1, &
         X148, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X148)
      call mem%dealloc(X150)
      call mem%alloc(X151, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X151, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X152, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X152, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X153, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X152, &
         wf%n_v**2, &
         X151, &
         wf%n_v**2, &
         zero, &
         X153, &
         wf%n_o**2)
!
      call mem%dealloc(X151)
      call mem%dealloc(X152)
      call mem%alloc(X154, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X154, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X155, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X153, X155, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X153)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X154, &
         wf%eri_t1%n_J, &
         X155, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X155)
      call mem%dealloc(X154)
      call mem%alloc(X156, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X156, &
         wf%n_o)
!
      call mem%alloc(X157, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X156, &
         wf%n_o**2, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X157, &
         wf%n_o**2)
!
      call mem%dealloc(X156)
      call mem%alloc(X158, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X157, X158, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X157)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X158, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X158)
      call mem%alloc(X159, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X159, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X160, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X160, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X161, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X159, &
         wf%n_v*wf%n_o, &
         X160, &
         wf%n_v*wf%n_o, &
         zero, &
         X161, &
         wf%n_o**2)
!
      call mem%dealloc(X159)
      call mem%dealloc(X160)
      call mem%alloc(X162, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X161, X162, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X161)
      call mem%alloc(X163, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X162, &
         wf%n_o**2, &
         zero, &
         X163, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X162)
      call add_132_to_123(one, X163, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X163)
      call mem%alloc(X164, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X164, &
         wf%n_o)
!
      call mem%alloc(X165, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X164, &
         wf%n_o**2, &
         Rt_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X165, &
         wf%n_o**2)
!
      call mem%dealloc(X164)
      call mem%alloc(X166, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X165, X166, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X165)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X166, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X166)
      call mem%alloc(X167, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X167, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X168, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X168, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X169, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X167, &
         wf%n_v*wf%n_o, &
         X168, &
         wf%n_v*wf%n_o, &
         zero, &
         X169, &
         wf%n_o**2)
!
      call mem%dealloc(X167)
      call mem%dealloc(X168)
      call mem%alloc(X170, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X169, X170, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X169)
      call mem%alloc(X171, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X170, &
         wf%n_o**2, &
         zero, &
         X171, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X170)
      call add_132_to_123(one, X171, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X171)
      call mem%alloc(X172, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X172, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X173, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X173, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X174, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X173, &
         wf%n_v**2, &
         X172, &
         wf%n_v**2, &
         zero, &
         X174, &
         wf%n_o**2)
!
      call mem%dealloc(X172)
      call mem%dealloc(X173)
      call mem%alloc(X175, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X175, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X176, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X174, X176, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X174)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X175, &
         wf%eri_t1%n_J, &
         X176, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X176)
      call mem%dealloc(X175)
      call mem%alloc(X177, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X177, &
         wf%n_o)
!
      call mem%alloc(X178, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X178, wf%n_v, wf%n_o)
      call mem%alloc(X179, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X178, 1, &
         zero, &
         X179, 1)
!
      call mem%dealloc(X178)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_o**2, &
         one, &
         X179, 1, &
         X177, 1, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X177)
      call mem%dealloc(X179)
      call mem%alloc(X180, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X180, &
         wf%n_o)
!
      call mem%alloc(X181, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X181, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X182, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call sort_to_132(X181, X182, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X181)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X182, &
         wf%eri_t1%n_J*wf%n_o, &
         X180, &
         wf%n_o, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X180)
      call mem%dealloc(X182)
      call mem%alloc(X183, wf%n_v, wf%n_o)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X183, 1)
!
      call mem%alloc(X184, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X183, 1, &
         zero, &
         X184, 1)
!
      call mem%dealloc(X183)
      call mem%alloc(X185, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X184, &
         wf%n_v, &
         zero, &
         X185, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X184)
      call add_132_to_123(one, X185, W_J_oo, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%dealloc(X185)
      call mem%alloc(X186, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X186, &
         wf%n_o)
!
      call mem%alloc(X187, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X186, &
         wf%n_o**2, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X187, &
         wf%n_o**2)
!
      call mem%dealloc(X186)
      call mem%alloc(X188, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X187, X188, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X187)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X188, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X188)
      call mem%alloc(X189, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X189, 1)
!
      call mem%alloc(X190, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         v_vovo, &
         wf%n_v, &
         X189, &
         wf%n_v, &
         zero, &
         X190, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X189)
      call mem%alloc(X191, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X190, X191, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X190)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X191, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X191)
      call mem%alloc(X192, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X192, &
         wf%n_o)
!
      call mem%alloc(X193, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X192, &
         wf%n_o**2, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X193, &
         wf%n_o**2)
!
      call mem%dealloc(X192)
      call mem%alloc(X194, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X193, X194, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X193)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X194, &
         wf%n_o**2, &
         one, &
         W_J_oo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X194)
!
   end subroutine W_J_oo_terms_qed_ccsd

