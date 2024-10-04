   subroutine W_J_vv_terms_qed_ccsd(wf, W_J_vv, LJ_oo, LJ_ov, LJ_tr, LJ_vo, LJ_vv, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Ru_vovo, Rv_vovo, s_vo, s_vovo, t_vovo, u_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(inout) :: W_J_vv
!
      real(dp), intent(in) :: Rs
      real(dp), dimension(wf%eri_t1%n_J), intent(in) :: LJ_tr
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(in) :: LJ_oo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(in) :: LJ_ov
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(in) :: LJ_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: LJ_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Ru_vovo, Rv_vovo, s_vovo, t_vovo, u_vovo, v_vovo
!
      real(dp), dimension(:), allocatable :: X16, X19, X62, X88, X117
      real(dp), dimension(:,:), allocatable :: X1, X8, X12, X15, X17, X18, X20, X21, X24, X25, X26, X47, X58, X61, X63, X80, X84, X87, X89, X116, X118, X119, X125
      real(dp), dimension(:,:,:), allocatable :: X2, X3, X4, X5, X6, X7, X9, X10, X11, X13, X14, X22, X23, X48, X49, X50, X51, X52, X53, X55, X57, X59, X60, X64, X67, X77, X78, X79, X81, X82, X83, X85, X86, X90, X93, X103, X111, X120, X121, X122, X123, X124, X126, X127, X128, X129, X130, X131
      real(dp), dimension(:,:,:,:), allocatable :: X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X54, X56, X65, X66, X68, X69, X70, X71, X72, X73, X74, X75, X76, X91, X92, X94, X95, X96, X97, X98, X99, X100, X101, X102, X104, X105, X106, X107, X108, X109, X110, X112, X113, X114, X115
!
      call mem%alloc(X1, wf%n_v, wf%n_v)
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
         X1, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_tr, 1, &
         X1, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_o, wf%eri_t1%n_J, wf%n_o)
      call sort_to_213(LJ_oo, X2, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%alloc(X3, wf%n_o, wf%eri_t1%n_J, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         X2, &
         wf%eri_t1%n_J*wf%n_o, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X2)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X3, &
         wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X4, &
         wf%eri_t1%n_J)
!
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X4, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X5, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X6, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X5, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X6, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X5)
      call mem%alloc(X7, wf%eri_t1%n_J, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X6, &
         wf%n_v*wf%eri_t1%n_J, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X6)
      call add_132_to_123(one, X7, W_J_vv, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_v, wf%n_v)
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
         X8, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_tr, 1, &
         X8, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X9, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X10, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         X9, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X10, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X9)
      call mem%alloc(X11, wf%eri_t1%n_J, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X10, &
         wf%n_v*wf%eri_t1%n_J, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X11, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X10)
      call add_132_to_123(one, X11, W_J_vv, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_v)
!
      call mem%alloc(X13, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X12, &
         wf%n_v, &
         zero, &
         X13, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X12)
      call mem%alloc(X14, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(X13, X14, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X13)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X14, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X15, wf%n_v, wf%n_o)
      call mem%alloc(X16, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X15, 1, &
         zero, &
         X16, 1)
!
      call mem%dealloc(X15)
      call mem%alloc(X17, wf%n_v, wf%n_v)
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
         X17, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         X16, 1, &
         X17, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X16)
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X18, wf%n_v, wf%n_o)
      call mem%alloc(X19, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X18, 1, &
         zero, &
         X19, 1)
!
      call mem%dealloc(X18)
      call mem%alloc(X20, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X20, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         X19, 1, &
         X20, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X19)
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_v)
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
         X21, &
         wf%n_v)
!
      call mem%alloc(X22, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X21, &
         wf%n_v, &
         zero, &
         X22, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X21)
      call mem%alloc(X23, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(X22, X23, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X22)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X23, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X23)
      call mem%alloc(X24, wf%n_v, wf%n_v)
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
         X24, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_tr, 1, &
         X24, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X25, 1)
!
      call mem%alloc(X26, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X25, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X26, &
         wf%n_v)
!
      call mem%dealloc(X25)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         LJ_tr, 1, &
         X26, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         X27, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X28, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X27, X28, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X27)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X28, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X29, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X30, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rs_vovo, X30, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X31, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X29, &
         wf%n_v**2, &
         X30, &
         wf%n_v**2, &
         zero, &
         X31, &
         wf%n_v**2)
!
      call mem%dealloc(X29)
      call mem%dealloc(X30)
      call mem%alloc(X32, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(X31, X32, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call mem%dealloc(X31)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X32, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X33, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X34, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rs_vovo, X34, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X35, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         X33, &
         wf%n_v*wf%n_o, &
         X34, &
         wf%n_v*wf%n_o, &
         zero, &
         X35, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X33)
      call mem%dealloc(X34)
      call mem%alloc(X36, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X35, X36, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X35)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X36, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         X37, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X38, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X37, X38, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X37)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X38, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X38)
      call mem%alloc(X39, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X39, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X40, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Ls_vovo, X40, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X41, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_v**2, &
         wf%n_o**2, &
         Rs, &
         X40, &
         wf%n_v**2, &
         X39, &
         wf%n_v**2, &
         zero, &
         X41, &
         wf%n_v**2)
!
      call mem%dealloc(X39)
      call mem%dealloc(X40)
      call mem%alloc(X42, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(X41, X42, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call mem%dealloc(X41)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_v**2, &
         one, &
         LJ_vv, &
         wf%eri_t1%n_J, &
         X42, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X42)
      call mem%alloc(X43, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X43, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X44, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X44, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X45, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         X44, &
         wf%n_v*wf%n_o, &
         X43, &
         wf%n_v*wf%n_o, &
         zero, &
         X45, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X43)
      call mem%dealloc(X44)
      call mem%alloc(X46, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X45, X46, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X45)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X46, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X47, 1)
!
      call mem%alloc(X48, wf%n_o, wf%eri_t1%n_J, wf%n_o)
      call sort_to_213(LJ_oo, X48, wf%eri_t1%n_J, wf%n_o, wf%n_o)
      call mem%alloc(X49, wf%n_o, wf%eri_t1%n_J, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X48, &
         wf%eri_t1%n_J*wf%n_o, &
         X47, &
         wf%n_v, &
         zero, &
         X49, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X48)
      call mem%dealloc(X47)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X49, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         zero, &
         X50, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X51, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X50, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X51, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X50)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X51, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X52, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X53, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X52, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X53, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X52)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X53, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X53)
      call mem%alloc(X54, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X54, &
         wf%n_o)
!
      call mem%alloc(X55, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X54, &
         wf%n_o**2, &
         zero, &
         X55, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X54)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X55, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X56, &
         wf%n_o)
!
      call mem%alloc(X57, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X56, &
         wf%n_o**2, &
         zero, &
         X57, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X56)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X57, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X57)
      call mem%alloc(X58, wf%n_v, wf%n_v)
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
         X58, &
         wf%n_v)
!
      call mem%alloc(X59, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X58, &
         wf%n_v, &
         zero, &
         X59, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X58)
      call mem%alloc(X60, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(X59, X60, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X59)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X60, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X60)
      call mem%alloc(X61, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X61, wf%n_v, wf%n_o)
      call mem%alloc(X62, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X61, 1, &
         zero, &
         X62, 1)
!
      call mem%dealloc(X61)
      call mem%alloc(X63, wf%n_v, wf%n_v)
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
         X63, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         X62, 1, &
         X63, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X62)
      call mem%dealloc(X63)
      call mem%alloc(X64, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X64, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X65, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
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
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X64, &
         wf%eri_t1%n_J, &
         X66, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X64)
      call mem%dealloc(X66)
      call mem%alloc(X67, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X67, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X68, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X69, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X70, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X68, &
         wf%n_v*wf%n_o, &
         X69, &
         wf%n_v*wf%n_o, &
         zero, &
         X70, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X68)
      call mem%dealloc(X69)
      call mem%alloc(X71, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X70, X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X70)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X67, &
         wf%eri_t1%n_J, &
         X71, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X67)
      call mem%dealloc(X71)
      call mem%alloc(X72, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X72, &
         wf%n_o)
!
      call mem%alloc(X73, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X72, X73, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X72)
      call mem%alloc(X74, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X74, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X75, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X73, &
         wf%n_v*wf%n_o, &
         X74, &
         wf%n_v**2, &
         zero, &
         X75, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X73)
      call mem%dealloc(X74)
      call mem%alloc(X76, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1423(X75, X76, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%dealloc(X75)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X76, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X76)
      call mem%alloc(X77, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X77, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X78, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X77, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X78, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X77)
      call mem%alloc(X79, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X78, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X79, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X78)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X79, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X79)
      call mem%alloc(X80, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X80, 1)
!
      call mem%alloc(X81, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X81, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X82, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X81, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X82, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X81)
      call mem%alloc(X83, wf%eri_t1%n_J, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X82, &
         wf%n_v*wf%eri_t1%n_J, &
         X80, &
         wf%n_v, &
         zero, &
         X83, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X80)
      call mem%dealloc(X82)
      call add_132_to_123(one, X83, W_J_vv, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%dealloc(X83)
      call mem%alloc(X84, wf%n_v, wf%n_v)
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
         X84, &
         wf%n_v)
!
      call mem%alloc(X85, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X84, &
         wf%n_v, &
         zero, &
         X85, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X84)
      call mem%alloc(X86, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(X85, X86, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X85)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X86, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X86)
      call mem%alloc(X87, wf%n_o, wf%n_v)
      call sort_to_21(Rt_vo, X87, wf%n_v, wf%n_o)
      call mem%alloc(X88, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X87, 1, &
         zero, &
         X88, 1)
!
      call mem%dealloc(X87)
      call mem%alloc(X89, wf%n_v, wf%n_v)
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
         X89, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         X88, 1, &
         X89, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X88)
      call mem%dealloc(X89)
      call mem%alloc(X90, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X90, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X91, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         s_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X91, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X92, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X91, X92, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X91)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X90, &
         wf%eri_t1%n_J, &
         X92, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X90)
      call mem%dealloc(X92)
      call mem%alloc(X93, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         Rt_vo, &
         wf%n_v, &
         zero, &
         X93, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X94, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X94, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X95, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s_vovo, X95, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X96, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X94, &
         wf%n_v*wf%n_o, &
         X95, &
         wf%n_v*wf%n_o, &
         zero, &
         X96, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X94)
      call mem%dealloc(X95)
      call mem%alloc(X97, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X96, X97, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X96)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X93, &
         wf%eri_t1%n_J, &
         X97, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X93)
      call mem%dealloc(X97)
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X98, &
         wf%n_o)
!
      call mem%alloc(X99, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X98, X99, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X98)
      call mem%alloc(X100, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s_vovo, X100, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X101, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X99, &
         wf%n_v*wf%n_o, &
         X100, &
         wf%n_v**2, &
         zero, &
         X101, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X99)
      call mem%dealloc(X100)
      call mem%alloc(X102, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1423(X101, X102, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%dealloc(X101)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X102, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X103, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X104, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         X104, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X105, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X104, X105, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X104)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X103, &
         wf%eri_t1%n_J, &
         X105, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X103)
      call mem%dealloc(X105)
      call mem%alloc(X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X106, &
         wf%n_o)
!
      call mem%alloc(X107, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X106, X107, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X106)
      call mem%alloc(X108, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(Rt_vovo, X108, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X109, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X107, &
         wf%n_v*wf%n_o, &
         X108, &
         wf%n_v**2, &
         zero, &
         X109, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X107)
      call mem%dealloc(X108)
      call mem%alloc(X110, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1423(X109, X110, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%dealloc(X109)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X110, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X110)
      call mem%alloc(X111, wf%eri_t1%n_J, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X111, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%alloc(X112, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Ls_vovo, X112, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X113, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(Rt_vovo, X113, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X114, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X112, &
         wf%n_v*wf%n_o, &
         X113, &
         wf%n_v*wf%n_o, &
         zero, &
         X114, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X112)
      call mem%dealloc(X113)
      call mem%alloc(X115, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X114, X115, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X114)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v**2, &
         wf%n_o**2, &
         one, &
         X111, &
         wf%eri_t1%n_J, &
         X115, &
         wf%n_v**2, &
         one, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X111)
      call mem%dealloc(X115)
      call mem%alloc(X116, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X116, wf%n_v, wf%n_o)
      call mem%alloc(X117, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X116, 1, &
         zero, &
         X117, 1)
!
      call mem%dealloc(X116)
      call mem%alloc(X118, wf%n_v, wf%n_v)
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
         X118, &
         wf%n_v)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v**2, &
         one, &
         X117, 1, &
         X118, 1, &
         W_J_vv, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X117)
      call mem%dealloc(X118)
      call mem%alloc(X119, wf%n_v, wf%n_v)
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
         X119, &
         wf%n_v)
!
      call mem%alloc(X120, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X119, &
         wf%n_v, &
         zero, &
         X120, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X119)
      call mem%alloc(X121, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(X120, X121, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%dealloc(X120)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X121, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X121)
      call mem%alloc(X122, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X122, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X123, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X122, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X123, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X122)
      call mem%alloc(X124, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X123, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X124, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X123)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X124, &
         wf%n_v*wf%eri_t1%n_J, &
         Rt_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X124)
      call mem%alloc(X125, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X125, 1)
!
      call mem%alloc(X126, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X126, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X127, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X126, &
         wf%eri_t1%n_J, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X127, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X126)
      call mem%alloc(X128, wf%eri_t1%n_J, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X127, &
         wf%n_v*wf%eri_t1%n_J, &
         X125, &
         wf%n_v, &
         zero, &
         X128, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X125)
      call mem%dealloc(X127)
      call add_132_to_123(one, X128, W_J_vv, wf%eri_t1%n_J, wf%n_v, wf%n_v)
      call mem%dealloc(X128)
      call mem%alloc(X129, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X129, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X130, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X129, &
         wf%eri_t1%n_J, &
         Ru_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X130, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X129)
      call mem%alloc(X131, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X130, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X131, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X130)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X131, &
         wf%n_v*wf%eri_t1%n_J, &
         s_vo, &
         wf%n_v, &
         one, &
         W_J_vv, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%dealloc(X131)
!
   end subroutine W_J_vv_terms_qed_ccsd

