   subroutine omega_2_aibj_qed_ccsd_2(wf, omega_vovo, F_oo, F_ov, F_vv, L_J_vv, L_ooov, L_ovov, L_vvov, d_oo, d_ov, d_vv, g_oooo, g_ooov, g_ovov, g_vooo, g_voov, g_vovv, g_vvoo, g_vvov, g_vvvv, s2, s2_2, t_vovo, v_2_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: omega_vovo
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: F_oo, d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: F_vv, d_vv
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: L_J_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_vooo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: s2, s2_2, t_vovo, v_2_vovo, v_vovo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_vovv
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_o), intent(in) :: g_vvoo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_v,wf%n_v), intent(in) :: g_vvvv
!
      real(dp) :: X24, X26
      real(dp), dimension(:,:), allocatable :: X2, X3, X4, X5, X17, X18, X19, X20, X21, X22, X23, X25, X27, X28, X29, X30, X31, X32, X33, X34, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X122, X124, X147, X149, X153, X155, X162, X163, X164, X165, X166, X167
      real(dp), dimension(:,:,:), allocatable :: X36, X37
      real(dp), dimension(:,:,:,:), allocatable :: X1, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X35, X38, X39, X40, X41, X42, X43, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112, X113, X114, X115, X116, X117, X118, X119, X120, X121, X123, X125, X126, X127, X128, X129, X130, X131, X132, X133, X134, X135, X136, X137, X138, X139, X140, X141, X142, X143, X144, X145, X146, X148, X150, X151, X152, X154, X156, X157, X158, X159, X160, X161, X168, X169, X170, X171, X172, X173, X174, X175, X176, X177, X178, X179, X180, X181, X182, X183, X184, X185, X186
!
      real(dp), external :: ddot
!
      call daxpy(wf%n_v**2*wf%n_o**2, two*wf%qed%frequencies(wf%mode), s2_2, 1, omega_vovo, 1)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         F_vv, &
         wf%n_v, &
         s2_2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         s2_2, &
         wf%n_v**2*wf%n_o, &
         F_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         d_vv, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         s2, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         two, &
         g_vovv, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X1, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_4123(g_vooo, X1, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         X1, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         two, &
         d_vv, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_v)
!
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X2, 1, &
         wf%s1, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         four, &
         d_vv, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_v)
!
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X3, 1, &
         wf%s1_2, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -four, &
         wf%s1, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         zero, &
         X4, &
         wf%n_v)
!
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X4, 1, &
         wf%s1_2, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         zero, &
         X5, &
         wf%n_v)
!
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X5, 1, &
         wf%s1, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X5)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         four*wf%s0_2, &
         d_vv, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -four*wf%s0_2, &
         s2, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two*wf%s0, &
         d_vv, &
         wf%n_v, &
         s2_2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two*wf%s0, &
         s2_2, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X6, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvvv, X6, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X7, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2_2, X7, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X8, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X6, &
         wf%n_v**2, &
         X7, &
         wf%n_v**2, &
         zero, &
         X8, &
         wf%n_v**2)
!
      call mem%dealloc(X6)
      call mem%dealloc(X7)
      call add_1324_to_1234(one, X8, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(g_vvoo, X9, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X9, &
         wf%n_v*wf%n_o, &
         s2_2, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_vvoo, X10, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X11, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2_2, X11, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X12, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X10, &
         wf%n_v*wf%n_o, &
         X11, &
         wf%n_v*wf%n_o, &
         zero, &
         X12, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X10)
      call mem%dealloc(X11)
      call add_1432_to_1234(one, X12, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(g_oooo, X13, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%alloc(X14, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2_2, X14, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X15, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X14, &
         wf%n_v**2, &
         X13, &
         wf%n_o**2, &
         zero, &
         X15, &
         wf%n_v**2)
!
      call mem%dealloc(X13)
      call mem%dealloc(X14)
      call add_1324_to_1234(one, X15, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_voov, X16, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X16, &
         wf%n_v*wf%n_o, &
         v_2_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         F_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X17, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X17, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         F_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X18, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2, &
         wf%n_v**2*wf%n_o, &
         X18, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         F_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X19, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X19, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         F_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X20, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X20, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -six, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X21, &
         wf%n_v, &
         s2_2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -six, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X22, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2_2, &
         wf%n_v**2*wf%n_o, &
         X22, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X23, wf%n_o, wf%n_v)
      X24 = four * ddot(wf%n_v*wf%n_o, X23, 1, wf%s1, 1)
      call mem%dealloc(X23)
      call daxpy(wf%n_v**2*wf%n_o**2, X24, s2_2, 1, omega_vovo, 1)
      call mem%alloc(X25, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X25, wf%n_o, wf%n_v)
      X26 = two * ddot(wf%n_v*wf%n_o, X25, 1, wf%s1_2, 1)
      call mem%dealloc(X25)
      call daxpy(wf%n_v**2*wf%n_o**2, X26, s2, 1, omega_vovo, 1)
      call mem%alloc(X27, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -six, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X27, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2, &
         wf%n_v**2*wf%n_o, &
         X27, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -six, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X28, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X28, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X29, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X30, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X30, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X31, wf%n_o, wf%n_v)
      call mem%alloc(X32, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v_2_vovo, &
         wf%n_v*wf%n_o, &
         X31, 1, &
         zero, &
         X32, 1)
!
      call mem%dealloc(X31)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%s1, 1, &
         X32, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X33, wf%n_o, wf%n_v)
      call mem%alloc(X34, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X33, 1, &
         zero, &
         X34, 1)
!
      call mem%dealloc(X33)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%s1_2, 1, &
         X34, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1, &
         wf%n_v, &
         g_voov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X35, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X35, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X35)
      call mem%alloc(X36, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X36, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X37, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_v*wf%eri_t1%n_J)
!
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%eri_t1%n_J, &
         one, &
         X36, &
         wf%eri_t1%n_J, &
         X37, &
         wf%eri_t1%n_J, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X36)
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_vvoo, X38, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X39, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         X38, &
         wf%n_v*wf%n_o**2, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X39, &
         wf%n_v*wf%n_o**2)
!
      call mem%dealloc(X38)
      call mem%alloc(X40, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_3142(X39, X40, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X39)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X40, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1342(g_oooo, X41, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%alloc(X42, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X41, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X42, &
         wf%n_o**3)
!
      call mem%dealloc(X41)
      call mem%alloc(X43, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X42, X43, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X42)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X43, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X44, &
         wf%n_o)
!
      call mem%alloc(X45, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X44, &
         wf%n_o, &
         zero, &
         X45, &
         wf%n_v)
!
      call mem%dealloc(X44)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%s1, 1, &
         X45, 1, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X45)
      call mem%alloc(X46, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X46, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0, &
         X46, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X47, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         s2, &
         wf%n_v**2*wf%n_o, &
         X47, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -four, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X48, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0_2, &
         X48, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -four, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X49, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         wf%s0_2, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X49, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X50, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0, &
         X50, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X51, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X51, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X52, wf%n_v, wf%n_o)
      call mem%alloc(X53, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         two, &
         L_vvov, &
         wf%n_v**2, &
         X52, 1, &
         zero, &
         X53, 1)
!
      call mem%dealloc(X52)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X53, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X53)
      call mem%alloc(X54, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X54, wf%n_v, wf%n_o)
      call mem%alloc(X55, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ooov, &
         wf%n_o**2, &
         X54, 1, &
         zero, &
         X55, 1)
!
      call mem%dealloc(X54)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2, &
         wf%n_v**2*wf%n_o, &
         X55, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X56, wf%n_v, wf%n_o)
      call mem%alloc(X57, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         two, &
         L_vvov, &
         wf%n_v**2, &
         X56, 1, &
         zero, &
         X57, 1)
!
      call mem%dealloc(X56)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X57, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X57)
      call mem%alloc(X58, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X58, wf%n_v, wf%n_o)
      call mem%alloc(X59, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ooov, &
         wf%n_o**2, &
         X58, 1, &
         zero, &
         X59, 1)
!
      call mem%dealloc(X58)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X59, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X59)
      call mem%alloc(X60, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvov, X60, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X61, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(s2, X61, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X62, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         -two, &
         X61, &
         wf%n_v**2, &
         X60, &
         wf%n_v*wf%n_o, &
         zero, &
         X62, &
         wf%n_o**2)
!
      call mem%dealloc(X60)
      call mem%dealloc(X61)
      call mem%alloc(X63, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(X62, X63, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X62)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X63, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X63)
      call mem%alloc(X64, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X64, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X65, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X64, X65, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X64)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X65, &
         wf%n_v*wf%n_o, &
         s2, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X65)
      call mem%alloc(X66, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         -two, &
         wf%s1, &
         wf%n_v, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X66, &
         wf%n_o)
!
      call mem%alloc(X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(s2, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X68, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X67, &
         wf%n_v*wf%n_o, &
         X66, &
         wf%n_v*wf%n_o, &
         zero, &
         X68, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X66)
      call mem%dealloc(X67)
      call add_1423_to_1234(one, X68, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X68)
      call mem%alloc(X69, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_4132(g_ooov, X69, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X70, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X69, &
         wf%n_v*wf%n_o, &
         s2, &
         wf%n_v*wf%n_o, &
         zero, &
         X70, &
         wf%n_o**2)
!
      call mem%dealloc(X69)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X70, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(g_ooov, X71, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X72, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2, X72, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X73, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X71, &
         wf%n_v*wf%n_o, &
         X72, &
         wf%n_v*wf%n_o, &
         zero, &
         X73, &
         wf%n_o**2)
!
      call mem%dealloc(X71)
      call mem%dealloc(X72)
      call mem%alloc(X74, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X73, X74, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X73)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X74, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         two, &
         g_ooov, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X75, &
         wf%n_o**3)
!
      call mem%alloc(X76, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X75, X76, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X75)
      call mem%alloc(X77, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2, X77, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X78, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X77, &
         wf%n_v**2, &
         X76, &
         wf%n_o**2, &
         zero, &
         X78, &
         wf%n_v**2)
!
      call mem%dealloc(X76)
      call mem%dealloc(X77)
      call add_1324_to_1234(one, X78, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X78)
      call mem%alloc(X79, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X79, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X80, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         two, &
         X79, &
         wf%n_v**2*wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X80, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X79)
      call mem%alloc(X81, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X80, X81, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X80)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X81, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X81)
      call mem%alloc(X82, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_ooov, X82, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X83, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X82, &
         wf%n_o**2, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X83, &
         wf%n_o**2)
!
      call mem%dealloc(X82)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X83, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X83)
      call mem%alloc(X84, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvov, X84, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X85, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(t_vovo, X85, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X86, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         -two, &
         X85, &
         wf%n_v**2, &
         X84, &
         wf%n_v*wf%n_o, &
         zero, &
         X86, &
         wf%n_o**2)
!
      call mem%dealloc(X84)
      call mem%dealloc(X85)
      call mem%alloc(X87, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(X86, X87, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X86)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X87, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X87)
      call mem%alloc(X88, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X88, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X89, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X88, X89, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X88)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X89, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X89)
      call mem%alloc(X90, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X90, &
         wf%n_o)
!
      call mem%alloc(X91, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X91, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X92, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X91, &
         wf%n_v*wf%n_o, &
         X90, &
         wf%n_v*wf%n_o, &
         zero, &
         X92, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X90)
      call mem%dealloc(X91)
      call add_1423_to_1234(one, X92, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X92)
      call mem%alloc(X93, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_4132(g_ooov, X93, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X94, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X93, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X94, &
         wf%n_o**2)
!
      call mem%dealloc(X93)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X94, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X94)
      call mem%alloc(X95, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(g_ooov, X95, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X96, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X96, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X95, &
         wf%n_v*wf%n_o, &
         X96, &
         wf%n_v*wf%n_o, &
         zero, &
         X97, &
         wf%n_o**2)
!
      call mem%dealloc(X95)
      call mem%dealloc(X96)
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X97, X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X97)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X98, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X98)
      call mem%alloc(X99, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         two, &
         g_ooov, &
         wf%n_o**3, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X99, &
         wf%n_o**3)
!
      call mem%alloc(X100, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X99, X100, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X99)
      call mem%alloc(X101, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X101, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X102, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X101, &
         wf%n_v**2, &
         X100, &
         wf%n_o**2, &
         zero, &
         X102, &
         wf%n_v**2)
!
      call mem%dealloc(X100)
      call mem%dealloc(X101)
      call add_1324_to_1234(one, X102, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X103, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X104, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         two, &
         X103, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X104, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X103)
      call mem%alloc(X105, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X104, X105, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X104)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X105, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X105)
      call mem%alloc(X106, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_ooov, X106, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X107, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X106, &
         wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X107, &
         wf%n_o**2)
!
      call mem%dealloc(X106)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X107, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X107)
      call mem%alloc(X108, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X108, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X109, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X108, &
         wf%n_v*wf%n_o, &
         s2, &
         wf%n_v*wf%n_o, &
         zero, &
         X109, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X108)
      call mem%alloc(X110, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2, X110, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X109, &
         wf%n_v*wf%n_o, &
         X110, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X109)
      call mem%dealloc(X110)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X111, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X112, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2, X112, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X113, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X111, &
         wf%n_o**2, &
         X112, &
         wf%n_v**2, &
         zero, &
         X113, &
         wf%n_o**2)
!
      call mem%dealloc(X111)
      call mem%dealloc(X112)
      call mem%alloc(X114, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2, X114, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X115, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X114, &
         wf%n_v**2, &
         X113, &
         wf%n_o**2, &
         zero, &
         X115, &
         wf%n_v**2)
!
      call mem%dealloc(X113)
      call mem%dealloc(X114)
      call add_1324_to_1234(one, X115, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X115)
      call mem%alloc(X116, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X116, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X117, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2, X117, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X118, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X116, &
         wf%n_v*wf%n_o, &
         X117, &
         wf%n_v*wf%n_o, &
         zero, &
         X118, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X116)
      call mem%dealloc(X117)
      call mem%alloc(X119, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2, X119, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X120, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X119, &
         wf%n_v*wf%n_o, &
         X118, &
         wf%n_v*wf%n_o, &
         zero, &
         X120, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X118)
      call mem%dealloc(X119)
      call add_1432_to_1234(one, X120, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X120)
      call mem%alloc(X121, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v_vovo, X121, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X122, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X121, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X122, &
         wf%n_o)
!
      call mem%dealloc(X121)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2, &
         wf%n_v**2*wf%n_o, &
         X122, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X122)
      call mem%alloc(X123, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X123, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X124, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X123, &
         wf%n_v*wf%n_o**2, &
         v_vovo, &
         wf%n_v, &
         zero, &
         X124, &
         wf%n_v)
!
      call mem%dealloc(X123)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X124, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X124)
      call mem%alloc(X125, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X125, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X126, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         s2, &
         wf%n_v*wf%n_o, &
         X125, &
         wf%n_v*wf%n_o, &
         zero, &
         X126, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X125)
      call mem%alloc(X127, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X126, X127, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X126)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X127, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X127)
      call mem%alloc(X128, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X128, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X129, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X129, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X130, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X128, &
         wf%n_o**2, &
         X129, &
         wf%n_v**2, &
         zero, &
         X130, &
         wf%n_o**2)
!
      call mem%dealloc(X128)
      call mem%dealloc(X129)
      call mem%alloc(X131, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2_2, X131, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X132, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X131, &
         wf%n_v**2, &
         X130, &
         wf%n_o**2, &
         zero, &
         X132, &
         wf%n_v**2)
!
      call mem%dealloc(X130)
      call mem%dealloc(X131)
      call add_1324_to_1234(one, X132, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X132)
      call mem%alloc(X133, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X133, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X134, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X133, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X134, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X133)
      call mem%alloc(X135, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2_2, X135, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X135, &
         wf%n_v*wf%n_o, &
         X134, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X134)
      call mem%dealloc(X135)
      call mem%alloc(X136, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X136, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X137, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X137, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X138, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X136, &
         wf%n_v*wf%n_o, &
         X137, &
         wf%n_v*wf%n_o, &
         zero, &
         X138, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X136)
      call mem%dealloc(X137)
      call mem%alloc(X139, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2_2, X139, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X140, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X139, &
         wf%n_v*wf%n_o, &
         X138, &
         wf%n_v*wf%n_o, &
         zero, &
         X140, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X138)
      call mem%dealloc(X139)
      call add_1432_to_1234(one, X140, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X140)
      call mem%alloc(X141, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X141, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X142, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2_2, X142, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X143, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X141, &
         wf%n_o**2, &
         X142, &
         wf%n_v**2, &
         zero, &
         X143, &
         wf%n_o**2)
!
      call mem%dealloc(X141)
      call mem%dealloc(X142)
      call mem%alloc(X144, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X144, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X145, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X144, &
         wf%n_v**2, &
         X143, &
         wf%n_o**2, &
         zero, &
         X145, &
         wf%n_v**2)
!
      call mem%dealloc(X143)
      call mem%dealloc(X144)
      call add_1324_to_1234(one, X145, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X145)
      call mem%alloc(X146, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(wf%u_aibj, X146, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X147, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X146, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X147, &
         wf%n_o)
!
      call mem%dealloc(X146)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         s2_2, &
         wf%n_v**2*wf%n_o, &
         X147, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X147)
      call mem%alloc(X148, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X148, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X149, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X148, &
         wf%n_v*wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v, &
         zero, &
         X149, &
         wf%n_v)
!
      call mem%dealloc(X148)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X149, &
         wf%n_v, &
         s2_2, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X149)
      call mem%alloc(X150, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_4123(g_ovov, X150, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X151, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X150, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X151, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X150)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         s2_2, &
         wf%n_v*wf%n_o, &
         X151, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X151)
      call mem%alloc(X152, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v_2_vovo, X152, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X153, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X152, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X153, &
         wf%n_o)
!
      call mem%dealloc(X152)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X153, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X153)
      call mem%alloc(X154, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X154, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X155, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X154, &
         wf%n_v*wf%n_o**2, &
         v_2_vovo, &
         wf%n_v, &
         zero, &
         X155, &
         wf%n_v)
!
      call mem%dealloc(X154)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X155, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X155)
      call mem%alloc(X156, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1243(g_ovov, X156, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X157, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v_2_vovo, &
         wf%n_v*wf%n_o, &
         X156, &
         wf%n_v*wf%n_o, &
         zero, &
         X157, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X156)
      call mem%alloc(X158, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X157, X158, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X157)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         X158, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X158)
      call mem%alloc(X159, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(v_vovo, X159, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X160, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X159, &
         wf%n_v*wf%n_o, &
         g_ovov, &
         wf%n_v*wf%n_o, &
         zero, &
         X160, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X159)
      call mem%alloc(X161, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X160, X161, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X160)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X161, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X161)
      call mem%alloc(X162, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X162, wf%n_v, wf%n_o)
      call mem%alloc(X163, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X162, 1, &
         zero, &
         X163, 1)
!
      call mem%dealloc(X162)
      call mem%alloc(X164, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X163, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X164, &
         wf%n_v)
!
      call mem%dealloc(X163)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X164, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X164)
      call mem%alloc(X165, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X165, wf%n_v, wf%n_o)
      call mem%alloc(X166, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X165, 1, &
         zero, &
         X166, 1)
!
      call mem%dealloc(X165)
      call mem%alloc(X167, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X166, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X167, &
         wf%n_o)
!
      call mem%dealloc(X166)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         X167, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X167)
      call mem%alloc(X168, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X168, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X169, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(t_vovo, X169, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X170, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X168, &
         wf%n_o**2, &
         X169, &
         wf%n_v**2, &
         zero, &
         X170, &
         wf%n_o**2)
!
      call mem%dealloc(X168)
      call mem%dealloc(X169)
      call mem%alloc(X171, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X170, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X171, &
         wf%n_o**3)
!
      call mem%dealloc(X170)
      call mem%alloc(X172, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X171, X172, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X171)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X172, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X172)
      call mem%alloc(X173, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X173, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X174, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X173, X174, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X173)
      call mem%alloc(X175, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X174, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X175, &
         wf%n_o**2)
!
      call mem%dealloc(X174)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X175, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X175)
      call mem%alloc(X176, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         wf%s1, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X176, &
         wf%n_o)
!
      call mem%alloc(X177, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X177, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X178, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X177, &
         wf%n_v*wf%n_o, &
         X176, &
         wf%n_o**2, &
         zero, &
         X178, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X176)
      call mem%dealloc(X177)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X178, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X178)
      call mem%alloc(X179, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X179, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X180, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X179, X180, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X179)
      call mem%alloc(X181, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X180, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X181, &
         wf%n_o**3)
!
      call mem%dealloc(X180)
      call mem%alloc(X182, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X182, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X183, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X182, &
         wf%n_v**2, &
         X181, &
         wf%n_o**2, &
         zero, &
         X183, &
         wf%n_v**2)
!
      call mem%dealloc(X181)
      call mem%dealloc(X182)
      call add_1342_to_1234(one, X183, omega_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X183)
      call mem%alloc(X184, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X184, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X185, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X184, X185, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X184)
      call mem%alloc(X186, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X185, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X186, &
         wf%n_o**2)
!
      call mem%dealloc(X185)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X186, &
         wf%n_o, &
         one, &
         omega_vovo, &
         wf%n_v)
!
      call mem%dealloc(X186)
!
   end subroutine omega_2_aibj_qed_ccsd_2
