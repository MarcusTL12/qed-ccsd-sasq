   subroutine jacobian_transpose_t1_qed_ccsd_2(wf, sigma_vo, F_ov, L_J_vv, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, bs2_vovo, bs_vo, bs_vovo, bγ, bγ2, d_oo, d_ov, d_vv, g_oooo, g_ooov, g_oovv, g_ovoo, g_ovov, g_ovvo, g_ovvv, g_voov, g_vvoo, g_vvov, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo, v₂_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: sigma_vo
!
      real(dp), intent(in) :: bγ, bγ2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: L_J_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_oovv
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_o), intent(in) :: g_ovvo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, bs_vovo, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo, v₂_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_o), intent(in) :: g_vvoo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp) :: X7, X27
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X6, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X26, X29, X31, X32, X33, X47, X48, X49, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X113, X114, X115, X116, X117, X118, X119, X120, X121, X122, X123, X124, X125, X126, X127, X128, X129, X130, X131, X132, X133, X134, X135, X136
      real(dp), dimension(:,:,:), allocatable :: X41, X42
      real(dp), dimension(:,:,:,:), allocatable :: X28, X30, X34, X35, X36, X37, X38, X39, X40, X43, X44, X45, X46, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112, X137, X138, X139, X140, X141, X142, X143, X144, X145, X146, X147, X148, X149, X150, X151, X152, X153, X154, X155, X156, X157, X158, X159, X160, X161, X162, X163, X164, X165, X166, X167, X168, X169, X170, X171, X172, X173, X174, X175, X176, X177, X178, X179, X180, X181, X182, X183, X184, X185, X186, X187, X188, X189, X190, X191, X192, X193, X194
!
      real(dp), external :: ddot
!
      call add_21_to_12(four*bγ*wf%s0_1, d_ov, sigma_vo, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -one, &
         bs_vo, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -four*wf%s0_1, &
         bs_vo, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         four*wf%s0_1, &
         d_vv, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
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
         wf%s1, &
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
         bs_vo, &
         wf%n_v, &
         X1, &
         wf%n_o, &
         one, &
         sigma_vo, &
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
         bs_vo, &
         wf%n_v, &
         wf%s1, &
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
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X3, wf%n_v, wf%n_o)
      call mem%alloc(X4, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four*bγ2, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X3, 1, &
         zero, &
         X4, 1)
!
      call mem%dealloc(X3)
      call add_21_to_12(bγ2, X4, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -four, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X5, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs_vo, &
         wf%n_v, &
         X5, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -four, &
         bs_vo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X6, &
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
         X6, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X6)
      X7 = eight * ddot(wf%n_v*wf%n_o, bs_vo, 1, wf%s1_2, 1)
      call add_21_to_12(X7, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call mem%alloc(X8, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X8, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         bs_vo, &
         wf%n_v, &
         X8, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         bs_vo, &
         wf%n_v, &
         wf%s1, &
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
         wf%s0, &
         d_ov, &
         wf%n_o, &
         X9, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         bs_vo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X10, &
         wf%n_v)
!
      call mem%alloc(X11, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X10, 1, &
         zero, &
         X11, 1)
!
      call mem%dealloc(X10)
      call add_21_to_12(one, X11, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         wf%s1, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         zero, &
         X12, &
         wf%n_o)
!
      call mem%alloc(X13, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X12, 1, &
         zero, &
         X13, 1)
!
      call mem%dealloc(X12)
      call add_21_to_12(one, X13, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X14, wf%n_v, wf%n_o)
      call mem%alloc(X15, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ooov, &
         wf%n_o**2, &
         X14, 1, &
         zero, &
         X15, 1)
!
      call mem%dealloc(X14)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs_vo, &
         wf%n_v, &
         X15, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X16, wf%n_v, wf%n_o)
      call mem%alloc(X17, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         one, &
         L_vvov, &
         wf%n_v**2, &
         X16, 1, &
         zero, &
         X17, 1)
!
      call mem%dealloc(X16)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X17, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X18, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X18, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X19, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         X19, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X20, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X20, &
         wf%n_v, &
         F_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X21, &
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
         X21, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         bs_vo, 1, &
         zero, &
         X22, 1)
!
      call mem%alloc(X23, wf%n_o, wf%n_v)
      call sort_to_21(X22, X23, wf%n_v, wf%n_o)
      call mem%dealloc(X22)
      call mem%alloc(X24, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X23, 1, &
         zero, &
         X24, 1)
!
      call mem%dealloc(X23)
      call add_21_to_12(one, X24, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X25, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X25, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs_vovo, &
         wf%n_v**2*wf%n_o, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X26, &
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
         X26, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X26)
      X27 = two * ddot(wf%n_v**2*wf%n_o**2, bs_vovo, 1, s₂_vovo, 1)
      call add_21_to_12(X27, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call mem%alloc(X28, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v₁_vovo, X28, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X29, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         g_ovov, &
         wf%n_o, &
         X28, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X29, &
         wf%n_o)
!
      call mem%dealloc(X28)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs_vo, &
         wf%n_v, &
         X29, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X30, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X31, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         X30, &
         wf%n_v*wf%n_o**2, &
         v₁_vovo, &
         wf%n_v, &
         zero, &
         X31, &
         wf%n_v)
!
      call mem%dealloc(X30)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X31, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X32, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X32, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X33, &
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
         X33, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X33)
      call mem%alloc(X34, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_oovv, &
         wf%n_v*wf%n_o**2, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X34, &
         wf%n_v*wf%n_o**2)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X34, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X35, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X36, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X35, X36, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X35)
      call mem%alloc(X37, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1423(g_oooo, X37, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X36, &
         wf%n_o**3, &
         X37, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X36)
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovvo, X38, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%alloc(X39, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         X38, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X39, &
         wf%n_o)
!
      call mem%dealloc(X38)
      call mem%alloc(X40, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X39, X40, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X39)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X40, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         two, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X41, &
         wf%n_v*wf%eri_t1%n_J)
!
      call mem%alloc(X42, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X41, &
         wf%eri_t1%n_J, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X42, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X41)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         X42, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X42)
      call mem%alloc(X43, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X43, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X44, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2413(X43, X44, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X43)
      call mem%alloc(X45, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_vvoo, X45, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X45, &
         wf%n_v*wf%n_o**2, &
         X44, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X44)
      call mem%dealloc(X45)
      call mem%alloc(X46, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X46, &
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
         X46, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X47, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0_1, &
         X47, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X48, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         wf%s0_1, &
         d_ov, &
         wf%n_o, &
         X48, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
         zero, &
         X49, 1)
!
      call mem%alloc(X50, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X50, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X49, &
         wf%n_v, &
         X50, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X49)
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X51, 1)
!
      call mem%alloc(X52, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X52, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X51, &
         wf%n_v, &
         X52, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X51)
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X53, 1)
!
      call mem%alloc(X54, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X53, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X54, &
         wf%n_o)
!
      call mem%dealloc(X53)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X54, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X54)
      call mem%alloc(X55, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
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
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X56, &
         wf%n_o)
!
      call mem%dealloc(X55)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X56, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X56)
      call mem%alloc(X57, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -four, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X57, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0_1, &
         X57, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X57)
      call mem%alloc(X58, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X58, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         wf%s0_1, &
         d_ov, &
         wf%n_o, &
         X58, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X58)
      call mem%alloc(X59, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X59, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0, &
         X59, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X59)
      call mem%alloc(X60, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X60, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         d_ov, &
         wf%n_o, &
         X60, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X60)
      call mem%alloc(X61, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X61, &
         wf%n_v)
!
      call mem%alloc(X62, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X61, 1, &
         zero, &
         X62, 1)
!
      call mem%dealloc(X61)
      call add_21_to_12(one, X62, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X62)
      call mem%alloc(X63, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X63, &
         wf%n_o)
!
      call mem%alloc(X64, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X63, 1, &
         zero, &
         X64, 1)
!
      call mem%dealloc(X63)
      call add_21_to_12(one, X64, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovvv, X65, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X66, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₂_vovo, X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X67, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v**2, &
         -two, &
         X65, &
         wf%n_v*wf%n_o, &
         X66, &
         wf%n_v**2, &
         zero, &
         X67, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X65)
      call mem%dealloc(X66)
      call mem%alloc(X68, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X67, X68, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X67)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X68, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X68)
      call mem%alloc(X69, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovoo, X69, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X70, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X69, &
         wf%n_o**2, &
         s₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X70, &
         wf%n_o**2)
!
      call mem%dealloc(X69)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X70, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X71, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X72, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s₂_vovo, X72, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X73, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X71, &
         wf%n_o**2, &
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
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X74, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         s₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X75, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X76, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X75, X76, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X75)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X76, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X76)
      call mem%alloc(X77, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(bs2_vovo, X77, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X78, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(s₂_vovo, X78, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X79, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X77, &
         wf%n_v*wf%n_o, &
         X78, &
         wf%n_v*wf%n_o, &
         zero, &
         X79, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X77)
      call mem%dealloc(X78)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X79, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X79)
      call mem%alloc(X80, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X80, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X81, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₂_vovo, X81, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X82, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X81, &
         wf%n_v**2, &
         X80, &
         wf%n_v**2, &
         zero, &
         X82, &
         wf%n_o**2)
!
      call mem%dealloc(X80)
      call mem%dealloc(X81)
      call mem%alloc(X83, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X83, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X83, &
         wf%n_o**3, &
         X82, &
         wf%n_o**3, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X82)
      call mem%dealloc(X83)
      call mem%alloc(X84, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_ooov, X84, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X85, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X84, &
         wf%n_o**2, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X85, &
         wf%n_o**2)
!
      call mem%dealloc(X84)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X85, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X85)
      call mem%alloc(X86, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X86, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X87, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X86, X87, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X86)
      call mem%alloc(X88, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X88, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X88, &
         wf%n_v**2*wf%n_o, &
         X87, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X87)
      call mem%dealloc(X88)
      call mem%alloc(X89, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
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
         X89, &
         wf%n_o**3)
!
      call mem%alloc(X90, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1243(X89, X90, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X89)
      call mem%alloc(X91, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X90, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X91, &
         wf%n_o**3)
!
      call mem%dealloc(X90)
      call mem%alloc(X92, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(X91, X92, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X91)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X92, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X92)
      call mem%alloc(X93, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_ovvv, &
         wf%n_v**2*wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X93, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X94, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(X93, X94, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X93)
      call mem%alloc(X95, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         wf%s1, &
         wf%n_v, &
         X94, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X95, &
         wf%n_o)
!
      call mem%dealloc(X94)
      call mem%alloc(X96, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X95, X96, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X95)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X96, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X96)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X97, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o**3, &
         wf%n_v, &
         two, &
         wf%s1, &
         wf%n_v, &
         X97, &
         wf%n_o**3, &
         zero, &
         X98, &
         wf%n_o)
!
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X98, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X99, &
         wf%n_o**3)
!
      call mem%dealloc(X98)
      call mem%alloc(X100, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(X99, X100, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X99)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X100, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X100)
      call mem%alloc(X101, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X101, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X102, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         one, &
         wf%s1, &
         wf%n_v, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X102, &
         wf%n_o)
!
      call mem%alloc(X103, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1243(X101, X103, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X101)
      call mem%alloc(X104, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1243(X102, X104, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%dealloc(X102)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X104, &
         wf%n_v*wf%n_o**2, &
         X103, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X103)
      call mem%dealloc(X104)
      call mem%alloc(X105, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X105, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X106, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X106, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X107, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         wf%n_v, &
         one, &
         wf%s1, &
         wf%n_v, &
         X106, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X107, &
         wf%n_o)
!
      call mem%dealloc(X106)
      call mem%alloc(X108, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X105, X108, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X105)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X107, &
         wf%n_v*wf%n_o**2, &
         X108, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X108)
      call mem%dealloc(X107)
      call mem%alloc(X109, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X109, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X110, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(X109, X110, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X109)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X110, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X111, &
         wf%n_o**3)
!
      call mem%dealloc(X110)
      call mem%alloc(X112, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X112, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X112, &
         wf%n_o**3, &
         X111, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X111)
      call mem%dealloc(X112)
      call mem%alloc(X113, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X113, &
         wf%n_o)
!
      call mem%alloc(X114, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X113, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X114, &
         wf%n_o)
!
      call mem%dealloc(X113)
      call mem%alloc(X115, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X114, 1, &
         zero, &
         X115, 1)
!
      call mem%dealloc(X114)
      call add_21_to_12(one, X115, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X115)
      call mem%alloc(X116, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X116, &
         wf%n_v)
!
      call mem%alloc(X117, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         wf%s1, &
         wf%n_v, &
         X116, &
         wf%n_v, &
         zero, &
         X117, &
         wf%n_o)
!
      call mem%dealloc(X116)
      call mem%alloc(X118, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X117, 1, &
         zero, &
         X118, 1)
!
      call mem%dealloc(X117)
      call add_21_to_12(one, X118, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X118)
      call mem%alloc(X119, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X119, wf%n_v, wf%n_o)
      call mem%alloc(X120, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X119, 1, &
         zero, &
         X120, 1)
!
      call mem%dealloc(X119)
      call mem%alloc(X121, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X121, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X121, &
         wf%n_v, &
         X120, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X120)
      call mem%dealloc(X121)
      call mem%alloc(X122, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X122, wf%n_v, wf%n_o)
      call mem%alloc(X123, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X122, 1, &
         zero, &
         X123, 1)
!
      call mem%dealloc(X122)
      call mem%alloc(X124, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X124, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X123, &
         wf%n_o, &
         X124, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X123)
      call mem%dealloc(X124)
      call mem%alloc(X125, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X125, &
         wf%n_o)
!
      call mem%alloc(X126, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X125, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X126, &
         wf%n_o)
!
      call mem%dealloc(X125)
      call mem%alloc(X127, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X126, 1, &
         zero, &
         X127, 1)
!
      call mem%dealloc(X126)
      call add_21_to_12(one, X127, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X127)
      call mem%alloc(X128, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X128, &
         wf%n_v)
!
      call mem%alloc(X129, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X128, &
         wf%n_v, &
         zero, &
         X129, &
         wf%n_o)
!
      call mem%dealloc(X128)
      call mem%alloc(X130, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X129, 1, &
         zero, &
         X130, 1)
!
      call mem%dealloc(X129)
      call add_21_to_12(one, X130, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X130)
      call mem%alloc(X131, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X131, wf%n_v, wf%n_o)
      call mem%alloc(X132, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X131, 1, &
         zero, &
         X132, 1)
!
      call mem%dealloc(X131)
      call mem%alloc(X133, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X133, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X133, &
         wf%n_v, &
         X132, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X132)
      call mem%dealloc(X133)
      call mem%alloc(X134, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X134, wf%n_v, wf%n_o)
      call mem%alloc(X135, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X134, 1, &
         zero, &
         X135, 1)
!
      call mem%dealloc(X134)
      call mem%alloc(X136, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X136, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X135, &
         wf%n_o, &
         X136, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X135)
      call mem%dealloc(X136)
      call mem%alloc(X137, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         wf%s1, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         zero, &
         X137, &
         wf%n_o)
!
      call mem%alloc(X138, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X138, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X139, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₁_vovo, X139, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X140, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X138, &
         wf%n_o**2, &
         X139, &
         wf%n_v**2, &
         zero, &
         X140, &
         wf%n_o**2)
!
      call mem%dealloc(X138)
      call mem%dealloc(X139)
      call mem%alloc(X141, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X137, X141, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X137)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X141, &
         wf%n_o**3, &
         X140, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X141)
      call mem%dealloc(X140)
      call mem%alloc(X142, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X142, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X143, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X142, X143, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X142)
      call mem%alloc(X144, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X143, &
         wf%n_o**2, &
         s₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X144, &
         wf%n_o**2)
!
      call mem%dealloc(X143)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X144, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X144)
      call mem%alloc(X145, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X145, &
         wf%n_o)
!
      call mem%alloc(X146, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s₁_vovo, X146, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X147, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X146, &
         wf%n_v*wf%n_o, &
         X145, &
         wf%n_o**2, &
         zero, &
         X147, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X145)
      call mem%dealloc(X146)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X147, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X147)
      call mem%alloc(X148, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X148, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X149, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X148, X149, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X148)
      call mem%alloc(X150, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X149, &
         wf%n_o**2, &
         s₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X150, &
         wf%n_o**2)
!
      call mem%dealloc(X149)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X150, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X150)
      call mem%alloc(X151, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X151, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X152, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s₁_vovo, X152, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X153, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X151, &
         wf%n_v*wf%n_o, &
         X152, &
         wf%n_v*wf%n_o, &
         zero, &
         X153, &
         wf%n_o**2)
!
      call mem%dealloc(X151)
      call mem%dealloc(X152)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X153, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X153)
      call mem%alloc(X154, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X154, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X155, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₁_vovo, X155, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X156, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X154, &
         wf%n_v**2, &
         X155, &
         wf%n_v**2, &
         zero, &
         X156, &
         wf%n_o**2)
!
      call mem%dealloc(X154)
      call mem%dealloc(X155)
      call mem%alloc(X157, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X156, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X157, &
         wf%n_o**3)
!
      call mem%dealloc(X156)
      call mem%alloc(X158, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X157, X158, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X157)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X158, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X158)
      call mem%alloc(X159, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X159, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X160, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X159, X160, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X159)
      call mem%alloc(X161, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X160, &
         wf%n_v*wf%n_o, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X161, &
         wf%n_o**2)
!
      call mem%dealloc(X160)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X161, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X161)
      call mem%alloc(X162, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X162, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X163, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X162, X163, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X162)
      call mem%alloc(X164, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X163, &
         wf%n_o**2, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X164, &
         wf%n_o**2)
!
      call mem%dealloc(X163)
      call mem%alloc(X165, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X164, X165, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X164)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X165, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X165)
      call mem%alloc(X166, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X166, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X167, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X167, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X168, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X167, &
         wf%n_v**2, &
         X166, &
         wf%n_o**2, &
         zero, &
         X168, &
         wf%n_o**2)
!
      call mem%dealloc(X166)
      call mem%dealloc(X167)
      call mem%alloc(X169, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X168, &
         wf%n_o**3, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X169, &
         wf%n_o**3)
!
      call mem%dealloc(X168)
      call mem%alloc(X170, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1423(X169, X170, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X169)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X170, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X170)
      call mem%alloc(X171, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X171, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X172, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X171, X172, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X171)
      call mem%alloc(X173, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X172, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X173, &
         wf%n_o**2)
!
      call mem%dealloc(X172)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X173, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X173)
      call mem%alloc(X174, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         wf%s1_2, &
         wf%n_v, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X174, &
         wf%n_o)
!
      call mem%alloc(X175, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X175, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X176, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X175, &
         wf%n_v*wf%n_o, &
         X174, &
         wf%n_o**2, &
         zero, &
         X176, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X174)
      call mem%dealloc(X175)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X176, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X176)
      call mem%alloc(X177, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X177, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X178, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X177, X178, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X177)
      call mem%alloc(X179, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
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
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X179, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X179)
      call mem%alloc(X180, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X180, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X181, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X181, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X182, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X180, &
         wf%n_v*wf%n_o, &
         X181, &
         wf%n_v*wf%n_o, &
         zero, &
         X182, &
         wf%n_o**2)
!
      call mem%dealloc(X180)
      call mem%dealloc(X181)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X182, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X182)
      call mem%alloc(X183, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X183, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X184, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X184, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X185, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X183, &
         wf%n_v**2, &
         X184, &
         wf%n_v**2, &
         zero, &
         X185, &
         wf%n_o**2)
!
      call mem%dealloc(X183)
      call mem%dealloc(X184)
      call mem%alloc(X186, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X185, &
         wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X186, &
         wf%n_o**3)
!
      call mem%dealloc(X185)
      call mem%alloc(X187, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X186, X187, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X186)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X187, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X187)
      call mem%alloc(X188, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X188, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X189, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X188, X189, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X188)
      call mem%alloc(X190, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X189, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X190, &
         wf%n_o**2)
!
      call mem%dealloc(X189)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X190, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X190)
      call mem%alloc(X191, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X191, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X192, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X191, X192, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X191)
      call mem%alloc(X193, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X192, &
         wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X193, &
         wf%n_o**2)
!
      call mem%dealloc(X192)
      call mem%alloc(X194, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X193, X194, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X194)
!
   end subroutine jacobian_transpose_t1_qed_ccsd_2
