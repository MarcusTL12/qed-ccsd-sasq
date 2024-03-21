   subroutine jacobian_transpose_s1_qed_ccsd_2(wf, sigma_vo, F_oo, F_ov, F_vv, L_J_vv, L_ovoo, L_ovov, L_ovvo, L_ovvv, bs2_vovo, bs_vo, bγ2, d_oo, d_ov, d_vv, g_oooo, g_ooov, g_oovv, g_ovoo, g_ovov, g_ovvo, g_ovvv, g_voov, g_vvoo, g_vvov, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo, v₂_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: sigma_vo
!
      real(dp), intent(in) :: bγ2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: F_oo, d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: F_vv, d_vv
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: L_J_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_ooov
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_oovv
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_o), intent(in) :: L_ovvo, g_ovvo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, s₁_vovo, s₂_vovo, t_vovo, v₁_vovo, v₂_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_o), intent(in) :: g_vvoo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: g_vvov
!
      real(dp) :: X5, X8, X24
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X6, X7, X9, X10, X11, X12, X13, X14, X15, X16, X17, X19, X21, X22, X23, X25, X26, X27, X28, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X80, X81, X82, X83, X84, X85, X86, X87, X88, X89, X90, X91
      real(dp), dimension(:,:,:), allocatable :: X36, X37
      real(dp), dimension(:,:,:,:), allocatable :: X18, X20, X29, X30, X31, X32, X33, X34, X35, X38, X39, X40, X41, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X92, X93, X94, X95, X96, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112, X113, X114, X115, X116, X117, X118, X119, X120
!
      real(dp), external :: ddot
!
      call daxpy(wf%n_v*wf%n_o, wf%qed%frequencies(wf%mode), bs_vo, 1, sigma_vo, 1)
      call add_21_to_12(four*bγ2, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call add_21_to_12(eight*bγ2*wf%s0_1, d_ov, sigma_vo, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -one, &
         bs_vo, &
         wf%n_v, &
         F_oo, &
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
         F_vv, &
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
         -wf%s0, &
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
         wf%s0, &
         d_vv, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%alloc(X1, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovvo, &
         wf%n_v*wf%n_o, &
         bs_vo, 1, &
         zero, &
         X1, 1)
!
      call add_21_to_12(one, X1, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X2, wf%n_v, wf%n_o)
      call mem%alloc(X3, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four*bγ2, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X2, 1, &
         zero, &
         X3, 1)
!
      call mem%dealloc(X2)
      call add_21_to_12(bγ2, X3, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X4, wf%n_o, wf%n_v)
      X5 = two * ddot(wf%n_v*wf%n_o, X4, 1, wf%s1, 1)
      call mem%dealloc(X4)
      call daxpy(wf%n_v*wf%n_o, X5, bs_vo, 1, sigma_vo, 1)
      call mem%alloc(X6, wf%n_o, wf%n_o)
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
         X6, &
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
         X6, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         bs_vo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X7, &
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
         X7, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X7)
      X8 = two * ddot(wf%n_v*wf%n_o, bs_vo, 1, wf%s1, 1)
      call add_21_to_12(X8, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call mem%alloc(X9, wf%n_v, wf%n_o)
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
         X9, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X9, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_v, wf%n_o)
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
         X10, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X10, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X11, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X11, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X12, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         X12, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_v, wf%n_v)
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
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_o)
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
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         bs_vo, 1, &
         zero, &
         X15, 1)
!
      call mem%alloc(X16, wf%n_o, wf%n_v)
      call sort_to_21(X15, X16, wf%n_v, wf%n_o)
      call mem%dealloc(X15)
      call mem%alloc(X17, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X16, 1, &
         zero, &
         X17, 1)
!
      call mem%dealloc(X16)
      call add_21_to_12(one, X17, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(wf%u_aibj, X18, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X19, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         g_ovov, &
         wf%n_o, &
         X18, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X19, &
         wf%n_o)
!
      call mem%dealloc(X18)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs_vo, &
         wf%n_v, &
         X19, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X20, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X21, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         X20, &
         wf%n_v*wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_v)
!
      call mem%dealloc(X20)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X21, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -six, &
         bs2_vovo, &
         wf%n_v, &
         s₂_vovo, &
         wf%n_v, &
         zero, &
         X22, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X22, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -six, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s₂_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X23, &
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
         X23, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X23)
      X24 = four * ddot(wf%n_v**2*wf%n_o**2, bs2_vovo, 1, s₂_vovo, 1)
      call add_21_to_12(X24, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call mem%alloc(X25, wf%n_v, wf%n_v)
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
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
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
      call mem%alloc(X27, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X27, wf%n_o, wf%n_v)
      call mem%alloc(X28, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         X27, 1, &
         zero, &
         X28, 1)
!
      call mem%dealloc(X27)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X28, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_oovv, &
         wf%n_v*wf%n_o**2, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X29, &
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
         X29, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X30, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X31, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X30, X31, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X30)
      call mem%alloc(X32, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1423(g_oooo, X32, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X31, &
         wf%n_o**3, &
         X32, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X31)
      call mem%dealloc(X32)
      call mem%alloc(X33, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovvo, X33, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call mem%alloc(X34, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1, &
         wf%n_v, &
         X33, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X34, &
         wf%n_o)
!
      call mem%dealloc(X33)
      call mem%alloc(X35, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X34, X35, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X34)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X35, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X35)
      call mem%alloc(X36, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         two, &
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
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X36, &
         wf%eri_t1%n_J, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X37, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X36)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         L_J_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         X37, &
         wf%n_v*wf%eri_t1%n_J, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X37)
      call mem%alloc(X38, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X38, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X39, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2413(X38, X39, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X38)
      call mem%alloc(X40, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_vvoo, X40, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         X40, &
         wf%n_v*wf%n_o**2, &
         X39, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X39)
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X41, &
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
         X41, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X41)
      call mem%alloc(X42, wf%n_o, wf%n_o)
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
         X42, &
         wf%n_o)
!
      call mem%alloc(X43, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X42, &
         wf%n_o, &
         zero, &
         X43, &
         wf%n_v)
!
      call mem%dealloc(X42)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X43, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
         zero, &
         X44, 1)
!
      call mem%alloc(X45, wf%n_o, wf%n_o)
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
         X45, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X44, &
         wf%n_v, &
         X45, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X44)
      call mem%dealloc(X45)
      call mem%alloc(X46, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
         zero, &
         X46, 1)
!
      call mem%alloc(X47, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X46, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X47, &
         wf%n_o)
!
      call mem%dealloc(X46)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         X47, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_v, wf%n_v)
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
         X48, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0, &
         X48, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_o, wf%n_o)
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
         X49, &
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
         X49, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X49)
      call mem%alloc(X50, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -four, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X50, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0_1, &
         X50, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X51, &
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
         X51, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         s₁_vovo, &
         wf%n_v, &
         zero, &
         X52, &
         wf%n_v)
!
      call mem%alloc(X53, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X52, 1, &
         zero, &
         X53, 1)
!
      call mem%dealloc(X52)
      call add_21_to_12(one, X53, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X53)
      call mem%alloc(X54, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         s₁_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X54, &
         wf%n_o)
!
      call mem%alloc(X55, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X54, 1, &
         zero, &
         X55, 1)
!
      call mem%dealloc(X54)
      call add_21_to_12(one, X55, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovvv, X56, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X57, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₁_vovo, X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X58, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v**2, &
         -two, &
         X56, &
         wf%n_v*wf%n_o, &
         X57, &
         wf%n_v**2, &
         zero, &
         X58, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X56)
      call mem%dealloc(X57)
      call mem%alloc(X59, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X58, X59, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X58)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X59, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X59)
      call mem%alloc(X60, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovoo, X60, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X61, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X60, &
         wf%n_o**2, &
         s₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X61, &
         wf%n_o**2)
!
      call mem%dealloc(X60)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X61, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X61)
      call mem%alloc(X62, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X62, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X63, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s₁_vovo, X63, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X64, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X62, &
         wf%n_o**2, &
         X63, &
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
         bs2_vovo, &
         wf%n_v, &
         X65, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X65)
      call mem%alloc(X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         s₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X66, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X67, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X66, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X66)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X67, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X67)
      call mem%alloc(X68, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(bs2_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(s₁_vovo, X69, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X70, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
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
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X70, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X72, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₁_vovo, X72, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X73, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
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
      call mem%alloc(X74, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X74, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X74, &
         wf%n_o**3, &
         X73, &
         wf%n_o**3, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X73)
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_ooov, X75, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X76, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X75, &
         wf%n_o**2, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X76, &
         wf%n_o**2)
!
      call mem%dealloc(X75)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X76, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X76)
      call mem%alloc(X77, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X77, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X78, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X77, X78, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X77)
      call mem%alloc(X79, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X79, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X79, &
         wf%n_v**2*wf%n_o, &
         X78, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X78)
      call mem%dealloc(X79)
      call mem%alloc(X80, wf%n_o, wf%n_o)
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
         X80, &
         wf%n_o)
!
      call mem%alloc(X81, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X80, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X81, &
         wf%n_o)
!
      call mem%dealloc(X80)
      call mem%alloc(X82, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X81, 1, &
         zero, &
         X82, 1)
!
      call mem%dealloc(X81)
      call add_21_to_12(one, X82, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X82)
      call mem%alloc(X83, wf%n_v, wf%n_v)
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
         X83, &
         wf%n_v)
!
      call mem%alloc(X84, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         wf%s1, &
         wf%n_v, &
         X83, &
         wf%n_v, &
         zero, &
         X84, &
         wf%n_o)
!
      call mem%dealloc(X83)
      call mem%alloc(X85, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X84, 1, &
         zero, &
         X85, 1)
!
      call mem%dealloc(X84)
      call add_21_to_12(one, X85, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X85)
      call mem%alloc(X86, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X86, wf%n_v, wf%n_o)
      call mem%alloc(X87, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X86, 1, &
         zero, &
         X87, 1)
!
      call mem%dealloc(X86)
      call mem%alloc(X88, wf%n_v, wf%n_v)
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
         X88, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X88, &
         wf%n_v, &
         X87, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X87)
      call mem%dealloc(X88)
      call mem%alloc(X89, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X89, wf%n_v, wf%n_o)
      call mem%alloc(X90, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X89, 1, &
         zero, &
         X90, 1)
!
      call mem%dealloc(X89)
      call mem%alloc(X91, wf%n_o, wf%n_o)
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
         X91, &
         wf%n_o)
!
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X90, &
         wf%n_o, &
         X91, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X90)
      call mem%dealloc(X91)
      call mem%alloc(X92, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X92, &
         wf%n_o)
!
      call mem%alloc(X93, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X93, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X94, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X94, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X95, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X93, &
         wf%n_o**2, &
         X94, &
         wf%n_v**2, &
         zero, &
         X95, &
         wf%n_o**2)
!
      call mem%dealloc(X93)
      call mem%dealloc(X94)
      call mem%alloc(X96, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(X92, X96, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X92)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X96, &
         wf%n_o**3, &
         X95, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X96)
      call mem%dealloc(X95)
      call mem%alloc(X97, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X97, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X98, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X97, X98, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X97)
      call mem%alloc(X99, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X98, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X99, &
         wf%n_o**2)
!
      call mem%dealloc(X98)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X99, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X99)
      call mem%alloc(X100, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X100, &
         wf%n_o)
!
      call mem%alloc(X101, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X101, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X102, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X101, &
         wf%n_v*wf%n_o, &
         X100, &
         wf%n_o**2, &
         zero, &
         X102, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X100)
      call mem%dealloc(X101)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X102, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X102)
      call mem%alloc(X103, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X105, &
         wf%n_o**2)
!
      call mem%dealloc(X104)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X105, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X105)
      call mem%alloc(X106, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X106, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X107, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X107, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X108, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X106, &
         wf%n_v*wf%n_o, &
         X107, &
         wf%n_v*wf%n_o, &
         zero, &
         X108, &
         wf%n_o**2)
!
      call mem%dealloc(X106)
      call mem%dealloc(X107)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X108, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X108)
      call mem%alloc(X109, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X109, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X110, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X110, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X109, &
         wf%n_v**2, &
         X110, &
         wf%n_v**2, &
         zero, &
         X111, &
         wf%n_o**2)
!
      call mem%dealloc(X109)
      call mem%dealloc(X110)
      call mem%alloc(X112, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_o**3, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X111, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X112, &
         wf%n_o**3)
!
      call mem%dealloc(X111)
      call mem%alloc(X113, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1342(X112, X113, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%dealloc(X112)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X113, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X113)
      call mem%alloc(X114, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X114, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X115, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(X114, X115, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X114)
      call mem%alloc(X116, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X115, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X116, &
         wf%n_o**2)
!
      call mem%dealloc(X115)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X116, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X116)
      call mem%alloc(X117, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X117, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X118, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X117, X118, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X117)
      call mem%alloc(X119, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X118, &
         wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X119, &
         wf%n_o**2)
!
      call mem%dealloc(X118)
      call mem%alloc(X120, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X119, X120, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X119)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         g_ovov, &
         wf%n_v*wf%n_o**2, &
         X120, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X120)
!
   end subroutine jacobian_transpose_s1_qed_ccsd_2
