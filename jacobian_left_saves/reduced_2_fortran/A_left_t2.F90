   subroutine jacobian_transpose_t2_qed_ccsd_2(wf, sigma_vovo, F_ov, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, bs2_vovo, bs_vo, bs_vovo, d_oo, d_ov, d_vv, g_ooov, g_ovoo, g_ovov, g_ovvv, g_vvov, s₂_vovo, v₂_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: sigma_vovo
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, bs_vovo, s₂_vovo, v₂_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp), dimension(:,:), allocatable :: X1, X3, X5, X6, X7, X9, X11, X12, X13, X14, X15, X17, X18, X19, X20, X21, X23, X24, X25, X26, X27, X28, X33, X34, X35, X36, X56, X58, X81, X83, X91, X92, X93, X94, X95, X96
      real(dp), dimension(:,:,:,:), allocatable :: X2, X4, X8, X10, X16, X22, X29, X30, X31, X32, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50, X51, X52, X53, X54, X55, X57, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X72, X73, X74, X75, X76, X77, X78, X79, X80, X82, X84, X85, X86, X87, X88, X89, X90, X97, X98, X99, X100, X101, X102, X103, X104, X105, X106, X107, X108, X109, X110, X111, X112
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X1, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs_vo, 1, &
         X1, 1, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         bs_vo, 1, &
         d_ov, 1, &
         X2, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X2, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X3, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         eight*wf%s0_1, &
         bs_vo, 1, &
         X3, 1, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -four*wf%s0_1, &
         bs_vo, 1, &
         d_ov, 1, &
         X4, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X4, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two*wf%s0_1, &
         bs_vovo, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two*wf%s0_1, &
         d_vv, &
         wf%n_v, &
         bs_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%alloc(X5, wf%n_o, wf%n_o)
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
         X5, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X5, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_v)
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
         X6, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X6, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         bs_vo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_v)
!
      call mem%alloc(X8, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X7, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X8, &
         wf%n_v)
!
      call mem%dealloc(X7)
      call add_1432_to_1234(one, X8, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
      call mem%alloc(X10, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X10, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X10, &
         wf%n_o, &
         X9, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X10)
      call mem%dealloc(X9)
      call mem%alloc(X11, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X11, wf%n_v, wf%n_o)
      call mem%alloc(X12, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X11, 1, &
         zero, &
         X12, 1)
!
      call mem%dealloc(X11)
      call mem%alloc(X13, wf%n_v, wf%n_o)
      call sort_to_21(X12, X13, wf%n_o, wf%n_v)
      call mem%dealloc(X12)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X13, 1, &
         bs_vo, 1, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X14, wf%n_v, wf%n_o)
      call mem%alloc(X15, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X14, 1, &
         zero, &
         X15, 1)
!
      call mem%dealloc(X14)
      call mem%alloc(X16, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs_vo, 1, &
         X15, 1, &
         X16, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X15)
      call add_1423_to_1234(one, X16, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%n_o, wf%n_o)
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
         X17, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs_vovo, &
         wf%n_v**2*wf%n_o, &
         X17, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_v, wf%n_v)
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
         X18, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X18, &
         wf%n_v, &
         bs_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X19, 1)
!
      call mem%alloc(X20, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X20, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X19, 1, &
         X20, 1, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X19)
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X21, 1)
!
      call mem%alloc(X22, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X21, 1, &
         d_ov, 1, &
         X22, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X21)
      call add_1423_to_1234(one, X22, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
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
         X23, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X23, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X23)
      call mem%alloc(X24, wf%n_v, wf%n_v)
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
         X24, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X24, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_o)
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
         X25, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         wf%s0_1, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X25, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_v, wf%n_v)
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
         X26, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0_1, &
         X26, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X26)
      call mem%alloc(X27, wf%n_o, wf%n_o)
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
         X27, &
         wf%n_o)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X27, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_v, wf%n_v)
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
         X28, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0, &
         X28, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         two, &
         L_ovvv, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X30, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X29, &
         wf%n_v*wf%n_o, &
         zero, &
         X30, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X29)
      call add_1243_to_1234(one, X30, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         zero, &
         X31, &
         wf%n_o)
!
      call mem%alloc(X32, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(L_ovoo, X32, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X32, &
         wf%n_v*wf%n_o, &
         X31, &
         wf%n_o**2, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X32)
      call mem%dealloc(X31)
      call mem%alloc(X33, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X33, wf%n_v, wf%n_o)
      call mem%alloc(X34, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ooov, &
         wf%n_o**2, &
         X33, 1, &
         zero, &
         X34, 1)
!
      call mem%dealloc(X33)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X34, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X34)
      call mem%alloc(X35, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X35, wf%n_v, wf%n_o)
      call mem%alloc(X36, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         two, &
         L_vvov, &
         wf%n_v**2, &
         X35, 1, &
         zero, &
         X36, 1)
!
      call mem%dealloc(X35)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X36, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         g_ovvv, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X37, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X38, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X37, &
         wf%n_v*wf%n_o, &
         zero, &
         X38, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X37)
      call add_1423_to_1234(one, X38, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X38)
      call mem%alloc(X39, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         two, &
         wf%s1_2, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         zero, &
         X39, &
         wf%n_o)
!
      call mem%alloc(X40, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X39, &
         wf%n_o**2, &
         g_ovoo, &
         wf%n_v*wf%n_o, &
         zero, &
         X40, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X39)
      call add_1423_to_1234(one, X40, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X40)
      call mem%alloc(X41, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
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
         X41, &
         wf%n_o**3)
!
      call mem%alloc(X42, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X42, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X43, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X41, X43, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X41)
      call mem%alloc(X44, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X42, &
         wf%n_v**2, &
         X43, &
         wf%n_o**2, &
         zero, &
         X44, &
         wf%n_v**2)
!
      call mem%dealloc(X42)
      call mem%dealloc(X43)
      call add_1324_to_1234(one, X44, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X44)
      call mem%alloc(X45, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X45, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X46, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_4123(g_ooov, X46, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X47, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X46, &
         wf%n_v*wf%n_o, &
         X45, &
         wf%n_v*wf%n_o, &
         zero, &
         X47, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X45)
      call mem%dealloc(X46)
      call add_1423_to_1234(one, X47, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X47)
      call mem%alloc(X48, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1432(g_ovvv, X48, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X49, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         X48, &
         wf%n_v**2*wf%n_o, &
         wf%s1_2, &
         wf%n_v, &
         zero, &
         X49, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X48)
      call mem%alloc(X50, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(bs2_vovo, X50, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X51, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X50, &
         wf%n_v*wf%n_o, &
         X49, &
         wf%n_v*wf%n_o, &
         zero, &
         X51, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X50)
      call mem%dealloc(X49)
      call add_1423_to_1234(one, X51, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X51)
      call mem%alloc(X52, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X52, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X53, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X52, X53, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X52)
      call mem%alloc(X54, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvov, X54, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X55, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X54, &
         wf%n_v*wf%n_o, &
         X53, &
         wf%n_o**2, &
         zero, &
         X55, &
         wf%n_v**2)
!
      call mem%dealloc(X53)
      call mem%dealloc(X54)
      call add_1342_to_1234(one, X55, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%n_v, wf%n_v)
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
         X56, &
         wf%n_v)
!
      call mem%alloc(X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X56, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X57, &
         wf%n_v)
!
      call mem%dealloc(X56)
      call add_1432_to_1234(one, X57, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X57)
      call mem%alloc(X58, wf%n_o, wf%n_o)
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
         X58, &
         wf%n_o)
!
      call mem%alloc(X59, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X59, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X59, &
         wf%n_o, &
         X58, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X59)
      call mem%dealloc(X58)
      call mem%alloc(X60, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_2143(L_ovov, X60, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X61, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X60, &
         wf%n_v*wf%n_o, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X61, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X60)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X61, &
         wf%n_v*wf%n_o, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X61)
      call mem%alloc(X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         s₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X62, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X63, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X63, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X64, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
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
      call add_1423_to_1234(one, X64, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X64)
      call mem%alloc(X65, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X65, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X66, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₂_vovo, X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X67, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X65, &
         wf%n_o**2, &
         X66, &
         wf%n_v**2, &
         zero, &
         X67, &
         wf%n_o**2)
!
      call mem%dealloc(X65)
      call mem%dealloc(X66)
      call mem%alloc(X68, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X68, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X69, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X68, &
         wf%n_v**2, &
         X67, &
         wf%n_o**2, &
         zero, &
         X69, &
         wf%n_v**2)
!
      call mem%dealloc(X68)
      call mem%dealloc(X67)
      call add_1324_to_1234(one, X69, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X69)
      call mem%alloc(X70, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X70, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X71, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s₂_vovo, X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X72, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X70, &
         wf%n_v*wf%n_o, &
         X71, &
         wf%n_v*wf%n_o, &
         zero, &
         X72, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X70)
      call mem%dealloc(X71)
      call mem%alloc(X73, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(bs2_vovo, X73, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X74, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X73, &
         wf%n_v*wf%n_o, &
         X72, &
         wf%n_v*wf%n_o, &
         zero, &
         X74, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X73)
      call mem%dealloc(X72)
      call add_1423_to_1234(one, X74, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X75, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X76, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s₂_vovo, X76, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X77, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X75, &
         wf%n_v**2, &
         X76, &
         wf%n_v**2, &
         zero, &
         X77, &
         wf%n_o**2)
!
      call mem%dealloc(X75)
      call mem%dealloc(X76)
      call mem%alloc(X78, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X78, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X79, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X78, &
         wf%n_o**2, &
         X77, &
         wf%n_o**2, &
         zero, &
         X79, &
         wf%n_v**2)
!
      call mem%dealloc(X77)
      call mem%dealloc(X78)
      call add_1324_to_1234(one, X79, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X79)
      call mem%alloc(X80, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v₂_vovo, X80, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X81, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X80, &
         wf%n_v**2*wf%n_o, &
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
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X81, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X81)
      call mem%alloc(X82, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X82, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X83, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X82, &
         wf%n_v*wf%n_o**2, &
         v₂_vovo, &
         wf%n_v, &
         zero, &
         X83, &
         wf%n_v)
!
      call mem%dealloc(X82)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X83, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X83)
      call mem%alloc(X84, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X84, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X85, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1243(X84, X85, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X84)
      call mem%alloc(X86, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X85, &
         wf%n_v*wf%n_o, &
         g_ovov, &
         wf%n_v*wf%n_o, &
         zero, &
         X86, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X85)
      call add_1423_to_1234(one, X86, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X86)
      call mem%alloc(X87, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X87, &
         wf%n_o)
!
      call mem%alloc(X88, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X88, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X89, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X87, X89, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X87)
      call mem%alloc(X90, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(X88, X90, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X88)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X89, &
         wf%n_o**2, &
         X90, &
         wf%n_o**2, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X89)
      call mem%dealloc(X90)
      call mem%alloc(X91, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X91, wf%n_v, wf%n_o)
      call mem%alloc(X92, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X91, 1, &
         zero, &
         X92, 1)
!
      call mem%dealloc(X91)
      call mem%alloc(X93, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X92, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X93, &
         wf%n_o)
!
      call mem%dealloc(X92)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X93, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X93)
      call mem%alloc(X94, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X94, wf%n_v, wf%n_o)
      call mem%alloc(X95, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X94, 1, &
         zero, &
         X95, 1)
!
      call mem%dealloc(X94)
      call mem%alloc(X96, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         X95, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X96, &
         wf%n_v)
!
      call mem%dealloc(X95)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X96, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X96)
      call mem%alloc(X97, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X97, &
         wf%n_o)
!
      call mem%alloc(X98, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X98, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X99, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X97, &
         wf%n_o**2, &
         X98, &
         wf%n_v*wf%n_o, &
         zero, &
         X99, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X97)
      call mem%dealloc(X98)
      call add_1423_to_1234(one, X99, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X99)
      call mem%alloc(X100, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X100, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X101, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X100, X101, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X100)
      call mem%alloc(X102, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X101, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X102, &
         wf%n_o**3)
!
      call mem%dealloc(X101)
      call mem%alloc(X103, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X103, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X104, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X103, &
         wf%n_v**2, &
         X102, &
         wf%n_o**2, &
         zero, &
         X104, &
         wf%n_v**2)
!
      call mem%dealloc(X103)
      call mem%dealloc(X102)
      call add_1324_to_1234(one, X104, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X104)
      call mem%alloc(X105, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         wf%s1, &
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
      call add_1423_to_1234(one, X107, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X107)
      call mem%alloc(X108, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         wf%n_v, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X108, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X109, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(X108, X109, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X108)
      call mem%alloc(X110, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**3, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X109, &
         wf%n_o**3, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X110, &
         wf%n_o**3)
!
      call mem%dealloc(X109)
      call mem%alloc(X111, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X111, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X112, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
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
      call add_1324_to_1234(one, X112, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X112)
!
   end subroutine jacobian_transpose_t2_qed_ccsd_2
