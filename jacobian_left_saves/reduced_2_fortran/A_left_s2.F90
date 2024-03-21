   subroutine jacobian_transpose_s2_qed_ccsd_2(wf, sigma_vovo, F_ov, L_ooov, L_ovoo, L_ovov, L_ovvv, L_vvov, bs2_vovo, bs_vo, d_oo, d_ov, d_vv, g_ooov, g_ovoo, g_ovov, g_ovvv, g_vvov, s2, v_vovo)
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
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, s2, v_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp) :: X11
      real(dp), dimension(:,:), allocatable :: X1, X3, X8, X9, X10, X12, X13, X14, X15, X16, X18, X19, X24, X25, X26, X27, X47, X49, X72, X74
      real(dp), dimension(:,:,:,:), allocatable :: X2, X4, X5, X6, X7, X17, X20, X21, X22, X23, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X48, X50, X51, X52, X53, X54, X55, X56, X57, X58, X59, X60, X61, X62, X63, X64, X65, X66, X67, X68, X69, X70, X71, X73, X75, X76, X77
!
      real(dp), external :: ddot
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(F_ov, X1, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X1, 1, &
         bs_vo, 1, &
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
         F_ov, 1, &
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
         two*wf%s0, &
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
         -wf%s0, &
         bs_vo, 1, &
         d_ov, 1, &
         X4, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X4, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_3214(L_ovoo, X5, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_o, &
         -one, &
         bs_vo, &
         wf%n_v, &
         X5, &
         wf%n_v*wf%n_o**2, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1243(L_ovvv, X6, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X7, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X6, &
         wf%n_v**2*wf%n_o, &
         bs_vo, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X6)
      call add_2134_to_1234(one, X7, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X7)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         bs2_vovo, &
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
         two, &
         d_vv, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -four*wf%s0_2, &
         bs2_vovo, &
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
         four*wf%s0_2, &
         d_vv, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%alloc(X8, wf%n_o, wf%n_o)
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
         X8, &
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
         X8, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_v)
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
         X9, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X9, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X10, wf%n_o, wf%n_v)
      X11 = two * ddot(wf%n_v*wf%n_o, X10, 1, wf%s1_2, 1)
      call mem%dealloc(X10)
      call daxpy(wf%n_v**2*wf%n_o**2, X11, bs2_vovo, 1, sigma_vovo, 1)
      call mem%alloc(X12, wf%n_o, wf%n_o)
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
         X12, &
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
         X12, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_v, wf%n_v)
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
         X13, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X13, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         eight, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X14, 1)
!
      call mem%alloc(X15, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X15, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X14, 1, &
         X15, 1, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X14)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -four, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1_2, 1, &
         zero, &
         X16, 1)
!
      call mem%alloc(X17, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X16, 1, &
         d_ov, 1, &
         X17, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X16)
      call add_1423_to_1234(one, X17, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_o)
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
         X18, &
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
         X18, &
         wf%n_o, &
         one, &
         sigma_vovo, &
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
         d_ov, &
         wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X19, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         wf%s0, &
         X19, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         two, &
         L_ovvv, &
         wf%n_v**2*wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X20, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X21, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X20, &
         wf%n_v*wf%n_o, &
         zero, &
         X21, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X20)
      call add_1243_to_1234(one, X21, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         -two, &
         wf%s1, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         zero, &
         X22, &
         wf%n_o)
!
      call mem%alloc(X23, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_2134(L_ovoo, X23, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X23, &
         wf%n_v*wf%n_o, &
         X22, &
         wf%n_o**2, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X23)
      call mem%dealloc(X22)
      call mem%alloc(X24, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X24, wf%n_v, wf%n_o)
      call mem%alloc(X25, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ooov, &
         wf%n_o**2, &
         X24, 1, &
         zero, &
         X25, 1)
!
      call mem%dealloc(X24)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X25, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X26, wf%n_v, wf%n_o)
      call mem%alloc(X27, wf%n_v, wf%n_v)
!
      call dgemv('N', &
         wf%n_v**2, &
         wf%n_v*wf%n_o, &
         two, &
         L_vvov, &
         wf%n_v**2, &
         X26, 1, &
         zero, &
         X27, 1)
!
      call mem%dealloc(X26)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X27, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
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
         X28, &
         wf%n_v**2*wf%n_o)
!
      call mem%alloc(X29, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X28, &
         wf%n_v*wf%n_o, &
         zero, &
         X29, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X28)
      call add_1423_to_1234(one, X29, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X30, &
         wf%n_o)
!
      call mem%alloc(X31, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X30, &
         wf%n_o**2, &
         g_ovoo, &
         wf%n_v*wf%n_o, &
         zero, &
         X31, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X30)
      call add_1423_to_1234(one, X31, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
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
         X32, &
         wf%n_o**3)
!
      call mem%alloc(X33, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X33, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X34, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(X32, X34, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%dealloc(X32)
      call mem%alloc(X35, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X33, &
         wf%n_v**2, &
         X34, &
         wf%n_o**2, &
         zero, &
         X35, &
         wf%n_v**2)
!
      call mem%dealloc(X33)
      call mem%dealloc(X34)
      call add_1324_to_1234(one, X35, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X35)
      call mem%alloc(X36, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
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
         X36, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X37, wf%n_v, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_4123(g_ooov, X37, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X38, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X37, &
         wf%n_v*wf%n_o, &
         X36, &
         wf%n_v*wf%n_o, &
         zero, &
         X38, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X36)
      call mem%dealloc(X37)
      call add_1423_to_1234(one, X38, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X38)
      call mem%alloc(X39, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1432(g_ovvv, X39, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X40, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         X39, &
         wf%n_v**2*wf%n_o, &
         wf%s1, &
         wf%n_v, &
         zero, &
         X40, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X39)
      call mem%alloc(X41, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(bs2_vovo, X41, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X42, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X41, &
         wf%n_v*wf%n_o, &
         X40, &
         wf%n_v*wf%n_o, &
         zero, &
         X42, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X41)
      call mem%dealloc(X40)
      call add_1423_to_1234(one, X42, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         wf%s1, &
         wf%n_v, &
         zero, &
         X43, &
         wf%n_v*wf%n_o**2)
!
      call mem%alloc(X44, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X43, X44, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X43)
      call mem%alloc(X45, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvov, X45, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X46, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         one, &
         X45, &
         wf%n_v*wf%n_o, &
         X44, &
         wf%n_o**2, &
         zero, &
         X46, &
         wf%n_v**2)
!
      call mem%dealloc(X44)
      call mem%dealloc(X45)
      call add_1342_to_1234(one, X46, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         zero, &
         X47, &
         wf%n_v)
!
      call mem%alloc(X48, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X47, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X48, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call add_1432_to_1234(one, X48, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
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
         s2, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X49, &
         wf%n_o)
!
      call mem%alloc(X50, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X50, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X50, &
         wf%n_o, &
         X49, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X50)
      call mem%dealloc(X49)
      call mem%alloc(X51, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_2143(L_ovov, X51, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X52, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X51, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X52, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X51)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X52, &
         wf%n_v*wf%n_o, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X52)
      call mem%alloc(X53, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X53, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X54, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X53, &
         wf%n_v*wf%n_o, &
         s2, &
         wf%n_v*wf%n_o, &
         zero, &
         X54, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X53)
      call mem%alloc(X55, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X54, &
         wf%n_v*wf%n_o, &
         zero, &
         X55, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X54)
      call add_1423_to_1234(one, X55, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X55)
      call mem%alloc(X56, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X56, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X57, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2, X57, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X58, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X56, &
         wf%n_o**2, &
         X57, &
         wf%n_v**2, &
         zero, &
         X58, &
         wf%n_o**2)
!
      call mem%dealloc(X56)
      call mem%dealloc(X57)
      call mem%alloc(X59, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X59, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X60, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X59, &
         wf%n_v**2, &
         X58, &
         wf%n_o**2, &
         zero, &
         X60, &
         wf%n_v**2)
!
      call mem%dealloc(X59)
      call mem%dealloc(X58)
      call add_1324_to_1234(one, X60, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X60)
      call mem%alloc(X61, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X61, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X62, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(s2, X62, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X63, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X61, &
         wf%n_v*wf%n_o, &
         X62, &
         wf%n_v*wf%n_o, &
         zero, &
         X63, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X61)
      call mem%dealloc(X62)
      call mem%alloc(X64, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(bs2_vovo, X64, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X65, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X64, &
         wf%n_v*wf%n_o, &
         X63, &
         wf%n_v*wf%n_o, &
         zero, &
         X65, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X64)
      call mem%dealloc(X63)
      call add_1423_to_1234(one, X65, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X65)
      call mem%alloc(X66, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X66, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X67, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(s2, X67, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X68, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X66, &
         wf%n_v**2, &
         X67, &
         wf%n_v**2, &
         zero, &
         X68, &
         wf%n_o**2)
!
      call mem%dealloc(X66)
      call mem%dealloc(X67)
      call mem%alloc(X69, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X69, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X70, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X69, &
         wf%n_o**2, &
         X68, &
         wf%n_o**2, &
         zero, &
         X70, &
         wf%n_v**2)
!
      call mem%dealloc(X68)
      call mem%dealloc(X69)
      call add_1324_to_1234(one, X70, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X70)
      call mem%alloc(X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v_vovo, X71, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X72, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X71, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X72, &
         wf%n_o)
!
      call mem%dealloc(X71)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X72, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X72)
      call mem%alloc(X73, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X73, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X74, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X73, &
         wf%n_v*wf%n_o**2, &
         v_vovo, &
         wf%n_v, &
         zero, &
         X74, &
         wf%n_v)
!
      call mem%dealloc(X73)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X74, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X74)
      call mem%alloc(X75, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1243(g_ovov, X75, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X76, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X75, &
         wf%n_v*wf%n_o, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X76, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X75)
      call mem%alloc(X77, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X76, &
         wf%n_v*wf%n_o, &
         zero, &
         X77, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X76)
      call add_1423_to_1234(one, X77, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X77)
!
   end subroutine jacobian_transpose_s2_qed_ccsd_2
