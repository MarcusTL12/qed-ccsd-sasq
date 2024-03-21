   subroutine jacobian_transpose_s1_2_qed_ccsd_2(wf, sigma_vo, F_ov, L_ovoo, L_ovvv, bs, bs2_vovo, bs_2, bs_vo, bs_vovo, d_oo, d_ov, d_vo, d_vv, g_ooov, g_oovo, g_ovoo, g_ovvv, g_vvov, g_vvvo, s2, t_vovo, v_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: sigma_vo
!
      real(dp), intent(in) :: bs, bs_2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo, d_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: g_ooov
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_o), intent(in) :: g_oovo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_o), intent(in) :: L_ovoo, g_ovoo
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_v), intent(in) :: L_ovvv, g_ovvv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, bs_vovo, s2, t_vovo, v_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: g_vvov
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_v,wf%n_o), intent(in) :: g_vvvo
!
      real(dp) :: X1, X16
      real(dp), dimension(:,:), allocatable :: X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X17, X18, X19, X20, X21, X22, X23, X24
      real(dp), dimension(:,:,:,:), allocatable :: X2, X3, X25, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X48
!
      integer :: i1
!
      real(dp), external :: ddot
!
      call add_21_to_12(four*bs_2, F_ov, sigma_vo, wf%n_v, wf%n_o)
      call add_21_to_12(four*bs, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call add_21_to_12(four*bs_2*wf%s0, d_ov, sigma_vo, wf%n_v, wf%n_o)
      X1 = zero
!
      do i1 = 1, wf%n_o
         X1 = X1 + d_oo(i1,i1)
      end do
!
      call daxpy(wf%n_v*wf%n_o, eight*X1, bs_vo, 1, sigma_vo, 1)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -four, &
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
         four, &
         d_vv, &
         wf%n_v, &
         bs_vo, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         d_vo, 1, &
         one, &
         sigma_vo, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         -two, &
         bs2_vovo, &
         wf%n_v, &
         g_oovo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%alloc(X2, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X2, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X3, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1342(g_vvvo, X3, wf%n_v, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         two, &
         X3, &
         wf%n_v**2*wf%n_o, &
         X2, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X2)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
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
         X4, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X4, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o)
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
         X5, &
         wf%n_v)
!
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X5, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
         zero, &
         X6, 1)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X6, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
         zero, &
         X7, 1)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         X7, &
         wf%n_v, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_v, wf%n_v)
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
         X8, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X8, &
         wf%n_v, &
         F_ov, &
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
         wf%n_v**2*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X9, &
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
         wf%n_v*wf%n_o**2, &
         -two, &
         bs_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X10, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X10, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_o)
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
         X11, &
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
         X11, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X12, wf%n_o, wf%n_v)
      call mem%alloc(X13, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         X12, 1, &
         zero, &
         X13, 1)
!
      call mem%dealloc(X12)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs_vovo, &
         wf%n_v*wf%n_o, &
         X13, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -six, &
         bs2_vovo, &
         wf%n_v, &
         s2, &
         wf%n_v, &
         zero, &
         X14, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X14, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -six, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         s2, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X15, &
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
         X15, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X15)
      X16 = two * ddot(wf%n_v**2*wf%n_o**2, bs2_vovo, 1, s2, 1)
      call add_21_to_12(X16, d_ov, sigma_vo, wf%n_v, wf%n_o)
      call mem%alloc(X17, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X17, wf%n_o, wf%n_v)
      call mem%alloc(X18, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four, &
         v_vovo, &
         wf%n_v*wf%n_o, &
         X17, 1, &
         zero, &
         X18, 1)
!
      call mem%dealloc(X17)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X18, 1, &
         one, &
         sigma_vo, 1)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_v)
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
         X19, &
         wf%n_v)
!
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         wf%s0, &
         X19, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_o)
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
         X20, &
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
         X20, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X20)
      call mem%alloc(X21, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         two, &
         bs2_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_v)
!
      call mem%alloc(X22, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v**2, &
         one, &
         L_ovvv, &
         wf%n_v*wf%n_o, &
         X21, 1, &
         zero, &
         X22, 1)
!
      call mem%dealloc(X21)
      call add_21_to_12(one, X22, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X23, &
         wf%n_o)
!
      call mem%alloc(X24, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         L_ovoo, &
         wf%n_v*wf%n_o, &
         X23, 1, &
         zero, &
         X24, 1)
!
      call mem%dealloc(X23)
      call add_21_to_12(one, X24, sigma_vo, wf%n_v, wf%n_o)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovvv, X25, wf%n_o, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X26, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X26, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X27, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         wf%n_v**2, &
         -two, &
         X25, &
         wf%n_v*wf%n_o, &
         X26, &
         wf%n_v**2, &
         zero, &
         X27, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X25)
      call mem%dealloc(X26)
      call mem%alloc(X28, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1324(X27, X28, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%dealloc(X27)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X28, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovoo, X29, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X30, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X29, &
         wf%n_o**2, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X30, &
         wf%n_o**2)
!
      call mem%dealloc(X29)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X30, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X30)
      call mem%alloc(X31, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovoo, X31, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call mem%alloc(X32, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(t_vovo, X32, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X33, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X31, &
         wf%n_o**2, &
         X32, &
         wf%n_v*wf%n_o, &
         zero, &
         X33, &
         wf%n_o**2)
!
      call mem%dealloc(X31)
      call mem%dealloc(X32)
      call mem%alloc(X34, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X33, X34, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X33)
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
      call mem%alloc(X35, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X35, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X36, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(X35, X36, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X35)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X36, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(bs2_vovo, X37, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X38, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X38, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X39, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X37, &
         wf%n_v*wf%n_o, &
         X38, &
         wf%n_v*wf%n_o, &
         zero, &
         X39, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X37)
      call mem%dealloc(X38)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         g_vvov, &
         wf%n_v**2*wf%n_o, &
         X39, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X39)
      call mem%alloc(X40, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1342(bs2_vovo, X40, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X41, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X41, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X42, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         two, &
         X41, &
         wf%n_v**2, &
         X40, &
         wf%n_v**2, &
         zero, &
         X42, &
         wf%n_o**2)
!
      call mem%dealloc(X40)
      call mem%dealloc(X41)
      call mem%alloc(X43, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1342(g_ovoo, X43, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o**3, &
         one, &
         X43, &
         wf%n_o**3, &
         X42, &
         wf%n_o**3, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X42)
      call mem%dealloc(X43)
      call mem%alloc(X44, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1243(g_ooov, X44, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
      call mem%alloc(X45, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X44, &
         wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X45, &
         wf%n_o**2)
!
      call mem%dealloc(X44)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         one, &
         bs2_vovo, &
         wf%n_v, &
         X45, &
         wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X45)
      call mem%alloc(X46, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X46, &
         wf%n_v*wf%n_o)
!
      call mem%alloc(X47, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(X46, X47, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X46)
      call mem%alloc(X48, wf%n_v, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1342(g_vvov, X48, wf%n_v, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         one, &
         X48, &
         wf%n_v**2*wf%n_o, &
         X47, &
         wf%n_v**2*wf%n_o, &
         one, &
         sigma_vo, &
         wf%n_v)
!
      call mem%dealloc(X47)
      call mem%dealloc(X48)
!
   end subroutine jacobian_transpose_s1_2_qed_ccsd_2
