   subroutine jacobian_transpose_s2_2_qed_ccsd_2(wf, sigma_vovo, F_oo, F_vv, L_ovov, L_ovvo, bs2_vovo, bs_2, bs_vo, bs_vovo, d_oo, d_ov, d_vv, g_oooo, g_oovv, g_ovov, g_ovvo, g_vvvv, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: sigma_vovo
!
      real(dp), intent(in) :: bs_2
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: F_oo, d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: bs_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: F_vv, d_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_o), intent(in) :: g_oooo
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_v,wf%n_v), intent(in) :: g_oovv
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_v,wf%n_o), intent(in) :: L_ovvo, g_ovvo
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: bs2_vovo, bs_vovo, t_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_v,wf%n_v), intent(in) :: g_vvvv
!
      real(dp) :: X3, X16
      real(dp), dimension(:,:), allocatable :: X1, X15, X17, X18, X19, X20, X21, X23, X25, X48, X50
      real(dp), dimension(:,:,:,:), allocatable :: X2, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X22, X24, X26, X27, X28, X29, X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, X40, X41, X42, X43, X44, X45, X46, X47, X49, X51, X52, X53
!
      integer :: i1
!
      real(dp), external :: ddot
!
      call add_2143_to_1234(two*bs_2, L_ovov, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X1, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         eight, &
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
         -four, &
         bs_vo, 1, &
         d_ov, 1, &
         X2, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X2, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X2)
      call daxpy(wf%n_v**2*wf%n_o**2, two*wf%qed%frequencies(wf%mode), bs2_vovo, 1, sigma_vovo, 1)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         F_oo, &
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
         F_vv, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      X3 = zero
!
      do i1 = 1, wf%n_o
         X3 = X3 + d_oo(i1,i1)
      end do
!
      call daxpy(wf%n_v**2*wf%n_o**2, two*X3, bs_vovo, 1, sigma_vovo, 1)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -two, &
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
         two, &
         d_vv, &
         wf%n_v, &
         bs_vovo, &
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
         -two*wf%s0, &
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
         two*wf%s0, &
         d_vv, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%alloc(X4, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         L_ovvo, &
         wf%n_v*wf%n_o, &
         zero, &
         X4, &
         wf%n_v*wf%n_o)
!
      call add_1243_to_1234(one, X4, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         g_ovvo, &
         wf%n_v*wf%n_o, &
         zero, &
         X5, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X5, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X6, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X7, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call sort_to_1324(g_oooo, X7, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
      call mem%alloc(X8, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X6, &
         wf%n_v**2, &
         X7, &
         wf%n_o**2, &
         zero, &
         X8, &
         wf%n_v**2)
!
      call mem%dealloc(X6)
      call mem%dealloc(X7)
      call add_1324_to_1234(one, X8, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
      call sort_to_1423(bs2_vovo, X9, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X10, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1423(g_oovv, X10, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call mem%alloc(X11, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X9, &
         wf%n_v*wf%n_o, &
         X10, &
         wf%n_v*wf%n_o, &
         zero, &
         X11, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X9)
      call mem%dealloc(X10)
      call add_1423_to_1234(one, X11, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X12, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X13, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call sort_to_1324(g_vvvv, X13, wf%n_v, wf%n_v, wf%n_v, wf%n_v)
      call mem%alloc(X14, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X13, &
         wf%n_v**2, &
         X12, &
         wf%n_v**2, &
         zero, &
         X14, &
         wf%n_v**2)
!
      call mem%dealloc(X12)
      call mem%dealloc(X13)
      call add_1324_to_1234(one, X14, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X15, wf%n_o, wf%n_v)
      X16 = four * ddot(wf%n_v*wf%n_o, X15, 1, wf%s1, 1)
      call mem%dealloc(X15)
      call daxpy(wf%n_v**2*wf%n_o**2, X16, bs2_vovo, 1, sigma_vovo, 1)
      call mem%alloc(X17, wf%n_o, wf%n_o)
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
         X17, &
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
         -six, &
         d_ov, &
         wf%n_o, &
         wf%s1, &
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
         bs2_vovo, &
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
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
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
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         wf%s1, 1, &
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
      call mem%alloc(X23, wf%n_v, wf%n_v)
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
         X23, &
         wf%n_v)
!
      call mem%alloc(X24, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X23, &
         wf%n_v, &
         L_ovov, &
         wf%n_v*wf%n_o**2, &
         zero, &
         X24, &
         wf%n_v)
!
      call mem%dealloc(X23)
      call add_1432_to_1234(one, X24, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X24)
      call mem%alloc(X25, wf%n_o, wf%n_o)
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
         X25, &
         wf%n_o)
!
      call mem%alloc(X26, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(L_ovov, X26, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         X26, &
         wf%n_o, &
         X25, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X26)
      call mem%dealloc(X25)
      call mem%alloc(X27, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_2143(L_ovov, X27, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X28, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X27, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X28, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X27)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X28, &
         wf%n_v*wf%n_o, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X28)
      call mem%alloc(X29, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X29, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X30, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         X29, &
         wf%n_v*wf%n_o, &
         t_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X30, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X29)
      call mem%alloc(X31, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X30, &
         wf%n_v*wf%n_o, &
         zero, &
         X31, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X30)
      call add_1423_to_1234(one, X31, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X32, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X33, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X33, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X34, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X32, &
         wf%n_o**2, &
         X33, &
         wf%n_v**2, &
         zero, &
         X34, &
         wf%n_o**2)
!
      call mem%dealloc(X32)
      call mem%dealloc(X33)
      call mem%alloc(X35, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X35, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X36, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X35, &
         wf%n_v**2, &
         X34, &
         wf%n_o**2, &
         zero, &
         X36, &
         wf%n_v**2)
!
      call mem%dealloc(X35)
      call mem%dealloc(X34)
      call add_1324_to_1234(one, X36, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X36)
      call mem%alloc(X37, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(bs2_vovo, X37, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X38, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(t_vovo, X38, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X39, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
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
      call mem%alloc(X40, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1423(g_ovov, X40, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X41, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X39, &
         wf%n_v*wf%n_o, &
         X40, &
         wf%n_v*wf%n_o, &
         zero, &
         X41, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X39)
      call mem%dealloc(X40)
      call add_1423_to_1234(one, X41, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X41)
      call mem%alloc(X42, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(bs2_vovo, X42, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X43, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1324(t_vovo, X43, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X44, wf%n_o, wf%n_o, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o**2, &
         wf%n_o**2, &
         wf%n_v**2, &
         one, &
         X42, &
         wf%n_v**2, &
         X43, &
         wf%n_v**2, &
         zero, &
         X44, &
         wf%n_o**2)
!
      call mem%dealloc(X42)
      call mem%dealloc(X43)
      call mem%alloc(X45, wf%n_o, wf%n_o, wf%n_v, wf%n_v)
      call sort_to_1324(g_ovov, X45, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X46, wf%n_v, wf%n_v, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_v**2, &
         wf%n_o**2, &
         wf%n_o**2, &
         one, &
         X45, &
         wf%n_o**2, &
         X44, &
         wf%n_o**2, &
         zero, &
         X46, &
         wf%n_v**2)
!
      call mem%dealloc(X44)
      call mem%dealloc(X45)
      call add_1324_to_1234(one, X46, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X46)
      call mem%alloc(X47, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(wf%u_aibj, X47, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X48, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X47, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X48, &
         wf%n_o)
!
      call mem%dealloc(X47)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v**2*wf%n_o, &
         X48, &
         wf%n_o, &
         one, &
         sigma_vovo, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X48)
      call mem%alloc(X49, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X49, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X50, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X49, &
         wf%n_v*wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v, &
         zero, &
         X50, &
         wf%n_v)
!
      call mem%dealloc(X49)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X50, &
         wf%n_v, &
         bs2_vovo, &
         wf%n_v, &
         one, &
         sigma_vovo, &
         wf%n_v)
!
      call mem%dealloc(X50)
      call mem%alloc(X51, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
      call sort_to_1243(g_ovov, X51, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X52, wf%n_o, wf%n_v, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -two, &
         X51, &
         wf%n_v*wf%n_o, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         zero, &
         X52, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X51)
      call mem%alloc(X53, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         bs2_vovo, &
         wf%n_v*wf%n_o, &
         X52, &
         wf%n_v*wf%n_o, &
         zero, &
         X53, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X52)
      call add_1423_to_1234(one, X53, sigma_vovo, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X53)
!
   end subroutine jacobian_transpose_s2_2_qed_ccsd_2
