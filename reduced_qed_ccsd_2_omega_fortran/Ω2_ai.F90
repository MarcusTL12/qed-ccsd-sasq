   subroutine omega_2_ai_qed_ccsd_2(wf, omega_vo, F_oo, F_ov, F_vv, L_ooov, L_ovov, L_voov, L_vvov, d_oo, d_ov, d_vv, g_ooov, g_ovov, g_vvov, v₁_vovo, v₂_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd_2), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o), intent(inout) :: omega_vo
!
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: F_oo, d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: F_ov, d_ov
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: F_vv, d_vv
      real(dp), dimension(wf%n_o,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_ooov, g_ooov
      real(dp), dimension(wf%n_o,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_ovov, g_ovov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_o,wf%n_v), intent(in) :: L_voov
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: v₁_vovo, v₂_vovo
      real(dp), dimension(wf%n_v,wf%n_v,wf%n_o,wf%n_v), intent(in) :: L_vvov, g_vvov
!
      real(dp) :: X6, X10
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X7, X8, X9, X11, X12, X13, X16, X17, X18, X19, X20, X21, X22, X23, X24, X25, X27, X29, X31, X33
      real(dp), dimension(:,:,:,:), allocatable :: X14, X15, X26, X28, X30, X32
!
      real(dp), external :: ddot
!
      call daxpy(wf%n_v*wf%n_o, four*wf%qed%frequencies(wf%mode), wf%s1_2, 1, omega_vo, 1)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         two, &
         F_vv, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         wf%s1_2, &
         wf%n_v, &
         F_oo, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         two, &
         d_vv, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -two, &
         wf%s1, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         four*wf%s0_1, &
         d_vv, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -four*wf%s0_1, &
         wf%s1, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         two*wf%s0, &
         d_vv, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         -two*wf%s0, &
         wf%s1_2, &
         wf%n_v, &
         d_oo, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(F_ov, X1, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         X1, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X2, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_voov, &
         wf%n_v*wf%n_o, &
         X2, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X3, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         X3, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_o, wf%n_o)
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
         X4, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X4, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X4)
      call mem%alloc(X5, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X5, wf%n_o, wf%n_v)
      X6 = four * ddot(wf%n_v*wf%n_o, X5, 1, wf%s1_2, 1)
      call mem%dealloc(X5)
      call daxpy(wf%n_v*wf%n_o, X6, wf%s1, 1, omega_vo, 1)
      call mem%alloc(X7, wf%n_o, wf%n_o)
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
         X7, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X7, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_o, wf%n_o)
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
         X8, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X8, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X9, wf%n_o, wf%n_v)
      X10 = eight * ddot(wf%n_v*wf%n_o, X9, 1, wf%s1, 1)
      call mem%dealloc(X9)
      call daxpy(wf%n_v*wf%n_o, X10, wf%s1_2, 1, omega_vo, 1)
      call mem%alloc(X11, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X11, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         four*wf%s0_1, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         X11, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X12, wf%n_o, wf%n_v)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*wf%s0, &
         v₂_vovo, &
         wf%n_v*wf%n_o, &
         X12, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%n_o, wf%n_o)
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
         X13, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         wf%s0, &
         wf%s1, &
         wf%n_v, &
         X13, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v₂_vovo, X14, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         two, &
         g_vvov, &
         wf%n_v, &
         X14, &
         wf%n_v**2*wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_o, wf%n_v, wf%n_o, wf%n_o)
      call sort_to_1432(g_ooov, X15, wf%n_o, wf%n_o, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v*wf%n_o**2, &
         -two, &
         v₂_vovo, &
         wf%n_v, &
         X15, &
         wf%n_v*wf%n_o**2, &
         one, &
         omega_vo, &
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
         two, &
         L_vvov, &
         wf%n_v**2, &
         X16, 1, &
         zero, &
         X17, 1)
!
      call mem%dealloc(X16)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X17, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X18, wf%n_v, wf%n_o)
      call mem%alloc(X19, wf%n_o, wf%n_o)
!
      call dgemv('N', &
         wf%n_o**2, &
         wf%n_v*wf%n_o, &
         -two, &
         L_ooov, &
         wf%n_o**2, &
         X18, 1, &
         zero, &
         X19, 1)
!
      call mem%dealloc(X18)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X19, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1, X20, wf%n_v, wf%n_o)
      call mem%alloc(X21, wf%n_o, wf%n_v)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X20, 1, &
         zero, &
         X21, 1)
!
      call mem%dealloc(X20)
      call mem%alloc(X22, wf%n_v, wf%n_o)
      call sort_to_21(X21, X22, wf%n_o, wf%n_v)
      call mem%dealloc(X21)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         v₁_vovo, &
         wf%n_v*wf%n_o, &
         X22, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X22)
      call mem%alloc(X23, wf%n_o, wf%n_v)
      call sort_to_21(wf%s1_2, X23, wf%n_v, wf%n_o)
      call mem%alloc(X24, wf%n_o, wf%n_v)
!
      call dgemv('T', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         L_ovov, &
         wf%n_v*wf%n_o, &
         X23, 1, &
         zero, &
         X24, 1)
!
      call mem%dealloc(X23)
      call mem%alloc(X25, wf%n_v, wf%n_o)
      call sort_to_21(X24, X25, wf%n_o, wf%n_v)
      call mem%dealloc(X24)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         wf%u_aibj, &
         wf%n_v*wf%n_o, &
         X25, 1, &
         one, &
         omega_vo, 1)
!
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(v₁_vovo, X26, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X27, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X26, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X27, &
         wf%n_o)
!
      call mem%dealloc(X26)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1, &
         wf%n_v, &
         X27, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X27)
      call mem%alloc(X28, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X28, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X29, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X28, &
         wf%n_v*wf%n_o**2, &
         v₁_vovo, &
         wf%n_v, &
         zero, &
         X29, &
         wf%n_v)
!
      call mem%dealloc(X28)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X29, &
         wf%n_v, &
         wf%s1, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X29)
      call mem%alloc(X30, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call sort_to_1432(wf%u_aibj, X30, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%alloc(X31, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -two, &
         g_ovov, &
         wf%n_o, &
         X30, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X31, &
         wf%n_o)
!
      call mem%dealloc(X30)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_o, &
         one, &
         wf%s1_2, &
         wf%n_v, &
         X31, &
         wf%n_o, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X31)
      call mem%alloc(X32, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call sort_to_1432(g_ovov, X32, wf%n_o, wf%n_v, wf%n_o, wf%n_v)
      call mem%alloc(X33, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -two, &
         X32, &
         wf%n_v*wf%n_o**2, &
         wf%u_aibj, &
         wf%n_v, &
         zero, &
         X33, &
         wf%n_v)
!
      call mem%dealloc(X32)
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X33, &
         wf%n_v, &
         wf%s1_2, &
         wf%n_v, &
         one, &
         omega_vo, &
         wf%n_v)
!
      call mem%dealloc(X33)
!
   end subroutine omega_2_ai_qed_ccsd_2
