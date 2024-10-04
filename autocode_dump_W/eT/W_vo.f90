   subroutine W_J_vo_terms_qed_ccsd(wf, W_J_vo, LJ_oo, LJ_ov, LJ_tr, LJ_vo, LJ_vv, Ls_vo, Ls_vovo, Rs, Rs_vo, Rs_vovo, Rt_vo, Rv_vovo, s_vo, t_vovo, u_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(inout) :: W_J_vo
!
      real(dp), intent(in) :: Rs
      real(dp), dimension(wf%eri_t1%n_J), intent(in) :: LJ_tr
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_o), intent(in) :: LJ_oo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_o,wf%n_v), intent(in) :: LJ_ov
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_o), intent(in) :: LJ_vo
      real(dp), dimension(wf%eri_t1%n_J,wf%n_v,wf%n_v), intent(in) :: LJ_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rv_vovo, t_vovo, u_vovo
!
      real(dp), dimension(:), allocatable :: X5, X27
      real(dp), dimension(:,:), allocatable :: X2, X4, X6, X7, X11, X15, X19, X20, X26, X28
      real(dp), dimension(:,:,:), allocatable :: X1, X3, X8, X9, X12, X13, X14, X16, X17, X18, X21, X23, X25
      real(dp), dimension(:,:,:,:), allocatable :: X10, X22, X24
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         LJ_tr, 1, &
         Ls_vo, 1, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%alloc(X1, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         -Rs, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X1, &
         wf%eri_t1%n_J*wf%n_o)
!
      call add_132_to_123(one, X1, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_v)
!
      call mem%alloc(X3, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X2, &
         wf%n_v, &
         zero, &
         X3, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X2)
      call add_132_to_123(one, X3, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X4, wf%n_v, wf%n_o)
      call mem%alloc(X5, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X4, 1, &
         zero, &
         X5, 1)
!
      call mem%dealloc(X4)
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X5, 1, &
         Ls_vo, 1, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X5)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         LJ_vo, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%alloc(X6, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X6, 1)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         LJ_tr, 1, &
         X6, 1, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X7, 1)
!
      call mem%alloc(X8, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_o, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J*wf%n_o, &
         X7, &
         wf%n_v, &
         zero, &
         X8, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X7)
      call add_132_to_123(one, X8, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v*wf%eri_t1%n_J, &
         wf%n_o, &
         wf%n_v, &
         one, &
         LJ_vv, &
         wf%n_v*wf%eri_t1%n_J, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X9, &
         wf%n_v*wf%eri_t1%n_J)
!
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X9, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X10, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         LJ_oo, &
         wf%eri_t1%n_J, &
         X10, &
         wf%n_o**2, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X11, &
         wf%n_v)
!
      call mem%alloc(X12, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X11, &
         wf%n_v, &
         zero, &
         X12, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X11)
      call add_132_to_123(one, X12, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X12)
      call mem%alloc(X13, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X13, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X14, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X13, &
         wf%eri_t1%n_J, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X14, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X13)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X14, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X14)
      call mem%alloc(X15, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -Rs, &
         Ls_vovo, &
         wf%n_v, &
         t_vovo, &
         wf%n_v, &
         zero, &
         X15, &
         wf%n_v)
!
      call mem%alloc(X16, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X15, &
         wf%n_v, &
         zero, &
         X16, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X15)
      call add_132_to_123(one, X16, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X16)
      call mem%alloc(X17, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call sort_to_132(LJ_ov, X17, wf%eri_t1%n_J, wf%n_o, wf%n_v)
      call mem%alloc(X18, wf%eri_t1%n_J, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X17, &
         wf%eri_t1%n_J, &
         u_vovo, &
         wf%n_v*wf%n_o, &
         zero, &
         X18, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X17)
!
      call dgemm('N', 'T', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         Rs, &
         X18, &
         wf%eri_t1%n_J, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X18)
      call mem%alloc(X19, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X19, 1)
!
      call mem%alloc(X20, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         s_vo, &
         wf%n_v, &
         X19, &
         wf%n_v, &
         zero, &
         X20, &
         wf%n_v)
!
      call mem%dealloc(X19)
      call mem%alloc(X21, wf%eri_t1%n_J, wf%n_o, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J*wf%n_o, &
         wf%n_v, &
         wf%n_v, &
         one, &
         LJ_ov, &
         wf%eri_t1%n_J*wf%n_o, &
         X20, &
         wf%n_v, &
         zero, &
         X21, &
         wf%eri_t1%n_J*wf%n_o)
!
      call mem%dealloc(X20)
      call add_132_to_123(one, X21, W_J_vo, wf%eri_t1%n_J, wf%n_v, wf%n_o)
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X22, &
         wf%n_o)
!
      call mem%alloc(X23, wf%eri_t1%n_J, wf%n_o, wf%n_o)
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
         X23, &
         wf%eri_t1%n_J*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X23, &
         wf%eri_t1%n_J, &
         X22, &
         wf%n_o**2, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X22)
      call mem%dealloc(X23)
      call mem%alloc(X24, wf%n_o, wf%n_o, wf%n_v, wf%n_o)
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
         X24, &
         wf%n_o)
!
      call mem%alloc(X25, wf%eri_t1%n_J, wf%n_o, wf%n_o)
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
         X25, &
         wf%eri_t1%n_J*wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         wf%n_o**2, &
         one, &
         X25, &
         wf%eri_t1%n_J, &
         X24, &
         wf%n_o**2, &
         one, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X24)
      call mem%dealloc(X25)
      call mem%alloc(X26, wf%n_o, wf%n_v)
      call sort_to_21(s_vo, X26, wf%n_v, wf%n_o)
      call mem%alloc(X27, wf%eri_t1%n_J)
!
      call dgemv('N', &
         wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         two, &
         LJ_ov, &
         wf%eri_t1%n_J, &
         X26, 1, &
         zero, &
         X27, 1)
!
      call mem%dealloc(X26)
      call mem%alloc(X28, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rt_vo, 1, &
         zero, &
         X28, 1)
!
!
      call dger(wf%eri_t1%n_J, &
         wf%n_v*wf%n_o, &
         one, &
         X27, 1, &
         X28, 1, &
         W_J_vo, &
         wf%eri_t1%n_J)
!
      call mem%dealloc(X27)
      call mem%dealloc(X28)
!
   end subroutine W_J_vo_terms_qed_ccsd

