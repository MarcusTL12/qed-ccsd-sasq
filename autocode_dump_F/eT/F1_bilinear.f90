   subroutine F_matrix_transformation_bilinear_photon_qed_ccsd(wf, rho, Ls1, Ls_vo, Ls_vovo, Rs_vo, Rs_vovo, Rt_vo, Rt_vovo, Rv_vovo, d_oo, d_ov, d_vv, s_vo, s_vovo, t_vovo)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), intent(inout) :: rho
!
      real(dp), intent(in) :: Ls1
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo, s_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo, Rs_vovo, Rt_vovo, Rv_vovo, s_vovo, t_vovo
!
      real(dp), dimension(:,:), allocatable :: X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20, X21, X22, X23
!
      real(dp), external :: ddot
!
      call mem%alloc(X1, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X1, wf%n_v, wf%n_o)
      rho = rho + two*Ls1 * ddot(wf%n_v*wf%n_o, X1, 1, d_ov, 1)
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rs_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X2, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X2, 1, d_oo, 1)
      call mem%dealloc(X2)
      call mem%alloc(X3, wf%n_v, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_vv, &
         wf%n_v, &
         Rs_vo, &
         wf%n_v, &
         zero, &
         X3, &
         wf%n_v)
!
      rho = rho + ddot(wf%n_v*wf%n_o, Ls_vo, 1, X3, 1)
      call mem%dealloc(X3)
      call mem%alloc(X4, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X4, wf%n_o, wf%n_v)
      call mem%alloc(X5, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         Rv_vovo, &
         wf%n_v*wf%n_o, &
         X4, 1, &
         zero, &
         X5, 1)
!
      call mem%dealloc(X4)
      rho = rho + ddot(wf%n_v*wf%n_o, Ls_vo, 1, X5, 1)
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Rt_vo, &
         wf%n_v, &
         Ls_vo, &
         wf%n_v, &
         zero, &
         X6, &
         wf%n_o)
!
      call mem%alloc(X7, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X7, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X6, 1, X7, 1)
      call mem%dealloc(X6)
      call mem%dealloc(X7)
      call mem%alloc(X8, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -one, &
         Ls_vo, &
         wf%n_v, &
         s_vo, &
         wf%n_v, &
         zero, &
         X8, &
         wf%n_o)
!
      call mem%alloc(X9, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X9, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X8, 1, X9, 1)
      call mem%dealloc(X8)
      call mem%dealloc(X9)
      call mem%alloc(X10, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Rs_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X10, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X10, 1, d_oo, 1)
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         one, &
         Ls_vovo, &
         wf%n_v, &
         Rs_vovo, &
         wf%n_v, &
         zero, &
         X11, &
         wf%n_v)
!
      rho = rho + ddot(wf%n_v**2, X11, 1, d_vv, 1)
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v)
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
         X12, &
         wf%n_v)
!
      call mem%alloc(X13, wf%n_v, wf%n_o)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_o, &
         wf%n_v, &
         one, &
         X12, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X13, &
         wf%n_v)
!
      call mem%dealloc(X12)
      rho = rho + ddot(wf%n_v*wf%n_o, X13, 1, Rs_vo, 1)
      call mem%dealloc(X13)
      call mem%alloc(X14, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         t_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X14, &
         wf%n_o)
!
      call mem%alloc(X15, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X15, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X14, 1, X15, 1)
      call mem%dealloc(X14)
      call mem%dealloc(X15)
      call mem%alloc(X16, wf%n_v, wf%n_v)
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
         X16, &
         wf%n_v)
!
      call mem%alloc(X17, wf%n_v, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X17, &
         wf%n_v)
!
      rho = rho + ddot(wf%n_v**2, X16, 1, X17, 1)
      call mem%dealloc(X16)
      call mem%dealloc(X17)
      call mem%alloc(X18, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         s_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X18, &
         wf%n_o)
!
      call mem%alloc(X19, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X19, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X18, 1, X19, 1)
      call mem%dealloc(X18)
      call mem%dealloc(X19)
      call mem%alloc(X20, wf%n_o, wf%n_o)
!
      call dgemm('T', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v**2*wf%n_o, &
         -one, &
         Rt_vovo, &
         wf%n_v**2*wf%n_o, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         zero, &
         X20, &
         wf%n_o)
!
      call mem%alloc(X21, wf%n_o, wf%n_o)
!
      call dgemm('N', 'N', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         one, &
         d_ov, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X21, &
         wf%n_o)
!
      rho = rho + ddot(wf%n_o**2, X20, 1, X21, 1)
      call mem%dealloc(X20)
      call mem%dealloc(X21)
      call mem%alloc(X22, wf%n_v, wf%n_v)
!
      call dgemm('N', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         -one, &
         Rt_vovo, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         zero, &
         X22, &
         wf%n_v)
!
      call mem%alloc(X23, wf%n_v, wf%n_v)
!
      call dgemm('T', 'T', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         one, &
         d_ov, &
         wf%n_o, &
         s_vo, &
         wf%n_v, &
         zero, &
         X23, &
         wf%n_v)
!
      rho = rho + ddot(wf%n_v**2, X22, 1, X23, 1)
      call mem%dealloc(X22)
      call mem%dealloc(X23)
!
   end subroutine F_matrix_transformation_bilinear_photon_qed_ccsd

