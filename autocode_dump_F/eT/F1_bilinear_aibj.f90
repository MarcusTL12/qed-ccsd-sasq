   subroutine F_matrix_transformation_bilinear_doubles_photon_qed_ccsd(wf, rho, Ls_vo, Ls_vovo, Rs, Rs_vo, Rt_vo, d_oo, d_ov, d_vv, s0)
!!
!! Generated function
!!
      implicit none
!
      class(qed_ccsd), intent(in) :: wf
!
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(inout) :: rho
!
      real(dp), intent(in) :: Rs, s0
      real(dp), dimension(wf%n_o,wf%n_o), intent(in) :: d_oo
      real(dp), dimension(wf%n_o,wf%n_v), intent(in) :: d_ov
      real(dp), dimension(wf%n_v,wf%n_o), intent(in) :: Ls_vo, Rs_vo, Rt_vo
      real(dp), dimension(wf%n_v,wf%n_v), intent(in) :: d_vv
      real(dp), dimension(wf%n_v,wf%n_o,wf%n_v,wf%n_o), intent(in) :: Ls_vovo
!
      real(dp) :: X4
      real(dp), dimension(:,:), allocatable :: X1, X3, X5, X6, X7, X8, X9, X11, X12
      real(dp), dimension(:,:,:,:), allocatable :: X2, X10
!
      real(dp), external :: ddot
!
      call mem%alloc(X1, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X1, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two*Rs, &
         Ls_vo, 1, &
         X1, 1, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X1)
      call mem%alloc(X2, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -Rs, &
         Ls_vo, 1, &
         d_ov, 1, &
         X2, &
         wf%n_v*wf%n_o)
!
      call add_1423_to_1234(one, X2, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X2)
!
      call dgemm('N', 'T', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         -Rs, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         d_oo, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         Rs, &
         d_vv, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%alloc(X3, wf%n_o, wf%n_v)
      call sort_to_21(Rs_vo, X3, wf%n_v, wf%n_o)
      X4 = ddot(wf%n_v*wf%n_o, X3, 1, d_ov, 1)
      call mem%dealloc(X3)
      call daxpy(wf%n_v**2*wf%n_o**2, X4, Ls_vovo, 1, rho, 1)
      call mem%alloc(X5, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -two, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X5, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X5, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X5)
      call mem%alloc(X6, wf%n_v, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -two, &
         Rs_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X6, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X6, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X6)
      call mem%alloc(X7, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         two, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X7, 1)
!
      call mem%alloc(X8, wf%n_v, wf%n_o)
      call sort_to_21(d_ov, X8, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X7, 1, &
         X8, 1, &
         rho, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X7)
      call mem%dealloc(X8)
      call mem%alloc(X9, wf%n_v, wf%n_o)
!
      call dgemv('N', &
         wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         -one, &
         Ls_vovo, &
         wf%n_v*wf%n_o, &
         Rs_vo, 1, &
         zero, &
         X9, 1)
!
      call mem%alloc(X10, wf%n_v, wf%n_o, wf%n_o, wf%n_v)
!
      call dger(wf%n_v*wf%n_o, &
         wf%n_v*wf%n_o, &
         one, &
         X9, 1, &
         d_ov, 1, &
         X10, &
         wf%n_v*wf%n_o)
!
      call mem%dealloc(X9)
      call add_1423_to_1234(one, X10, rho, wf%n_v, wf%n_o, wf%n_v, wf%n_o)
      call mem%dealloc(X10)
      call mem%alloc(X11, wf%n_o, wf%n_o)
!
      call dgemm('T', 'T', &
         wf%n_o, &
         wf%n_o, &
         wf%n_v, &
         -s0, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X11, &
         wf%n_o)
!
!
      call dgemm('N', 'N', &
         wf%n_v**2*wf%n_o, &
         wf%n_o, &
         wf%n_o, &
         one, &
         Ls_vovo, &
         wf%n_v**2*wf%n_o, &
         X11, &
         wf%n_o, &
         one, &
         rho, &
         wf%n_v**2*wf%n_o)
!
      call mem%dealloc(X11)
      call mem%alloc(X12, wf%n_v, wf%n_v)
!
      call dgemm('N', 'N', &
         wf%n_v, &
         wf%n_v, &
         wf%n_o, &
         -s0, &
         Rt_vo, &
         wf%n_v, &
         d_ov, &
         wf%n_o, &
         zero, &
         X12, &
         wf%n_v)
!
!
      call dgemm('T', 'N', &
         wf%n_v, &
         wf%n_v*wf%n_o**2, &
         wf%n_v, &
         one, &
         X12, &
         wf%n_v, &
         Ls_vovo, &
         wf%n_v, &
         one, &
         rho, &
         wf%n_v)
!
      call mem%dealloc(X12)
!
   end subroutine F_matrix_transformation_bilinear_doubles_photon_qed_ccsd

