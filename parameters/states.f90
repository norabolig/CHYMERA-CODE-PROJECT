Module states
!=======================================================================
!
!    states
!
! Description:
!
!   The states module replaces the common block STATES in globals.h
!
! Method:
!
! History:
! Version      Date           Author
! ----------   ----------     ---------------
! 1.0          06/23/2012     Craig Rasmussen
!
! Notes: 
!
!  Original STATES common block:
!
!      real*8 p,cv,eps,poly_constant
!      COMMON /STATES/ENON,                                              &
!           P(JMAX2,KMAX2,LMAX),                                         &
!           CV(JMAX2,KMAX2,LMAX),                                        &
!           EPS(JMAX2,KMAX2,LMAX),                                       &
!           poly_constant(JMAX2,KMAX2,LMAX)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

real(kreal)  ::  enon         ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  P              ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Cv             ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Eps            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Poly_constant  ! ???

contains
 
subroutine states_allocate(jmax2, kmax2, lmax)
!=======================================================================
! 
!    states_initialize
!
! Subroutine states_allocate allocates the states variables.
!
!=======================================================================
 
implicit none

integer, intent(in) :: jmax2, kmax2, lmax

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (.not. allocated(P))              allocate(P             (jmax2, kmax2, lmax))
if (.not. allocated(Cv))             allocate(Cv            (jmax2, kmax2, lmax))
if (.not. allocated(Eps))            allocate(Eps           (jmax2, kmax2, lmax))
if (.not. allocated(Poly_constant))  allocate(Poly_constant (jmax2, kmax2, lmax))

end subroutine states_allocate

subroutine states_initialize
!=======================================================================
! 
!    states_initialize
!
! Subroutine states_initialize initializes the states variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

P             = 0.0_kreal
Cv            = 0.0_kreal
Eps           = 0.0_kreal
Poly_constant = 0.0_kreal

end subroutine states_initialize

end module states
