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

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)  ::  enon                   ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  P              ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Cv             ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Eps            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Poly_constant  ! ???

contains
 
subroutine states_allocate
!=======================================================================
! 
!    states_initialize
!
! Subroutine states_allocate allocates the states variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:states_allocate: attempt to allocate states arrays twice. Stopping job."
   stop
else
   allocate(P             (jmax2, kmax2, lmax))
   allocate(Cv            (jmax2, kmax2, lmax))
   allocate(Eps           (jmax2, kmax2, lmax))
   allocate(Poly_constant (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

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

if (alloc_flag) then
   P             = 0.0_kreal
   Cv            = 0.0_kreal
   Eps           = 0.0_kreal
   Poly_constant = 0.0_kreal
else
   print *, "ERROR:states_initialize: attempt to define unallocated states arrays. Stopping job."
   stop
end if

end subroutine states_initialize

end module states
