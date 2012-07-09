module pois
!=======================================================================
!
!    pois
!
! Description:
!
!   The pois module replaces the common block POIS in globals.h
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
!  Original POIS common block:
!
!      real*8 phi,rho,rhotot,indirectx,indirecty
!      COMMON /POIS/ indirectx,indirecty,                                &
!         PHI(POT3JMAX2,POT3KMAX2,LMAX),                                 &
!         RHO(POT3JMAX2,POT3KMAX2,LMAX),                                 &
!         RHOTOT(POT3JMAX2,POT3KMAX2,LMAX)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical, private ::  alloc_flag = .FALSE. ! true if module arrays have been allocated
real(kreal)      ::  indirectx            ! ???
real(kreal)      ::  indirecty            ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  Phi            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Rho            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Rhotot         ! ???

contains
 
subroutine pois_allocate
!=======================================================================
! 
!    pois_allocate
!
! Subroutine pois_allocate allocates the pois variables.
!
!=======================================================================
use hydroparams, only : pot3jmax2, pot3kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:pois_allocate: attempt to allocate pois arrays twice. Stopping job."
   stop
else
   allocate(Phi    (pot3jmax2, pot3kmax2, lmax))
   allocate(Rho    (pot3jmax2, pot3kmax2, lmax))
   allocate(Rhotot (pot3jmax2, pot3kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine pois_allocate

subroutine pois_initialize
!=======================================================================
! 
!    pois_initialize
!
! Subroutine pois_initialize initializes the pois variables.
!
!=======================================================================
use constants, only: zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Phi    = zero
   Rho    = zero
   Rhotot = zero
else
   print *, "ERROR:pois_initialize: attempt to define unallocated pois arrays. Stopping job."
   stop
end if

end subroutine pois_initialize

end module pois
