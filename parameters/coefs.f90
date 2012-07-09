module coefs
!=======================================================================
!
!    coefs
!
! Description:
!
!   The coefs module replaces the common block COEFS
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
!  Original COEFS common block:
!
!     COMMON /COEFS/COEF(POT3JMAX2,POT3KMAX2,LMAX2,2)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical, private ::  alloc_flag = .FALSE. ! true if module arrays have been allocated

!... Array variables

real(kreal), dimension(:,:,:,:), allocatable  ::  Coef  ! ???

!---------------------------------------------------------------------

contains
 
subroutine coefs_allocate
!=======================================================================
! 
!    coefs_allocate
!
! Subroutine coefs_allocate allocates the Coefs variables.
!
!=======================================================================
use hydroparams, only : pot3jmax2, pot3kmax2, lmax2
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:coefs_allocate: attempt to allocate coefs arrays twice. Stopping job."
   stop
else
   allocate(Coef(pot3jmax2, pot3kmax2, lmax2, 2))
   alloc_flag = .TRUE.
end if

end subroutine coefs_allocate

subroutine coefs_initialize
!=======================================================================
! 
!    coefs_initialize
!
! subroutine coefs_initialize initializes the coefs variables.
!
!=======================================================================
use constants, only : zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Coef = zero
else
   print *, "ERROR:coefs_initialize: attempt to define unallocated coefs arrays. Stopping job."
   stop
end if

end subroutine coefs_initialize

end module coefs
