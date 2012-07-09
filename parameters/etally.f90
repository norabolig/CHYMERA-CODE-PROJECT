module etally
!=======================================================================
!
!    etally
!
! Description:
!
!   The etally module replaces the common block ETALLY in globals.h
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
!  Original ETALLY common block:
!
!      real*8 etotfl, efl, eflufftot, gamma1
!      COMMON /etally/ etotfl, efl, eflufftot, gamma1(JMAX2,KMAX2,LMAX)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)  ::  etotfl                 ! ???
real(kreal)  ::  efl                    ! ???
real(kreal)  ::  eflufftot              ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  Gamma1     ! ???

!---------------------------------------------------------------------

contains
 
subroutine etally_allocate
!=======================================================================
! 
!    etally_allocate
!
! Subroutine etally_allocate allocates the Etally variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:etally_allocate: attempt to allocate etally arrays twice. Stopping job."
   stop
else
   allocate(Gamma1 (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine etally_allocate

subroutine etally_initialize
!=======================================================================
! 
!    etally_initialize
!
! subroutine etally_initialize initializes the etally variables.
!
!=======================================================================
use constants, only : zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Gamma1 = zero
else
   print *, "ERROR:etally_initialize: attempt to define unallocated etally arrays. Stopping job."
   stop
end if

end subroutine etally_initialize

end module etally
