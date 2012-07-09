module irrad
!=======================================================================
!
!    irrad
!
! Description:
!
!   The irrad module replaces the common block IRRAD in globals.h
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
!  Original IRRAD common block:
!
!      real*8 Igamma,totirr
!      COMMON /IRRAD/                                             &
!     &     IGAMMA(JMAX2,KMAX2,LMAX),                             &
!     &     totirr
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical, private ::  alloc_flag = .FALSE. ! true if module arrays have been allocated
real(kreal)      ::  totirr               ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  Igamma     ! ???

!---------------------------------------------------------------------

contains
 
subroutine irrad_allocate
!=======================================================================
! 
!    irrad_allocate
!
! Subroutine irrad_allocate allocates the Irrad variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:irrad_allocate: attempt to allocate irrad arrays twice. Stopping job."
   stop
else
   allocate(Igamma (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine irrad_allocate

subroutine irrad_initialize
!=======================================================================
! 
!    irrad_initialize
!
! subroutine irrad_initialize initializes the irrad variables.
!
!=======================================================================
use constants, only : zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Igamma = zero
else
   print *, "ERROR:irrad_initialize: attempt to define unallocated irrad arrays. Stopping job."
   stop
end if

end subroutine irrad_initialize

end module irrad
