module blok6
!=======================================================================
!
!    blok6
!
! Description:
!
!   The blok6 module replaces the common block BLOK6 in globals.h
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
!  Original BLOK6 common block:
!
!      integer :: KWFW
!      real*8 dtheta, pi, grav, bgden, gsoft
!      common /blok6/dtheta, pi, grav, bgden, gsoft, KWFW
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

integer      ::  kwfw           ! ???

real(kreal)  ::  dtheta         ! ???
real(kreal)  ::  grav           ! ???
real(kreal)  ::  bgden          ! ???
real(kreal)  ::  gsoft          ! ???

!---------------------------------------------------------------------

contains

subroutine blok6_initialize
!=======================================================================
! 
!    blok6_initialize
!
! subroutine blok6_initialize initializes the blok6 variables.
!
!=======================================================================
use constants,   only : one, two
use hydroparams, only : kmax

implicit none

!---------------------------------------------------------------------

!... Initialize the variables

kwfw = int(log10(dble(kmax))/log10(two)) - 1    ! this is used in pot3.f

dtheta = one
grav   = one
bgden  = one
gsoft  = one

end subroutine blok6_initialize

end module blok6
