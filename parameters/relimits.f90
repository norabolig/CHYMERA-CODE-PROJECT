module relimits
!=======================================================================
!
!    relimits
!
! Description:
!
!   The relimits module replaces the common block RELIMITS in globals.h
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
!  Original RELIMITS common block:
!
!      real*8 rholmt,epslmt,dumlmt,sound,rholmt_p,rhoa
!      COMMON /RELIMITS/RHOLMT,EPSLMT,DUMLMT,sound,rholmt_p,rhoa
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

real(kreal)  ::  rholmt              ! ???
real(kreal)  ::  epslmt              ! ???
real(kreal)  ::  dumlmt              ! ???
real(kreal)  ::  sound               ! ???
real(kreal)  ::  rholmt_p            ! ???
real(kreal)  ::  rhoa                ! ???

!---------------------------------------------------------------------

end module relimits
