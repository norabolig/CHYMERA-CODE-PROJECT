module convert
!=======================================================================
!
!    convert
!
! Description:
!
!   The convert module replaces the common block CONVERT in globals.h
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
!  Original CONVERT common block:
!
!      real*8 Msyscgs,PKcgs,Tconv,Sconv,Dconv,Pconv,sigma,rhoconv,     &
!     &       engconv,bkmpcode
!      COMMON /CONVERT/                                                &
!     &     Msyscgs,PKcgs,Tconv,Sconv,                                 &
!     &     dconv,Pconv,sigma,rhoconv,engconv,bkmpcode
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables
real(kreal)  ::  msyscgs               ! ???
real(kreal)  ::  pkcgs                 ! ???
real(kreal)  ::  tconv                 ! ???
real(kreal)  ::  sconv                 ! ???
real(kreal)  ::  dconv                 ! ???
real(kreal)  ::  pconv                 ! ???
real(kreal)  ::  sigma                 ! ???
real(kreal)  ::  rhoconv               ! ???
real(kreal)  ::  engconv               ! ???
real(kreal)  ::  bkmpcode              ! ???

!---------------------------------------------------------------------

end module convert
