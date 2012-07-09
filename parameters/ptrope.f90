module ptrope
!=======================================================================
!
!    ptrope
!
! Description:
!
!   The ptrope module replaces the common block PTROPE in globals.h
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
!  Original PTROPE common block:
!
!      REAL*8 KONST,NPRIME,xn,gamma,toverw
!      COMMON /PTROPE/XN,GAMMA,KONST,NPRIME,TOVERW
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

real(kreal)  ::  konst               ! ???
real(kreal)  ::  nprime              ! ???
real(kreal)  ::  xn                  ! ???
real(kreal)  ::  gamma               ! ???
real(kreal)  ::  toverw              ! ???

!---------------------------------------------------------------------

end module ptrope
