module blok7
!=======================================================================
!
!    blok7
!
! Description:
!
!   The blok7 module replaces the common block BLOK7 in globals.h
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
!  Original BLOK7 common block:
!
!      real*8 rcloud,constp,delt,bdytem,den,time,cormas,epscen
!      COMMON /BLOK7/RCLOUD,CONSTP,DELT,BDYTEM,DEN,TIME,CORMAS,epscen
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables
real(kreal)  ::  rcloud               ! ???
real(kreal)  ::  constp               ! ???
real(kreal)  ::  delt                 ! ???
real(kreal)  ::  bdytem               ! ???
real(kreal)  ::  den                  ! ???
real(kreal)  ::  time                 ! ???
real(kreal)  ::  cormas               ! ???
real(kreal)  ::  epscen               ! ???

!---------------------------------------------------------------------

end module blok7
