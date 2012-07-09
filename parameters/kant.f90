module kant
!=======================================================================
!
!    kant
!
! Description:
!
!   The kant module replaces the common block KANT in globals.h
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
!  Original KANT common block:
!
!      logical radtrigger
!      integer HHIT, HCOUNT, LO, CCOUNTER, HCOUNTER
!      common /kant/radtrigger,HHIT,HCOUNT,CCOUNTER,HCOUNTER,LO
!
!=======================================================================
implicit none
save
 
!... Scalar variables

logical :: radtrigger         ! ???
integer :: hhit               ! ???
integer :: hcount             ! ???
integer :: lo                 ! ???
integer :: ccounter           ! ???
integer :: hcounter           ! ???

!---------------------------------------------------------------------

end module kant
