module inside
!=======================================================================
!
!    inside
!
! Description:
!
!   The inside module replaces the common block INSIDE
!
! Notes: 
!
!  Original INSIDE common block:
!
!      COMMON /INSIDE/TMASS,ENEW,ELOST,EDIF,PHICHK,KLOCAT
!
!=======================================================================
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

integer     :: klocat
real(kreal) :: tmass, enew, elost, edif, phichk

!---------------------------------------------------------------------

end module inside
