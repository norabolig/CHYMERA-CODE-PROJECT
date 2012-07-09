module misc_module
!=======================================================================
!
!    misc_module
!
! Description:
!
!   The misc_module module replaces the common block MISC
!
! Notes: 
!
!  Original MISC common block:
!
!      real*8 :: EPSJR,RHOJR,OMMAX
!      COMMON  /MISC/EPSJR,RHOJR,OMMAX
!
!=======================================================================
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

real(kreal) :: epsjr, rhojr, ommax

!---------------------------------------------------------------------

end module misc_module
