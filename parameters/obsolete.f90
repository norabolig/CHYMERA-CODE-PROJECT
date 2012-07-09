module obsolete
!=======================================================================
!
!    obsolete
!
! Description:
!
!   The obsolete module replaces the common block OBSOLETE
!
! Notes: 
!
!  Original OBSOLETE common block:
!
!      real*8 :: a1newr,a1newz
!      common/obsolete/a1newr,a1newz
!
!=======================================================================
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

real(kreal) :: a1newr, a1newz

!---------------------------------------------------------------------

end module obsolete
