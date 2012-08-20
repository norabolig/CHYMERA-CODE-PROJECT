module timest
!=======================================================================
!
!    timest
!
! Description:
!
!   The timest module replaces the common block TIMEST & ITS
!
! Notes: 
!
!  Original TIMEST common block (from io.f, 3dhyd.f, housekeeping.f):
!           ITS common block (from io.f, 3dhyd.f)

!      COMMON /TIMEST/INDX,ISOADI,ALLOW,DMAX,CHGMAX
!      COMMON /ITS/ITSTRT,ITSTOP,ITSTEP
!=======================================================================
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

integer     :: indx,isoadi,itstrt,itstop,itstep
real(kreal) :: allow,dmax,chgmax

contains 

   subroutine timest_initialize
      
      use constants, only : zero
     
! begin initializing 

     indx      = zero
     isoadi    = zero
     itstrt    = zero
     itstop    = zero
     itstep    = zero
     
!---------------------------------------------------------------------
   end subroutine
end module timest
