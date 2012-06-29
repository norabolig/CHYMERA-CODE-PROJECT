module avis
!=======================================================================
!
!    avis
!
! Description:
!
!   The avis module replaces the common block AVIS in globals.h
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
!  Original AVIS common block:
!
!      real*8 qrr,qzz,qtt,hgamma,cs,totheat
!      COMMON /AVIS/                                                     &
!           QRR(JMAX2,KMAX2,LMAX),                                       &
!           QZZ(JMAX2,KMAX2,LMAX),                                       &
!           QTT(JMAX2,KMAX2,LMAX),                                       &
!           HGAMMA(JMAX2,KMAX2,LMAX),                                    &
!           CS,totheat
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)  ::  cs                     ! ???
real(kreal)  ::  totheat                ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  Qrr            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Qzz            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Qtt            ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Hgamma         ! ???

Contains
 
subroutine avis_allocate(jmax2, kmax2, lmax)
!=======================================================================
! 
!    avis_allocate
!
! Subroutine avis_allocate allocates the Avis variables.
!
!=======================================================================
 
implicit none

integer, intent(in) :: jmax2, kmax2, lmax

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:avis_allocate: attempt to allocate avis arrays twice. Stopping job."
   stop
else
   allocate(Qrr    (jmax2, kmax2, lmax))
   allocate(Qzz    (jmax2, kmax2, lmax))
   allocate(Qtt    (jmax2, kmax2, lmax))
   allocate(Hgamma (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine avis_allocate

subroutine avis_initialize
!=======================================================================
! 
!    avis_initialize
!
! subroutine avis_initialize initializes the avis variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Qrr    = 0.0_kreal
   Qzz    = 0.0_kreal
   Qtt    = 0.0_kreal
   Hgamma = 0.0_kreal
else
   print *, "ERROR:avis_initialize: attempt to define unallocated avis arrays. Stopping job."
   stop
end if

end subroutine avis_initialize

end module avis
