module cooling
!=======================================================================
!
!    cooling
!
! Description:
!
!   The cooling module replaces the common block COOLING in globals.h
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
!  Original COOLING common block:
!
!      real*8 lambda,tau,TempK,TeffK,TphK,Surfcgs,divflux,radflux
!      real*8 totcool,totdflux
!      COMMON /COOLING/                                                  &
!           LAMBDA(JMAX2,KMAX2,LMAX),                                    &
!           TAU(JMAX2,KMAX2,LMAX,4),                                     &
!           TEMPK(JMAX2,KMAX2,LMAX),                                     &
!           TEFFK(JMAX2,LMAX),                                           &
!           TPHK(JMAX2,LMAX),                                            &
!           SURFCGS(JMAX2,LMAX),                                         & 
!           DIVFLUX(JMAX2,KMAX2,LMAX),                                   &
!           RADFLUX(JMAX2,KMAX2,LMAX,3),                                 &
!           totcool,totdflux
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)  ::  totcool                ! ???
real(kreal)  ::  totdflux               ! ???

!... Array variables

real(kreal), dimension(:,:,:),   allocatable  ::  Lambda         ! ???
real(kreal), dimension(:,:,:,:), allocatable  ::  Tau            ! ???
real(kreal), dimension(:,:,:),   allocatable  ::  TempK          ! ???
real(kreal), dimension(:,:),     allocatable  ::  TeffK          ! ???
real(kreal), dimension(:,:),     allocatable  ::  TphK           ! ???
real(kreal), dimension(:,:),     allocatable  ::  Surfcgs        ! ???
real(kreal), dimension(:,:,:),   allocatable  ::  Divflux        ! ???
real(kreal), dimension(:,:,:,:), allocatable  ::  Radflux        ! ???

contains
 
subroutine cooling_allocate(jmax2, kmax2, lmax)
!=======================================================================
! 
!    cooling_allocate
!
! Subroutine cooling_allocate allocates the cooling variables.
!
!=======================================================================
 
implicit none

integer, intent(in) :: jmax2, kmax2, lmax

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:cooling_allocate: attempt to allocate cooling arrays twice. Stopping job."
   stop
else
   allocate(Lambda  (jmax2, kmax2, lmax))
   allocate(Tau     (jmax2, kmax2, lmax, 4))
   allocate(TempK   (jmax2, kmax2, lmax))
   allocate(TeffK   (jmax2, lmax))
   allocate(TphK    (jmax2, lmax))
   allocate(Surfcgs (jmax2, lmax))
   allocate(Divflux (jmax2, kmax2, lmax))
   allocate(Radflux (jmax2, kmax2, lmax, 3))
   alloc_flag = .TRUE.
end if

End subroutine cooling_allocate

subroutine cooling_initialize
!=======================================================================
! 
!    cooling_initialize
!
! Subroutine cooling_initialize initializes the cooling variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Lambda  = 0.0_kreal
   Tau     = 0.0_kreal
   Tempk   = 0.0_kreal
   Teffk   = 0.0_kreal
   Tphk    = 0.0_kreal
   Surfcgs = 0.0_kreal
   Divflux = 0.0_kreal
   Radflux = 0.0_kreal
else
   print *, "ERROR:cooling_initialize: attempt to define unallocated cooling arrays. Stopping job."
   stop
end if

end subroutine cooling_initialize

end module cooling
