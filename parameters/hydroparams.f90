module hydroparams
!=======================================================================
!
!    hydroparam
!
! Description:
!
!     This file includes the parameter statements for the 3dhyd.f code.
!     These can be modified as necessary to run different sized grids;
!     however, parameters starting with 'pot3' must be powers of 2.
!     
!     -rpl, Sep, 1996
!
! Method:
!
! History:
! Version      Date           Author
! ----------   ----------     ---------------
! 1.0          07/06/2012     Craig Rasmussen
!
! Notes: 
!
!  Original POIS common block:
!
!      real*8 phi,rho,rhotot,indirectx,indirecty
!      COMMON /POIS/ indirectx,indirecty,                                &
!         PHI(POT3JMAX2,POT3KMAX2,LMAX),                                 &
!         RHO(POT3JMAX2,POT3KMAX2,LMAX),                                 &
!         RHOTOT(POT3JMAX2,POT3KMAX2,LMAX)
!
!=======================================================================
implicit none
save
 
!... Do not currently support different resolutions in the l-direction.

integer, parameter :: jmax  = 256            ! radial direction
integer, parameter :: jmax1 = jmax+1
integer, parameter :: jmax2 = jmax+2

integer, parameter :: kmax  = 64             ! vertical direction
integer, parameter :: kmax1 = kmax+1
integer, parameter :: kmax2 = kmax+2

integer, parameter :: lmax  = 512            ! azimuthal direction
integer, parameter :: lmax2 = lmax/2

integer, parameter :: lrjmax  = 32
integer, parameter :: lrkmax  = 8
integer, parameter :: lrlmax  = 64
integer, parameter :: lrjmax1 = lrjmax+1

!... High resolution problem.

integer, parameter :: pot3jmax  = 256
integer, parameter :: pot3jmax1 = pot3jmax+1
integer, parameter :: pot3jmax2 = pot3jmax+2

integer, parameter :: pot3kmax  = 64
integer, parameter :: pot3kmax1 = pot3kmax+1
integer, parameter :: pot3kmax2 = pot3kmax+2

!... Minimum radial grid point, for cutting out central star.
integer, parameter :: jmin  = 10
integer, parameter :: jmin1 = jmin-1
integer, parameter :: jmin2 = jmin-2

!... Other parameters.

integer, parameter :: hj  = 128
integer, parameter :: hk  = 128
integer, parameter :: hj1 = hj+1
integer, parameter :: hj2 = hj+2
integer, parameter :: hk1 = hk+1
integer, parameter :: hk2 = hk+2

integer, parameter :: itable = 100
integer, parameter :: TTABLE = 400

integer, parameter :: MAXTHREADS = 200

!... change the following for passive array size modification.
integer :: PAS = 0

contains

subroutine hydroparams_initialize(passive)
!=======================================================================
! 
!    hydroparams_initialize
!
! Subroutine pois_initialize initializes the pois variables.
!
!=======================================================================
implicit none

integer, intent(in) :: passive

!---------------------------------------------------------------------

!... Initialize the variables

PAS = passive

end subroutine hydroparams_initialize

end module hydroparams
