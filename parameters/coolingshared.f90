module coolingshared
!=======================================================================
!
!    coolingshared
!
! Description:
!
!   The coolingshared module replaces the common block COOLINGSHARED
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
!  Original COOLINGSHARED common block:
!
!      REAL*8 Oross(jmax2,kmax2,lmax),Oplck(jmax2,kmax2,lmax),
!     &       Oabs(jmax2,kmax2,lmax),Otot(jmax2,kmax2,lmax),
!     &       oplck_env(JMAX2,KMAX2,LMAX),sum
!
!      COMMON /COOLINGSHARED/Oross,Oplck,Oabs,Otot,sum,oplck_env
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)  ::  sum                    ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  Oross      ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Oplck      ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Oabs       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Otot       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Oplck_env  ! ???

contains
 
subroutine coolingshared_allocate
!=======================================================================
! 
!    coolingshared_allocate
!
! Subroutine coolingshared_allocate allocates the Coolingshared variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:coolingshared_allocate: attempt to allocate coolingshared arrays twice. Stopping job."
   stop
else
   allocate(Oross     (jmax2, kmax2, lmax))
   allocate(Oplck     (jmax2, kmax2, lmax))
   allocate(Oabs      (jmax2, kmax2, lmax))
   allocate(Otot      (jmax2, kmax2, lmax))
   allocate(Oplck_env (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine coolingshared_allocate

subroutine coolingshared_initialize
!=======================================================================
! 
!    coolingshared_initialize
!
! subroutine coolingshared_initialize initializes the coolingshared variables.
!
!=======================================================================
use constants, only : zero 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Oross     = zero
   Oplck     = zero
   Oabs      = zero
   Otot      = zero
   Oplck_env = zero
else
   print *, "ERROR:coolingshared_initialize: attempt to define unallocated coolingshared arrays. Stopping job."
   stop
end if

end subroutine coolingshared_initialize

end module coolingshared
