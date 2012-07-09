module grid
!=======================================================================
!
!    grid
!
! Description:
!
!   The grid module replaces the common block GRID in globals.h
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
!  Original GRID common block:
!
!      real*8 r,z,rhf,zhf,rof3n,zof3n,enon
!      integer jreq,kzpol
!      COMMON /GRID/JREQ,KZPOL,                                      &
!     &     R(pot3jmax2),                                            &
!     &     Z(POT3KMAX2),                                            &
!     &     RHF(POT3JMAX2),                                          &
!     &     ZHF(POT3KMAX2),                                          &
!     &     ROF3N,ZOF3N
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
integer      ::  jreq                   ! ???
integer      ::  kzpol                  ! ???
real(kreal)  ::  rof3n                  ! ???
real(kreal)  ::  zof3n                  ! ???

!... Array variables

real(kreal), dimension(:), allocatable  ::  R            ! ???
real(kreal), dimension(:), allocatable  ::  Z            ! ???
real(kreal), dimension(:), allocatable  ::  Rhf          ! ???
real(kreal), dimension(:), allocatable  ::  Zhf          ! ???

contains
 
subroutine grid_allocate
!=======================================================================
! 
!    grid_allocate
!
! Subroutine grid_allocate allocates the grid variables.
!
!=======================================================================
use hydroparams, only : pot3jmax2, pot3kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:grid_allocate: attempt to allocate grid arrays twice. Stopping job."
   stop
else
   allocate(R   (pot3jmax2))
   allocate(Z   (pot3kmax2))
   allocate(Rhf (pot3jmax2))
   allocate(Zhf (pot3kmax2))
   alloc_flag = .TRUE.
end if

end subroutine grid_allocate

subroutine grid_initialize
!=======================================================================
! 
!    grid_initialize
!
! Subroutine grid_initialize initializes the grid variables.
!
!=======================================================================
use constants, only: zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   R   = zero
   Z   = zero
   Rhf = zero
   Zhf = zero
else
   print *, "ERROR:grid_initialize: attempt to define unallocated grid arrays. Stopping job."
   stop
end if

end subroutine grid_initialize

end module grid
