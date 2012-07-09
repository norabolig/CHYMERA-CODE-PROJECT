module opacity
!=======================================================================
!
!    opacity
!
! Description:
!
!   The opacity module replaces the common block OPACITY in globals.h
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
!  Original OPACITY common block:
!
!      real*8 xmmwtable,rostable,plktable,irrtable,scatable,ptab,ttab    
!      COMMON /OPACITY/                                                & 
!          XMMWtable(ITABLE,ITABLE),                                   &
!          ROStable(ITABLE,ITABLE),                                    &
!          PLKtable(ITABLE,ITABLE),                                    &
!          IRRtable(ITABLE,ITABLE),                                    &
!          SCAtable(ITABLE,ITABLE),                                    &
!          Ptab(ITABLE),                                               &
!          Ttab(ITABLE)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical :: alloc_flag = .FALSE.   ! true if module arrays have been allocated

!... Array variables

real(kreal), dimension(:,:), allocatable  ::  Xmmwtable    ! ???
real(kreal), dimension(:,:), allocatable  ::  Rostable     ! ???
real(kreal), dimension(:,:), allocatable  ::  Plktable     ! ???
real(kreal), dimension(:,:), allocatable  ::  Irrtable     ! ???
real(kreal), dimension(:,:), allocatable  ::  Scatable     ! ???
real(kreal), dimension(:  ), allocatable  ::  Ptab         ! ???
real(kreal), dimension(:  ), allocatable  ::  Ttab         ! ???

!---------------------------------------------------------------------

contains
 
subroutine opacity_allocate
!=======================================================================
! 
!    opacity_allocate
!
! Subroutine opacity_allocate allocates the Opacity variables.
!
!=======================================================================
use hydroparams, only : itable
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:opacity_allocate: attempt to allocate opacity arrays twice. Stopping job."
   stop
else
   allocate(Xmmwtable (itable,itable))
   allocate(Rostable  (itable,itable))
   allocate(Plktable  (itable,itable))
   allocate(Irrtable  (itable,itable))
   allocate(Scatable  (itable,itable))
   allocate(Ptab      (itable       ))
   allocate(Ttab      (itable       ))
   alloc_flag = .TRUE.
end if

end subroutine opacity_allocate

subroutine opacity_initialize
!=======================================================================
! 
!    opacity_initialize
!
! subroutine opacity_initialize initializes the opacity variables.
!
!=======================================================================
use constants, only : zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Xmmwtable = zero
   Rostable  = zero
   Plktable  = zero
   Irrtable  = zero
   Scatable  = zero
   Ptab      = zero
   Ttab      = zero
else
   print *, "ERROR:opacity_initialize: attempt to define unallocated opacity arrays. Stopping job."
   stop
end if

end subroutine opacity_initialize

end module opacity
