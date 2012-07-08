module eom
!=======================================================================
!
!    eom
!
! Description:
!
!   The eom module replaces the common block EOM in globals.h
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
!  Original EOM common block:
!
!      REAL*8 JN,s,t,a,u,w,omega
!      COMMON /EOM/                                                      &
!           S(JMAX2,KMAX2,LMAX),                                         &
!           T(JMAX2,KMAX2,LMAX),                                         &
!           A(JMAX2,KMAX2,LMAX),                                         &
!           U(JMAX2,KMAX2,LMAX),                                         &
!           W(JMAX2,KMAX2,LMAX),                                         &
!           JN(JMAX2,KMAX2,LMAX),                                        &
!           OMEGA(JMAX2,KMAX2,LMAX)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables
logical :: alloc_flag = .FALSE.    ! true if module arrays have been allocated

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  S       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  T       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  A       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  U       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  W       ! ???
real(kreal), dimension(:,:,:), allocatable  ::  JN      ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Omega   ! ???

contains
 
subroutine eom_allocate
!=======================================================================
! 
!    eom_allocate
!
! Subroutine eom_allocate allocates the eom variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:eom_allocate: attempt to allocate eom arrays twice. Stopping job."
   stop
else
   allocate(S     (jmax2, kmax2, lmax))
   allocate(T     (jmax2, kmax2, lmax))
   allocate(A     (jmax2, kmax2, lmax))
   allocate(U     (jmax2, kmax2, lmax))
   allocate(W     (jmax2, kmax2, lmax))
   allocate(JN    (jmax2, kmax2, lmax))
   allocate(Omega (jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine eom_allocate

subroutine eom_initialize
!=======================================================================
! 
!    eom_initialize
!
! Subroutine eom_initialize initializes the eom variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   S     = 0.0_kreal
   T     = 0.0_kreal
   A     = 0.0_kreal
   U     = 0.0_kreal
   W     = 0.0_kreal
   JN    = 0.0_kreal
   OMEGA = 0.0_kreal
else
   print *, "ERROR:eom_initialize: attempt to define unallocated eom arrays. Stopping job."
   stop
end if

end subroutine eom_initialize

end module eom
