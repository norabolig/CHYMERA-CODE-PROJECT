module passive
!=======================================================================
!
!    passive
!
! Description:
!
!   The passive module replaces variables that were once in a common block
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
!  Original comments:
!
! The following arrays are local to the main program, and should not need 
! to be placed in common. However, some systems may try to put them on the
! stack, which may result in stack overflow. Putting them in a common block
! guarantees they will go on the heap. ! COMMON BLOCK REMOVED. ACB
!      REAL*8 SS(JMAX2,KMAX2,LMAX),
!     &       TT(JMAX2,KMAX2,LMAX),
!     &       AA(JMAX2,KMAX2,LMAX),
!     &       RRHO(JMAX2,KMAX2,LMAX),
!     &       EEPS(JMAX2,KMAX2,LMAX),
!#if PASSIVE>0
!     &       PPASSFLUX(JMAX2,KMAX2,LMAX,PAS)
!#enif
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated

!... Array variables

real(kreal), dimension(:,:,:  ), allocatable  ::  SS        ! ???
real(kreal), dimension(:,:,:  ), allocatable  ::  TT        ! ???
real(kreal), dimension(:,:,:  ), allocatable  ::  AA        ! ???
real(kreal), dimension(:,:,:  ), allocatable  ::  Rrho      ! ???
real(kreal), dimension(:,:,:  ), allocatable  ::  Eeps      ! ???
real(kreal), dimension(:,:,:,:), allocatable  ::  Ppassflux ! ???

contains
 
subroutine passive_allocate
!=======================================================================
! 
!    passive_allocate
!
! Subroutine passive_allocate allocates the passive variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax, pas
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:passive_allocate: attempt to allocate passive arrays twice. Stopping job."
   stop
else
   allocate(SS       (jmax2, kmax2, lmax))
   allocate(TT       (jmax2, kmax2, lmax))
   allocate(AA       (jmax2, kmax2, lmax))
   allocate(Rrho     (jmax2, kmax2, lmax))
   allocate(Eeps     (jmax2, kmax2, lmax))
#if PASSIVE>0
   allocate(Ppassflux(jmax2, kmax2, lmax, pas))
#endif
   alloc_flag = .TRUE.
end if

end subroutine passive_allocate

subroutine passive_initialize
!=======================================================================
! 
!    assive_initialize
!
! Subroutine passive_initialize initializes the passive variables.
!
!=======================================================================
use constants, only : zero 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   SS   = zero
   TT   = zero
   AA   = zero
   Rrho = zero
   Eeps = zero
   if (allocated(Ppassflux)) Ppassflux = 0.0_kreal
else
   print *, "ERROR:passive_initialize: attempt to define unallocated passive arrays. Stopping job."
   stop
end if

end subroutine passive_initialize

end module passive
