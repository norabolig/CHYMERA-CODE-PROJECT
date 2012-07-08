module gap
!=======================================================================
!
!    gap
!
! Description:
!
!   The gap module replaces the common block GAP in globals.h
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
!  Original GAP common block:
!
!      integer jin,itype
!      real*8  tmassini,tmassadd,tmassout,tmassacc,mass_star, mdot
!      real*8  starphi,tinphi
!      COMMON /GAP/starphi(jmax2,kmax2,lmax),                            &
!           tinphi(jmax2,kmax2,lmax),                                    &
!           tmassini,tmassadd,tmassout,tmassacc, mdot, mass_star,        &
!           jin,itype
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical      ::  alloc_flag = .FALSE.   ! true if module arrays have been allocated
integer      ::  jin                    ! ???
integer      ::  itype                  ! ???

real(kreal)  ::  tmassini    ! ???
real(kreal)  ::  tmassadd    ! ???
real(kreal)  ::  tmassout    ! ???
real(kreal)  ::  tmassacc    ! ???
real(kreal)  ::  mass_star   ! ???
real(kreal)  ::  mdot        ! ???

!... Array variables

real(kreal), dimension(:,:,:), allocatable  ::  starphi    ! ???
real(kreal), dimension(:,:,:), allocatable  ::  tinphi     ! ???

contains
 
subroutine gap_allocate
!=======================================================================
! 
!    gap_allocate
!
! Subroutine gap_allocate allocates the GAP variables.
!
!=======================================================================
use hydroparams, only : jmax2, kmax2, lmax
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:gap_allocate: attempt to allocate gap arrays twice. Stopping job."
   stop
else
   allocate(starphi(jmax2, kmax2, lmax))
   allocate( tinphi(jmax2, kmax2, lmax))
   alloc_flag = .TRUE.
end if

end subroutine gap_allocate

subroutine gap_initialize
!=======================================================================
! 
!    gap_initialize
!
! Subroutine gap_initialize initializes the GAP variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   starphi = 0.0_kreal
    tinphi = 0.0_kreal
else
   print *, "ERROR:gap_initialize: attempt to define unallocated gap arrays. Stopping job."
   stop
end if

end subroutine gap_initialize

end module gap
