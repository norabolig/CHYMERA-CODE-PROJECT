module engtables
!=======================================================================
!
!    engtables
!
! Description:
!
!   The engtables module replaces the common block ENGTABLES in globals.h
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
!  Original ENGTABLES common block:
!
!      real*8 :: temptable,engtable,gammatable,muc
!      common /engtables/temptable(TTABLE),engtable(TTABLE),          &
!              gammatable(TTABLE),muc
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables

logical, private :: alloc_flag = .FALSE.   ! true if module arrays have been allocated
real(kreal)      :: muc                    ! ???

!... Array variables

real(kreal), dimension(:), allocatable  ::  Temptable    ! ???
real(kreal), dimension(:), allocatable  ::  Engtable     ! ???
real(kreal), dimension(:), allocatable  ::  Gammatable   ! ???

!---------------------------------------------------------------------

contains
 
subroutine engtables_allocate
!=======================================================================
! 
!    engtables_allocate
!
! Subroutine engtables_allocate allocates the Engtables variables.
!
!=======================================================================
use hydroparams, only : ttable
 
implicit none

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:engtables_allocate: attempt to allocate engtables arrays twice. Stopping job."
   stop
else
   allocate(Temptable  (ttable))
   allocate(Engtable   (ttable))
   allocate(Gammatable (ttable))
   alloc_flag = .TRUE.
end if

end subroutine engtables_allocate

subroutine engtables_initialize
!=======================================================================
! 
!    engtables_initialize
!
! subroutine engtables_initialize initializes the engtables variables.
!
!=======================================================================
use constants, only : zero
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Temptable  = zero
   Engtable   = zero
   Gammatable = zero
else
   print *, "ERROR:engtables_initialize: attempt to define unallocated engtables arrays. Stopping job."
   stop
end if

end subroutine engtables_initialize

end module engtables
