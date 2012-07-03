module kinds

!==============================================================================
! Description:
!   The precision (kind) module
!
! Method:
!   Fortran 90 intrinsics KIND
!
! Externals:
!   None
!
!==============================================================================

implicit none
save

public

integer, parameter :: kreal = kind(1.0D)  ! double precision

!------------------------------------------------------------------------------

end module kinds
