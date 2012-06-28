module kinds

!==============================================================================
! Description:
!   The precision (kind) module
!
! Method:
!   Fortran 90 intrinsics SELECTED_REAL_KIND
!
! Externals:
!   None
!
!==============================================================================

implicit none
save

public

!!! TODO - need to get this from the make/build process
#define DBL_PREC

#ifdef DBL_PREC
integer, parameter :: kreal = SELECTED_REAL_KIND(12)  ! double precision
#else
integer, parameter :: kreal = SELECTED_REAL_KIND(6)   ! single precsion
#endif

!------------------------------------------------------------------------------

end module kinds
