module intensity
!=======================================================================
!
!    intensity
!
! Description:
!
!   The intensity module replaces the common block INTENSITY in globals.h
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
!  Original INTENSITY common block:
!
!      integer :: KFITA
!      real*8 :: temporary,dsdt,sfunc,l_tau_z,dtau_z,                    &
!                intensity_in_z,intensity_z,ddsdtt,                      &
!                int_temp,init_int_in 
!      common /intensity/ KFITA(JMAX+2,LMAX),                            &
!                temporary(JMAX+2,KMAX+2,LMAX),                          &
!                dsdt(JMAX+2,KMAX+2,LMAX),                               &
!                sfunc(JMAX+2,KMAX+2,LMAX),                              &
!                l_tau_z(JMAX+2,KMAX+2,LMAX),                            &
!                dtau_z(JMAX+2,KMAX+2,LMAX),                             &
!                intensity_in_z(JMAX+2,KMAX+2,LMAX),                     &
!                intensity_z(JMAX+2,KMAX+2,LMAX),                        &
!                ddsdtt(JMAX+2,KMAX+2,LMAX),                             &
!                int_temp(JMAX+2,KMAX+2,LMAX),                           &
!                init_int_in(JMAX+2,LMAX)
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables
logical :: alloc_flag = .FALSE.   ! true if module arrays have been allocated

!... Array variables

integer,     dimension(:,  :), allocatable  ::  Kfita          ! ???

real(kreal), dimension(:,:,:), allocatable  ::  Temporary      ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Dsdt           ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Sfunc          ! ???
real(kreal), dimension(:,:,:), allocatable  ::  L_tau_z        ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Dtau_z         ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Intensity_in_z ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Intensity_z    ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Ddsdtt         ! ???
real(kreal), dimension(:,:,:), allocatable  ::  Int_temp       ! ???
real(kreal), dimension(:,  :), allocatable  ::  Init_int_in    ! ???

contains
 
subroutine intensity_allocate(jmax2, kmax2, lmax)
!=======================================================================
! 
!    intensity_allocate
!
! Subroutine intensity_allocate allocates the INTENSITY variables.
!
!=======================================================================
 
implicit none

integer, intent(in) :: jmax2, kmax2, lmax

!---------------------------------------------------------------------

!... Allocate memory for the variables

if (alloc_flag) then
   print *, "ERROR:intensity_allocate: attempt to allocate intensity arrays twice. Stopping job."
   stop
else
   allocate(Kfita         (jmax2,        lmax))
   allocate(Temporary     (jmax2, kmax2, lmax))
   allocate(Dsdt          (jmax2, kmax2, lmax))
   allocate(Sfunc         (jmax2, kmax2, lmax))
   allocate(L_tau_z       (jmax2, kmax2, lmax))
   allocate( Dtau_z       (jmax2, kmax2, lmax))
   allocate(Intensity_in_z(jmax2, kmax2, lmax))
   allocate(Intensity_z   (jmax2, kmax2, lmax))
   allocate(Ddsdtt        (jmax2, kmax2, lmax))
   allocate(Int_temp      (jmax2, kmax2, lmax))
   allocate(Init_int_in   (jmax2,        lmax))
   alloc_flag = .TRUE.
end if

end subroutine intensity_allocate

subroutine intensity_initialize
!=======================================================================
! 
!    intensity_initialize
!
! Subroutine intensity_initialize initializes the INTENSITY variables.
!
!=======================================================================
 
implicit none

!---------------------------------------------------------------------

!... Initialize the variables

if (alloc_flag) then
   Kfita          = 0
   Temporary      = 0.0_kreal
   Dsdt           = 0.0_kreal
   Sfunc          = 0.0_kreal
   L_tau_z        = 0.0_kreal
   Dtau_z         = 0.0_kreal
   Intensity_in_z = 0.0_kreal
   Intensity_z    = 0.0_kreal
   Ddsdtt         = 0.0_kreal
   Int_temp       = 0.0_kreal
   Init_int_in    = 0.0_kreal
else
   print *, "ERROR:intensity_initialize: attempt to define unallocated intensity arrays. Stopping job."
   stop
end if

end subroutine intensity_initialize

end module intensity
