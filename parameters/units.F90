module units
!=======================================================================
!
!    units
!
! Description:
!
!   The units module replaces the header file units.h
!
!=======================================================================
 
use kinds, only: kreal

implicit none
save
 
!... Scalar variables
!    The following units: Mstar, Rdiskau, Tstar are model
!    dependent.  Must be set by the user.   
!    The rest are astronomical units.

real(kreal) Mstar,Rstar,Rdiskau,Tstar,gridlim,phylim
real(kreal) Msuncgs,Rsuncgs,AUcgs,sigmacgs,Gcgs,bkmpcgs
PARAMETER (Mstar=1.0d0,Rstar=2d0,Rdiskau=40d0,Tstar=4d3)
PARAMETER (Msuncgs=1.989d33, Rsuncgs=6.96d10, AUcgs=1.496d13)
PARAMETER (sigmacgs=5.670d-5, Gcgs=6.672d-8, phylim=1.d-4) 
PARAMETER (bkmpcgs=8.254d7,gridlim=1.d-6)
real(kreal),parameter::psize=1d1/AUcgs,dust_to_gas=0.01d0,rhoacgs=3d0
real(kreal),parameter::poly_factor=4d0
real(kreal),parameter::massBin=1.,ebin=0.,abin=100.

!    Most of the following variables are used in the source routine only. 
!    torp is one ORP in code time units, defined as 2*pi/omega(200,2,1) 
!    of the starting model.
!    cq = 3 for AV on, 0 for AV off. 
!    irtype = 0 for no irr, 1 for irradiation.
!    ictype is the cooling type: 1 for const. Tcool, 2 for 
!    Edd. grey atm. only, 3 for diff. aprox., 4 for diff. 
!    aprox. with shining atm.
!    cct is only used when using type 1.  It is the cooling
!    time in ORPS.      
!    tcoollmt is the lower limit of cooling time in ORP.
!    tirrlmt  is the lower limit of irradiation time in ORP.
!    Tbgrnd is the lower limit of the temperature
!    irop is used to spread the irradiation to a few more cells by dividing the
!    opacities by some number. 
!    jirr is the first first j zone to be irradiated.
!    jcool is the j zone at which cooling starts.  Disabled in most ictypes.
!    fk = metallicity factor: fraction of standard metallicity to keep.
!    tenvk = constant envelop temperature
!    SETUNITS = 0 for polytropic units (G=K=M=1) and 1 for Aaron's units
!       (G=1, 1 R = 1 AU, 1 M = Msun)
!    Use H2STAT to select what type of mixture you want.
!    0 = pure para
!    1 = pure ortho -- this is not really physical for astro applications
!    2 = set mixture
!    3 = equilibrium
!    4 = single gamma law
!    Be sure to set ac and bc for mixture (ac: para component, bc: ortho component)
!    also pick a metallicity by adjusting xabun, yabun, and zabun.


real(kreal) torp,cct,Tbgrnd,tcoollmt,theatlmt,cq,irop,fk,tenvk,amp0
real(kreal), parameter :: fgsoft=0d0 ! set softening for star
real(kreal), parameter :: ac = 1.d0, bc = 3.d0
real(kreal), parameter :: xabun = .73d0,yabun=.25d0,zabun=.02d0
real(kreal), parameter :: tk_eos_cutoff=1d3
real(kreal), parameter :: tk_bgrnd=3d0
real(kreal), parameter :: dtk_eos=3d1
real(kreal), parameter :: mp=1.67d-24
integer, parameter :: NEOS_RHO=500
real(kreal),parameter::rho_eos_high=1d-4
real(kreal),parameter::rho_eos_low=1d-15

INTEGER ictype,irtype,jcool,jirr,SETUNITS,H2STAT
integer,parameter::JCONST=80
PARAMETER(torp=11d0,cct=2d0,cq=3d0,tcoollmt=5d1,theatlmt=5d1)
PARAMETER(ictype=0,irtype=0,jcool=10)
PARAMETER(jirr=10,irop=1.,Tbgrnd=3d0)
PARAMETER(tenvk=150d0,fk=1d0)
PARAMETER(SETUNITS=1,H2STAT=0) 
logical,parameter::find_eos_rho=.true.
PARAMETER(amp0=0.05d0) ! amplitude of initial, random perturbation

#if EXTERNAL_POT>0
      logical, parameter :: external_pot = .true.
#else
      logical, parameter :: external_pot = .false.
#endif
#if FLUID_RESTART>0
      logical, parameter :: restart_fluid = .true.
#else
      logical, parameter :: restart_fluid = .false.
#endif
#if WIGGLE_RESTART>0
      logical, parameter :: restart_wiggle = .true.
#else
      logical, parameter :: restart_wiggle = .false.
#endif
#if ROTATING>0
      real(kreal), parameter :: omega_frame=5d-2  
#endif

!     leave a space after the file name, or set the exact character length.
!     This will help to ensure that all compilers do the right thing.
character(len=11), parameter :: mmwfile='mmw_new.dat'
character(len=33), parameter :: rosstablefile='rosseland.abpoll.p3p5.amax1.dat'
character(len=30), parameter :: plancktablefile='planck.abpoll.p3p5.amax1.dat'
character(len=37), parameter :: irrtablefile='planck.abpoll.p3p5.amax1.ts4000.dat'
character(len=37), parameter :: scatablefile='planck.abpoll.p3p5.amax1.ts4000.dat'

!---------------------------------------------------------------------

end module units
