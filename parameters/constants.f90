module constants
!==============================================================================
!   Mathematical and physical constants
!==============================================================================
use kinds

implicit none
save

public

!... Physical constants
real(kreal), parameter :: msun    = 1.989d33
real(kreal), parameter :: pi      = 3.1415926535897931d0

!... Natural numbers
real(kreal), parameter :: zero  =  0.0d0
real(kreal), parameter :: one   =  1.0d0
real(kreal), parameter :: two   =  2.0d0
real(kreal), parameter :: three =  3.0d0
real(kreal), parameter :: four  =  4.0d0
real(kreal), parameter :: five  =  5.0d0
real(kreal), parameter :: six   =  6.0d0
real(kreal), parameter :: seven =  7.0d0
real(kreal), parameter :: eight =  8.0d0
real(kreal), parameter :: nine  =  9.0d0
real(kreal), parameter :: ten   = 10.0d0

!... Fractions
real(kreal), parameter :: half    = (one/two)
real(kreal), parameter :: third   = (one/three)
real(kreal), parameter :: twothree= (two/three)
real(kreal), parameter :: quarter = (one/four)
real(kreal), parameter :: sixth   = (one/six)
real(kreal), parameter :: tenth   = (one/ten)

!------------------------------------------------------------------------------

end module constants
