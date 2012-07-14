module potential
!=======================================================================
!
!    potential
!
! Description:
!
!   The potential module incorporates the potential solver pot3 and 
!   arrays that were formerly allocated on the stack.
!
!  Revision notes     (Don Berry)
!
!  v-0: Original code.
!
!  v-1: o Changed DO loops to use DO...ENDDO construct.
!       o Moved most index arithmetic directly into array subscript lists.
!       o Completely reworked calculation of philag to use comprehensible
!           IF..ELSE..ENDIF constructs.
!       o Inserted DOACROSS directives.
!
!  v-3: o created potential module (Craig Rasmussen)
!       o moved local arrays to module scope and make private
!=======================================================================
use kinds, only : kreal
use hydroparams, only : jmax, jmax1, kmax, kmax1, kmax2, lmax

implicit none
save

!... Array variables
!-------------------

! A1 and B1  should be dimensioned lmax.  They're used in fft.
real(kreal) :: A1(LMAX), B1(LMAX)

integer, private, parameter :: jmax3 = jmax - 1
integer, private, parameter :: kmax3 = kmax - 1
integer, private, parameter :: lmax1 = lmax/2 + 1

! These next arrays are used in blktri. The size of wfw(max) is given by
! max. ge. (2*(kmax3+2)*(log2(kmax3+1)-1) + 6*jmax3+2).
real(kreal), private :: AN(KMAX3),BN(KMAX3),CN(KMAX3), AM(JMAX3),BM(JMAX3)
real(kreal), private :: CM(JMAX3), Y(JMAX3,KMAX3)

! The following arrays store quantities that are used to calculate blktri
! coefficients.  C(jmax-1) stores laplacian's angular operator.
real(kreal), private :: C(JMAX3),DENOMR(JMAX),RD2(JMAX),RD3(JMAX)
real(kreal), private :: DENOMZ(KMAX), ZD2(KMAX), XLAMM(LMAX1)

!     PHAC IS DIMENSIONED KMAX2? PH IS DIMENSIONED LMAX/2.
! But it would not hurt to dimension PH to LMAX.
real(kreal), private :: X(10),PHI2(10),PH(LMAX),PHAC(KMAX2)

real(kreal), private, allocatable :: WFW(:)

contains

subroutine pot3(npoint, iprint)
!=======================================================================
! 
!    pot3
!
! Subroutine pot3 is the entry point to the potential solver.
!
!=======================================================================

use kinds, only : kreal
use blok6, only : dtheta, grav, kwfw
use coefs, only : Coef
use grid,  only : Rhf, Zhf
use pois,  only : Phi, Rho

use constants,   only : pi
use hydroparams, only : jmax, jmax1, kmax, kmax1, lmax

implicit none

integer, intent(in) :: npoint, iprint

integer :: j, k, l, m, n, ierror

real(kreal) :: sf1, sf2, x1, philag, dthet2, pig4, cmmax, cnmax, ammin, powrm

! These common blocks are used in blktri and its subroutines. (I should not
! need to declare them in pot3, but I am not sure OpenMP compiler will do
! the right thing if I do not.)
!!!!!!! FIXME      COMMON /CBLKT/ NPP,Kx,EPSLON,CNV,Lx,NCMPLX,IK,IZ,DUM(1)
!!!!!!! FIXME      COMMON /BLOKJ1/IWCN,IW1,IW2,IW3,IWD,IWW,IWU
!!!!!!! FIXME C$OMP THREADPRIVATE(/CBLKT/,/BLOKJ1/)

logical :: INIT_BLKTRI

!---------------------------------------------------------------------

!... TODO - move this to an initialize routine and do only once
allocate( WFW(2*(KMAX3+2)*KWFW+6*JMAX3+2) )

#if PARTICLE>0
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(PHI,RHOTOT,COEF,
#else
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(PHI,RHO,COEF,
#endif
!$OMP& r,rhf,z,zhf,rof3n,zof3n,dtheta,pi
!$OMP&         ,grav,bgden,JREQ,KZPOL)
!$OMP&  PRIVATE(J,K,L,M,WFW)

N=LMAX/2
IF(LMAX.EQ.1) N=1
SF1=0.5/FLOAT(N)
SF2=0.5

DO L=1,LMAX                                                     
   A1(L)=0.0
   B1(L)=0.0
ENDDO

! For convenience, put all rho's into phi array -- leave boundary phi's
! alone.
!$OMP DO
DO L=1,LMAX                                                     
   DO K=2,KMAX
      DO J=2,JMAX
#if PARTICLE>0
         PHI(J,K,L)=RHOTOT(J,K,L)
#else
         PHI(J,K,L)=RHO(J,K,L)
#endif
      ENDDO
   ENDDO
ENDDO
!$OMP ENDDO

!-----------------------------------------------------------------------
!
!  If lmax>1 then for all j,k, calculate fourier transform of rho's
!  and boundary phi's.

IF(LMAX.gt.1) THEN
         
!$OMP DO
   DO K=2,KMAX1
      DO J=2,JMAX1
         DO L=1,N
            A1(L) = PHI(J,K,2*L-1)
            B1(L) = PHI(J,K,2*L)
         ENDDO
         CALL FFT(A1,B1,N,N,N,1)
         CALL REALTR(A1,B1,N,1)
!     Cosine coefficients are in a1? Sine coef's are in b1.
!     Put cosine coef's in phi(j,k,1:n+1)?  Put sine coef's in      
!     phi(j,k,n+2:lmax).  Normalize computed values with 0.5/n.     
!     Compute amplitudes of modes.
         X1=ABS(A1(1))
         IF(X1.NE.0.0) THEN
            DO L=2,N
               COEF(J,K,L-1,1) = 2.0*SQRT( A1(L)**2 + B1(L)**2 )/X1
            ENDDO
            COEF(J,K,N,1) = ABS(A1(N+1))/X1
         ENDIF

!     Compute phase angles of modes. The following code simply calculates
!     philag=arctan(b/a) in the range -pi/2 <= philag < 3*pi/2. It sets
!     philag=4*pi if a and b are both 0.
         DO L=2,N
            IF(A1(L).GT.0.0) THEN
               PHILAG = ATAN(B1(L)/A1(L))
            ELSE IF(A1(L).LT.0.0) THEN
               PHILAG = ATAN(B1(L)/A1(L))+PI
            ELSE
               IF(B1(L).GT.0.0) THEN
                  PHILAG = 0.5*PI
               ELSE IF(B1(L).LT.0.0) THEN
                  PHILAG = -0.5*PI
               ELSE
                  PHILAG = 4.0*PI
               ENDIF
            ENDIF
            COEF(J,K,L-1,2)=PHILAG*180./PI
         ENDDO
         COEF(J,K,N,2)=A1(1)

!     Copy fourier transform back into phi array.
         DO L=1,N+1
            PHI(J,K,L)=A1(L)*SF1
            IF(L.LT.N) THEN
               PHI(J,K,L+N+1)=B1(L+1)*SF1
            ENDIF
         ENDDO
         DO L=N+1,LMAX
            A1(L) = 0.0
            B1(L) = 0.0
         ENDDO
      ENDDO
   ENDDO
!$OMP   ENDDO
         
ENDIF


!-----------------------------------------------------------------------
!  Now solve the transformed problem. The fourier transform in the L
!  direction has turned the 3-D Poisson problem into a set of LMAX
!  uncoupled 2-D problems which may be solved concurrently.
!     

DTHET2 = 1.0/(DTHETA*DTHETA)
PIG4   = 4.0*PI*GRAV
RD2(1) = 0.5/RHF(2)
RD3(1) = 1.0/RHF(1)
DO J=2,JMAX                                                    
   DENOMR(J)=1.0/(RHF(J+1)-RHF(J-1))
   RD2(J) = 1.0/(RHF(J+1)-RHF(J))
   RD3(J) = 1.0/RHF(J)
ENDDO
ZD2(1) = 0.5/ZHF(2)
DO K=2,KMAX                                                    
   DENOMZ(K) = 1.0/(ZHF(K+1)-ZHF(K-1))
   ZD2(K) = 1.0/(ZHF(K+1)-ZHF(K))
ENDDO
DO J=2,JMAX                                                    
   CM(J-1) = 2.*(RD2(J)+0.5*RD3(J))*DENOMR(J)
   AM(J-1) = 2.*(RD2(J-1)-0.5*RD3(J))*DENOMR(J)
   C(J-1)  = RD3(J)*RD3(J)*DTHET2
ENDDO
DO K=2,KMAX                                                    
   CN(K-1) = 2.*ZD2(K)*DENOMZ(K)
   AN(K-1) = 2.*ZD2(K-1)*DENOMZ(K)
   BN(K-1) = -CN(K-1)-AN(K-1)
ENDDO

!  Special conditions:
CMMAX = CM(JMAX-1)
CM(JMAX-1) = 0.0
AMMIN = AM(1)
AM(1) = 0.0
CNMAX = CN(KMAX-1)
CN(KMAX-1) = 0.0
BN(1) = -CN(1)
AN(1) = 0.0
DO M=1,N+1                                                     
   XLAMM(M)=COS((M-1)*DTHETA)
ENDDO


!  Coefficients bm and y will vary with m, so they haven't been calculated
!  yet. Now for each value of l, thru lmax, calculate phi's in transformed
!  space.
      
INIT_BLKTRI=.TRUE.
!$OMP DO
DO L=1,LMAX                                                
   IF(L.LE.N+1) THEN
      M=L-1                                                  
   ELSE
      M=L-(N+1)                                                 
   ENDIF
   POWRM=(-1.0)**M

!     (remember, densities are in array phi right now.)
   DO K=2,KMAX                                                    
      DO J=2,JMAX
         Y(J-1,K-1)=PIG4*PHI(J,K,L)
      ENDDO
   ENDDO

!     Treat y on boundaries where phi is known.
   DO J=2,JMAX                                                    
      Y(J-1,KMAX-1)=Y(J-1,KMAX-1)-CNMAX*PHI(J,KMAX+1,L)
   ENDDO
   DO K=2,KMAX                                                    
      Y(JMAX-1,K-1)=Y(JMAX-1,K-1)-CMMAX*PHI(JMAX+1,K,L)
   ENDDO

   BM(1) = -CM(1)+(POWRM-1.0)*AMMIN+2.0*(XLAMM(M+1)-1.0)*C(1)
   DO J=2,JMAX-2
      BM(J) = -CM(J)-AM(J)+2.0*(XLAMM(M+1)-1.0)*C(J)
   ENDDO
   BM(JMAX-1) = -CMMAX-AM(JMAX-1)+2.0*(XLAMM(M+1)-1.0)*C(JMAX-1)
!     This completes the set up of coefficients for given m.
!     
   IF(INIT_BLKTRI) THEN
      CALL BLKTRI(0,1,KMAX-1,AN,BN,CN,1,JMAX-1, &
           AM,BM,CM,JMAX-1,Y,IERROR,WFW)
      INIT_BLKTRI=.FALSE.
   ENDIF
   CALL BLKTRI(1,1,KMAX-1,AN,BN,CN,1,JMAX-1, &
        AM,BM,CM,JMAX-1,Y,IERROR,WFW)         
!     Solution on 2-D grid is complete.

!     Put transformed phi's from y into phi array.
   DO K=2,KMAX                                                    
      DO J=2,JMAX
         PHI(J,K,L)=Y(J-1,K-1)
      ENDDO
   ENDDO
ENDDO
!$OMP END DO

!-----------------------------------------------------------------------
!
!     All transformed phi's have been calculated.  Now obtain real
!     phi's by inverse fourier transform.

IF(LMAX.NE.1) THEN
!$OMP   DO
   DO K=2,KMAX1                                                   
      DO J=2,JMAX1
         DO L=1,LMAX
            A1(L)=0.0
            B1(L)=0.0
         ENDDO
         DO L=1,N+1
            A1(L)=PHI(J,K,L)
            IF(L.LT.N) THEN
               B1(L+1)=PHI(J,K,L+N+1)
            ENDIF
         ENDDO
         CALL REALTR(A1,B1,N,-1)
         CALL FFT(A1,B1,N,N,N,-1)
!     Now real phi's are in first n places of both a1 and b1.  With
!     increasing l, phi's alternate between a1 and b1.  Put phi's into
!     array phi, normalizing them by 0.5.
!     
         DO L=1,N
            PHI(J,K,2*L-1) = SF2*A1(L)
            PHI(J,K,2*L)   = SF2*B1(L)
         ENDDO
      ENDDO
   ENDDO
!$OMP   END DO
ENDIF

!     Calculation of phi's is now finished. 

!$OMP DO
DO L=1,N                                                       
   DO K=2,KMAX1                                                   
      PHI(1,K,L)   = PHI(2,K,N+L)
      PHI(1,K,N+L) = PHI(2,K,L)
   ENDDO
   DO J=1,JMAX1                                                   
      PHI(J,1,L)   = PHI(J,2,L)
      PHI(J,1,N+L) = PHI(J,2,N+L)
   ENDDO
ENDDO
!$OMP ENDDO

!$OMP END PARALLEL

CALL ZAXPHI(npoint, iprint)

!... TODO - move this to an initialize routine and do only once
deallocate( WFW )

end subroutine pot3

subroutine ZAXPHI(npoint, iprint)
!=======================================================================
! 
!    zaxphi
!
! Subroutine zaxphi ???
!
!=======================================================================

use kinds,  only : kreal
use coefs,  only : Coef
use grid,   only : Rhf
use inside, only : klocat, phichk
use pois,   only : Phi

use hydroparams, only : jmax, jmax2, kmax1, kmax2, lmax

implicit none

integer, intent(in) :: npoint, iprint

!... Local variables
!-------------------

!... use local variable so that input variable isn't changed
integer :: npoint_loc

integer :: lm, n, ichk, num, ispecl, jsp, j, i1, i2, k, l, lp, im, i, ist, &
           m, mr, mp

real(kreal) :: xpoint, phmax, p1, xinv, xr, xratio, p2, err, er, xx

!---------------------------------------------------------------------

npoint_loc = npoint

! With X and PHI2 dimensioned 10, 10-point interpolation is the
! maximum you can do.
IF(NPOINT.GT.10) THEN
   WRITE(3,10000) NPOINT
10000 FORMAT(///,' YOU ARE LIMITED TO A 10-POINT INTERPOLATION HERE IN', &
             ' SUBROUTINE ZAXPHI.',/,' BUT YOU PUT NPOINT =',I4,///)
   npoint_loc = 10
ENDIF
XPOINT=0.0
LM=LMAX/2
N=npoint_loc
ICHK=MOD(N,2)
N=N-ICHK
NUM=N/2
ISPECL=NUM+NUM+1
JSP=NUM+1
DO J=2,JSP
   I1=NUM+(J-1)
   I2=NUM-(J-2)
   X(I1)=RHF(J)
   X(I2)=-RHF(J)
ENDDO
IF(ICHK.EQ.1) X(ISPECL)=RHF(NUM+2)

DO K = 2, kmax1
   PHMAX=0.0
!       For given k, calculate phi on z-axis at all angles.  store
!       results in PH(L) array and in PHI(JMAX2,K,L).
   DO L=1,LM
      LP=L+LM
      DO J=2,JSP
         I1=NUM+(J-1)
         I2=NUM-(J-2)
         PHI2(I1)=PHI(J,K,L)
         PHI2(I2)=PHI(J,K,LP)
      ENDDO
      IF(ICHK.EQ.1) PHI2(ISPECL)=PHI(NUM+2,K,L)
      IM = npoint_loc - 1
      DO I=1,IM
         P1=PHI2(I)
         XINV=1.0/X(I)
         XR=XPOINT*XINV
         IST=I+1
         DO J=IST, npoint_loc
            XRATIO=X(J)*XINV
            P2=PHI2(J)
            PHI2(J)=(P1*(XRATIO-XR)+P2*(XR-1.0))/(XRATIO-1.0)
         ENDDO
      ENDDO
      PH(L)=PHI2(npoint_loc)
      IF(ABS(PH(L)).GT.ABS(PHMAX))PHMAX=PH(L)
      PHI(JMAX2,K,L)=PH(L)
      PHI(JMAX2,K,LP)=PH(L)
   end do

!... At this k, find maximum deviation in ph(l)'s? Put result in phac(k).
   ERR=0.0
   DO L=1,LM
      ER=1.0-PH(L)/PHMAX
      IF(ABS(ER).GT.ABS(ERR))ERR=ER
   ENDDO
   PHAC(K)=ERR
end do

! Find largest of all deviations in PHI's on z-axis and put value in PHICHK
! and k-value in KLOCAT.
ERR=0.0
DO K=2,KMAX1
   XX=ABS(PHAC(K))
   IF(XX.GT.ABS(ERR)) THEN
      ERR=PHAC(K)
      KLOCAT=K
   ENDIF
ENDDO
PHICHK=ERR
 
! Write the amplitude and phase to the results file.
IF (IPRINT.EQ.1) THEN
   WRITE(3,10100) 'PHAC(1:KMAX1):'
   WRITE(3,10040)(PHAC(K),K=2,KMAX1)
   WRITE(3,10100) 'Fourier amplitudes and phases:'
10040 FORMAT(1P11E11.3)
10100 FORMAT(a)
   IF(LMAX.GT.8) THEN
      MR=1
      MP=8
!     LQ=LM/8
!     DO K=1,LQ
         WRITE(3,10110)(J,((COEF(J,2,M,I),M=MR,MP),I=1,2),J=2,JMAX)
         MR=MR+8
         MP=MP+8
!     ENDDO
   END IF
END IF
10110 FORMAT(20X,I5,' COEFS=',1P8E12.3,/,25X,' PHASE=',0P8F12.2)

end subroutine ZAXPHI


end module potential
