      subroutine ExternalPot()

      use blok6, only : dtheta
      use blok7, only : time
      use kinds, only : kreal
      use gap,   only : starphi, mass_star, tmassacc
      use grid,  only : Rhf, Zhf, rof3n, zof3n
      use pois,  only : rho, phi, indirectx, indirecty

      use constants,   only : half, two
      use hydroparams, only : jmin, jmax1, jmax2, kmax1, kmax2, lmax

      implicit none
#include "globals.h"

      integer::J,K,L
      real(kreal)::thisMass,omegaBin2,rcom,theta,x,y
      real(kreal)::rBin,thetaBin,thisTime,omegaBin,massDisk,muMass,rdist

      thisTime=time

#if BINARY>0
      call getBinaryPosition(thisTime,rBin,thetaBin,omegaBin,
     &   massDisk,muMass)
      omegaBin2=omegabin**2
      rcom=rbin-rbin*mass_star/muMass
#endif

#if INDIRECT>0
!$OMP DO SCHEDULE(STATIC) REDUCTION(+:indirectx,indirecty)
      do L=1,LMAX
        do K=2,KMAX1
          do J=JMIN,JMAX1
             thisMass=rho(J,K,L)*rhf(J)*rof3n*zof3n*two*dtheta ! take into account both sides
             indirectx=indirectx+thisMass*cos((dble(L-1)+half)*dtheta)
     &                *rhf(J)/(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))**3
             indirecty=indirecty+thisMass*sin((dble(L-1)+half)*dtheta)
     &                *rhf(J)/(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))**3
          enddo
        enddo
      enddo
!$OMP ENDDO
#endif
 
!$OMP DO SCHEDULE(STATIC) PRIVATE(theta,rdist,x,y)
            do l=1,lmax
       theta=(dble(L)-half)*dtheta
                do k=1,kmax2
                  do j=1,jmax2
                     starphi(j,k,l) = -(mass_star+tmassacc)
     &                       /(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))
                     phi(j,k,l)= phi(j,k,l)+starphi(j,k,l) ! ELIMINATED diskphi. ACB.

#if BINARY>0
       !!!!!!!!!!!!!!
       !! Add Binary!
       !!!!!!!!!!!!!!

       rdist=sqrt(rBin*rBin+rhf(J)*rhf(J)-two*rBin*rhf(J)*
     &       cos( theta- pi ) +
     &       zhf(K)*zhf(K) )

       x=rhf(J)*cos( theta )
       y=rhf(J)*sin( theta )

       phi(J,K,L)=phi(J,K,L)-massBin/rdist-half*omegaBin2*
     &            ( (x+rcom)**2+y**2 )
#endif

#if INDIRECT>0
       !!!!!!!!!!!!!!!!
       !! Add Indirect!
       !!!!!!!!!!!!!!!!
                     phi(J,K,L)=phi(J,K,L)
     &+indirectx*rhf(J)*cos((dble(L-1)+half)*dtheta)
     &+indirecty*rhf(J)*sin((dble(L-1)+half)*dtheta)
#endif 
                  enddo
               enddo
            enddo
!$OMP END DO

      return
      end subroutine ExternalPot
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine ExternalPotInit() ! this should be obsolete now

      use blok6, only : dtheta
      use kinds, only : kreal
      use gap,   only : starphi, mass_star, tmassacc
      use grid,  only : Rhf, Zhf, rof3n, zof3n
      use pois,  only : rho, phi, indirectx, indirecty

      use constants,   only : half, two
      use hydroparams, only : jmin, jmax1, jmax2, kmax1, kmax2, lmax

      implicit none
#include "globals.h"

      real(kreal)::rBin,thetaBin,thisTime,omegaBin,massDisk,muMass,theta
      integer::J,K,L
      real(kreal)::thisMass,omegaBin2,rcom,x,y,rdist

#if BINARY>0
      call getBinaryPosition(thisTime,rBin,thetaBin,omegaBin,
     &   massDisk,muMass)
      omegaBin2=omegaBin**2
      rcom=rBin-rBin*(mass_star)/muMass
#endif

#if INDIRECT>0
!$OMP DO SCHEDULE(STATIC) REDUCTION(+:indirectx,indirecty) 
!$OMP&PRIVATE(thisMass)
      do L=1,LMAX
        do K=2,KMAX1
          do J=JMIN,JMAX1
             thisMass=rho(J,K,L)*rhf(J)*rof3n*zof3n*two*dtheta ! take into account both sides
             indirectx=indirectx+thisMass*cos((dble(L-1)+half)*dtheta)
     &                *rhf(J)/(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))**3
             indirecty=indirecty+thisMass*sin((dble(L-1)+half)*dtheta)
     &                *rhf(J)/(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))**3
          enddo
        enddo
      enddo
!$OMP ENDDO
#endif

 
!$OMP DO SCHEDULE(STATIC) private(rdist,x,y)
            do l=1,lmax
       theta=(dble(L)-half)*dtheta
                do k=1,kmax2
                  do j=1,jmax2
                     starphi(j,k,l) = -(mass_star+tmassacc)
     &                       /(sqrt(rhf(J)*rhf(J)+zhf(K)*zhf(K)))
                     phi(j,k,l)= phi(j,k,l)+starphi(j,k,l) ! ELIMINATED diskphi. ACB.
#if BINARY>0
       !!!!!!!!!!!!!!
       !! Add Binary!
       !!!!!!!!!!!!!!

       rdist=sqrt(rBin*rBin+rhf(J)*rhf(J)-two*rBin*rhf(J)*
     &       cos( theta- pi ) +
     &       zhf(K)*zhf(K) )

       x=rhf(J)*cos( theta )
       y=rhf(J)*sin( theta )

       phi(J,K,L)=phi(J,K,L)-massBin/rdist-half*omegaBin2*
     &            ( (x+rcom)**2+y**2 )
#endif

#if INDIRECT>0
       !!!!!!!!!!!!!!!!
       !! Add Indirect!
       !!!!!!!!!!!!!!!!
                     phi(J,K,L)=phi(J,K,L)
     &+indirectx*rhf(J)*cos((dble(L-1)+half)*dtheta)
     &+indirecty*rhf(J)*sin((dble(L-1)+half)*dtheta)
#endif
                  enddo
               enddo
            enddo
!$OMP END DO

      return
      end subroutine ExternalPotInit
