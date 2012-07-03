      subroutine State() ! this routine calculates the pressure
        use kinds,   only : kreal
        use cooling, only : TempK
        use pois,    only : rho
        use states,  only : eps, P
        use eos

        implicit none
     
#include "hydroparam.h"
#include "globals.h"
#include "units.h"

        integer JSTART,J,K,L
        real(kreal) :: mu

        if (jmin.gt.2) then
           jstart=jmin2
        else   
           jstart=1
        endif

C$OMP DO SCHEDULE(STATIC) PRIVATE(mu)
      DO L=1,LMAX
         DO K=1,KMAX2
            DO J=jstart,jmax2

              call get_gamma2(eps(J,K,L),rho(J,K,L),tempk(J,K,L),
     &             mu,gamma1(J,K,L))
              p(J,K,L) = bkmpcgs*rho(J,K,L)*tempk(J,K,L)*rhoconv
     &            / (mu*pconv)

            end do
         ENDDO
      ENDDO
C$OMP END DO

      RETURN
      END
 

