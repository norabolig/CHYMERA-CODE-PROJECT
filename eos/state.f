      subroutine State() ! this routine calculates the pressure
        use eos,         only : get_gamma2
        use kinds,       only : kreal
        use cooling,     only : TempK
        use convert,     only : rhoconv, pconv
        use etally,      only : gamma1
        use hydroparams, only : jmax2, jmin, jmin2, kmax2, lmax
        use pois,        only : rho
        use states,      only : eps, P
        use units,       only : bkmpcgs

        implicit none
     
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
 

