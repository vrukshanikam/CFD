cssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine bcnsst
c                      
c     boundary-conditions for the navier-stokes equations
c     conditions for the confining surfaces
c     fin-plate with stamping
c     This also calls bco.f
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c     
$include 'covar'
c
c     write(6,*) 'entering bcnsst'
c
c
      j=1
      do 10 i=1,iim
      do 10 k=1,kim
      u2(i,j,k)=jstamp(i,k)*u2(i,jre,k)
     *          -(jstamp(i,k)-1)*jn1*u2(i,j+1,k)
      w2(i,j,k)=jstamp(i,k)*w2(i,jre,k)
     *         -(jstamp(i,k)-1)*jn1*w2(i,j+1,k)         
      v2(i,j,k)=jstamp(i,k)*(v2(i,jre,k)+v2(i,j,k))*0.5
      p(i,j,k)  =jstamp(i,k)*p(i,jre,K)        
 10   continue
c
c
      j = jim
      do 20 i=1,iim
      do 20 k=1,kim
      u2(i,j,k)=jstamp(i,k)*u2(i,2,k)
     *         -(jstamp(i,k)-1)*jnim*u2(i,j-1,k)
      w2(i,j,k)=jstamp(i,k)*w2(i,2,k)
     *         -(jstamp(i,k)-1)*jnim*w2(i,j-1,k)
      v2(i,j-1,k)=jstamp(i,k)*(v2(i,1,k)+v2(i,j-1,k))*0.5
      v2(i,j,k )   =jstamp(i,k)*v2(i,2,k)
      p(i,j,k)      =jstamp(i,k)*p(i,2,k)    
 20   continue
c
c
      k = 1
      do 30 i=1,iim
      do 30 j=1,jim
      u2(i,j,k)=kstamp(i,j)*u2(i,j,kre)
     *         -(kstamp(i,j)-1)*kn1*u2(i,j,k+1)
      v2(i,j,k)=kstamp(i,j)*v2(i,j,kre)
     *         -(kstamp(i,j)-1)*kn1*v2(i,j,k+1)
      w2(i,j,k)=kstamp(i,j)*(w2(i,j,kre)+w2(i,j,k))*0.5
      p(i,j,k)  =kstamp(i,j)*p(i,j,kre)
 30   continue
c
c
      k = kim
      do 40 i=1,iim
      do 40 j=1,jim
      u2(i,j,k)=kstamp(i,j)*u2(i,j,2)
     *         -(kstamp(i,j)-1)*knim*u2(i,j,k-1)
      v2(i,j,k)=kstamp(i,j)*v2(i,j,2)
     *         -(kstamp(i,j)-1)*knim*v2(i,j,k-1)
      w2(i,j,k-1)=kstamp(i,j)*(w2(i,j,1)+w2(i,j,k-1))*0.5
      w2(i,j,k)   =kstamp(i,j)*w2(i,j,2)
      p(i,j,k)     =kstamp(i,j)*p(i,j,2)
 40   continue
c
c
c
c
      if(ipara.eq.1) then
      i=1
      do 70 j=1,jim
      do 70 k=1,kim
      u2(i,j,k) = uinlet(1,j,k)
      u2(i+1,j,k)=uinlet(2,j,k)
      v2(i,j,k) = vinlet(1,j,k)
      v2(i+1,j,k)=vinlet(2,j,k)
      w2(i,j,k) = winlet(1,j,k)
      w2(i+1,j,k)=winlet(2,j,k)      
c      v2(i,j,k) = -vinlet(2,j,k)
c      w2(i,j,k) = -winlet(2,j,k)
 70   continue
      else
      i=1
      do 50 j=1,jim
      do 50 k=1,kim
            u2(i,j,k) = qinlet(j,k)
            v2(i,j,k) =-v2(i+1,j,k)  
            w2(i,j,k) =-w2(i+1,j,k)       
 50   continue
      end if
c
c
c**************** obstacle-boundary-conditions *****************
c
      call bco
c
c***************************************************************
c
c
c     write(6,*) 'leaving bcnsst'
c
c   
      return
      end
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
