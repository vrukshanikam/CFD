cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c   
      subroutine ticorr
c
c
c     calculation of the time-incrrement
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c      write(6,*)'has entered ticorr'
c
      utop=0.
      vtop=0.
      wtop=0.
c
      do 40 j=2,jre
      do 40 i=2,ire
      do 40 k=2,kre
c
      uchek=0.5*(u2(i,j,k)+u2(i-1,j,k))
      vchek=0.5*(v2(i,j,k)+v2(i,j-1,k))
      wchek=0.5*(w2(i,j,k)+w2(i,j,k-1))
c
      utop =dmax1(utop,dabs(uchek))
      vtop =dmax1(vtop,dabs(vchek))
      wtop =dmax1(wtop,dabs(wchek))
c
 40   continue
c
c
      if(utop.le.0.or.vtop.le.0.or.wtop.le.0) go to 50
c
      umax =deltax/utop
      vmax =deltay/vtop
      wmax =deltaz/wtop
c
      deltat = dmin1(umax,vmax,wmax)
      go to 60
 50   continue
c
      deltat=deltax/utop
c
60    continue
c
      deltat = stab*0.5*deltat
      deltch=0.1*(deltx2*delty2*deltz2/
     *           (deltx2*delty2+delty2*deltz2+deltz2*deltx2))*re
c
c     write(6,1) deltat
c     write(6,2) deltch
c     deltat = dmin1(deltat,deltch)
 2    format(3x,' deltch = ',e13.5)
 1    format(3x,' deltat = ',e13.5)
c
c
c******************alpha-factor*************************************
c
      if(utop.le.0.or.vtop.le.0.or.wtop.le.0) then 
      alpha =updef*utop*deltat/deltax
      else
      udel = deltat/umax
      vdel = deltat/vmax
      wdel = deltat/wmax
c
      alpha = updef*dmax1(udel,vdel,wdel)
      end if
c
c
c      write(6,3)alpha
 3    format(3x,' alpha = ',e13.5)
c
c      write(6,*) 'leaving ticorr'
c
c
      return
      end
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

