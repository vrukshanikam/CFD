cccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine tigrad
c
c
cccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c     write(6,*)'has entered tigrad'
      dtmax=0.
c      dumax=0.
c      dvmax=0.
c      dwmax=0.
c
c
      do 80 i=2,(ire-1)
      do 80 j=2,jre
      do 80 k=2,kre
c
c
      dudt = dabs(u1(i,j,k)-u2(i,j,k))/deltat
      dvdt = dabs(v1(i,j,k)-v2(i,j,k))/deltat
      dwdt = dabs(w1(i,j,k)-w2(i,j,k))/deltat
c
c
      dact=dtmax
      dtmax=dmax1(dtmax,dudt,dvdt,dwdt)
      if(dtmax.le.dact) go to 80
      idtm=i
      jdtm=j
      kdtm=k
 80   continue
c
c************** constant properties *******************
c
c
      if(icopro.eq.1) then
c
c
      write(68,2)ita,iti,isum,dtmax,idtm,jdtm,kdtm,deltat,alpha
 2    format(2x,3i5,e13.5,2x,3i5,2x,2e13.5)
      else
 
      write(68,3)ita,iti,isum,dtmax,idtm,jdtm,kdtm,itt,
     *           itmax,jtmax,ktmax
 3    format(2x,3i5,e13.5,3i4,2x,i5,2x,3i4)
c
c
      end if
c
c
c      write(6,4)idtm,jdtm,kdtm,dtmax
c 4    format(2x,'maximum change in velocity :  ',2x,3i5,e13.5)

      if(dtmax*.1 .lt. epsi) epsi = dtmax*.1
c
c     write(6,*)'leaving tigrad'
c
c
      return
      end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccc
