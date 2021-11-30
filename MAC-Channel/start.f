csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine start
c
c
c     start conditions
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
$include 'covar'
c
c************ start of a parabolic calculation ******************
c
      write (6,*)'has entered start'
c
      if (ipara.eq.1) go to 3000   
c
c*********************************************************************
c
c
c
c*********************************************************************
c
      jpr =jim-1
      jpl =2
      rpmj =(jpr+jpl)*0.5
      rj =float (jpr)+0.5-rpmj
c
      kpl=2
      kpr=kim-1
      rpmk=(kpr+kpl)*0.5
      rk =float(kpr)+0.5-rpmk
c
c
c**************** parabolic velocity profile ************************
c
c
      if(iuprof.eq.2) then
      if((dabs(fy-1.0)).le.0.0001) then
      do 10 j=1,jim
      do 10 k=1,kim
c     qinlet(j,k)=(1.-((rpmj-float(j))**2/rj**2))
c    *           *(1.-((rpmk-float(k))**2/rk**2))
      qinlet(j,k)=1.5*(1.-((rpmj-float(j))**2/rj**2))
 10   continue
      else
      do 20 j=1,jim
      do 20 k=1,kim
      qinlet(j,k)=1.5*(1.-((rpmk-float(k))**2/rk**2))
 20   continue
      end if
      else
      do 25 j=1,jim
      do 25 k=1,kim
      qinlet(j,k) =1.0
 25   continue
      end if
c 
c
c
c**************** constant temperature profile ***********************
c
      do 35 j=1,jim
      do 35 k=1,kim
      tinlet(j,k)=tentry
 35   continue
c
c
c
      do 30 i=1,iim
      do 30 j=1,jim
      do 30 k=1,kim
c
      u2(i,j,k) =qinlet(j,k)
      v2(i,j,k) =0.
      w2(i,j,k) =0.
      p(i,j,k) =1.0
      t1(i,j,k) =tentry
      if(icopro.ne.1) then
      ro(i,j,k)=1.
      eta(1,j,k) =1.
      else
      go to 30
      end if
c
c
c
c
 30   continue
c
      go to 888
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c c
c     start of a parabolic calculation
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c 3000 if (irest.eq.1)  go to 888
 3000 open(92,file='result1',form='formatted')
      do 6666 i = 1,iim
      do 6666 j = 1,jim
      do 6666 k = 1,kim
      read(92,*) l,m,n,u1(i,j,k),v1(i,j,k),w1(i,j,k),p(i,j,k),t2(i,j,K)  
 6666 continue
      close(92)
c
c
      i =icpara
      do 130 j=1,jim
      do 130 k =1,kim
      uinlet(1,j,k) =u1(i,j,k)
      uinlet(2,j,k) =u1(i+1,j,k)
      vinlet(1,j,k) =v1(i,j,k)
      vinlet(2,j,k) =v1(i+1,j,k)
      winlet(1,j,k) =w1(i,j,k)
      winlet(2,j,k) =w1(i+1,j,k) 
      tinlet(j,k) =t2(i,j,k)
 130  continue
c
c
      if(irest.eq.1) go to 888
c                     
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
      do 140 i=2,iim
      do 140 j=1,jim
      do 140 k=1,kim
      u2(i,j,k) =u1(icpara+1,j,k)
      v2(i,j,k) =v1(icpara+1,j,k)
      w2(i,j,k) =w1(icpara+1,j,k)
      p(i,j,k)  =p(icpara+1,j,k)
      t1(i,j,k) =tinlet(j,k)
 140  continue
c
      write(6,*) 'leaving start '
c
 888  return
c
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
