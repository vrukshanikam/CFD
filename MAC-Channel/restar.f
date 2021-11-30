cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
           subroutine restar
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
$include 'covar'
c
c
c      write(6,*) 'has entered restar'
      open(91,file='result',form='formatted')
      do 666 i = 1,iim
      do 666 j = 1,jim
      do 666 k = 1,kim
      read(91,*) l,m,n,u2(i,j,k),v2(i,j,k),w2(i,j,k),p(i,j,k),t1(i,j,K)  
 666  continue
c
      close(91)
c
      if(itcalc.eq.0) go to 1001
c
c
      if(ipara .eq.1) go to 888
c
      i=1
      do 20 j=1,jim
      do 20 k=1,kim
      qinlet(j,k)=u2(i,j,k)
 20   continue
c
c
      go to 888
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     restart (variable properties) for a given u v w p of a
c     calculation with constant properties, not given t
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 1001 if((dabs(fy-1.0)).le.0.001) then
      do 50 j=1,jim
      do 50 k=1,kim
c      tinlet(j,k)=tentry
      tinlet(j,k)=t1(1,j,k)
 50   continue
      end if
c
c
      if((dabs(fz-1.0)).le.0.001) then
      do 60 j=1,jim
      do 60 k=1,kim
      tinlet(j,k)=tentry
 60   continue
      end if
c
 777  continue
      i=1
      do 70 j=1,jim
      do 70 k=1,kim
      tinlet(j,k)=t1(i,j,k)
 70   continue
c
c
      i=1
      do 100 j=1,jim
      do 100 k=1,kim
      qinlet(j,k)=u2(i,j,k)
 100  continue
c
c
c888  write(6,*) 'leaving restar'
 888  return
      end
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
