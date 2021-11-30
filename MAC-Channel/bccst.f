csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine bccst
c
c
c     boundary-conditions for the mass continuity equation
c     conditions for the confining surfaces
c     plate-fin with stamping
c     This also calls bco.f
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
$include 'covar'
c
c     write(6,*) ' entering bccst'
c
      if(ipara.eq.1) then
      i=1
      do 70 j=1,jim
      do 70 k=1,kim
      u2(i,j,k)=uinlet(1,j,k)
      v2(i+1,j,k)=vinlet(2,j,k)
      w2(i+1,j,k)=winlet(2,j,k)
 70   continue
c
      else
c
      i=1
      do 10 j=1,jim
      do 10 k=1,kim
      u2(i,j,k)=qinlet(j,k)
 10   continue
c
      end if
c
      j=1
      do 20 i=1,iim
      do 20 k=1,kim
      v2(i,j,k)=jstamp(i,k)*(v2(i,jre,k)+v2(i,j,k))*0.5
 20   continue
c
c
      j=jim
      do 30 i=1,iim
      do 30 k=1,kim
      v2(i,j-1,k)=jstamp(i,k)*(v2(i,1,k)+v2(i,j-1,k))*0.5
 30   continue
c
c
      k=1
      do 40 i=1,iim
      do 40 j=1,jim
      w2(i,j,k)=kstamp(i,j)*(w2(i,j,kre)+w2(i,j,k))*0.5
 40   continue
c
c
      k=kim
      do 50 i=1,iim
      do 50 j=1,jim
      w2(i,j,k-1)=kstamp(i,j)*(w2(i,j,1)+w2(i,j,k-1))*0.5
 50   continue
c
c
c************obstacle-boundary-conditions********************
c
      call bco
c
c************************************************************
c
c
      if(iti.ge.1) go to 888
c
      if(iexit.eq.1) then
      i=iim-2
      do 60 j=1,jim
      do 60 k=1,kim
        u2(i+1,j,k)=u2(i,j,k)
        v2(i+1,j,k)=v2(i,j,k)
        w2(i+1,j,k)=w2(i,j,k)
 60   continue
      end if
      if(iexit.eq.2) then
      i=iim-2       
      do 80 j=1,jim
      do 80 k=1,kim
        u2(i+1,j,k)=2.*u2(i,j,k)-u2(i-1,j,k)
        v2(i+1,j,k)=2.*v2(i,j,k)-v2(i-1,j,k)
        w2(i+1,j,k)=2.*w2(i,j,k)-w2(i-1,j,k)
 80   continue
      end if
c
c     write(6,*) ' leaving bccst'
 888  return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
