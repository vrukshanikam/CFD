csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine bcns
c
c
c     boundary-conditions for the navier-stokes equations
c     conditions for the confining surfaces. This also
c     calls bco.f
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'covar'
c
c      write(6,*)'entering bcns'
c
      j =2
      do 10 i=1,iim
      do 10 k=1,kim
        u2(i,j-1,k) =jn1*u2(i,j,k)
        w2(i,j-1,k) =jn1*w2(i,j,k)
        v2(i,j-1,k)=0.
 10   continue
c
c
      j =jim
      do 20 i=1,iim
      do 20 k=1,kim
        u2(i,j,k) =jnim*u2(i,j-1,k)
        w2(i,j,k) =jnim*w2(i,j-1,k)   
        v2(i,j-1,k)=0.
 20   continue
c
c
      k =2
      do 30 i=1,iim
      do 30 j=1,jim
        u2(i,j,k-1) =kn1*u2(i,j,K)
        v2(i,j,k-1) =kn1*v2(i,j,k)
        w2(i,j,k-1) =0.
 30   continue
c
c
      if (ipara.eq.1) then
      i = 1
      do 70 j=1,jim
      do 70 k=1,kim
        u2(i,j,k) = uinlet(1,j,k)
        u2(i+1,j,k)=uinlet(2,j,k)
        v2(i,j,k) =vinlet(1,j,k)
        v2(i+1,j,k)=vinlet(2,j,k)
        w2(i,j,k) =winlet(1,j,k)
        w2(i+1,j,k)=winlet(2,j,k)  
 70   continue
      else
      i =1
      do 50 j=1,jim
      do 50 k=1,kim
        u2(i,j,k) =qinlet(j,k)
        v2(i,j,k) =-v2(i+1,j,k)
        w2(i,j,k) =-w2(i+1,j,k)
 50   continue
      end if
c
c************ obstacle-boundary-conditions ***************
c
      call bco
c
c*********************************************************
c
      k = kim
      do 40 i=1,iim
      do 40 j=1,jim
        u2(i,j,k)=knim*u2(i,j,k-1)
        v2(i,j,k)=knim*v2(i,j,k-1)       
        w2(i,j,k-1)=0.
 40   continue
c
c      write(6,*) 'leaving bcns'
c
      return
      end
cssssssssssssssssssssssssssssssssssssssssssssssssssssssss

