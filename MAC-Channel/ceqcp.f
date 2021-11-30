csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine ceqcp
c
c
c     mass continuity equation for constant properties
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c     write(6,*) 'entering ceqcp'
c
      divmax=0.
      dalt=0.
      imax=0
      jmax=0
      kmax=0
c
c
      do 20 i=2,ire
      do 20 j=2,jre
      do 20 k=2,kre
c
      div=(u2(i,j,k)-u2(i-1,j,k))/deltax
     *   +(v2(i,j,k)-v2(I,j-1,k))/deltay
     *   +(w2(i,j,k)-w2(i,j,k-1))/deltaz
c
      deltap=-beta*div
      a=deltat*deltap
      ax=a/deltax
      ay=a/deltay
      az=a/deltaz
c
      p(i,j,k)=p(i,j,k)+deltap
      u2(i,j,k)=u2(i,j,k)+ax
      u2(i-1,j,k)=u2(i-1,j,k)-ax
      v2(i,j,k)=v2(i,j,k)+ay
      v2(i,j-1,k)=v2(i,j-1,k)-ay
      w2(i,j,k)=w2(i,j,k)+az
      w2(i,j,k-1)=w2(i,j,k-1)-az
c
      dab=dabs(div)
      if(dab.gt.dalt) then
            divmax=dab
            imax=i
            jmax=j
            kmax=k
      end if
      dalt=divmax
c
  20  continue
c
      iti=iti+1
      isum=isum+1
c
c      write(6,1) ita,iti,isum,divmax,imax,jmax,kmax
c  1   format(2x,3i5,4x,e15.8,3i5,1x,'from ceqcp')
c
c
c     write(6,*) 'leaving ceqcp'
c
c
      return
      end
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
