csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine ceqvp
c
c
c     mass-continuity equation for variable properties
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c     write(6,*) 'entering ceqvp'
c    
      divmax=0.
      dalt=0.
      imax=0
      jmax=0
      kmax=0
c
      do 20 i=2,ire
      do 20 j=2,jre
      do 20 k=2,kre
c
      dro1 =(ro(i,j,k)+ro(i+1,J,k))*0.5
      dro11=(ro(i,j,k)+ro(i-1,j,K))*0.5
      dro2 =(ro(i,j,k)+ro(i,j+1,K))*0.5
      dro22=(ro(i,j,k)+ro(i,j-1,k))*0.5
      dro3 =(ro(i,j,k)+ro(i,j,k+1))*0.5
      dro33=(ro(i,j,k)+ro(i,j,k-1))*0.5
c
c
      div = (dro1*u2(i,j,k)-dro11*u2(i-1,j,k))/deltax
     *     +(dro2*v2(i,j,k)-dro22*v2(i,j-1,k))/deltay 
     *     +(dro3*w2(i,j,k)-dro33*w2(i,j,k-1))/deltaz
c
      deltap = -beta*div
      a = deltat*deltap
      ax =a/deltax
      ay =a/deltay
      az =a/deltaz
c
      p(i,j,k)=p(i,j,k)+deltap
      u2(i,j,k) =u2(i,j,k)+ax/dro1
      v2(i,j,k) =v2(i,j,k)+ay/dro2
      w2(i,j,k) =w2(i,j,k)+az/dro3
      u2(i-1,j,k) =u2(i-1,j,k)-ax/dro11
      v2(i,j-1,k) =v2(i,j-1,k)-ay/dro22
      w2(i,j,k-1) =w2(i,j,k-1)-az/dro33
c
      dab = dabs(div)
      if(dab.gt.dalt) then
        divmax = dab
        imax = i
        jmax = j
        kmax = k
      end if
      dalt = divmax
c
 20   continue
c
      iti = iti+1
      isum = isum+1
c
      write(6,1)ita,iti,isum,divmax,imax,jmax,kmax
 1    format(2x,3i5,4x,e15.8,2x,3i5,2x,'from ceqvp')
c
c
c     write(6,*) 'leaving ceqvp'
c
c
      return
      end
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
