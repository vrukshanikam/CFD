csssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine bco
c
c
c     obstacle boundary conditions for the WINGLET to be
c     called by the subroutines bcc bccst bcns bcnsst
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
$include 'covar'
c
c
      do 70 i=ia,ib
      do 70 j=2,(2+(i-ia))
      k=(kb+1)+(i-ia)
      u2(i,j,k)=0.
      w2(i,j,k-1)=0.
c
c
 70   continue
c
c
      return
c
      end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

