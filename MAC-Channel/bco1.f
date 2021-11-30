csssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine bco1
c
c
c     obstacle boundary conditions for bcc bccst bcns bcnsst
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'covar'
c
c
      j=2
      do 70 i=ia,ib
      k=(kb+1)+(i-ia)
c     u1(i,j,k)=0.
c     w1(i,j,k-1)=0.
c     u2(i,j,k)=0.
c     w2(i,j,k-1)=0.
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

