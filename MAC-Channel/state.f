csssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine state
c
c
c     state of the properties ro,eta
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c
      do 10 i=1,iim
      do 10 j=1,jim
      do 10 k=1,kim
c
      tstr= (t1(i,j,k)*(rtwti-1.))+1.0
      ro(i,j,k)=1./tstr+gam*xm2*p(i,j,k)/tstr
      eta(i,j,k)=tstr**0.5
c
 10   continue
c
c
      return
      end
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

