csssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c     
      subroutine velalt
c
c     alternation of the velocity-arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
$include 'covar'
c
c      write(6,*)'has entered velalt'
c
      do 30 i=1,iim
      do 30 j=1,jim
      do 30 k=1,kim
c
      u1(i,j,k)=u2(i,j,k)
      v1(i,j,k)=v2(i,j,k)
 30   w1(i,j,k)=w2(i,j,k)
c
c      write(6,*) 'leaving velalt'
      return
      end
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssss

