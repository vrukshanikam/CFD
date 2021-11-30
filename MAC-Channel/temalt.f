csssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine temalt
c
c
c     alternation of the temperature-arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c     write(6,*) 'has entered temalt'
c
      do 10 i=1,iim
      do 10 j=1,jim
      do 10 k=1,kim
      t1(i,j,k) = t2(i,j,k)
 10   continue
c
c
c     write (6,*) 'leaving temalt '
c
      return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssss

