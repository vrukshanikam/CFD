csssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c     
      subroutine kdalt
c
c     alternation of the velocity-arrays
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	 include 'header'
c
c      write(6,*)'has entered kdalt'
c
      do 30 i=1,iim
      do 30 j=1,jim
c
      xk1(i,j)=xk2(i,j)
      d1(i,j)=d2(i,j)
c
 30   continue
c
c      write(6,*) 'leaving kdalt'
       return
       end
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssss
