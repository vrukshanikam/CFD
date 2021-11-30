ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine bckd
c
c
c     boundary-conditions for the k-e equation
c     conditions for the confining surfaces
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c       write (*,*)'has entered bckd'
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        i=2
        do 20 j=1,jim
 	xk2(i,j)=xkin(j)
 	d2(i,j)=din(j)
 20     continue
c
        i=ire
c
        do  30  j=1,jim
c
	xk2(i,j)=xk2(i-1,j)
	d2(i,j)=d2(i-1,j)
c
 30     continue
c
        j=1
c
        do  40  i=1,iim
c
	xk2(i,j)=-xk2(i,j+1)
	d2(i,j)=d2(i,j+1)
	vist(i,j)=vist(i,j+1)
c
 40     continue
c
        j=jim
c
        do  50  i=1,iim
c
	xk2(i,j)=-xk2(i,j-1)
	d2(i,j)=d2(i,j-1)
	vist(i,j)=vist(i,j-1)
c
 50     continue
c
	call bcobwkd
c
c       write (6,*)'has leaving bckd'
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        return
        end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
