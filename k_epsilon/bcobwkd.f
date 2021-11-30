ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine bcobwkd
c
c     boundary-conditions for the mass continuity equation
c     conditions for the confining surfaces
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c       write (*,*)'has entered bcobwkd'
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	do 90 i=ia,ib
	do 90 j=ja,jb
	xk1(i,j)=0.
	xk2(i,j)=0.
	d1(i,j)=0.
	d2(i,j)=0.
	vist(i,j)=0.
 90	continue
c
        j=ja
c
        do  40  i=ia,ib
c
	xk2(i,j)=-xk2(i,j-1)
	d2(i,j)=d2(i,j-1)
	vist(i,j)=vist(i,j-1)
c
 40     continue
c
        j=jb
c
        do  50  i=ia,ib
c
	xk2(i,j)=-xk2(i,j+1)
	d2(i,j)=d2(i,j+1)
	vist(i,j)=vist(i,j+1)
c
 50     continue
c
	i=ia
c
	do 60 j=ja+1,jb-1
c
	xk2(i,j)=-xk2(i-1,j)
	d2(i,j)=d2(i-1,j)
	vist(i,j)=vist(i-1,j)
c
  60    continue
c
	i=ib
c
	do 70 j=ja+1,jb-1
c
	xk2(i,j)=-xk2(i+1,j)
	d2(i,j)=d2(i+1,j)
	vist(i,j)=vist(i+1,j)
c
  70    continue
c
c       write (6,*)'leaving bcobwkd'
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        return
        end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
