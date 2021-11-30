cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine wallkd_y(i,j)
c
c	Wall function treatment
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'entered wallkd_y'
c
	yp=deltax/2.0
	up=dabs(0.5*(v2(i,j)+v2(i,j-1)))
	sqrtk=dsqrt(dabs(xk1(i,j)))
	yplus=sqrtk*(cm**0.25)*re*yp
c
	ftau=(0.419*up)/(dlog(9.743*yplus))
	xk2(i,j)=(ftau**2.0)/(cm**0.5)
	d2(i,j)=((ftau)**3.0)/(0.419*yp)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	write(*,*)'leaving wallkd_y'
c
	return
	end
