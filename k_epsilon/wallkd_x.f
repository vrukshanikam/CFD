cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine wallkd_x(i,j)
c
c	Wall function treatment
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'entered wallkd_x'
c
	yp=deltay/2.0
	up=dabs(0.5*(u2(i,j)+u2(i-1,j)))
	sqrtk=dsqrt(dabs(xk1(i,j)))
	yplu=sqrtk*(cm**0.25)*re*yp
c
	ftau=(0.419*up)/(dlog(9.743*yplu))
	xk2(i,j)=(ftau**2.0)/(cm**0.5)
	d2(i,j)=((ftau)**3.0)/(0.419*yp)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	write(*,*)'leaving wallkd_x'
c
	return
	end
