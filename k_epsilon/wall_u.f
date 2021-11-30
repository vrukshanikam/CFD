cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine wall_u(i,j,ftaum)
c
c	Wall function treatment
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	yp=deltay/2.0
	up=u1(i,j)
	xxk=0.5*(xk2(i,j)+xk2(i+1,j))
	sqrtk=dsqrt(dabs(xxk))
	yplu=sqrtk*(cm**0.25)*re*yp
c
	ftaum=(0.419*up*sqrtk*cm**0.25)/(dlog(9.743*yplu))
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	write(*,*)'leaving wall_u'
c
	return
	end
