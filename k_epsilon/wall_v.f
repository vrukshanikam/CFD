cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine wall_v(i,j,ftaum)
c
c	Wall function treatment
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	yp_x=deltax/2.0
	up_x=v1(i,j)
	xxk=0.5*(xk2(i,j)+xk2(i,j+1))
	sqrtk=dsqrt(dabs(xxk))
	yplu_x=sqrtk*(cm**0.25)*re*yp_x
c
	ftaum=(0.419*up_x*sqrtk*cm**0.25)/(dlog(9.743*yplu_x))
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c	write(*,*)'leaving wall_v'
c
	return
	end
