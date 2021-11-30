ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine start
c
c
c       start conditions
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	zeit=0.
c      
c  ***** start of a parabolic calculation *****
c
c 	write(*,*)'has entered start'
c
	jpr=jim-1
	jpl=2
	rpmj=(jpr+jpl)*0.5
	rj=float(jpr)+0.5-rpmj
c
	if((dabs(fy-1.0)) .le. 0.001) then
c
c  ***** parabolic velocity profile  *****
c
	if(iuprof.eq.3) then
	do 10 j=2,jre
	qinlet(j)=1.5*(1.-(((rpmj-float(j))**2)/(rj**2)))
   10   continue
	endif
c   
c  ***** Fully Developed turbulent flow ******
c
	if(iuprof.eq.2) then
	do 20 j=2,(jre/2+1)
	yy=(j-1.5)*deltay
	qinlet(j)=1.0-(1.0-2.0*yy)**8.0
	qinlet(jim+1-j)=qinlet(j)
c
  20    continue
	endif
c       
c  ***** constnt velocity profile  ******
c
	if(iuprof.eq.1) then
	do 30 j=2,jre
	qinlet(j)=1.0
   30   continue
	endif
c
	endif
c
	qinlet(jim)=-qinlet(jre)
	qinlet(1)=-qinlet(2)
c
	do 11 j=1,jim
	xkin(j)=1.5*(xin/100.0)**2.0
   11   continue	
c
	do 14 j=1,jim
c	if(j.eq.2 .or. j.eq.jre)then
c	fgj=0.419/(jim-2)
c	else
        fgj=0.09
c	endif
c	din(j)=((cm**0.75)*(xkin(j)**1.5))/fgj
	din(j)=(cm*re*(xkin(j))**2.0)/10.0
  14    continue
c
c ****** defining the inlet profile *******
c
	do 31 i=1,iim
	do 31 j=1,jim
        u2(i,j)=qinlet(j)
	v2(i,j)=0.0
        u1(i,j)=qinlet(j)
	v1(i,j)=0.0
	p(i,j)=1.0
	xk2(i,j)=xkin(j)
	xk1(i,j)=xkin(j)
	d2(i,j)=din(j)
c
c	vist(i,j)=(cm*re*(xk2(i,j))**2.0)/d2(i,j)
	vist(i,j)=10.0
c
   31   continue
c
c	write(*,*)'leaving start'
c
	return
	end
