ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine ticorr
c
c       calculation of time-increment
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	utop=0.
	vtop=0.
c
	do 90 i=2,ire
	do 90 j=2,jre
	uchek=0.5*(u2(i,j)+u2(i-1,j))
	vchek=0.5*(v2(i,j)+v2(i,j-1))
	utop=dmax1(utop,abs(uchek))
	vtop=dmax1(vtop,abs(vchek))
 90	continue
c
	if(vtop.gt.0)then
c
	umax=deltax/utop
	vmax=deltay/vtop
c
	endif
c
	if(itime.eq.0 .or. zeit.lt.6.0)then
c
	if(vtop.le.0) goto 91
c
	deltat=dmin1(umax,vmax)
	goto 92
c
 91	continue
c
	deltat=deltax/utop
c
 92 	continue
c
	deltat=stab*deltat
	deltch=0.5*(deltx2*delty2/(deltx2+delty2))*re
c
	endif
c
c	write(*,93)deltat
c	write(*,94)deltch
c  94    format(3x,'deltach  ',e13.5)
c  93	format(3x,'deltat   ',e13.5)
c	deltat=dmin1(deltat,deltch)
c	write(*,*)'deltatmin=  ',deltat

c
c  ***** alpha factor *****
c
	udel=deltat/umax
	vdel=deltat/vmax
c
	alpha=updef*dmax1(udel,vdel)
c
	alpha=0.35
c
c	write(*,95)alpha
c 95	format(3x,'  alpha = ',e13.5)
c
	return
	end
