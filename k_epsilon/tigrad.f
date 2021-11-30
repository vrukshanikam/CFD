cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine tigrad
c
c       maximum of the velocity-alternation during a zeit increment
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	dtmax=0.
	dumax=0.
	dvmax=0.
c
	do 180 i=2,(ire-1)
	do 180 j=2,jre
c
	dudt=dabs(u1(i,j)-u2(i,j))/deltat
	dvdt=dabs(v1(i,j)-v2(i,j))/deltat
c
	dact=dtmax
	dtmax=dmax1(dtmax,dudt,dvdt)
	if(dtmax.le.dact) goto 180
	idtm=i
	jdtm=j
 180	continue
c
c ****** constant properties ********
        open(7,file='prolt',form='formatted')
	write(7,101)itkd,ita,iti,isum,dtmax,deltat,alpha
 101    format(2x,4i7,2x,2e13.5,e13.5)
	close(7)
c
	write(6,182)ita,idtm,jdtm,dtmax
	write(68,999)itkd,ita,iti,isum,dtmax,idtm,jdtm,
     &	deltat,alpha,divkd
 999    format(2x,4i6,e13.5,2x,2i4,2x,e10.5,e10.5,f7.4)
 182	format(2x,'maximum change in velocity ',2x,3i5,e13.5)
	if (dtmax*.1 .le. epsi) epsi = dtmax*.1
	return
	end
