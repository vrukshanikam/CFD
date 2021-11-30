ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine restar
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	include 'header'
c
	write(6,*) 'has entered restar'
        open (1,file='result1',form='formatted')
	read(1,*)re,ita,zeit, deltat
	read(1,*)iim,jim,ia,ib,ja,jb
	do 200 i=1,iim
	do 200 j=1,jim
	read(1,*)u2(i,j),v2(i,j),p(i,j),xk2(i,j),d2(i,j)
	u1(i,j)=u2(i,j)
	v1(i,j)=v2(i,j)
 200	continue
	close (1)
c
	i=1
	do 202 j=1,jim
	qinlet(j)=u2(i,j) 
	xkin(j)=xk2(i,j)
	din(j)=d2(i,j)
 202    continue
c
	if(ita.gt.5500)then
        open (11,file='res_m',form='formatted')
	read(11,*)re,ita,dt_vel_m,deltat
	read(11,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	read(11,*)um(i,j),vm(i,j),xkm(i,j),dsm(i,j)
	umi(i,j)=um(i,j)/dt_vel_m
	vmi(i,j)=vm(i,j)/dt_vel_m
	enddo
	enddo
	close (11)
	else
	dt_vel_m=0.0
	do i=1,iim
	do j=1,jim
	um(i,j)=0.0
	vm(i,j)=0.0
	xkm(i,j)=0.0
	dsm(i,j)=0.0
	enddo
	enddo
	endif	
c
	if(ita.gt.7000)then
        open (12,file='res_rms',form='formatted')
	read(12,*)re,ita,dt_rms,deltat
	read(12,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	read(12,*)u2p(i,j),v2p(i,j),uv2p(i,j)
	enddo
	enddo
	close (12)
	else
	dt_rms=0.0
	do i=1,iim
	do j=1,jim
	u2p(i,j)=0.0
	v2p(i,j)=0.0
	uv2p(i,j)=0.0
	enddo
	enddo
	endif	
c
c	write(*,*)'leaving restar'
	return
	end
