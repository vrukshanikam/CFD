ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine otpt
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c     
c        write(6,*) 'has entered otpt'
c
c	Writing the v-velocity 
c
	open(2,file='frea1000',access='append')
	write(2,*)zeit,v2(120,41)
	close(2)
c
c     **********calculation of cl****************
c
	cpsum=0.0
c
	do 192 i=ia,ib
	q1=8.*re*deltay
	q2=3.*(v2(i,ja-2)-2.*v2(i,ja-1))
	q3=9.*p(i,ja-1)/8.
	q4=p(i,ja-2)/8.
	ppb=q3-q4+(q2/q1)
	q5=3.*(v2(i,jb+2)-2.0*v2(i,jb+1))
	q6=9.*p(i,jb+1)/8.
	q7=p(i,jb+2)/8.
	ppt=q6-q7-(q5/q1)
	cpsum=cpsum+2.0*(ppb-ppt)
 192    continue
        cp1=cpsum/(ib-ia)
	sl=0.
	sr=0.
	s1=2./re
	cfsum=0.
	do 193 j=ja,jb
	y1=3.0*deltax
	y2=9.*v2(ia-1,j)-v2(ia-2,j)
	sl=(y2/y1)
	y3=9.*v2(ib+1,j)-v2(ib+2,j)
	sr=(y3/y1)
	cfsum=cfsum+s1*(sl+sr)
 193    continue
        cf1=cfsum/(jb-ja)
c	cl=cp1+cf1
c
c       ************** writing cl into file cl ******************
c
	open(12,file='cl1000',access='append')
        write(12,*)zeit,cp1,cf1
	close(12)
c
c     **********calculation of cd****************
c
	cpsum1=0.0
	do 199 j=ja,jb
	q11=8.*re*deltax
	q22=3.*(u2(ia-2,j)-2.*u2(ia-1,j))
	q33=9.*p(ia-1,j)/8.
	q44=p(ia-2,j)/8.
	ppl=q33-q44+(q22/q11)
	q55=3.*(u2(ib+2,j)-2.0*u2(ib+1,j))
	q66=9.*p(ib+1,j)/8.
	q77=p(ib+2,j)/8.
	ppr=q66-q77-(q55/q11)
	cpsum1=cpsum1+2.0*(ppl-ppr)
 199    continue
        cpd1=cpsum1/(jb-ja)
	sl1=0.
	sr1=0.
	cfsumd=0.
	do 191 i=ia,ib
	y11=3.0*deltay
	y22=9.*u2(i,ja-1)-u2(i,ja-2)
	sl1=(y22/y11)
	y33=9.*u2(i,jb+1)-u2(i,jb+2)
	sr1=(y33/y11)
	cfsumd=cfsumd+s1*(sl1+sr1)
 191    continue
        cfd1=cfsumd/(ib-ia)
c	cd=cpd1+cfd1
c
c       ************** writing cd into file cd ******************
c
	open(17,file='cd1000',access='append')
        write(17,*)zeit,cpd1,cfd1
	close(17)
c
	if(ita.gt.5500)then
	dt_vel_m=dt_vel_m+deltat
	do i=1,iim
	do j=1,jim
	um(i,j)=um(i,j)+deltat*u2(i,j)
	vm(i,j)=vm(i,j)+deltat*v2(i,j)
	xkm(i,j)=xkm(i,j)+deltat*xk2(i,j)
	dsm(i,j)=dsm(i,j)+deltat*d2(i,j)
	enddo
	enddo
	endif
c
	if(ita.gt.7000)then
	dt_rms=dt_rms+deltat
	do i=1,iim
	do j=1,jim
	if(ita.eq.ita/150*150)then
	umi(i,j)=um(i,j)/dt_vel_m
	vmi(i,j)=vm(i,j)/dt_vel_m
	endif
	u2p(i,j)=u2p(i,j)+deltat*(u2(i,j)-umi(i,j))**2.0
	v2p(i,j)=v2p(i,j)+deltat*(v2(i,j)-vmi(i,j))**2.0
	uv2p(i,j)=uv2p(i,j)+deltat*(v2(i,j)-vmi(i,j))*
     1            (u2(i,j)-umi(i,j))
	enddo
	enddo
	endif
c
c
	if(ita.eq.ita/25*25)goto 777
        if(dtmax.le.stat .or. ita.ge.itamax)goto 777 
	goto 888
  777	open (1, file='result1',form='formatted')
	write(1,*)re,ita,zeit,deltat
	write(1,*)iim,jim,ia,ib,ja,jb
	do 197 i=1,iim
	do 197 j=1,jim
	write(1,*)u2(i,j),v2(i,j),p(i,j),xk2(i,j),d2(i,j)
 197	continue
	close(1)
c
	if(ita.gt.5500)then
	open(11,file='res_m',form='formatted')
	write(11,*)re,ita,dt_vel_m,deltat
	write(11,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	write(11,*)um(i,j),vm(i,j),xkm(i,j),dsm(i,j)
	enddo
	enddo
	close (11)
	endif
c
	if(ita.gt.7000)then
	open(12,file='res_rms',form='formatted')
	write(12,*)re,ita,dt_rms,deltat
	write(12,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	write(12,*)u2p(i,j),v2p(i,j),uv2p(i,j)
	enddo
	enddo
	close (12)
	endif	
c
 888    if(dtmax.le.stat .or. ita.ge.itamax) then
	stop
	endif
	return
	end
