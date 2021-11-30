cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine nseqcp
c
c
c       navier stokes equations for constant properties
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'has entered nseqcp'
c
	zeit=zeit+deltat
c
	do 150 i=2,ire-1
	do 150 j=2,jre
c
c ************ obstacle boundary  conditions *************
c                
c  	if (i.ge.(ia-1).and.i.le.ib) then
c	 if (j.eq.ja) then
c	    u1(i,j)=-u1(i,j-1)
c	    endif
c	    if(j.eq.jb) then
c	    u1(i,j)=-u1(i,j+1)
c	    endif
c	  endif
c	
c	if(i.eq.(ia-1)) then
c	if(j.ge.(ja-1).and.j.le.jb) then
c	v1(i+1,j)=-v1(i,j)
c	endif
c	endif
c
c	if(i.eq.(ib+1)) then
c	if(j.ge.(ja-1).and.j.le.jb) then
c	v1(i-1,j)=-v1(i,j)
c	endif
c	endif
c
ccccccccccccccccccccccccc  d i f f - u ccccccccccccccccccccccccccccccc
c
c       duv /dy
	u5 =u1(i,j-1) +  u1(i,j)
	u6 =u1(i,j-1) -  u1(i,j)
	u7 =u1(i,j)   +  u1(i,j+1)
	u8 =u1(i,j)   -  u1(i,j+1)
c
c       duv /dx
	u13=u1(i-1,j) + u1(i-1,j+1)
	u14=u7
	u153=u1(i-1,j) - u1(i-1,j+1)
c
c       duudx
	u1a=u1(i-1,j) + u1(i,j)
	u22=u1(i-1,j) - u1(i,j)
	u3 =u1(i,j)   + u1(i+1,j)
	u4 =u1(i,j)   - u1(i+1,j)
c
ccccccccccccccccccccccccc  d i f f - v ccccccccccccccccccccccccccccccc
c       dvu /dx
	v1a=v1(i,j-1) + v1(i+1,j-1)
	v22=v1(i,j)   + v1(i+1,j)
c
c       duv /dx
	v3 =v1(i-1,j)  + v1(i,j)
	v4 =v1(i-1,j)  - v1(i,j)
	v5 =v1(i,j)    + v1(i+1,j)
	v6 =v1(i,j)    - v1(i+1,j)
c
	v154=v1(i,j-1) - v1(i+1,j-1)
c
c       dvv /dy
	v7 =v1(i,j-1)  + v1(i,j)
	v8 =v1(i,j-1)  - v1(i,j)
	v9 =v1(i,j)    + v1(i,j+1)
	v10=v1(i,j)    - v1(i,j+1)
c
c	viscosity are calculated at the cell faces
c
	vist1=vist(i-1,j)+vist(i-1,j+1)
	vist2=vist(i,j)+vist(i,j+1)
	vist3=vist(i,j)+vist(i,j-1)
	vist4=vist(i+1,j-1)+vist(i+1,j)
	vist5=vist2
	vist6=vist(i+1,j)+vist(i+1,j+1)
cccccccccccccccccccccccc   u - deq   ccccccccccccccccccccccccccccccccc
c
	dpdx=(p(i,j)-p(i+1,j))/deltax
c
	duudx=-0.25*(u1a*u1a + alpha*dabs(u1a)*u22 - 
     $            u3*u3 - alpha*dabs(u3)*u4)/deltax
	dvudy=-0.25*( v1a*u5 + alpha *dabs(v1a)*u6 -
     $             v22*u7  - alpha*dabs(v22)*u8)/deltay
c
	xt1a=(1.0+vist(i+1,j))*u4
	xt2a=(1.0+vist(i,j))*u22
	xt1=(xt2a-xt1a)/(deltx2*re)
c
	xt1aa=0.25*(vist5+vist6)
	xt2aa=0.25*(vist3+vist4)
c
c
	xt1ab=-((u8/deltay)+(v6/deltax))
	xt1a=(1.0+xt1aa)*xt1ab
	xt2ab=-((u6/deltay)+(v154/deltax))
	xt2a=(1.0+xt2aa)*xt2ab
	xt2=(xt1a-xt2a)/(deltay*re)
c	write(*,*)'xt2',i,j,xt2
c
	if(j.eq.2)then 
c
	xt1ab=-((u8/deltay)+(v6/deltax))
	xt1a=(1.0+xt1aa)*xt1ab
c
	call wall_u(i,j,ftaum)
c
	xt2=(xt1a/re-ftaum)/deltay
c	write(*,*)'xt2a',i,j,xt2
c
	endif
c
	if(j.eq.jre)then 
c
	xt2ab=-((u6/deltay)+(v154/deltax))
	xt2a=(1.0+xt2aa)*xt2ab
c
	call wall_u(i,j,ftaum)
c
	xt2=(-ftaum-xt2a/re)/deltay
c
c	write(*,*)'xt2a',i,j,xt2
	endif
c
	if(i.ge.(ia-1) .and. i.le.(ib))then
	if(j.eq.(ja-1))then
c
	xt2ab=-((u6/deltay)+(v154/deltax))
	xt2a=(1.0+xt2aa)*xt2ab
c
	call wall_u(i,j,ftaum)
c
	xt2=(-ftaum-xt2a/re)/deltay
c
	endif
c
	if(j.eq.(jb+1))then
c
	xt1ab=-((u8/deltay)+(v6/deltax))
	xt1a=(1.0+xt1aa)*xt1ab
c
	call wall_u(i,j,ftaum)
c
	xt2=(xt1a/re-ftaum)/deltay
c
	endif
c
	endif
c
	xtt=(2.0*xt1+xt2)
c
        residu=(-duudx-dvudy+xtt)
	if(ita.eq.1 .or. irest.eq.1)then
	u2(i,j)=u1(i,j)+deltat*(residu+dpdx)
	else
	u2(i,j)=u1(i,j)+deltat*(0.5*(3.0*residu-residu_0(i,j))+dpdx)
	endif
	residu_0(i,j)=residu
c
ccccccecccccccccccccccc    v - deq   ccccccccccccccccccccccccccccccc
c
	dpdy = (p(i,j) - p(i,j+1))/deltay
c
	duvdx = -0.25*(u13*v3 +alpha*dabs(u13)*v4-
     $              u14*v5 - alpha*dabs(u14)*v6)/deltax
c
	dvvdy= -0.25*(v7*v7 + alpha*dabs(v7)*v8 -
     $             v9*v9 - alpha*dabs(v9)*v10)/deltay
c
	yt1a=(1.0+vist(i,j+1))*v10
	yt2a=(1.0+vist(i,j))*v8
	yt1=(yt2a-yt1a)/(delty2*re)
c
	yt1aa=0.25*(vist5+vist6)
	yt2aa=0.25*(vist1+vist2)
c
	yt1ab=-((v6/deltax)+(u8/deltay))
	yt1a=(1.0+yt1aa)*yt1ab
	yt2ab=-((u153/deltay)+(v4/deltax))
	yt2a=(1.0+yt2aa)*yt2ab
c
	yt2=(yt1a-yt2a)/(deltax*re)
c
	if(j.ge.(ja-1) .and. j.le.(jb))then
c
	if(i.eq.(ia-1))then
c
	yt1ab=-((v6/deltax)+(u8/deltay))
	yt1a=(1.0+yt1aa)*yt1ab
c
	call wall_v(i,j,ftaum)
c
	yt2=(yt1a/re-ftaum)/deltax
c
	endif
c
	if(i.eq.(ib+1))then
c
	yt2ab=-((u153/deltay)+(v4/deltax))
	yt2a=(1.0+yt2aa)*yt2ab
c
	call wall_v(i,j,ftaum)
c
	yt2=(-ftaum-yt2a/re)/deltax
c
	endif
c
	endif
c
	ytt=(2.0*yt1+yt2)
c
        residv=(-duvdx-dvvdy+ytt)
        if(ita.eq.1 .or. irest.eq.1)then
        v2(i,j)=v1(i,j)+deltat*(residv+dpdy)
        else
        v2(i,j)=v1(i,j)+deltat*(0.5*(3.0*residv-residv_0(i,j))+dpdy)
        endif
        residv_0(i,j)=residv
c
 150	continue
c
	return
	end
