cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine kdeqcp
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
	 include 'header'
c
c      write(6,*)'has entered kdeqcp'
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
       al = alphaxk
c
       itkd=itkd+1
c
      do 20 i=2,ire
      do 20 j=2,jre
c
ccccccccccccccccc  diff-u     ccccccccccccccccccccccccccccccc
c
	if(i.ge.ia .and. i.le.ib .and. 
     $	j.ge.ja .and. j.le.jb) goto 20
c
c     duudx
        u1a = u2(i-1,j)  +  u2(i,j)
        u22 = u2(i-1,j)  -  u2(i,j)
        u3  = u2(i,j)    +  u2(i+1,j)
        u4  = u2(i,j)    -  u2(i+1,j)
c
c     duv  / dy
        u5 = u2(i,j-1)   +  u2(i,j)
        u6 = u2(i,j-1)   -  u2(i,j)
        u7 = u2(i,j)     +  u2(i,j+1)
        u8 = u2(i,j)     -  u2(i,j+1)
c
c     duv/dx
        u13 = u2(i-1,j)  + u2(i-1,j+1)
        u14 = u7
c
cccccccccccccccccccccc    diff -v    ccccccccccccccccccccccc
c     dvu / dx
        v1a = v2(i,j-1)   + v2(i+1,j-1)     
        v22 = v2(i,j)     + v2(i+1,j) 
c
c     duv / dx
        v3 = v2(i-1,j)    + v2(i,j)
        v4 = v2(i-1,j)    - v2(i,j)
        v5 = v2(i,j)      + v2(i+1,j)
        v6 = v2(i,j)      - v2(i+1,j)
c
c     dvv / dy
        v7 = v2(i,j-1)    + v2(i,j)
        v8 = v2(i,j-1)    - v2(i,j)
        v9 = v2(i,j)      + v2(i,j+1)
        v10= v2(i,j)      - v2(i,j+1)  
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       xk1a = xk1(i,j)+xk1(i+1,j)
       xk2a = xk1(i,j)-xk1(i+1,j)
       xk3  = xk1(i-1,j)+xk1(i,j)
       xk4  = xk1(i-1,j)-xk1(i,j)
       xk5  = xk1(i,j)+xk1(i,j+1)
       xk6  = xk1(i,j)-xk1(i,j+1)
       xk7  = xk1(i,j-1)+xk1(i,j)
       xk8  = xk1(i,j-1)-xk1(i,j)
c
       d1a = d1(i,j)+d1(i+1,j)
       d2a = d1(i,j)-d1(i+1,j)
       d3  = d1(i-1,j)+d1(i,j)
       d4  = d1(i-1,j)-d1(i,j)
       d5  = d1(i,j)+d1(i,j+1)
       d6  = d1(i,j)-d1(i,j+1)
       d7  = d1(i,j-1)+d1(i,j)
       d8  = d1(i,j-1)-d1(i,j)
c
       vis1a = vist(i,j)+vist(i+1,j)
       vis3  = vist(i-1,j)+vist(i,j)
       vis5  = vist(i,j)+vist(i,j+1)
       vis7  = vist(i,j-1)+vist(i,j)
c
	ut1=(0.25*(u2(i,j+1)+u2(i-1,j+1)
     &       -u2(i,j-1)-u2(i-1,j-1)))/deltay
c
        xv1a=v2(i+1,j)+v2(i+1,j-1)
        xv3=v2(i-1,j)+v2(i-1,j-1)
c
c
	if(i.ge.ia-1 .and. i.le.ib)then
	if(j.eq.(ja-1))then
	ut1=(0.25*(-u2(i,j)-u2(i-1,j)
     &       -u2(i,j-1)-u2(i-1,j-1)))/deltay
	else
	endif
	endif
c
	if(i.ge.ia-1 .and. i.le.ib)then
	if(j.eq.(jb+1))then
c
	ut1=(0.25*(u2(i,j+1)+u2(i-1,j+1)
     &       +u2(i,j)+u2(i-1,j)))/deltay
	else
	endif
	endif
c
	if(j.ge.ja-1 .and. j.le.jb)then
	if(i.eq.(ia-1))then
c
        xv1a=-v2(i,j)-v2(i,j-1)
	else
	endif
	endif
c
	if(j.ge.ja-1 .and. j.le.jb)then
	if(i.eq.(ib+1))then
c
        xv3=-v2(i,j)-v2(i,j-1)
	else
	endif
	endif
c
	xt1=(-u22/deltax)**2.0
	xt2=(-v8/deltay)**2.0
c
	vt1=(0.25*(xv1a-xv3))/deltax
	xt4=(ut1+vt1)**2.0
	xt5=(ut1-vt1)
c
	xttg=2.0*(xt1+xt2)+xt4
c
	ss=dsqrt(xttg)*(xk1(i,j)/d1(i,j))
	ro=(xk1(i,j)/d1(i,j))*dabs(xt5)
c
c        GG=vist(i,j)*rev*xttg
	GG=cm*d1(i,j)*ss*ro
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        xka1=0.5*vis1a
	xka2=-xk2a/deltax
        xkb1=0.5*vis3
	xkb2=-xk4/deltax
	xkt1=(xka1*xka2-xkb1*xkb2)/deltax
c
        xka1=0.5*vis5
	xka2=-xk6/deltay
        xkb1=0.5*vis7
	xkb2=-xk8/deltay
c
	xkt2=(xka1*xka2-xkb1*xkb2)/deltay
c
        xkt=xkt1+xkt2
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        da1=0.5*vis1a
	da2=-d2a/deltax
        db1=0.5*vis3
	db2=-d4/deltax
	ddt1=(da1*da2-db1*db2)/deltax
c
        da1=0.5*vis5
	da2=-d6/deltay
        db1=0.5*vis7
	db2=-d8/deltay
	ddt2=(da1*da2-db1*db2)/deltay
c
        ddt=ddt1+ddt2
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c             WEIGHTED-AVERAGED SCHEME 
c
ccccccccccccccccccccccc K-EQN cccccccccccccccccccccccccccccccc
c
c
      dxkudx =(u2(i,j)*xk1a+al*dabs(u2(i,j))*xk2a
     *      -  u2(i-1,j)*xk3-al*dabs(u2(i-1,j))*xk4)
     *         *0.5/deltax
c
      dxkvdy =(v2(i,j)*xk5+al*dabs(v2(i,j))*xk6      
     *        -v2(i,j-1)*xk7-al*dabs(v2(i,j-1))*xk8)
     *        *0.5/deltay
c
cccccccccccccccccccccc D-EQN ccccccccccccccccccccccccccccccc
c
      ddudx = (u2(i,j)*d1a+al*dabs(u2(i,j))*d2a
     *      -  u2(i-1,j)*d3-al*dabs(u2(i-1,j))*d4)
     *         *0.5/deltax
c
      ddvdy = (v2(i,j)*d5+al*dabs(v2(i,j))*d6      
     *        -v2(i,j-1)*d7-al*dabs(v2(i,j-1))*d8)
     *        *0.5/deltay
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc     
c                                               
       residxk =deltat_ke*(-dxkudx-dxkvdy+((rev/sk)*xkt))
c                                               
       residd = deltat_ke*(-ddudx-ddvdy+(rev/sd)*ddt
     *         +((d1(i,j)/xk1(i,j))*(c1*GG-c2*d1(i,j))))
c                                               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c                                               
      residxk=residxk+deltat_ke*(GG-d1(i,j))
c                                               
      xk2(i,j) = xk1(i,j)+residxk
      d2(i,j) = d1(i,j)+residd
      if(xk2(i,j).le.0.0)then
      xk2(i,j)=dabs(xk2(i,j))
c      xk2(i,j)=0.0
      endif
      if(d2(i,j).le.0.0)then
      d2(i,j)=dabs(d2(i,j))
c      d2(i,j)=0.1e-9
      endif
c                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c
	if(j.eq.2 .or. j.eq.jre)then
	call wallkd_x(i,j)
	endif
c                                               
c	if(i.ge.ia .and. i.le.(ib-1))then
	if(i.ge.ia-1 .and. i.le.ib+1)then
	if(j.eq.(ja-1))then
	call wallkd_x(i,j)
	endif
	if(j.eq.(jb+1))then
	call wallkd_x(i,j)
	endif
	endif
c
	if(j.ge.ja .and. j.le.jb)then
	if(i.eq.(ia-1))then
	call wallkd_y(i,j)
	endif
	if(i.eq.(ib+1))then
	call wallkd_y(i,j)
	endif
	endif
c
 20    continue
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c      write(6,*) 'leaving kdeqcp'
c
      return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
