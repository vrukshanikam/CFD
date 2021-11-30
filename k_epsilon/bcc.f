ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         subroutine bcc
c
c
c        boundary conditions for the mass continuity equation
c        conditions for the confining surfaces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	 include 'header'
c
c  	 write(*,*)'entered bcc'
c
	 i=1
	 do 50 j=1,jim
	 u2(i,j)=qinlet(j)
 50 	 continue
c
	 j=1
	 do 51 i=1,iim
	 v2(i,j)=0.
 51	 continue
c
	 j=jim
	 do 52 i=1,iim
	 v2(i,j-1)=0.
 52      continue
c
c  ******  obstacle boundary conditions ********
c
	 call bcobw
c
c **********************************************
c
         if(iti.ge.1) goto 55
	 if(iexit.eq.1) then
	 i=iim-2
	 do 53 j=1,jim
	 u2(i+1,j)=u2(i,j)
	 v2(i+1,j)=v2(i,j)
 53      continue
	 endif
	 if (iexit.eq.2) then
	 i=iim-2
	 do 54 j=1,jim
	 u2(i+1,j)=2.0*u2(i,j)-u2(i-1,j)
	 v2(i+1,j)=2.*v2(i,j)-v2(i-1,j)
 54      continue
	 endif
c
	 if (iexit.eq.3) then
	 i=iim-2
	 do 75 j=1,jim
	 u2(i+1,j)=u1(i+1,j)-(deltat/deltax)*uc*
     $	           (u1(i+1,j)-u1(i,j))
	 v2(i+1,j)=v1(i+1,j)-(deltat/deltax)*uc*
     $	           (v1(i+1,j)-v1(i,j))
 75	 continue
	 endif
c
c	 write(*,*)'leaving bcc'
 55      return
         end
