ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
         subroutine ceqcp
c
c      mass continuity equation for  constant properties
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
      	divmax=0.
	dalt=0.
	imax=0
	jmax=0
c
	do 70 i=2,ire
	do 70 j=2,jre
c
	div=(u2(i,j)-u2(i-1,j))/deltax
     $    +(v2(i,j)-v2(i,j-1))/deltay
        deltap=-beta*div                      
	a=deltat*deltap
	ax=a/deltax
 	ay=a/deltay
c
	p(i,j)=p(i,j)+deltap
	u2(i,j)=u2(i,j)+ax
	u2(i-1,j)=u2(i-1,j)-ax
	v2(i,j)=v2(i,j)+ay
	v2(i,j-1)=v2(i,j-1)-ay
c
	dab=dabs(div)
	if(dab.gt.dalt) then
	divmax=dab
	imax=i
	jmax=j
	end if
	dalt=divmax
c
 70	continue
c
	iti=iti+1
	isum=isum+1
c
c	if(iti.eq.1 .or. iti.eq.(iti/50*50)) then
c        write(6,71)ita,iti,isum,divmax,imax,jmax 
c 71	format(2x,3i7,4x,e15.8,2i5,1x,'from ceqcp')
c	end if
	return
	end
