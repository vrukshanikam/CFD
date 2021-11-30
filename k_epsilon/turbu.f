csssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine turbu 
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'header'
c
c      write(*,*)'entered turbu'
c
	itkd=0
c
	deltat_ke=stab_ke*deltat
c
        call bcc
c
        call bcns
c
   10	divkd=0.0
c
        call bckd
c
        call kdalt
c
        call kdeqcp
c
        call turvis
c
        call bckd
c
	do 180 i=2,ire
	do 180 j=2,jre
c
	if(i.ge.ia .and. i.le.ib .and. 
     $	j.ge.ja .and. j.le.jb)goto 180
c
c
        dkdt=dabs((xk2(i,j)-xk1(i,j)))/deltat_ke
        dddt=dabs((d2(i,j)-d1(i,j)))/deltat_ke
c
	dact=divkd
	divkd=dmax1(divkd,dkdt,dddt)
	if(divkd.le.dact) goto 180
c	write(*,*)i,j,'dkdt',dkdt,'dddt',dddt
	idtm=i
	jdtm=j
 180	continue
c
c	if(itkd.eq.(itkd/100*100))then
c	write(*,*)'divkd',itkd,idtm,jdtm,divkd
c	endif
c
c	if(ita.gt.1500)then
         if(itkd.lt.10)goto 10
c	else
c        if(divkd.gt.epsi_ke)goto 10
c	endif
c
c	write(*,*)'xk2(49,22),xk1(49,22)',xk2(49,22),xk1(49,22)
c	write(*,*)'d2(49,22),d1(49,22)',d2(49,22),d1(49,22)
c      write(*,*)'leaving turbu'
c
        return
c
        end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
