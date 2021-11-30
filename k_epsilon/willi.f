cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
c
         program willi 
c
c*******************************************************************
c*       program willi for computing flow field in a channel 
c*       with built in square cylinder
c***********************************************************************
c
	 include 'header'
c
c        write(*,23)
c23      format('has entered willi')
c
	open(68,file='pro',form='formatted')
c	open(2,file='frea1000',form='formatted')
c	open(12,file='cl1000',form='formatted')
c	open(17,file='cd1000',form='formatted')
c	open(27,file='phase1',form='formatted')
c	open(37,file='phase2',form='formatted')
c	open(47,file='phase3',form='formatted')
c	open(57,file='phase4',form='formatted')
c	open(67,file='phase5',form='formatted')
c
	call init
	if (irest.eq.0) call start
	if (irest.eq.1) call restar
 10     call conti
	open(3,file='iwrite',form='formatted')
	read (3,*) iwrite
	read (3,*) itime
	close(3)	
c
c
	if (iwrite.eq.1) then
	call otre
	end if
c
	call turbu
	call velalt
	call ticorr
	call otpt
	call bcyl2d
	call nseqcp
	call bcns
	call tigrad
c
c	*******************************
	if(ita.gt.3000) epsi=0.0001
 	if(ita.lt.3000 .and. ita.gt.2400)epsi=0.0005
 	if(ita.lt.2400 .and. ita.gt.1600)epsi=0.001
 	if(ita.lt.1600 .and. ita.gt.800)epsi=0.005
 	if(ita.lt.800 .and. ita.gt.600)epsi=0.01
 	if(ita.lt.600 .and. ita.gt.400)epsi=0.05
 	if(ita.lt.400 .and. ita.gt.200)epsi=0.1
 	if(ita.lt.200)epsi=0.5
c 	if(ita.gt.3000 .and.ita.lt.2000)epsi_ke=0.05
c 	if(ita.gt.1500 .and.ita.lt.1000)epsi_ke=1.00
c 	if(ita.ge.500 .and.ita.lt.1000)epsi_ke=2.5
c 	if(ita.lt.500)epsi_ke=5.0
c
c 	write(*,123) epsi
c 123    format(  'epsi = ',e10.4,$)
	goto 10
c	*******************************
c
	end
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
