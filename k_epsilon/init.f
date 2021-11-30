ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine init 
c
c       initiation of computing data
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(6,*)'has entered init'
    	open(8,file='daten')
	read(8,*)irest,iuprof,iexit,stab,updef,iim,jim,jn1,
     &		 jnim,ia,ib,ja,jb,itamax,fx,fy,epsi,epsi_ke,stat,
     &		 beta0,re,cm,c1,c2,sk,sd,alphaxk,xin,stab_ke,uc
	write(*,*)irest,iuprof,iexit,stab,updef,iim,jim,jn1,
     &		 jnim,ia,ib,ja,jb,itamax,fx,fy,epsi,epsi_ke,stat,
     &		 beta0,re,cm,c1,c2,sk,sd,alphaxk,xin,stab_ke,uc
c	write(*,*)' irest =',irest,' iuprof =',iuprof,' iexit =',
c     &	iexit,' stab =',stab,' updef =',updef,' iim =',iim,' jim =',
c     &	jim,' jn1 =',jn1
c     	write(*,*)' jnim =',jnim,' ia =',ia,' ib =',ib,' ja =',ja,
c     &	' jb =',jb,' itamax =',itamax,' fx =',fx,' fy =',fy,' epsi =',
c     &  epsi,' stat =',stat, 'epsi_ke =',epsi_ke
c    	write(*,*)' tstat =',tstat,' beta0 =',beta0,' alphat =',alphat,
c     &	' re =',re,' pr =',pr,' twall =',twall,' tobs =',tobs,
c     &  ' tentry =',tentry,' grf =',grf,' ifcp =',ifcp,
c     &  'cm='cm,'c1='c1,'c2='c2,'sk='sk,'sd='sd,'alphaxk='alphaxk
c
	ire=iim-1
	jre=jim-1
	ita=0
	isum=0
c    
	deltay=fy/(jre-1)
	deltax=fx*deltay
	dtmax=1.
c      
	rdx=1./deltax
	rdy=1./deltay
c
	deltx2=deltax**2
	delty2=deltay**2
c
c   *******  constant properties  *******
c
	deltat=stab*deltax
	rev=1./re
	pev=1./(re*pr)
	idt=iim-2
	jdt=jim-2
c
c	write(*,*)'leaving init'
	return
	end
