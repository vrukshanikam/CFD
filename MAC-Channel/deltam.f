cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
c
      program deltam
c
c
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
$include 'covar'
c
c
c      write(6,*) 'entered deltam'
c
c
cccccccccccccccccccccccccccccccccccccccc ianf=mclock()
c
c
      totime=0.0
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      open(68,file='pro',form='formatted')  
c
c
      call init
c
c
      if(ipara.eq.1) call start
      if(irest.eq.1) call restar
      if(ipara.eq.1) go to 11
      if(irest.eq.0) call start
c
c
c
c
 11   if(icopro.ne.1) then
      if(jstam.eq.1) call bctst
      if(jstam.eq.0) call bct
      call state
      endif
c
c
c
c
 14   call conti
c


c
c********plate-fin with or without stamping**********
c
      if(jstam.eq.1) then
      call bcnsst
      else
      call bcns
      end if 
c
c****************************************************
c
      call velalt
c
c
      call ticorr
c
c
      call output
c
c
c     call energy
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c    
c     energy should not be commented for coupled
c     N-S and energy equations
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(icopro.ne.1) then
      call state
      else
      go to 16
      end if
c
c******constant or variable property****************** 
c
 16   if(icopro.eq.1) then
      call nseqcp
      else
      call nseqvp
      end if
c*****************************************************
c
c
c*****plate-fin with or without stamping**************
c
      if(jstam.eq.1) then
      call bcnsst
      else
      call bcns
      end if
c*****************************************************
c
      call tigrad
c
c
      go to 14
c
cccccccccccccccccccccccccccccccccccccc write(*,*) ianf
c
      end
c
c
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

