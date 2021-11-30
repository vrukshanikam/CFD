csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine init
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
$include 'covar'                       
c
c     write(6,*) 'has entered init'
c
      open(60,file='daten',form='formatted')
c
      read(60,*)icopro,ipara,icpara,irest,iuprof,
     *          jstam,iexit,itcalc,               
     *          stab,updef,
     *          iim,jim,kim,jn1,jnim,kn1,knim,kb,ia,ib,itamax,
     *          fx,fy,fz,epsi,stat,tstat,beta0,alphat,
     *          re,pr,gam,xm2,twall,tentry,rtwti,grf 
c
c
      write(16,111) icopro,ipara,icpara,irest,iuprof,
     *             jstam,iexit,itcalc,
     *             stab,updef,
     *             iim,jim,kim,jn1,jnim,kn1,knim,kb,ia,ib,itamax,
     *             fx,fy,fz,epsi,stat,tstat,beta0,alphat,
     *             re,pr,gam,xm2,twall,tentry,rtwti,grf
c
c
      open(16,file= 'trident',form='formatted')
c
c
      write(16,111) icopro,ipara,icpara,irest,iuprof,
     *             jstam,iexit,itcalc,
     *             stab,updef,
     *             iim,jim,kim,jn1,jnim,kn1,knim,kb,ia,ib,itamax,
     *             fx,fy,fz,epsi,stat,tstat,beta0,alphat,
     *             re,pr,gam,xm2,twall,tentry,rtwti,grf
c
c
 111  format(2x,8i4,/
     *	     2x,2e13.5,/
     *	     2x,11i5,/
     *	     4x,3f8.4,/
     *       2x,5e13.5,/
     *	     2x,8e13.5)
c
c
      close(60)
      close(16)
c
c***********plate-fin with or without stamping*****************
c
      if(jstam.ne.1) then
      go to 777
      else
c       
      open(70,file= 'datj',form='formatted')
      do 10 i=1,iim
      read (70,*) (jstamp(i,k),k=1,kim)
 10   continue 
      close (70)
c
c
c
c     do 20 i=1,iim
c     read(70,*) (kstamp(i,j),j=1,jim)
c20   continue
c
      end if
c
c
 777  ire =iim-1
      jre =jim-1
      kre= kim-1
c
c
      if ((abs(fy-1.0)).le.0.001) then
      deltay = 1./(jre-1)
      deltax = fx*deltay
      deltaz = fz*deltay
      end if
c
c
      if ((abs(fz-1.0)).le.0.001) then
      deltaz = 1./(kre-1)
      deltay =fy*deltaz
      deltax =fx*deltaz
      end if 
c
      rdx =1./deltax
      rdy =1./deltay
      rdz =1./deltaz
c
      deltx2 =deltax**2
      delty2 =deltay**2
      deltz2 =deltaz**2
c
c*******************constant properties**************************
c
      if (icopro.eq.1) go to 11
c
      gam1 =gam-1
      gam2 =2.-gam
      garepr =gam/(re*pr)
c
 11   deltat =stab*deltax
      rev =1./re
      pev =1./(re*pr)
      idt =iim-2
      jdt =jim-2
      kdt =kim-2
c
c
c     write(6,*)'leaving init'
c
      return
      end
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss

