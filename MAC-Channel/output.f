csssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine output
c
c
c      output of results
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c
c      write(6,*)'has entered output'
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      totime=totime+deltat
c      ip=55
c      jp=7
c      kp=22
c      up=u2(ip,jp,kp)
c      vp=v2(ip,jp,kp)
c      wp=-w2(ip,jp,kp)
c      open  (2, file='frequency1',form='formatted')
c      write (2 , 222) totime,up,vp,wp
c 222  format(2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5)
c      open  (3, file='frequency2',form='formatted')
c      write (3 , 333) totime,wp
c 333  format(2x,f10.5,2x,f10.5)
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
cccccccccccccccc   if(ita.eq.((ita/20)*20)) go to 9998
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if(ita.ge.itamax) go to 999
      if(dtmax.le.stat.and.ita.ne.1) go to 999
        open(67,file='iwrite',form='formatted')
        read(67,*) iwrite
        close(67)
      if(iwrite.eq.1)   go to 999
c
       go to 888
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
 999  if (itcalc.eq.0 .and. irest.eq.1) go to 9998 
c
      call energy
c
 9998 open(98,file='converge',form='formatted')
      write(98,*) ita,dtmax
      open(91,file='result',form='formatted')
      do 6666 i = 1,iim
      do 6666 j = 1,jim
      do 6666 k = 1,kim
      write(91,*) i,j,k,u2(i,j,k),v2(i,j,k),w2(i,j,k),p(i,j,k),t2(i,j,K)
 6666 continue
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      
       close(91)
c            
c
c****************** constant properties **************
c
      if(icopro.eq.1) go to 777
c
      open(66,file='result2',form='formatted')
c
      do 5555  i=1,iim
      do 5555  j=1,jim
      do 5555  k=1,kim
      write(66,*) i,j,k,ro(i,j,k),eta(i,j,k)
 5555 continue
c
      close(66)
c
c              
c
 777  if(dtmax.le.stat.and.ita.ne.1) stop
      if(ita.ge.itamax) then
      write(6,1111)
 1111 format(2x, 'solution did not converge')
      stop 
      end if
      if(iwrite.eq.1) stop
c888  write(6,*) 'leaving output '
 888  return 
      end
c
c
csssssssssssssssssssssssssssssssssssssssssssssssssssss
