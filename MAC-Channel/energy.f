csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine energy
c
c
c     solution of the energy-equation
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c      write(6,*)'has entered energy'
c
      itt=1
c
c*************** plate-fin with stamping *********************
c
 10   if(jstam.eq.1) then  
      call bctst
      else
      call bct
      end if
c
c**************** constant properties ************************
c
      if(icopro.eq.1)then
      call teqcp
      else
      call teqvp
      end if
c
c      write(6,2) itt,dt,itmax,jtmax,ktmax
c     if(icopro.eq.1) then
c     write(68,2) itt,dt,itmax,jtmax,ktmax
c     end if
c 2    format(2x,i4,2x,'delta - t:',e13.5,' </itert>',3i4)
c
      call temalt
c
      itt = itt+1
c
      if(dt.ge.tstat) go to 10
c
c***************** plate-fin with stamping *******************
c
      if(jstam.eq.1) then
      call bctst
      else
      call bct
      end if
c***************** constant properties ***********************
c     if(icopro.eq.1) then
c     write(65) t2
c     rewind 65
c     stop
c     end if
c
c
      write(6,*) 'leaving energy '
      return
      end
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
