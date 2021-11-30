csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine conti
c
c     pressure iteration
c
csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
$include 'covar'
c
c
c      write(6,*)'has entered conti'
c
c
      iti=0
      ita=ita+1
      beta=beta0/(2.*deltat*(1./deltx2+1./delty2+1./deltz2))
c
c
c********* plate fin with or without stamping *********************
c
c
 1    if(jstam.eq.1) then
      call bccst
      else
      call bcc
      end if
c********** constant properties or variable properties *************
      if(icopro.eq.1) then
      call ceqcp
      else
      call ceqvp
      end if
      if(divmax.ge.epsi) go to 1
c
c
c      write(6,*)'leaving conti'
c
      return
      end
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
