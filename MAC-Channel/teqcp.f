csssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine teqcp
c
c
c     energy-equation for constant properties
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
$include 'covar'
c
      write(6,*)'has entered teqcp'
      al = alphat
      dt =0.
      dalt = 0.
      deltt  = 0.015
c
c
      do 10 i=2,ire
      do 10 j=2,jre
      do 10 k=2,kre
c
c
      t1a = t1(i,j,k)+t1(i+1,j,k)
      t2a = t1(i,j,k)-t1(i+1,j,k)
      t3  = t1(i-1,j,k)+t1(i,j,k)
      t4  = t1(i-1,j,k)-t1(i,j,k)
      t5  = t1(i,j,k)+t1(i,j+1,k)
      t6  = t1(i,j,k)-t1(i,j+1,k)
      t7  = t1(i,j-1,k)+t1(i,j,k)
      t8  = t1(i,j-1,k)-t1(i,j,k)
      t9  = t1(i,j,k)+t1(i,j,k+1)
      t10 = t1(i,j,k)-t1(i,j,k+1)
      t11 = t1(i,j,k-1)+t1(i,j,k)
      t12 = t1(i,j,k-1)-t1(i,j,k)
c
c
c**************** temperature-obstacle-boundary-conditions ***********
c
      call bcot(i,j,k)   
c
c*********************************************************************
c
c
c
      dtudx = (u2(i,j,k)*t1a+al*dabs(u2(i,j,k))*t2a
     *      -  u2(i-1,j,k)*t3-al*dabs(u2(i-1,j,k))*t4)
     *         *0.5/deltax
c
      dtvdy = (v2(i,j,k)*t5+al*dabs(v2(i,j,k))*t6      
     *        -v2(i,j-1,k)*t7-al*dabs(v2(i,j-1,k))*t8)
     *        *0.5/deltay
      dtwdz = (w2(i,j,k)*t9+al*dabs(w2(i,j,k))*t10
     *      -w2(i,j,k-1)*t11-al*dabs(w2(i,j,k-1))*t12)
     *      *0.5/deltaz
c
c
      d2tdx2 = (t4-t2a)/deltx2
      d2tdy2 = (t8-t6)/delty2
      d2tdz2 = (t12-t10)/deltz2
c
c
      residt = deltt*(-dtudx-dtvdy-dtwdz+pev*(d2tdx2+d2tdy2+d2tdz2))
c                                               
      t2(i,j,k) = t1(i,j,k)+residt
c
c
c
      dab = dabs(t2(i,j,k)-t1(i,j,k))
      if(dab.gt.dalt) then
         dt = dab
         itmax = i
         jtmax = j
         ktmax = k
      end if
      dalt=dt
c
 10   continue
c
      write(6,*) 'leaving teqcp'
      return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss




