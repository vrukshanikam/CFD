csssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
c
      subroutine nseqcp
c
c
c     navier-stokes equations for constant properties
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c      write(6,*)'has entered nseqcp'   
c
c
c
      do 70 i=2,(ire-1)
      do 70 j=2,jre
      do 70 k=2,kre
c
c
c
ccccccccccccccccc  diff-u     ccccccccccccccccccccccccccccccc
c     duudx
        u1a = u1(i-1,j,k)  +  u1(i,j,k)
        u22 = u1(i-1,j,k)  -  u1(i,j,k)
        u3  = u1(i,j,k)    +  u1(i+1,j,k)
        u4  = u1(i,j,k)    -  u1(i+1,j,k)
c
c     duv  / dy
        u5 = u1(i,j-1,k)   +  u1(i,j,k)
        u6 = u1(i,j-1,k)   -  u1(i,j,k)
        u7 = u1(i,j,k)     +  u1(i,j+1,k)
        u8 = u1(i,j,k)     -  u1(i,j+1,k)
c
c     dwu  /  dz
        u9 = u1(i,j,k-1)   + u1(i,j,k)
        u10= u1(i,j,k-1)   - u1(i,j,k)
        u11= u1(i,j,k)     + u1(i,j,k+1)
        u12= u1(i,j,k)     - u1(i,j,k+1)
c
c     duv/dx
        u13 = u1(i-1,j,k)  + u1(i-1,j+1,k)
        u14 = u7
c
c     duw / dx
        u15 = u1(i-1,j,k)  + u1(i-1,j,k+1)
        u16 = u11
c
c
c
c
cccccccccccccccccccccc    diff -v    ccccccccccccccccccccccc
c     dvu / dx
        v1a = v1(i,j-1,k)   + v1(i+1,j-1,k)     
        v22 = v1(i,j,k)     + v1(i+1,j,k) 
c
c     duv / dx
        v3 = v1(i-1,j,k)    + v1(i,j,k)
        v4 = v1(i-1,j,k)    - v1(i,j,k)
        v5 = v1(i,j,k)      + v1(i+1,j,k)
        v6 = v1(i,j,k)      - v1(i+1,j,k)
c
c     dvv / dy
        v7 = v1(i,j-1,k)    + v1(i,j,k)
        v8 = v1(i,j-1,k)    - v1(i,j,k)
        v9 = v1(i,j,k)      + v1(i,j+1,k)
        v10= v1(i,j,k)      - v1(i,j+1,k)  
c
c     dwu / dz
        v11 = v1(i,j,k-1)   + v1(i,j,k)
        v12 = v1(i,j,k-1)   - v1(i,j,k) 
        v13 = v1(i,j,k)     + v1(i,j,k+1)
        v14 = v1(i,j,k)     - v1(i,j,k+1)    
c
c     dvw / dy
        v15 = v1(i,j-1,k)   + v1(i,j-1,k+1)
        v16 = v13
c
c
c
c
cccccccccccccccccc  diff - w  cccccccccccccccccccccc
c     dwu / dz
        w1a = w1(i,j,k-1)   + w1(i+1,j,k-1)
        w22 = w1(i,j,k)     + w1(i+1,j,k)
c
c     dwv / dz     
        w3 = w1(i,j,k-1)    + w1(i,j+1,k-1)
        w4 = w1(i,j,k)      + w1(i,j+1,k)
c
c     duw / dx   
        w5 = w1(i-1,j,k)    + w1(i,j,k)
        w6 = w1(i-1,j,k)    - w1(i,j,k)
        w7 = w1(i,j,k)      + w1(i+1,j,k)
        w8 = w1(i,j,k)      - w1(i+1,j,k)
c
c     dvw / dy
        w9 = w1(i,j-1,k)    + w1(i,j,k)
        w10 = w1(i,j-1,k)   - w1(i,j,k)
        w11 = w1(i,j,k)     + w1(i,j+1,k)
        w12 = w1(i,j,k)     - w1(i,j+1,k)
c
c     dww / dz 
       w13 = w1(i,j,k-1)    + w1(i,j,k)
       w14 = w1(i,j,k-1)    - w1(i,j,k)
       w15 = w1(i,j,k)      + w1(i,j,k+1)
       w16 = w1(i,j,k)      - w1(i,j,k+1)
c
c
c
c************* obstacle-boundary-conditions *********************
c
      call bcov(i,j,k)
c
c****************************************************************
c
c
cccccccccccccccccccccccc u - deq cccccccccccccccccccccccccccccccc
c
      d2udx2 = (u22 - u4 ) / deltx2
      d2udy2 = (u6  - u8 ) / delty2
      d2udz2 = (u10 - u12 )/ deltz2
c
      dpdx = (p(i,j,k) - p(i+1,j,k)) / deltax
c
c
      duudx = 0.25*( u1a*u1a + alpha*dabs(u1a)*u22 -
     *               u3*u3 -   alpha*dabs(u3)*u4)/deltax
c
      dvudy = 0.25*( v1a*u5 + alpha*dabs(v1a)*u6 -
     *               v22*u7 - alpha*dabs(v22)*u8)/deltay
c
      dwudz = 0.25*( w1a*u9  + alpha*dabs(w1a)*u10 -
     *               w22*u11 - alpha*dabs(w22)*u12) /deltaz
c
c     
      residu=deltat*(duudx+dvudy+dwudz+rev*(d2udx2+d2udy2+d2udz2))
     *       +u1(i,j,k)
c
c
      u2(i,j,k) = residu + deltat*dpdx
c
c
c
c
ccccccccccccccccccccccccccccc v - deq ccccccccccccccccccccccccccccccc
c
      d2vdx2 = (v4 - v6)/deltx2
      d2vdy2 = (v8 - v10)/delty2
      d2vdz2 = (v12 - v14)/deltz2
c
c
      dpdy = (p(i,j,k) - p(i,j+1,k))/deltay
c
c
      duvdx = 0.25*(u13*v3 + alpha*dabs(u13)*v4-
     *              u14*v5 - alpha*dabs(u14)*v6)/deltax
c
      dvvdy = 0.25*(v7*v7 + alpha*dabs(v7)*v8 -
     *              v9*v9 - alpha*dabs(v9)*v10)/deltay
c
      dwvdz = 0.25*(w3*v11 + alpha*dabs(w3)*v12 -
     *              w4*v13 - alpha*dabs(w4)*v14)/deltaz
c
c
c     body force due to buoyancy
      thf = t1(i,j,k) + t1(i,j+1,k)
       gy = 0.5*grf*rev*rev*thf
c
      residv = deltat*(duvdx+dvvdy+dwvdz+rev*(d2vdx2+d2vdy2+
     *              d2vdz2)+gy)+v1(i,j,k)
      v2(i,j,k) = residv + deltat*dpdy
c
c
c
c
c
c
ccccccccccccccccccccccccccc  w - deq ccccccccccccccccccccccccccccccc
c
      d2wdx2 = (w6 - w8)/deltx2
      d2wdy2 = (w10 - w12)/delty2
      d2wdz2 = (w14 - w16)/deltz2
c
      dpdz = (p(i,j,k)-p(i,j,k+1))/deltaz
c
c
      duwdx = 0.25*(u15*w5 + alpha*dabs(u15)*w6 -
     *              u16*w7 - alpha*dabs(u16)*w8)/deltax
c
      dvwdy = 0.25*(v15*w9 + alpha*dabs(v15)*w10 -
     *              v16*w11 -alpha*dabs(v16)*w12)/deltay
c
      dwwdz = 0.25*(w13*w13 + alpha*dabs(w13)*w14 -
     *              w15*w15 - alpha*dabs(w15)*w16)/deltaz
c
c
      residw = deltat*(duwdx+dvwdy+dwwdz+rev*(d2wdx2+d2wdy2+d2wdz2))
     *        +w1(i,j,k)       
c
c
      w2(i,j,k) = residw+deltat*dpdz
c
c
 70   continue
c
c      write(6,*) 'leaving nseqcp '
      return
      end
c
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss




