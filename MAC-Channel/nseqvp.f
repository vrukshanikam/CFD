ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      subroutine nseqvp
c
c
c     navier-stokes equations for variable properties
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
$include 'covar'
c
c     write(6,*)'has entered nseqvp'   
c
c
      do 70 i=2,(ire-1)
      do 70 j=2,jre
      do 70 k=2,kre
c
c
c
ccccccccccccccccccc  diff-u     cccccccccccccccccccccccccccccccccccc
c       duudx
        u1a = u1(i-1,j,k)  +  u1(i,j,k)
        u22 = u1(i-1,j,k)  -  u1(i,j,k)
        u3  = u1(i,j,k)    +  u1(i+1,j,k)
        u4  = u1(i,j,k)    -  u1(i+1,j,k)
c
c       duv  / dy
        u5 = u1(i,j-1,k)   +  u1(i,j,k)
        u6 = u1(i,j-1,k)   -  u1(i,j,k)
        u7 = u1(i,j,k)     +  u1(i,j+1,k)
        u8 = u1(i,j,k)     -  u1(i,j+1,k)
c
c       dwu  /  dz
        u9 = u1(i,j,k-1)   + u1(i,j,k)
        u10= u1(i,j,k-1)   - u1(i,j,k)
        u11= u1(i,j,k)     + u1(i,j,k+1)
        u12= u1(i,j,k)     - u1(i,j,k+1)
c
c       duv/dx
        u13 = u1(i-1,j,k)  + u1(i-1,j+1,k)
        u14 = u7
c
c       duw / dx
        u15 = u1(i-1,j,k)  + u1(i-1,j,k+1)
        u16 = u11
c
c       divdy
        u17   =  u1(i-1,j+1,k)- u1(i,j+1,k)
c
c       divdz
        u18   =  u1(i-1,j,k+1) - u1(i,j,k+1)
c
c
c
cccccccccccccccccccccc  diff -v   ccccccccccccccccccccccccccccccccccc
c       dvu / dx
        v1a = v1(i,j-1,k)   + v1(i+1,j-1,k)     
        v22 = v1(i,j,k)     + v1(i+1,j,k) 
c
c       duv / dx
        v3 = v1(i-1,j,k)    + v1(i,j,k)
        v4 = v1(i-1,j,k)    - v1(i,j,k)
        v5 = v1(i,j,k)      + v1(i+1,j,k)
        v6 = v1(i,j,k)      - v1(i+1,j,k)
c
c       dvv / dy
        v7 = v1(i,j-1,k)    + v1(i,j,k)
        v8 = v1(i,j-1,k)    - v1(i,j,k)
        v9 = v1(i,j,k)      + v1(i,j+1,k)
        v10= v1(i,j,k)      - v1(i,j+1,k)  
c
c       dwu / dz
        v11 = v1(i,j,k-1)   + v1(i,j,k)
        v12 = v1(i,j,k-1)   - v1(i,j,k) 
        v13 = v1(i,j,k)     + v1(i,j,k+1)
        v14 = v1(i,j,k)     - v1(i,j,k+1)    
c
c       dvw / dy
        v15 = v1(i,j-1,k)   + v1(i,j-1,k+1)
        v16 = v13
c
c       divdx
        v17  = v1(i+1,j-1,k)  - v1(i+1,j,k)
c
c       divdz
        v18  = v1(i,j-1,k+1)  - v1(i,j,k+1)
c
c
ccccccccccccccccccccccc  diff - w ccccccccccccccccccccccccccccccccccc
c       dwu/dz
        w1a = w1(i,j,k-1)   + w1(i+1,j,k-1)
        w22 = w1(i,j,k)     + w1(i+1,j,k)
c
c       dwv / dz     
        w3 = w1(i,j,k-1)    + w1(i,j+1,k-1)
        w4 = w1(i,j,k)      + w1(i,j+1,k)
c
c       duw / dx   
        w5 = w1(i-1,j,k)    + w1(i,j,k)
        w6 = w1(i-1,j,k)    - w1(i,j,k)
        w7 = w1(i,j,k)      + w1(i+1,j,k)
        w8 = w1(i,j,k)      - w1(i+1,j,k)
c
c       dvw / dy
        w9 = w1(i,j-1,k)    + w1(i,j,k)
        w10 = w1(i,j-1,k)   - w1(i,j,k)
        w11 = w1(i,j,k)     + w1(i,j+1,k)
        w12 = w1(i,j,k)     - w1(i,j+1,k)
c
c      dww / dz 
       w13 = w1(i,j,k-1)    + w1(i,j,k)
       w14 = w1(i,j,k-1)    - w1(i,j,k)
       w15 = w1(i,j,k)      + w1(i,j,k+1)
       w16 = w1(i,j,k)      - w1(i,j,k+1)
c
c      divdx
       w17 = w1(i+1,j,k-1)  - w1(i+1,j,k)
c
c      divdy
       w18 = w1(i,j+1,k-1)  - w1(i,j+1,k)
c
c
c************* obstacle-boundary-conditions ***********************
c
      call bcov(i,j,k)
c
c******************************************************************
c
c
cccccccccccccccccccccccccc   u - dgl  cccccccccccccccccccccccccccccccc
c
      d2udx2 = (u22 - u4 ) / deltx2
      d2udy2 = (u6  - u8 ) / delty2
      d2udz2 = (u10 - u12 )/ deltz2
c
      dpdx = (p(i,j,k) - p(i+1,j,k)) / deltax
c
c
      duudx = 0.25*( u1a*u1a + alpha*dabs(u1a)*u22 -
     *               u3*u3 - alpha*dabs(u3)*u4)/deltax
      dvudy = 0.25*( v1a*u5 + alpha*dabs(v1a)*u6 -
     *               v22*u7 - alpha*dabs(v22)*u8)/deltay
      dwudz = 0.25*( w1a*u9 +alpha*dabs(w1a)*u10 -
     *               w22*u11 - alpha*dabs(w22)*u12) /deltaz  
      divdx = ((u22-u4)/deltax+(v8-v17)/deltay
     *                        +(w14-w17)/deltaz)/deltax
c                                                      
      udiv = 0.5*u1(i,j,k)*((u3-u1a)/deltax+(v22-v1a)/deltay
     *                      +(w22-w1a)/deltaz)
c
      dro1 = 0.5*(ro(i+1,j,k)+ro(i,j,k))
      deta1 = 0.5*(eta(i+1,j,k)+eta(i,j,k))
c                                 
      residu=deltat*(duudx+dvudy+dwudz+udiv+rev*deta1/dro1*
     *              (d2udx2+d2udy2+d2udz2)
     *               +rev*deta1/(3.*dro1)*divdx)
c
c
      u2(i,j,k) = residu + deltat*dpdx/dro1+u1(i,j,k)
c
c
c
c    

cccccccccccccccccc v - dgl ccccccccccccccccccccccccccccccccccc
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
      dvvdy = 0.25*(v7*v7 + alpha*dabs(v7)*v8 -
     *              v9*v9 - alpha*dabs(v9)*v10)/deltay
      dwvdz = 0.25*(w3*v11 + alpha*dabs(w3)*v12 -
     *              w4*v13 - alpha*dabs(w4)*v14)/deltaz
c                                                       
      divdy = ((u22-u17)/deltax+(v8-v10)/deltay
     *        +(w14-w18)/deltaz)/deltay
c                                      
      vdiv = 0.5*v1(i,j,k)*((u7-u13)/deltax+(v9-v7)/deltay
     *                     +(w11-w3)/deltaz)   
c
      dro2 = 0.5*(ro(i,j+1,k)+ro(i,j,k))
      deta2 = 0.5*(eta(i,j+1,k)+eta(i,j,k))
c     body force due to buoyancy
      thf = t1(i,j+1,k) + t1(i,j,k)
      gy = 0.5*grf*rev*rev*thf
c           

      residv = deltat*(duvdx+dvvdy+dwvdz+vdiv+rev*deta2/dro2*
     *       (d2vdx2+d2vdy2+d2vdz2)
     *       +rev*deta2/(3.*dro2)*divdy+gy) 
c
      v2(i,j,k) = residv + deltat*dpdy/dro2+v1(i,j,k)
c
c
c
cccccccccccccccccc w - dgl cccccccccccccccccccccccc
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
      dvwdy = 0.25*(v15*w9 +alpha*dabs(v15)*w10 -
     *              v16*w11 - alpha*dabs(v16)*w12)/deltay
c
      dwwdz = 0.25*(w13*w13 + alpha*dabs(w13)*w14 -
     *           w15*w15 - alpha*dabs(w15)*w16)/deltaz
c                                                           
      divdz = ((u22-u18)/deltax+(v8-v18)/deltay
     *       +(w14-w16)/deltaz)/deltaz
c
      wdiv = 0.5*w1(i,j,k)*((u11-u15)/deltax+(v13-v15)/deltay
     *                     +(w15-w13)/deltaz)
c                                            
      dro3 = 0.5*(ro(i,j,k+1)+ro(i,j,k))
      deta3 = 0.5*(eta(i,j,k+1)+eta(i,j,k))
c
      residw = deltat*(duwdx+dvwdy+dwwdz+wdiv+rev*deta3/dro3*
     *                  (d2wdx2+d2wdy2+d2wdz2)
     *                  +rev*deta3/(3.*dro3)*divdz)       
c
c
      w2(i,j,k) = residw+deltat*dpdz/dro3+w1(i,j,k)
c
c
 70   continue
c
c     write(6,*) 'leaving nseqvp '
      return
      end
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc







