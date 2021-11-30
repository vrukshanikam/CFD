ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine bcot(i,j,k)
c
c
c     temperature-bountary-contitions for the obstacle: winglet
c     to be called by teqcp.f or teqvp.f
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	 include 'covar'
c
c
      if(i.ge.ia.and.i.le.ib) then    
          if(j.le.(2+(i-ia))) then 
            if(k.eq.((kb+1)+(i-ia))) then
                t11 =2.*twall
                t12 =-2.*t1(i,j,k)+2.*twall
               end if
c
c
            if(k.eq.((kb)+(i-ia))) then
                t9 =2.*twall
                t10 =2.*t1(i,j,k)-2.*twall
               end if
            end if
          end if
c
c
      if(i.ge.ia.and.i.le.ib) then
         if(j.le.(2+(i-ia))) then
              if(k.eq.((kb+1)+(i-ia))) then
               t1a =2.*twall
               t2a =2.*t1(i,j,k)-2.*twall
              end if
         end if
      end if
c
c
      if(i.ge.(ia+1).and.i.le.(ib+1)) then
         if(j.le.(1+(i-ia))) then
              if(k.eq.((kb)+(i-ia))) then
               t3 =2.*twall
               t4 =-2.*t1(i,j,k)+2.*twall
              end if
         end if
      end if
c
      return
      end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccc
