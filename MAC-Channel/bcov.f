cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
         subroutine bcov(i,j,k)
c
c
c        velocity-boundary-conditions for the obstacle: WINGLET 
c        to be called by the nseqcp.f and nseqvp.f
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
$include 'covar'
c
c
	 if(i.ge.(ia-1).and.i.le.ib) then
	   if(k.eq.((kb+1)+(i-ia))) then
	     if(j.le.(3+(i-ia))) then
	       u5=0.0
	       u6=-2.*u1(i,j,k)
             endif
           endif
         endif
c
c
         if(i.ge.ia.and.i.le.(ib+1)) then
	       if(k.eq.(kb+(i-ia))) then
c		if(i.le.ib) then
                 if(j.le.(1+(i-ia))) then
		 v13=0.0
		 v14=2.*v1(i,j,k)
                 endif
c
                 if(j.le.((2)+(i-ia))) then
		 v15=0.0
c                endif
                endif
c
                 if(j.le.((1)+(i-ia))) then
		 v3=0.0
		 v4=-2.*v1(i,j,k) 
                 endif
             endif
           endif
c
c
	  if(i.ge.(ia-1).and.i.le.ib) then
	    if(k.eq.((kb+1)+(i-ia))) then
	      if(j.le.(3+(i-ia))) then
		v1a=0.0
              endif
c
          if(j.le.(2+(i-ia))) then
	    v5=0.0
	    v6=2.*v1(i,j,k)
	    v22=0.0
	  endif
c   
c
	  if(j.le.(1+(i-ia))) then
	    v11=0.0
	    v12=-2.*v1(i,j,k)
          endif
        endif
      endif
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	  return
	  end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
