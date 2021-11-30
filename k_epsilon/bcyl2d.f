cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	    subroutine bcyl2d
c
cccccccccccc velocity boundary-conditions for obstacle cccccccccc
c
            include 'header'
c
	    j=ja
	    do i=(ia-1),ib
	    u1(i,j)=-u1(i,j-1)
	    enddo
c
	    j=jb
	    do i=(ia-1),ib
	    u1(i,j)=-u1(i,j+1)
	    enddo
c
	    i=ia
	    do j=(ja-1),jb
	    v1(i,j)=-v1(i-1,j)
	    enddo
c
	    i=ib
	    do j=(ja-1),jb
	    v1(i,j)=-v1(i+1,j)
	    enddo
c
	    return
	    end
