      function addvector(x,n,vsize,err)
      implicit none
      integer n,err,vsize
      integer i,j,k
      real x(vsize),y(vsize),addvector

      err = 0

      j = n

      y(1) = x(1)

      do i = 2,j,2
        y(i-1) = x(i-1)+x(i)
      end do

      if (mod(j,2).ne.0) then
        j = j - 1
        y(j-1) = y(j-1) + x(j+1)
      end if

      k = 2

      do 
        k = k * 2
        if (k.gt.n) exit

        do i = k,j,k
          y(i-k+1) = y(i-k+1) + y(i-k/2+1)
        end do
        if (mod(j,k).ne.0) then
          j = j - k/2
          y(j-k+1) = y(j-k+1) + y(j+1)
        end if
      end do
      addvector = y(1)

      return
      end
        
