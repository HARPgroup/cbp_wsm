************************************************************************
**  sorts Break dates in case they were not entered sequentially      **
************************************************************************
      subroutine sortB(jday,BBfile,n)
      implicit none

      include 'mbtc.f'
      integer jday(maxTimeBreaks)
      character*20 tempfile
      integer i,j,n,itemp

      do i = 1,n
        do j = 1, n-i
          if (jday(j).gt.jday(j+1)) then
            itemp = jday(j)
            jday(j) = jday(j+1)
            jday(j+1) = itemp
            tempfile = BBfile(j)
            BBfile(j) = BBfile(j+1)
            BBfile(j+1) = tempfile
          end if
        end do
      end do

      end
      
      
