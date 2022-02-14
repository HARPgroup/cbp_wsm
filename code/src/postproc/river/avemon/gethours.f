************************************************************************
**  subroutine to get the first and last hours to average             **
************************************************************************

      subroutine gethours(sdate,year1,year2,hour1,hour2)

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/wdm.inc'

      integer sdate(ndate),asdate(ndate)

      integer year1,year2,hour1,hour2

      integer i,hour     ! indices

      do i = 1,6
        asdate(i) = sdate(i)
      end do

      i = 0.
      hour = 0
      do while (asdate(1).lt.year1)
        hour = hour + 1
        i = i + 1
        if (hour.eq.24) then
          hour = 0
          call tomorrow(asdate(1),asdate(2),asdate(3))
        end if
      end do
      hour1 = i + 1

      do while (asdate(1).le.year2)
        hour = hour + 1
        i = i + 1
        if (hour.eq.24) then
          hour = 0
          call tomorrow(asdate(1),asdate(2),asdate(3))
        end if
      end do
      hour2 = i 

      return

      end

