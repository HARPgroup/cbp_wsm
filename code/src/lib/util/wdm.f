************************************************************************
**  subroutine to copy time1 to time2                                 **
************************************************************************
      subroutine timecp(time1,time2)

      implicit none
      integer time1(6),time2(6),i

      do i = 1,6
        time2(i) = time1(i)
      end do

      end
