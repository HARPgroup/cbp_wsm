************************************************************************
**  blanks out the label variable                                     **
************************************************************************

      subroutine ziplabel(label)

      implicit none
      include 'Rdaily.inc'

      integer i

      do i = 1,10
        label(i) = '                         '
      end do
      return
      end
