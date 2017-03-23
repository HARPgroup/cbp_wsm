************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine zipall(
     M      limitsKATRAD)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'tempcal.inc'

      integer i,j

      do i = 1,4
        limitsKATRAD(i) = -9.0
      end do

      return
      end

