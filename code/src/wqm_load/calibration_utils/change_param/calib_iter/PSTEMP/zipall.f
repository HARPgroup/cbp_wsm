************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine zipall(
     M      limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'tempcal.inc'

      integer i,j

      do i = 1,nlu
        BSLT(i) = 0.0
        ULTP2(i) = 0.0
      end do


      do i = 1,4
        limitsLGTP1(i) = -9.0
        limitsASLT(i) = -9.0
        limitsULTP1(i) = -9.0
      end do

      return
      end

