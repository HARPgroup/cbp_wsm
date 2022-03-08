************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine printall(
     I            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'tempcal.inc'

      integer i,j

      print*,(BSLT(i) ,i=1,nlu)
      print*,(ULTP2(i) ,i=1,nlu)


      print*,(limitsASLT(i) ,i=1,4)
      print*,(limitsULTP1(i) ,i=1,4)
      print*,(limitsLGTP1(i) ,i=1,4)

      return
      end

