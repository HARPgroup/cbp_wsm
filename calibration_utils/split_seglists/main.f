************************************************************************
**  program to read in a segment list and split into as many equal    **
**   parts as requested                                               **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'

      integer splits  ! number of splits requested

************ END DECLARATIONS ******************************************

      read(*,*,err=981,end=982) fnam,splits  

      if (splits.gt.99) go to 983
      if (splits.lt. 1) go to 984

      call lencl(fnam,last)

      if (fnam(last-2:last).eq.'riv') call splitriv(fnam,last,splits)
      if (fnam(last-3:last).eq.'land') call splitland(fnam,last,splits)
      go to 980  ! should not get here

      stop

******************* ERROR SPACE ****************************************
980   report(1) = ' Error in split_seglists'
      report(2) = 'file must end in .riv, .land, or .calib'
      report(3) = ' '
      go to 999

981   report(1) = 'Error on input'
      report(2) = ' '
      report(3) = ' '
      go to 999

982   report(1) = 'too few input variables provided'
      report(2) = ' '
      report(3) = ' '
      go to 999

983   report(1) = ' you must run fewer than 100 splits or reprogram'
      report(2) = '  the only reprogramming issue is the file names'
      report(3) = ' '
      go to 999

984   report(1) = ' you must run at least 1 split'
      report(2) = '  '
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end
