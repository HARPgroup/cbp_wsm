************************************************************************
** This program stores information about the parameters to optimize   **
************************************************************************
      subroutine readparams(
     I                      calscen,
     O                      parLKflag,parModule,parTable,parName,
     O                      parAorM,parstart,parmin,parmax,npar)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'
      character*300 longline

      call lencl(calscen,lenrscen)
      fnam = tree//'config/control/calib/WQ/'//calscen(:lenrscen)//
     .       '/param_list.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)') longline
      call d2x(longline,last)

      npar = 0

      read(dfile,'(a300)') longline
      call d2x(longline,last)
      do while(longline(:3).ne.'end')
        npar = npar + 1
        read(longline,*,end=992,err=993) 
     .       parLKflag(npar),parModule(npar),parTable(npar),
     .       parName(npar),parstart(npar),parmin(npar),parmax(npar),
     .       parAorM(npar)
        read(dfile,'(a300)') longline
        call d2x(longline,last)
      end do
      close(dfile)

      return
********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

992   report(1) = 'Problem reading file: near line:  end found'
      report(2) = fnam
      report(3) = longline  
      go to 999

993   report(1) = 'Problem reading file: near line:  '
      report(2) = fnam
      report(3) = longline  
      go to 999

999   call stopreport(report)

      end


