************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program reads the file of parameters                          **
************************************************************************
      subroutine readparams(
     I                      paramscen,
     O                     parLKflag,parModule,parTable,parName,parAorM,
     O                      parstart,parmin,parmax,parval,header,npar)
      implicit none
      include 'sens.inc'
      character*300 longline

      call lencl(paramscen,lenrscen)
      fnam = pardir//'river/'//paramscen(:lenrscen)//'/param_list.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)') longline
      call d2x(longline,last)
      header = longline(:last)

      npar = 0

      read(dfile,'(a300)') longline
      call d2x(longline,last)
      do 
        npar = npar + 1
        read(longline,*,end=992,err=992) 
     .       parLKflag(npar),parModule(npar),parTable(npar),
     .       parName(npar),parstart(npar),parmin(npar),parmax(npar),
     .       parAorM(npar),(parval(npar,nv),nv=1,nval)
        read(dfile,'(a300)',end=111,err=993) longline
        call d2x(longline,last)
      end do
111   close(dfile)

      return
********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

992   report(1) = 'Problem reading line:  in file'
      report(2) = fnam
      report(3) = longline  
      go to 999

993   report(1) = 'Problem reading file: near line:  '
      report(2) = fnam
      report(3) = longline  
      go to 999

999   call stopreport(report)

      end

************************************************************************
** This program writes the file of parameters                         **
************************************************************************
      subroutine writeparams(
     I                       paramscen,
     I                     parLKflag,parModule,parTable,parName,parAorM,
     I                       parstart,parmin,parmax,parval,header,npar)
      implicit none
      include 'sens.inc'
      character*300 longline

      call lencl(paramscen,lenrscen)
      fnam = pardir//'river/'//paramscen(:lenrscen)//'/param_list.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(header,dfile)

      do np = 1,npar
        write(longline,*)
     .       parLKflag(np),',',parModule(np),',',parTable(np),',',
     .       parName(np),',',parstart(np),',',parmin(np),',',
     .       parmax(np),',',parAorM(np),',',
     .       (parval(np,nv),',',nv=1,nval)
        if (longline(299:300).ne.'  ') go to 994
        call rytdos(longline,dfile)
      end do

      close(dfile)

      return
********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file for writing'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

994   report(1) = ' Problem writing file'
      report(2) = fnam
      report(3) = 'need longer reading and writing variable'
      go to 999

999   call stopreport(report)

      end

