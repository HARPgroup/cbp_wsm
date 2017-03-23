************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program stores the current value of np and nv for the sens    **
**  program to read at a later time                                   **
************************************************************************
      subroutine storenpnv(
     I                     paramscen,np,nv)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      integer np,nv

      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//'/npnv.prn'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      write(dfile,*,err=951) np
      write(dfile,*,err=951) nv
      write(dfile,*,err=951) 'this file is a temporary file used in the'
      write(dfile,*,err=951) 'calculation of sensitivities.  ',
     .                       'You can delete'
      write(dfile,*,err=951) 'it if you are not currently running',
     .                        'sensitvities'
      close(dfile)

      return
********************** ERROR SPACE *************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end

************************************************************************
** This program reads the current value of np and nv                  **
************************************************************************
      subroutine getnpnv(
     I                   paramscen,
     O                   np,nv)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      integer np,nv

      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//'/npnv.prn'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      read(dfile,*) np
      read(dfile,*) nv
      close(dfile)

      return
********************** ERROR SPACE *************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end

