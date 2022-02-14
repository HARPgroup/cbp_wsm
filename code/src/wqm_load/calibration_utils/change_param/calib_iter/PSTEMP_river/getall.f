************************************************************************
**    subroutine to get river calibration stats                       **
************************************************************************
      subroutine getoldstats(
     I                   lmodule,calscen,calseg,itnum,
     O                   KATRAD,efficiency)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      integer i

      call lencl(calscen,lencalscen)
      call lencl(lmodule,lenlmod)
      fnam = controldir//'calib/'//lmodule(:lenlmod)//'/'//
     .       calscen(:lencalscen)//'/oldstats/'//calseg//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,itnum-1
        read(dfile,*)KATRAD(i),efficiency(i)
      end do

      close(dfile)
 
      return

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end

************************************************************************
**    subroutine to put river calibration stats in a file             **
************************************************************************
      subroutine putoldstats(
     I                   lmodule,calscen,calseg,itnum,
     O                   KATRAD,efficiency)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      integer i

      call lencl(calscen,lencalscen)
      call lencl(lmodule,lenlmod)
      fnam = controldir//'calib/'//lmodule(:lenlmod)//'/'//
     .       calscen(:lencalscen)//'/oldstats/'//calseg//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      do i = 1,itnum
        write(dfile,*,err=951)KATRAD(i),',',efficiency(i)
      end do

      close(dfile)
 
      return

951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'error opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end

************************************************************************
** subroutine to get efficiency of current run                        **
************************************************************************
      subroutine getefficiency(
     I               rscen,calscen,calseg,cy1,cy2,
     O               eff)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      character*(*) calscen
      character*(*) calseg
      character*(*) cy1,cy2
      real eff  ! efficiency
      character*10 efficiency

      call lencl(rscen,lenrscen)
      fnam = outdir//'river/stats/'//rscen(:lenrscen)//'/'//
     .       calseg//'_'//cy1//'_'//cy2//'_conc.WTMP'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do 
        read(dfile,*,end=992,err=993) efficiency
        if (efficiency.eq.'efficiency') then
          backspace dfile
          read(dfile,*,err=994) efficiency,eff
          exit
        end if
      end do
      close(dfile)
      return

*********************** ERROR SPACE ************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find tag '//char(39)//'efficiency'//char(39)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = efficiency
      go to 999

994   report(1) = 'problem reading file: on line containing efficiency:'
      report(2) = fnam
      write(report(3),*) efficiency,' ',eff
      go to 999

999   call stopreport(report)
      end
