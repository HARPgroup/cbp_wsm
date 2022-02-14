************************************************************************
** subroutine get river data                                          **
************************************************************************
      subroutine getRAbasins(
     I                       calscen,maxstations,
     O                       RAbasins,nRAbasins)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'
      character*(*) calscen
      logical comment
      external comment
      integer ic,lencalscen,maxstations,nRAbasins
      character*13 RAbasins(maxstations)

      call lencl(calscen,lencalscen)
      nRAbasins = 0
      fnam = controldir//'calib/WQ/'//calscen(:lencalscen)//
     .             '/regional_adjustment_basins.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do 
        read(dfile,'(a100)',err=992,end=111)line
        call d2x(line,last)
        if (comment(line)) cycle
        nRAbasins = nRAbasins + 1
        if (nRAbasins.gt.maxstations) go to 993
        read(line,*) RAbasins(nRAbasins) 
      end do
111   close (dfile)

      return

***************** ERROR SPACE ******************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem with file:'
      report(2) = fnam
      report(3) = 'more stations than allowed'
      go to 999

999   call stopreport(report)

      end

