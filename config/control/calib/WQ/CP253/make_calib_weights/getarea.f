************************************************************************
** finds the area of a segment                                        **
************************************************************************
      subroutine getarea(
     I                   rseg,rscen,lenrscen,
     O                   area)
      implicit none
      include '../../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../../code/src/lib/inc/locations.inc'
      character*6,Ldum
      real lrsize  ! area of land-river segment
      real area    ! area of river segment
      logical comment
      external comment

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam=catdir//'geo/'//geoscen(:lengeoscen)//'/land_water_area.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      area = 0.0
      do 
        read(dfile,'(a100)',end=111,err=992) line
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,end=992,err=992)Tseg,Ldum,lrsize
        if (Tseg.eq.rseg) area = area + lrsize
      end do
111   close(dfile)

      if (area.lt.1) go to 993

      return
*********************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file:'
      report(2) = fnam
      write(report(3),*) Tseg,' ',Ldum,' ',lrsize
      go to 999

993   report(1) = 'Problem with file:'
      report(2) = fnam
      report(3) = 'segment '//Tseg//' has no size'
      go to 999

999   call stopreport(report)
      end

