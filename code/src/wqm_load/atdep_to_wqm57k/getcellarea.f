      function getcellarea(cell,linkdir,lenlink)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      real getcellarea
      integer cell,ifl,icell
      character*1 dummy
      character*(*)linkdir
      integer lenlink

      call findopen(ifl)
      fnam = tree//'input/unformatted/atdep/CMAQ/'//
     .       linkdir(:lenlink)//'/cellsize.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(ifl,*)dummy
      do
        read(ifl,*,end=992)icell,getcellarea
        if (icell.eq.cell) then
          close(ifl)
          return
        end if
      end do
      go to 992

*********** ERROR SPACE ***********************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   write(report(1),*) 'did not find cell ',cell,' in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end


