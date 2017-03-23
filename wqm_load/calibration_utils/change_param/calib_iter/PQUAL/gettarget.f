************************************************************************
** subroutine target scenario from ./run/control/calib/PQUAL/calscen/ **
************************************************************************
      subroutine gettargscen(
     I                       calscen,
     O                       targscen)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      character*(*) calscen,targscen
      integer lencalscen

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/PQUAL/'//
     .       calscen(:lencalscen)//'/target'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
 
      read(dfile,*) targscen
      print*,'Using targets from ',targscen
      close(dfile)
      return

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)
      end

************************************************************************
** subroutine get EOF target from ./pp/data/targets directory         **
************************************************************************
      subroutine gettarget(
     I                     targscen,Tname,lsegs,nlsegs,clu,
     O                     Tval)

      implicit none
      include 'pqual.inc'

      character*4 Tname  ! target name
      real Tval(maxlsegs)      ! target values

      character*500 dline
      character*(*) targscen
      integer order        ! index of column to search
      integer i,ns,nc
      character*6 Tlseg
      character*3 clu
      integer lentargscen
      
      logical foundseg(maxlsegs)

**************END DECLARATION ******************************************

************ open EOF targets file
      call lencl(targscen,lentargscen)
      fnam = calibdir//'target/'//targscen(:lentargscen)//'/'//
     .       TName//'_target.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a500)',err=996)dline            ! read header line
      call d2x(dline,i)
      call shift(dline)

      order = 1        ! find order
      do while (dline(:3).ne.clu)
        order = order + 1
        call shift(dline)
        if (dline(:3).eq.'   ') go to 992
      end do

      do ns = 1, nlsegs
        foundseg(ns) = .false.
      end do

*************** loop over all segments in file, look for active segs
      do
        read(dfile,'(a500)',err=996,end=111)dline
        call d2x(dline,i)
        read(dline,*) Tseg
        do ns = 1,nlsegs
          if (lsegs(ns).eq.Tseg) then
            foundseg(ns) = .true.
            if (dline(len(dline)-3:len(dline)).ne.'    ') go to 995
            do nc = 1,order
              call shift(dline)
            end do
            read(dline,*,err=993) Tval(ns)
            exit
          end if
        end do
      end do

111   close (dfile)

      do ns = 1, nlsegs
        if (.not.foundseg(ns)) go to 994
      end do

      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'could not find land use '//clu//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'error reading target value in file'
      report(2) = fnam
      report(3) = dline
      go to 999

994   report(1) = 'did not find segment '//lsegs(ns)//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

995   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/.../PQUAL/gettarget.f'
      go to 999

996   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = dline
      go to 999

997   report(1) = 'did not literal '//char(39)//'end'//char(39)//' in'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end 

