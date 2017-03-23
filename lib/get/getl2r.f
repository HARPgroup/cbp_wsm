************************************************************************
** sub to get land segments for a river segment                       **
************************************************************************
      subroutine getl2r(
     I                  rseg,rscen,lenrscen,
     O                  numsegs,l2r)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'

      integer i,numsegs,lwc
      logical found,comment
      external comment

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)

      call lencl(geoscen,lengeoscen)
      
      call findopen(lwc)
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//
     .       '/land_water_area.csv'
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      do i = 1,maxL2R
        l2r(i) = '0'
      end do
      numsegs = 0
      read(lwc,'(a1)') c  ! ditch header

      found = .false.
      do
        read(lwc,'(a100)',end=111,err=991) line
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,err=993,end=993) Tseg
        if (Tseg.eq.rseg) then
          found = .true.
          numsegs = numsegs + 1
          if (numsegs.gt.maxL2R) go to 995
          read(line,*,err=993,end=993)Tseg,l2r(numsegs)
        end if
      end do

111   close(lwc)

      if (.not.found) go to 994

      return

*********** ERROR SPACE
991   report(1) = 'error reading file,near line'
      report(2) = fnam
      report(3) = line
      go to 999

992   report(1) = 'error opening file'
      report(2) = fnam
      write(report(3),*)' error = ',err
      go to 999

993   report(1) = 'error parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',rseg,' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'too many land segments for river segment'//rseg
      report(2) = 'increase the maxL2R variable in inc/lib/standard.inc'
      report(3) = ' and recompile all code.'

999   call stopreport(report)

      end

************************************************************************
** sub to get land segments for a river segment                       **
************************************************************************
      subroutine getr2l(
     I                  lseg,maxr2l,
     O                  numsegs,r2l)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'

      integer maxr2l  ! max number of river segments in one land seg
      character*13 r2l(maxr2l)  ! names of the river segments

      integer i,numsegs,lwc,nlsegs

      logical scompare
      logical found
      character*3 end
      data end /'end'/

      call findopen(lwc)
      fnam = catdir//'geo/land_water_connect.csv'
      open (lwc,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      do i = 1,maxr2l
        r2l(i) = '0'
      end do
      numsegs = 0
      read(lwc,'(a1)') c
      line = '0'
      found = .false.
      do while (.not.scompare(line,end))
        read(lwc,'(a100)',err=991,end=994) line
        call d2x(line,last)
        if (line(:3).eq.end) exit

        read(line,*,err=993) rseg,nlsegs,(l2r(i),i=1,nlsegs)

        do i = 1,nlsegs
          if (l2r(i).eq.lseg) then
            numsegs = numsegs + 1
            r2l(numsegs) = rseg
          end if
        end do
      end do

      if (numsegs.eq.0) go to 1001

      close (lwc)

      return

*********** ERROR SPACE
991   report(1) = 'error reading file,near line'
      report(2) = fnam
      report(3) = line
      go to 999

992   report(1) = 'error opening file'
      report(2) = fnam
      write(report(3),*)' error = ',err
      go to 999

993   report(1) = 'error parsing line in file'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = 'literal '//char(39)//'end'//char(39)//' not found'
      go to 999

1001  write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',lseg,' in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

