************************************************************************
**    subroutines to get the output of the river                      **
************************************************************************
      subroutine getrsegmon(rseg,lenrseg,rscen,lenrscen,
     I                ndel,delname,sdate,edate,
     O                outmon)
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm

      integer numsegs        ! number of land segs for this river

      character*4 Tload

      real value                        ! temp reading variable

*************************** END DECLARATIONS ***************************
      do ndl = 1,ndel           ! initialize
        ny = sdate(1)
        do nm = sdate(2),12
          outmon(ndl,ny,nm) = -9.0
        end do
        do ny = sdate(1)+1,edate(1)-1
          do nm = 1,12
            outmon(ndl,ny,nm) = -9.0
          end do
        end do
        ny = edate(1)
        do nm = 1,edate(2)
          outmon(ndl,ny,nm) = -9.0
        end do
      end do


      do ndl = 1,ndel
        Tload = delname(ndl)
        do i = 1,4
          if (Tload(i:i).eq.' ') Tload(i:i) = '_'
        end do
        fnam = outdir//'river/monthly/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//Tload//'_month.prn'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a100)',err=992) line  ! get rid of header
        do 
          read(dfile,*,err=993,end=111) Tload,ny,nm,value
          outmon(ndl,ny,nm) = value
        end do
111     close (dfile)
      end do

      do ndl = 1,ndel           ! check
        ny = sdate(1)
        do nm = sdate(2),12
          if (outmon(ndl,ny,nm).lt.-8.0) go to 994
        end do
        do ny = sdate(1)+1,edate(1)-1
          do nm = 1,12
            if (outmon(ndl,ny,nm).lt.-8.0) go to 994
          end do
        end do
        ny = edate(1)
        do nm = 1,edate(2)
          if (outmon(ndl,ny,nm).lt.-8.0) go to 994
        end do
      end do

      return

*************** ERROR SPACE ******************************
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = '  First line not readable'
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' Should be in format (a4,i6,i3,e14) '
      go to 999

994   write(report(1),*) ' did not find year ',ny,' month ',nm
      write(report(2),*) '  for load type ',delname(ndl),' in file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

************************************************************************
************************************************************************

      subroutine getrsegann(rseg,lenrseg,rscen,lenrscen,
     I                ndel,delname,sdate,edate,
     O                outann)
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm

      integer numsegs        ! number of land segs for this river

      character*4 Tload

      real value                        ! temp reading variable

*************************** END DECLARATIONS ***************************

      do ndl = 1,ndel           ! initialize
        do ny = sdate(1)+1,edate(1)-1
          outann(ndl,ny) = -9.0
        end do
      end do

      do ndl = 1,ndel
        Tload = delname(ndl)
        do i = 1,4
          if (Tload(i:i).eq.' ') Tload(i:i) = '_'
        end do
        fnam = outdir//'river/annual/'//rscen(:lenrscen)//
     .       '/'//rseg(:lenrseg)//'_'//Tload//'_year.prn'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a100)',err=992) line  ! get rid of header
        do 
          read(dfile,*,err=993,end=111) Tload,ny,value
          outann(ndl,ny) = value
        end do
111     close (dfile)
      end do

      do ndl = 1,ndel           ! check
        do ny = sdate(1)+1,edate(1)-1
          if (outann(ndl,ny).lt.-8.0) go to 994
        end do
      end do

      return

*************** ERROR SPACE ******************************
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = '  First line not readable'
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' Should be in format (a4,i6,i3,e14) '
      go to 999

994   write(report(1),*) ' did not find year ',ny
      write(report(2),*) '  for load type ',delname(ndl),' in file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

************************************************************************
************************************************************************

      subroutine getrsegave(rseg,lenrseg,rscen,lenrscen,
     I                ndel,delname,year1,year2,
     O                outave)
      implicit none

      include 'tfs.inc'

      integer i,ndl
      character*4 cy1,cy2,units

      integer numsegs        ! number of land segs for this river

      character*4 Tload

      real value                        ! temp reading variable

*************************** END DECLARATIONS ***************************
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      do ndl = 1,ndel
        outave(ndl) = -9.0
      end do

      fnam = outdir//'river/aveann/'//rscen(:lenrscen)//
     .     '/'//rseg(:lenrseg)//'_'//cy1//'_'//cy2//'.ave'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)',err=992) line  ! get rid of header
      do 
        read(dfile,*,err=993,end=111) Tload,units,value
        do ndl = 1,ndel
          if (Tload.eq.delname(ndl)) then
            outave(ndl) = value
            exit
          end if
        end do
      end do
111   close (dfile)

      do ndl = 1,ndel 
        if (outave(ndl).lt.-8.0) go to 994
      end do

      return

*************** ERROR SPACE ******************************
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = '  First line not readable'
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' Should be in format (a4,i6,e16) '
      go to 999

994   write(report(1),*) ' did not find '
      write(report(2),*) '  load type ',delname(ndl),' in file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

