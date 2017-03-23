************************************************************************
** programs to read in a seglist file and store the segments          **
************************************************************************
      subroutine readRiverSeglist(
     I                            basin,
     O                            rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*20000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.riv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a20000)',err=992,end=992) segline
      call lencl(segline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a20000)',err=992,end=992) segline
        call lencl(segline,last)
      end do

      if (segline(20000-5:20000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along segline
      do while (segline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      do while (segline(i:i).ne.')')
        i = i + 1
      end do
      segline = segline(shifti+1:i-1)
      segline(i:) = ' '

******** segline is now just the segments, read
      do ns = 1,maxrsegs
        rsegs(ns)='      '
      end do
      nrsegs = 0
      last = 1
      do while (last.ne.0)
        nrsegs = nrsegs + 1
        read(segline,*) rsegs(nrsegs)
        call spaceshift(segline,last)
      end do
      if (nrsegs.eq.1.and.rsegs(1).eq.'             ') then
        nrsegs = 0
        return
      end if

      if (nrsegs.gt.maxrsegs) go to 995

******** check for duplicates
      do i = 1,nrsegs-1
        do j = i+1,nrsegs
          if (rsegs(i).eq.rsegs(j)) go to 994
        end do
      end do

      return

************ ERROR SPACE ***********************************************
991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'problem reading segments in file'
      report(2) = fnam
      report(3) = 'increase the size of variable '//char(39)//'segline'
     .            //char(39)//' in ./pp/src/lib/get/readseglist.f'
      go to 999

994   report(1) = 'file has a duplicate river segment'
      report(2) = fnam
      report(3) = rsegs(i)
      go to 999

995   report(1) = 'too many segments in file'
      report(2) = fnam
      write(report(3),*) 'max allowed is ',maxrsegs,'.  Tried ',nrsegs

999   call stopreport(report)

      end


      subroutine writeRiverSeglist(
     I                             basin,rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.riv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (rsegs(ns),' ',ns=1,nrsegs),')'
      close(dfile)

1233  format(2000a)

      return

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

999   call stopreport(report)

      end


      subroutine readLandSeglist(
     I                           basin,
     O                           lsegs,nlsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/lsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.land'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a12000)',err=992,end=992) segline
      call lencl(segline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a12000)',err=992,end=992) segline
        call lencl(segline,last)
      end do

      if (segline(12000-5:12000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along segline
      do while (segline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      do while (segline(i:i).ne.')')
        i = i + 1
      end do
      segline = segline(shifti+1:i-1)
      segline(i:) = ' '

******** segline is now just the segments, read
      do ns = 1,maxlsegs
        lsegs(ns)='      '
      end do
      nlsegs = 0
      last = 1
      do while (last.ne.0)
        nlsegs = nlsegs + 1
        read(segline,*) lsegs(nlsegs)
        call spaceshift(segline,last)
      end do
      if (nlsegs.eq.1.and.lsegs(1).eq.'      ') then
        nlsegs = 0
        return
      end if

******** check for duplicates
      do i = 1,nlsegs-1
        do j = i+1,nlsegs
          if (lsegs(i).eq.lsegs(j)) go to 994
        end do
      end do

      return

************ ERROR SPACE ***********************************************
991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'problem reading segments in file'
      report(2) = fnam
      report(3) = 'increase the size of variable '//char(39)//'segline'
     .            //char(39)//' in ./pp/src/lib/read/readseglist.f'
      go to 999

994   report(1) = 'file has a duplicate land segment'
      report(2) = fnam
      report(3) = lsegs(i)
      go to 999

999   call stopreport(report)

      end


      subroutine writeLandSeglist(
     I                            basin,lsegs,nlsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/lsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.land'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (lsegs(ns),' ',ns=1,nlsegs),')'
      close(dfile)

1233  format(2000a)

      return

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

999   call stopreport(report)

      end


************************************************************************
** programs to read in a seglist file and store the segments          **
************************************************************************
      subroutine readCalibSeglist(
     I                            basin,
     O                            rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*20000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.calib'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a20000)',err=992,end=992) segline
      call lencl(segline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a20000)',err=992,end=992) segline
        call lencl(segline,last)
      end do

      if (segline(20000-5:20000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along segline
      do while (segline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      do while (segline(i:i).ne.')')
        i = i + 1
      end do
      segline = segline(shifti+1:i-1)
      segline(i:) = ' '

******** segline is now just the segments, read
      do ns = 1,maxrsegs
        rsegs(ns)='      '
      end do
      nrsegs = 0
      last = 1
      do while (last.ne.0)
        nrsegs = nrsegs + 1
        read(segline,*) rsegs(nrsegs)
        call spaceshift(segline,last)
      end do

      if (nrsegs.gt.maxrsegs) go to 995

******** check for duplicates
      do i = 1,nrsegs-1
        do j = i+1,nrsegs
          if (rsegs(i).eq.rsegs(j)) go to 994
        end do
      end do

      return

************ ERROR SPACE ***********************************************
991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'problem reading segments in file'
      report(2) = fnam
      report(3) = 'increase the size of variable '//char(39)//'segline'
     .            //char(39)//' in ./pp/src/lib/get/readseglist.f'
      go to 999

994   report(1) = 'file has a duplicate river segment'
      report(2) = fnam
      report(3) = rsegs(i)
      go to 999

995   report(1) = 'too many segments in file'
      report(2) = fnam
      write(report(3),*) 'max allowed is ',maxrsegs,'.  Tried ',nrsegs

999   call stopreport(report)

      end


      subroutine writeCalibSeglist(
     I                             basin,rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = seglistdir//basin(:last)//'.calib'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (rsegs(ns),' ',ns=1,nrsegs),')'
      close(dfile)

1233  format(2000a)

      return

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** programs to read in a seglist file and store the segments          **
************************************************************************
      subroutine readRiverSeglistCurrentDir(
     I                            basin,
     O                            rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*20000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = basin(:last)//'.riv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a20000)',err=992,end=992) segline
      call lencl(segline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a20000)',err=992,end=992) segline
        call lencl(segline,last)
      end do

      if (segline(20000-5:20000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along segline
      do while (segline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      do while (segline(i:i).ne.')')
        i = i + 1
      end do
      segline = segline(shifti+1:i-1)
      segline(i:) = ' '

******** segline is now just the segments, read
      do ns = 1,maxrsegs
        rsegs(ns)='      '
      end do
      nrsegs = 0
      last = 1
      do while (last.ne.0)
        nrsegs = nrsegs + 1
        read(segline,*) rsegs(nrsegs)
        call spaceshift(segline,last)
      end do
      if (nrsegs.eq.1.and.rsegs(1).eq.'             ') then
        nrsegs = 0
        return
      end if

      if (nrsegs.gt.maxrsegs) go to 995

******** check for duplicates
      do i = 1,nrsegs-1
        do j = i+1,nrsegs
          if (rsegs(i).eq.rsegs(j)) go to 994
        end do
      end do

      return

************ ERROR SPACE ***********************************************
991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'problem reading segments in file'
      report(2) = fnam
      report(3) = 'increase the size of variable '//char(39)//'segline'
     .            //char(39)//' in ./pp/src/lib/get/readseglist.f'
      go to 999

994   report(1) = 'file has a duplicate river segment'
      report(2) = fnam
      report(3) = rsegs(i)
      go to 999

995   report(1) = 'too many segments in file'
      report(2) = fnam
      write(report(3),*) 'max allowed is ',maxrsegs,'.  Tried ',nrsegs

999   call stopreport(report)

      end


      subroutine writeRiverSeglistCurrentDir(
     I                             basin,rsegs,nrsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/rsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = basin(:last)//'.riv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (rsegs(ns),' ',ns=1,nrsegs),')'
      close(dfile)

1233  format(2000a)

      return

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

999   call stopreport(report)

      end


      subroutine readLandSeglistCurrentDir(
     I                           basin,
     O                           lsegs,nlsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/lsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = basin(:last)//'.land'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a12000)',err=992,end=992) segline
      call lencl(segline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a12000)',err=992,end=992) segline
        call lencl(segline,last)
      end do

      if (segline(12000-5:12000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along segline
      do while (segline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      do while (segline(i:i).ne.')')
        i = i + 1
      end do
      segline = segline(shifti+1:i-1)
      segline(i:) = ' '

******** segline is now just the segments, read
      do ns = 1,maxlsegs
        lsegs(ns)='      '
      end do
      nlsegs = 0
      last = 1
      do while (last.ne.0)
        nlsegs = nlsegs + 1
        read(segline,*) lsegs(nlsegs)
        call spaceshift(segline,last)
      end do
      if (nlsegs.eq.1.and.lsegs(1).eq.'      ') then
        nlsegs = 0
        return
      end if

******** check for duplicates
      do i = 1,nlsegs-1
        do j = i+1,nlsegs
          if (lsegs(i).eq.lsegs(j)) go to 994
        end do
      end do

      return

************ ERROR SPACE ***********************************************
991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'problem reading segments in file'
      report(2) = fnam
      report(3) = 'increase the size of variable '//char(39)//'segline'
     .            //char(39)//' in ./pp/src/lib/read/readseglist.f'
      go to 999

994   report(1) = 'file has a duplicate land segment'
      report(2) = fnam
      report(3) = lsegs(i)
      go to 999

999   call stopreport(report)

      end


      subroutine writeLandSeglistCurrentDir(
     I                            basin,lsegs,nlsegs)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/lsegs.inc'

      character*12000 segline
      character*(*) basin

      integer ns,i,j,shifti  ! index

*************** END DECLARATIONS ***************************************
      call lencl(basin,last)
      fnam = basin(:last)//'.land'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (lsegs(ns),' ',ns=1,nlsegs),')'
      close(dfile)

1233  format(2000a)

      return

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) =  'could not open file'
      report(2) = fnam
      report(3) = 'check seglist code'
      go to 999

999   call stopreport(report)

      end


