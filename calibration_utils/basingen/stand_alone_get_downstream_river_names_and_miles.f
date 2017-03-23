************************************************************************
** Program to generate a list of downstream river segments            **
**  to anyone following:  apologies for the twisted program logic     **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'

      integer maxdown,nd,n,nd2
      parameter (maxdown = 100)
      character*13 downsegs(100)
      character*40 downrivers(100),getRname,downrivers2(100)
      external getRname
      integer lastname(100)
      real getRmiles,rivermiles
      external getRmiles

      logical pourpoint
***************** end of declarations **********************************

      read*,rseg,rscen
      call lencl(rscen,lenrscen)

      nd = 1 
      downsegs(nd) = rseg
      pourpoint = .false.
      downrivers(nd) = getRname(downsegs(nd),rscen,lenrscen)
      rivermiles = getRmiles(rscen,lenrscen,downsegs(nd))

      do while (.not.pourpoint)
        nd = nd + 1
        call downstream(downsegs(nd-1),rscen,lenrscen,
     O                  pourpoint,downsegs(nd))
        if (.not.pourpoint) then
          downrivers(nd) = getRname(downsegs(nd),rscen,lenrscen)
          rivermiles = rivermiles + getRmiles(rscen,lenrscen,
     .                                        downsegs(nd))
        end if
      end do

      nd = nd - 1

C      print 1234,nd,(downsegs(n),n=1,nd)

*********** get rid of duplicates
      nd2 = 1
      downrivers2(nd2) = downrivers(1)
      
      do n = 2,nd
        if (downrivers2(nd2).ne.downrivers(n)) then
          nd2 = nd2 + 1
          downrivers2(nd2) = downrivers(n)
        end if
      end do

      do n = 1,nd2
        call lencl(downrivers2(n),lastname(n))
      end do
      print 1235,rseg,rivermiles,
     .           (downrivers2(n)(:lastname(n)),' to ',n=1,nd2-1),
     .            downrivers2(nd2)(:lastname(nd2))
        
        
      

      stop

1234  format(i2,100(',',a13))
1235  format(a13,',',f10.1,',',100(a))
      end



************************************************************************
**  Subroutine to find the downstream segment if one exists.          **
**   returns the name of the downstream segment and a logical         **
**   true or false for whether the passed stream is a pour point      **
************************************************************************
      subroutine downstream(
     I                      rseg,rscen,lenrscen,
     O                      pourpoint,downseg)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*13 downseg
      logical pourpoint             ! true if this is the pourpoint
      logical found,comment

      character*4 uniqid  ! unique ID for the downstream river

      uniqid = rseg(10:13)

      if (uniqid.eq.'0001'.or.uniqid.eq.'0000'.or.uniqid.eq.'0004') then
        pourpoint = .true.
        downseg = ' '
        return
      end if
      pourpoint = .false.

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam=catdir//'/geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status = 'old',iostat=err)
      if (err.ne.0) go to 991

      found = .false.
      read(dfile,'(a100)',err=992,end=992) line
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then       

          call findcomma(line,last)
          line = line(:last-1)
          call trims(line,last)
          if (last.ne.13) go to 993

          if (line(5:8).eq.uniqid) then
            downseg = line(:last)
            found = .true.
          end if
        end if
        read(dfile,'(a100)',err=992,end=992) line
      end do

      close(dfile)

      if (.not.found) go to 994

      return

********************************** ERROR SPACE *************************
991   report(1) = '  Problem opening file '
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' Problem reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'trouble reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'could not find downstream segment for seg in file'
      report(2) = rseg
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  Function to find the name of the input rseg                       **
************************************************************************
      function getRname(rseg,rscen,lenrscen)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      character*40 getRname

      logical found,comment
      external comment

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam=catdir//'/geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status = 'old',iostat=err)
      if (err.ne.0) go to 991

      found = .false.
      read(dfile,'(a100)',err=992,end=992) line
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then       

          call findcomma(line,last)
          line = line(:last-1)
          call trims(line,last)
          if (last.ne.13) go to 993

          if (line(:last).eq.rseg) then
            found = .true.
            backspace dfile
            read(dfile,'(a100)') line
            call shift(line)
            getRname = line(:40)
            exit
          end if

        end if
        read(dfile,'(a100)',err=992,end=992) line
      end do

      close(dfile)

      if (.not.found) go to 994

      return

********************************** ERROR SPACE *************************
991   report(1) = '  Problem opening file '
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' Problem reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'trouble reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'could not find segment in file'
      report(2) = rseg
      report(3) = fnam
      go to 999

999   call stopreport(report)
      end

************************************************************************
**  Function to find the name of the input rseg                       **
************************************************************************
      function getRmiles(rscen,lenrscen,rseg)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      real getRmiles

      logical found,comment
      external comment

      if (rseg(10:13).eq.'0000'.or.rseg(10:13).eq.'0004') then
        getRmiles = 0.0
        return
      end if

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam=catdir//'/geo/'//geoscen(:lengeoscen)//
     .        '/segment_river_miles.csv'
      open(dfile,file=fnam,status = 'old',iostat=err)
      if (err.ne.0) go to 991

      found = .false.
      read(dfile,'(a100)',err=992,end=992) line
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then       

          call findcomma(line,last)
          line = line(:last-1)
          call trims(line,last)
          if (last.ne.13) go to 993

          if (line(:last).eq.rseg) then
            found = .true.
            backspace dfile
            read(dfile,'(a100)') line
            call d2x(line,last)
            call shift(line)
            read(line,*) getRmiles
            return
          end if

        end if
        read(dfile,'(a100)',err=992,end=992) line
      end do

      close(dfile)

      if (.not.found) go to 994

      return

********************************** ERROR SPACE *************************
991   report(1) = '  Problem opening file '
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' Problem reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'trouble reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'could not find segment in file'
      report(2) = rseg
      report(3) = fnam
      go to 999

999   call stopreport(report)
      end
