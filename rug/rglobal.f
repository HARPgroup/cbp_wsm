************************************************************************
** writes the GLOBAL block using the control and description files    **
************************************************************************
      subroutine rglobal(rseg,rscen,lenrscen,
     O                   startmonth)
      implicit none

      include 'rug.inc'

      integer ic         ! column of first column

      character*60 placename

      write(uci,'(a6)',err=951) 'GLOBAL'             ! write first line

      line = '  1234567890 123 | P5 | 1234567890 | Description'
      call getplace(rseg,rscen,lenrscen,
     O              placename)            
      line(3:12) = rseg
      line(14:16) = 'riv'
      line(25:34) = rscen
      line(38:) = placename
      call ryt(line,uci)

      line ='  START       YYYY/MM/DD        END    YYYY/MM/DD'
      call getstart(rscen,lenrscen,line(15:18),line(20:21),line(23:24),
     .                             line(40:43),line(45:46),line(48:49))
      call ryt(line,uci)
      read (line(20:21),'(i2)') startmonth

      line = '  RUN INTERP OUTPUT LEVEL    1    1'
      call ryt(line,uci)

      line = '  RESUME     0 RUN     1 TSSFL    15 WDMSFL   16'
      call ryt(line,uci)

      line = 'END GLOBAL'
      call ryt(line,uci)

      line = '          '
      call ryt(line,uci)
      return

************************ error reporting
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** finds the start and end date in character format from the scen file *
************************************************************************
      subroutine getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      implicit none

      include 'rug.inc'

      character*4 y1,y2
      character*2 m1,m2,d1,d2
      logical comment

      fnam = controldir//'/river/'//rscen(:lenrscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:4).eq.'TIME') then
          read(dfile,1234) y1,m1,d1
          read(dfile,1234) y2,m2,d2
          close (dfile)
          return
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      go to 992

1234  format(a4,1x,a2,1x,a2)

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' Got to end of file:'
      report(2) = fnam
      report(3) = ' without finding TIME block'
      go to 999

999   call stopreport(report)

      end




************************************************************************
** finds the long name for the segment
************************************************************************
      subroutine getplace(rseg,rscen,lenrscen,
     O                    placename)
      implicit none

      include 'rug.inc'

      integer ic         ! column of first comma
      logical comment,scompcase

      character*60 placename

      call readcontrol_Rgeoscen(rscen,lenrscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)

      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/rivernames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      placename = 'NO PLACENAME FOUND'

      do 
        read(dfile,'(a100)',end=995,err=992)line
        call d2x(line,last)
        if (comment(line)) cycle
        read(line,*,end=993,err=994)Tseg,placename
        if (scompcase(Tseg,rseg)) exit
      end do

      close (dfile)

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading line from file'
      report(2) = line
      report(3) = fnam
      go to 999

993   report(1) = 'Problem reading line in file: not enough fields'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'Problem reading line in file: read error'
      report(2) = line
      report(3) = fnam
      go to 999

995   report(1) = 'Segment not found in file'
      report(2) = rseg
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end

