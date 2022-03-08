************************************************************************
** writes the GLOBAL block using the control and description files    **
************************************************************************
      subroutine global(lseg,clu,lscen,lenlscen,
     I           startY,startM,startD,endY,endM,endD)
      implicit none

      include 'lug.inc'

      integer ic         ! column of first column

      character*60 placename

      write(uci,'(a6)',err=951) 'GLOBAL'             ! write first line

      line = '  1234567890 123 | P5 | 1234567890 | Description'
      call getplace(
     I              lscen,lenlscen,lseg,
     O              placename)            
      line(3:12) = lseg
      line(14:16) = clu
      line(25:34) = lscen
      line(38:) = placename
      call ryt(line,uci)

      line ='  START       YYYY/MM/DD        END    YYYY/MM/DD'
      write(line(15:18),'(i4)') startY
      write(line(20:21),'(i2)') startM
      write(line(23:24),'(i2)') startD
      write(line(40:43),'(i4)') endY
      write(line(45:46),'(i2)') endM
      write(line(48:49),'(i2)') endD
      call ryt(line,uci)

      line = '  RUN INTERP OUTPUT LEVEL    0    0'
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
** finds the long name for the segment
************************************************************************
      subroutine getplace(
     I                    lscen,lenlscen,lseg,
     O                    placename)            
      implicit none

      include 'lug.inc'

      integer ic         ! column of first comma
      logical comment

      character*60 placename

      character*13 Tlseg
      integer llseg,ltseg

      call readcontrol_Lgeoscen(
     I                          lscen,lenlscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)
      fnam = catdir//'geo/'//geoscen(:lengeoscen)//'/landnames.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      placename = 'NO PLACENAME FOUND'

      Tlseg = lseg
      call trims(Tlseg,llseg)

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,ltseg)
          if (Tseg.eq.Tlseg.and.ltseg.eq.llseg)
     .          placename = line(ic+1:last)
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      close (dfile)

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

