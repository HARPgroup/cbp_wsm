************************************************************************
**  handles operations for external targets block                     **
************************************************************************
      subroutine rexttargets(ioscen,lenioscen,rseg,Nexits,timestep)
      implicit none

      include 'rug.inc'

      character*300 templine
      character*17 variable
      character cl
      character*4 name
      integer j                 ! counting index
      integer dsn

      logical found,comment

      external comment

      line = ''
      call ryt(line,uci)

      line = 'EXT TARGETS'
      call ryt(line,uci)
      line = '<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Volume->'//
     .       ' <Member> Tsys Tgap Amd ***'
      call ryt(line,uci)
      line = '<Name>   #        <Name> # #<-factor->strg <Name>   #'//
     .       ' <Name> #  tem strg strg***'
      call ryt(line,uci)

      line = 'RCHRES   1 <-Grp> <-Member->          SAME WDM4   dsn'//
     .       ' name     ENGL      REPL'
      if (timestep.lt.60) line(39:42) = 'SUM '
      if (timestep.lt.60) line(69:72) = 'AGGR'


      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_Xx'
      call lencl(fnam,last)
      write(fnam(last-1:last-1),'(i1)') Nexits
      open(dfile,file=fnam,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      read(dfile,'(a100)') templine
      call d2x(templine,last)
      do while (templine(:3).ne.'end')
        if (.not.comment(templine).and.templine(:3).ne.'   ') then
          line(12:17) = templine(14:19)  ! group
          line(19:28) = templine(21:30) ! member
          read(templine(8:10),'(i3)')dsn ! dsn
          write(line(51:53),'(i3)') dsn
          line(55:58) = templine(35:38) ! name
          call ryt(line,uci)
        end if
        read(dfile,'(a100)')templine
        call d2x(templine,last)
      end do

      line = 'END EXT TARGETS'
      call ryt(line,uci)

      close(dfile)

      line = '               '
      call ryt(line,uci)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end

