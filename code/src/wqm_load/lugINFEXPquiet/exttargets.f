************************************************************************
**  handles operations for external targets block                     **
************************************************************************

      subroutine exttargets(ioscen,lenioscen,lseg,clu,perlnd,implnd)
      implicit none

      include 'lug.inc'

      logical perlnd,implnd             ! type of land use for this uci

      character*300 templine
      character*17 variable
      character cl
      character*3 Tclu          ! temp reading variable for land use
      character*4 name
      integer j                 ! counting index

      logical found,comment

      external comment

      line = 'EXT TARGETS'
      call ryt(line,uci)
      line = '<-Volume-> <-Grp> <-Member-><--Mult-->Tran <-Volume->'//
     .       ' <Member> Tsys Tgap Amd ***'
      call ryt(line,uci)
      line = '<Name>   #        <Name> # #<-factor->strg <Name>   #'//
     .       ' <Name> #  tem strg strg***'
      call ryt(line,uci)

      line = 'YYYYYY   1 ***variable****            SAME WDM4   dsn'//
     .       ' name     ENGL      REPL'
      if (perlnd) line(:6) = 'PERLND'
      if (implnd) line(:6) = 'IMPLND'

      if (perlnd) fnam = catdir//'iovars/'//
     .            ioscen(:lenioscen)//'/perlnd'
      if (implnd) fnam = catdir//'iovars/'//
     .            ioscen(:lenioscen)//'/implnd'
      open(dfile,file=fnam,status='old',iostat=err)     ! open
      if (err.ne.0) go to 991

      read(dfile,'(a300)') templine
      if (templine(300-1:300).ne.'  ') go to 992
      do while (templine(:3).ne.'end')
        read(dfile,'(a300)') templine
        if (.not.comment(templine)) then
          j = 68
          found = .false.
          do while (j.lt.300-2)
            do while (j.lt.300-2)
              if (templine(j:j).ne.' ') exit
              j = j + 1
            end do
            Tclu = templine(j:j+2)
            if (clu.eq.Tclu) found = .true.
            do while (j.lt.300-2)
              if (templine(j:j).eq.' ') exit
              j = j + 1
            end do
          end do
          if (found) then
            line(12:28) = templine(7:23)    ! hspf variable name
            line(55:58) = templine(26:29)   ! wdm variable name
            line(51:53) = templine(1:3)     ! dsn
            call ryt(line,uci)
          end if
        end if
      end do

      close(dfile)

      line = 'END EXT TARGETS'
      call ryt(line,uci)

      line = '               '
      call ryt(line,uci)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

992   report(1) = 'line too long in file'
      report(2) = fnam
      report(3) = 'update code ./pp/src/lug/exttargets.f'
      go to 999

999   call stopreport(report)

      end

