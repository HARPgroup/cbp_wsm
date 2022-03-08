************************************************************************
** writes the FILES  block using the control and description files    **
************************************************************************
      subroutine files(lseg,lenlseg,clu,lscen,lenlscen,pltgen,
     I                 metscen,pradscen)
      implicit none

      include 'lug.inc'
      integer lenfnam,lenmetscen,lenpradscen

      logical found
      integer i

      line = 'FILES'
      call ryt(line,uci)

*********** read the control file for met and prad scenarios
      call lencl(metscen,lenmetscen)
      call lencl(pradscen,lenpradscen)

      line = '<FILE>  <UN#>***<----FILE NAME---------------------------'
     .       //'---------------------->'
      call ryt(line,uci)

      line = 'WDM1       21   '//ScenDatDir//'climate/met/'//
     .                        metscen(:lenmetscen)//'/met_'//
     .                        lseg(:lenlseg)//'.wdm'
      call ryt(line,uci)

      line = 'WDM2       22   '//ScenDatDir//'climate/prad/'//
     .                        pradscen(:lenpradscen)//'/prad_'//
     .                        lseg(:lenlseg)//'.wdm'
      call ryt(line,uci)

      line = 'WDM4       24   '//clu//lseg(:lenlseg)//'.wdm'
      call ryt(line,uci)

c      line = 'MESSU      25   '//clu//lseg(:lenlseg)//'.ech'
      line = 'MESSU      25   '//'/dev/null'
      call ryt(line,uci)

      line = '           26   '//clu//lseg(:lenlseg)//'.out'
      call ryt(line,uci)

      if (pltgen) then
        do i = 1,numplts
          line = '           XX   '//outdir//'pltgen/land/'//
     .            lscen(:lenlscen)//'/'//clu//'_'//lseg(:lenlseg)//'.'
     .            //pltname(i)
          write(line(12:13),'(i2)') 30+i
          call ryt(line,uci)
        end do
      end if

      line = 'END FILES'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      end

************************************************************************
** given the land scenario, finds the scenario names for the land wdms**
************************************************************************
      subroutine readcontrol_metprad(
     I                               lscen,lenlscen,
     O                               metscen,pradscen)
      implicit none
      include 'lug.inc'

      logical comment

      logical findprad,findmet

      data findprad,findmet
     .     /.false.,.false./


      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')

        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:11).eq.'METEOROLOGY') then
            findmet = .true.
            read(dfile,'(a25)') metscen
            call d2x(metscen,last)
            read(dfile,'(a100)',err=1001)line
            do while (comment(line))
              read(dfile,'(a100)',err=1001)line
            end do
            if (line(:15).ne.'END METEOROLOGY') then
              print*,' control file ',fnam,
     .               ' only allowed one row in table METEOROLOGY'
              call stopit(fnam,'METEOROLOGY')
            end if

          else if (line(:23).eq.'PRECIP ATMOS DEPOSITION') then
            findprad = .true.
            read(dfile,'(a25)') pradscen
            call d2x(pradscen,last)
            read(dfile,'(a100)',err=1001)line
            do while (comment(line))
              read(dfile,'(a100)',err=1001)line
            end do
            if (line(:27).ne.'END PRECIP ATMOS DEPOSITION') then
              print*,' control file ',fnam,
     .          ' only allowed one row in table PRECIP ATMOS DEPOSITION'
              call stopit(fnam,'PRECIP ATMOS DEPOSITION')
            end if

          end if
        end if
      end do

      close (dfile)

      if (.not.findprad) call stopit(fnam,'PRECIP ATMOS DEPOSITION')
      if (.not.findmet) call stopit(fnam,'METEOROLOGY')

      return

*********** ERROR SPACE
1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      call stopreport(report)

      end

************************************************************************
**  subroutine to call subroutine stopreport for different reading    **
***     errors.                                                       **
************************************************************************
      subroutine stopit(fnam,tabnam)
      character*64 report(3),fnam,tabnam

      report(1) = 'Problem reading control file for table'
      report(2) = fnam
      report(3) = tabnam
      call stopreport(report)

      end


