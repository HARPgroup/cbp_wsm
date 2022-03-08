************************************************************************
** writes the FILES  block using the control and description files    **
************************************************************************
      subroutine rfiles(rseg,lenrseg,rscen,lenrscen,pltgen)
      implicit none

      include 'rug.inc'
      integer lenfnam,lenmetscen,lenpradscen
      character*25 metscen,pradscen

      character*5 Stype,Vtype         ! dummy variable to send into getwdm

      logical found
      integer i

      line = 'FILES'
      call ryt(line,uci)

*********** read the control file for met and prad scenarios
      call readcontrol_rwdm(rscen,lenrscen,metscen,pradscen)
      call lencl(metscen,lenmetscen)
      call lencl(pradscen,lenpradscen)

      line = '<FILE>  <UN#>***<----FILE NAME---------------------------'
     .       //'---------------------->'
      call ryt(line,uci)

************ get the met wdm name associated with this river segment
      Stype = 'river'
      Vtype = 'met'
      call getwdm(rseg,Stype,Vtype,rscen,lenrscen,
     O            fnam,found)

      line = 'WDM1       21   '//ScenDatDir//'climate/met/'//
     .                        metscen(:lenmetscen)//'/'//fnam
      call ryt(line,uci)

************ get the prad wdm name associated with this river segment
      Vtype = 'prad'
      call getwdm(rseg,Stype,Vtype,rscen,lenrscen,
     O            fnam,found)

      line = 'WDM2       22   '//ScenDatDir//'climate/prad/'//
     .                        pradscen(:lenpradscen)//'/'//fnam
      call ryt(line,uci)

      line = 'WDM3       23   ps_sep_div_ams_'//
     .       rscen(:lenrscen)//'_'//rseg(:lenrseg)//'.wdm'
      call ryt(line,uci)

      line = 'WDM4       24   '//rseg(:lenrseg)//'.wdm'
      call ryt(line,uci)

      line = 'MESSU      25   '//rseg(:lenrseg)//'.ech'
      call ryt(line,uci)

      line = '           26   '//rseg(:lenrseg)//'.out'
      call ryt(line,uci)

      if (pltgen) then
        do i = 1,numplts
          line = '           XX   '//outdir//'pltgen/river/'//
     .            rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.'
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


      subroutine readcontrol_rwdm(rscen,lenrscen,metscen,pradscen)
      implicit none
      include 'rug.inc'

      logical comment
      character*25 metscen, pradscen

      logical findprad,findmet

      data findprad,findmet
     .     /.false.,.false./


      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')

        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:11).eq.'METEOROLOGY') then
            findmet = .true.
            read(dfile,'(a25)') metscen
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

