************************************************************************
**  writes the FTABLES block using ./pp/param/river/$scen/ftables or  **
**    ./pp/param/river/$scen/variable_ftables if a reservoir          **
************************************************************************
      subroutine ftables(rseg,lenrseg,rscen,lenrscen,resflag)
      implicit none

      include 'rug.inc'
      logical found

************* END DECLARATIONS *****************************************

      line = 'FTABLES'
      call ryt(line,uci)

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

         ! depending on resflag, call different ftable writing routines

      if (resflag.eq.'S'.or.resflag.eq.'C'.or.
     .    resflag.eq.'J'                       ) then  ! regular river
                                              ! just copy ftable

        fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/ftables/'//rseg(:lenrseg)//'.ftable'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        call Sftable

      else if (resflag.eq.'V'.or.resflag.eq.'W'.or.
     .         resflag.eq.'R'.or.resflag.eq.'B') then  ! variable ftable
                                                       ! reservoir

        fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/variable_ftables/'//rseg(:lenrseg)//'.varftable'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        call Vftable(fnam)

      else

        go to 992

      end if

      return

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Lakeflag must be 0, 1, or -1'
      report(2) = '1=variable ftables, -1=external forcing'
      report(3) = 'file '//pardir//'river/'
     .            //paramscen(:lenparamscen)//'/gen_info_rseg.csv'
      go to 999

993   report(1) = 'trouble reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'end of file found before ftable found'
      report(2) = fnam
      write(report(3),*) 'need ftable associated with month ',startmonth
      go to 999

995   report(1) = 'problem reading months from file:  line: '
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)

      end



