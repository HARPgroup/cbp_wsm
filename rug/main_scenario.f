************************************************************************
**  The River UCI Generator automatically makes river ucis generated  **
**    from databases.  One run of this program is required for each   **
**    Rsegment / scenario combination.  Control files that            **
**    define the activity of the scenario are in ./run/control/river/ **
**    The database of table names and headers for each module is      **
**    ./pp/lib/catalogs/modules/tables  The parameters for these      **
**    tables are called ./pp/lib/catalogs/modules/river/$tabname      **
************************************************************************

      implicit none

      include 'rug.inc'
      logical IamRiver,rivercheck
      external rivercheck

*************** END OF DECLARATION *************************************      
      read*, rseg,rscen

      IamRiver = rivercheck(rseg)

      if (.not.IamRiver) then
        print*,'Segment ',rseg,' is not a river.'
        stop
      end if

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      fnam = ucidir//'river/'//rscen(:lenrscen)//'/'//
     .       rseg(:lenrseg)//'.uci'
      open(uci,file=fnam,status='unknown',iostat=err)     ! open uci
      if (err.ne.0) go to 991


      call readcontrol_rpltgen(rscen,lenrscen,
     O                         pltgen)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,lakeflag,resflag,timestep)

      write (uci,'(a3)',err=951) 'RUN'
      write (uci,'(a)',err=951) ' '

      call ttyput ('global  ')
      call rglobal(rseg,rscen,lenrscen,startmonth)

      call ttyput ('files   ')
      call rfiles(rseg,lenrseg,rscen,lenrscen,pltgen)

      call ttyput ('opseq   ')
      call ropseq(pltgen,timestep)

      call ttyput('tables  ')
      call scenario_rtables(rseg,rscen,lenrscen,Nexits,lakeflag)

      call ttyput('ftables  ')
      call ftables(rseg,lenrseg,rscen,lenrscen,resflag)

      call ttyput('extsources  ')
      call rextsources(rscen,lenrscen,ioscen,lenioscen,
     .                 rseg,Nexits,timestep)

      call ttyput('exttargets  ')
      call rexttargets(ioscen,lenioscen,rseg,Nexits,timestep)

      if (pltgen) then
        call ttyput('network  ')
        call rnetwork
      end if

      if (pltgen) then
        call ttyput('pltgen  ')
        call rpgen(rseg,timestep)
      end if

      call ttyput ('spec  ')
      call rspecact(rseg,lenrseg,rscen,lenrscen,resflag)

      write (uci,'(a7)',err=951) 'END RUN'
      print*,' '

      return

************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

************************************************************************
** routine to read the control file and populate pltgen variables     **
************************************************************************
      subroutine readcontrol_rpltgen(rscen,lenrscen,
     O                               pltgen)
      implicit none
      include 'rug.inc'

      integer i
      logical comment
      external comment

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      numplts = 0
      read(dfile,'(a100)',err=992,end=993)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=992,end=993)line
        call d2x(line,last)
        if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
          read(dfile,'(a100)',err=992,end=993)line
          call d2x(line,last)
          do while (line(:10).ne.'END PLTGEN')
            if (.not.comment(line)) then
              numplts = numplts + 1
              read(line,*,err=996,end=997) 
     .                   pltname(numplts),pltgrp(numplts),
     .                   pltmem(numplts),pltnum(numplts,1),
     .                   pltnum(numplts,2),plttran1(numplts),
     .                   pltmnpt(numplts),pltpivl(numplts),
     .                   plttran2(numplts),pltlabel(numplts)
            end if
            read(dfile,'(a100)',err=992,end=993)line
            call d2x(line,last)
          end do
        end if
      end do

      if (numplts.eq.0) then
        pltgen = .false.
      else
        pltgen = .true.
      end if

      close(dfile)

      return
************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'End of file found before expected'
      report(2) = fnam
      report(3) = ' '
      go to 999

996   report(1) = 'problem with file: can not read pltgen line'
      report(2) = fnam
      report(3) = line
      go to 999

997   report(1) = 'problem with file: pltgen line: not enough info'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      end


