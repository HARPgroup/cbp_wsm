************************************************************************
**  The Land UCI Generator automatically makes land ucis generated    **
**    from databases.  One run of this program is required for each   **
**    Lsegment / scenario / land use triplet.  Control files that     **
**    define the activity of the scenario are in ./run/control/land/  **
**    The database of table names and headers for each module is      **
**    ./pp/catalog/modules/tables  The parameters for these           **
**    tables are called ./pp/param/$lu/$scen/$tabname.csv             **
************************************************************************

      implicit none
      include 'lugtables.inc'
      include 'acts.inc'

      logical perlnd,implnd
      real evap
             
********** END DECLARATION ********************************************

      read*, lseg,clu,lscen

********** house keeping
      call lencl(lscen,lenlscen)
      call lencl(lseg,lenlseg)
      call getlutype(lscen,lenlscen,clu,perlnd,implnd)
      
********** get info from control file
c      call readcontrol_pltgen(clu,lscen,lenlscen,
c     O                        pltgen)
      call readcontrol_Lioscen(
     I                         lscen,lenlscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)
      call readcontrol_metprad(
     I                         lscen,lenlscen,
     O                         metscen,pradscen)
      call readcontrol_Lmodules(
     I                          clu,lscen,lenlscen,perlnd,implnd,
     O                          modules,nmod)
      call readcontrol_Ltime(
     I                       lscen,lenlscen,
     O                       startY,startM,startD,endY,endM,endD)

      call readtables(
     I                lseg,clu,lscen,lenlscen,perlnd,implnd,
     I                modules,nmod,
     O                table,nTabs,header,nHead,Var,tabformat,
     O                iPar,fPar,evap)

************ open file and write UCI
c      fnam = ucidir//'land/'//clu//'/'//lscen(:lenlscen)//'/'//clu//
c     .       lseg(:lenlseg)//'.uci'
c      open(uci,file=fnam,status='unknown',iostat=err)     ! open uci
c      if (err.ne.0) go to 991

c      write (uci,'(a3)',err=951) 'RUN'
c      write (uci,'(a)',err=951) ' '

c      call ttyput ('global  ')
c      call global(lseg,clu,lscen,lenlscen,
c     I            startY,startM,startD,endY,endM,endD)

c      call ttyput ('files   ')
c      call files(lseg,lenlseg,clu,lscen,lenlscen,pltgen,
c     I           metscen,pradscen)

c      call ttyput ('opseq   ')
c      call opseq(perlnd,implnd,pltgen)
          
      call lspecact_monthly(                          !GY
     I              lseg,lenlseg,lscen,lenlscen,clu,
     I              startY,startM,startD,endY,endM,endD,
     I              perlnd,implnd,pradscen,
     I              modules,nmod,
     I              table,nTabs,Var,tabformat,
     M              iPar,fPar)
      
c      call ttyput('tables  ')
c      call writetables(lseg,clu,lscen,lenlscen,perlnd,implnd,
c     I                 modules,nmod,
c     I                 table,nTabs,header,nHead,Var,tabformat,
c     I                 iPar,fPar,evap)

c      call ttyput('extsources  ')
c      call extsources(ioscen,lenioscen,perlnd,implnd,evap)

c      call ttyput('exttargets  ')
c      call exttargets(ioscen,lenioscen,lseg,clu,perlnd,implnd)

c      if (pltgen) then
c        call ttyput('network  ')
c        call network(perlnd,implnd)
c      end if

c      if (pltgen) then
c        call ttyput('pltgen  ')
c        call pgen(lseg,clu)
c      end if

c      write (uci,'(a7)',err=951) 'END RUN'
      print*,' '

      return

************* ERROR SPACE **********************************************
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
c      subroutine readcontrol_pltgen(clu,lscen,lenlscen,
c     O                              pltgen)
c      implicit none
c      include 'lug.inc'

c      integer i
c      logical comment
c      external comment

c      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
c      open(dfile,file=fnam,status='old',iostat=err)
c      if (err.ne.0) go to 991

c      numplts = 0
c      read(dfile,'(a100)')line
c      call d2x(line,last)
c      do while (line(:3).ne.'end')
c        read(dfile,'(a100)')line
c        call d2x(line,last)
c        if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
c          read(dfile,'(a100)')line
c          call d2x(line,last)
c          do while (line(:10).ne.'END PLTGEN')
c            i = 1
c            pltgen = .false.
c            if (.not.comment(line)) then
c              do while (line(i:i+2).ne.'   '.and.i.lt.100-6)
c                if (line(i:i+2).eq.clu) then
c                  numplts = numplts + 1
c                  if (numplts.gt.maxplts) go to 992
c                  pltgen = .true.
c               end if
c                i = i + 4
c              end do
c            end if
c            read(dfile,'(a100)')line
c            if (pltgen) then
c              call d2x(line,last)
c              read(line,*) pltname(numplts),pltgrp(numplts),
c     .                     pltmem(numplts),pltnum(numplts,1),
c     .                     pltnum(numplts,2),plttran1(numplts),
c     .                     pltmnpt(numplts),pltpivl(numplts),
c     .                     plttran2(numplts),pltlabel(numplts)
c            end if
c            read(dfile,'(a100)')line
c            call d2x(line,last)
c          end do
c        end if
c      end do

c      if (numplts.eq.0) then
c        pltgen = .false.
c      else
c        pltgen = .true.
c      end if

c      close(dfile)

c      return
************* ERROR SPACE **********************************************
c991   report(1) = 'Problem opening file:'
c      report(2) = fnam
c      report(3) = 'error = '
c      write(report(3)(9:11),'(i3)')err
c      go to 999

c992   report(1) = 'Too many pltgens called in control file'
c      report(2) = fnam
c      report(3) = 'decrease pltgens or increase maxplts variable '
c      go to 999

c999   call stopreport(report)
c      end


