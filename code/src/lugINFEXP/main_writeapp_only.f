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
      include 'lug.inc'
      include 'acts.inc'

      logical perlnd,implnd
             
********** END DECLARATION ********************************************

      read*, lseg,clu,lscen

      call lencl(lscen,lenlscen)
      call lencl(lseg,lenlseg)

      call getlutype(lscen,lenlscen,clu,perlnd,implnd)

      call readcontrol_metprad(
     I                         lscen,lenlscen,
     O                         metscen,pradscen)

      call readcontrol_Lmodules(
     I                          clu,lscen,lenlscen,perlnd,implnd,
     O                          modules,nmod)

      call readcontrol_Ltime(
     I                       lscen,lenlscen,
     O                       startY,startM,startD,endY,endM,endD)

      call getSpecSpecies(
     I                    lscen,lenlscen,
     O                    species,nspecies)

      call writeatdep(
     I                lscen,lenlscen,lseg,modules,nmod,
     I                pradscen,clu,species,nspecies,
     I                startY,startM,startD,endY,endM,endD)

      call lspecwrite(
     I                lseg,lenlseg,lscen,lenlscen,clu,
     I                startY,startM,startD,endY,endM,endD,
     I                perlnd,modules,nmod,species,nspecies)

      stop

************* ERROR SPACE **********************************************
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
      subroutine readcontrol_pltgen(clu,lscen,lenlscen,
     O                              pltgen)
      implicit none
      include 'lug.inc'

      integer i
      logical comment
      external comment

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      numplts = 0
      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        read(dfile,'(a100)')line
        call d2x(line,last)
        if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
          read(dfile,'(a100)')line
          call d2x(line,last)
          do while (line(:10).ne.'END PLTGEN')
            i = 1
            pltgen = .false.
            if (.not.comment(line)) then
              do while (line(i:i+2).ne.'   '.and.i.lt.100)
                if (line(i:i+2).eq.clu) then
                  numplts = numplts + 1
                  if (numplts.gt.maxplts) go to 992
                  pltgen = .true.
                end if
                i = i + 4
              end do
            end if
            read(dfile,'(a100)')line
            if (pltgen) then
              call d2x(line,last)
              read(line,*) pltname(numplts),pltgrp(numplts),
     .                     pltmem(numplts),pltnum(numplts,1),
     .                     pltnum(numplts,2),plttran1(numplts),
     .                     pltmnpt(numplts),pltpivl(numplts),
     .                     plttran2(numplts),pltlabel(numplts)
            end if
            read(dfile,'(a100)')line
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

992   report(1) = 'Too many pltgens called in control file'
      report(2) = fnam
      report(3) = 'decrease pltgens or increase maxplts variable '
      go to 999

999   call stopreport(report)
      end


