************************************************************************
** populates the tables for the PERLND or IMPLND simulation           **
** using the scenario file and module catalogs                        **
**  This file contains the major subroutines called from the major    **
**    subroutine 'tables'                                             **
************************************************************************
      subroutine writetables(lseg,clu,lscen,lenlscen,perlnd,implnd,
     I                       modules,nmod,
     I                       table,nTabs,header,nHead,Var,tabformat,
     I                       iPar,fPar,evap)
      implicit none

      include 'lugtables.inc'
      include 'acts.inc'

      logical perlnd,implnd

      integer nm,nt,nh,nv              ! indices

      real twelveval(12)

      character*1 IorF        ! integer or real format

      real evap   ! evaporation modifier

      line = 'PERLND'
      if (implnd) line = 'IMPLND'
      call ryt(line,uci)

      if (perlnd) call PERactivity(modules,nmod)
      if (implnd) call IMPactivity(modules,nmod)

      call geninfo(lseg,clu)

      do nm = 1,nmod
        do nt = 1,nTabs(nm)

          call lencl(table(nm,nt),last)  ! deal with repeated table name
          if (ichar(table(nm,nt)(last-1:last-1)).eq.35) then  ! if '#'
            table(nm,nt) = table(nm,nt)(:last-2)  ! get rid of suffix
          end if

          line = '  '//table(nm,nt)
          call ryt(line,uci)
          do nh = 1,nHead(nm,nt)
            call ryt(header(nm,nt,nh),uci)
          end do
          if (tabformat(nm,nt,2).eq.0) then    ! integer table
            line = '    1'
            if (tabformat(nm,nt,3).eq.5) then  ! 5 places per variable
              do nv = 1,tabformat(nm,nt,1)
                write(line(5*nv+6:5*(nv+2)),'(i5)') iPar(nm,nt,nv)
              end do
            else if (tabformat(nm,nt,3).eq.10) then ! 10 places per var
              do nv = 1,tabformat(nm,nt,1)
                write(line(10*nv+1:10*(nv+1)),'(i10)') iPar(nm,nt,nv)
              end do
            else
              go to 991
            end if
            call ryt(line,uci)
          else if (tabformat(nm,nt,2).eq.1) then     ! real table
            do nv = 1,tabformat(nm,nt,1)
              twelveval(nv) = fPar(nm,nt,nv)
            end do
            if (tabformat(nm,nt,3).eq.5) then   ! 5 places per variable
              call rite12f5(uci,tabformat(nm,nt,1),twelveval)
            else if (tabformat(nm,nt,3).eq.10) then ! 10 places per var
              call rite7f10(uci,tabformat(nm,nt,1),twelveval)
            else if (tabformat(nm,nt,3).eq.8) then  ! 8 places per var
              call rite8f8(uci,tabformat(nm,nt,1),twelveval)
            else
              go to 991
            end if
          else if (tabformat(nm,nt,2).eq.2) then ! unique table format
            line = '    1'
            call Uformat(modules(nm),table(nm,nt),
     O                   Unvar,Ustart,Uend,IorF)
            if (IorF.eq.'i'.or.IorF.eq.'I') then
              do nv = 1,Unvar
                call Uwritei(Ustart(nv),Uend(nv),ipar(nm,nt,nv),line)
              end do
              call ryt(line,uci)
            else if (IorF.eq.'f'.or.IorF.eq.'F') then
              do nv = 1,Unvar
                call fchar(fpar(nm,nt,nv),line(Ustart(nv):Uend(nv)))
              end do
              call ryt(line,uci)
            else
              go to 991
            end if
          else
            go to 991
          end if
          line = '  END '//table(nm,nt)
          call ryt(line,uci)
          line = ' '
          call ryt(line,uci)
        end do
      end do

      line = 'END PERLND'
      if (implnd) line = 'END IMPLND'
      call ryt(line,uci)

      line = '          '
      call ryt(line,uci)

      return

************* ERROR SPACE    *******************************************

991   report(1) = 'need more coding for different table formats'
      report(2) = ' file ./pp/lug/src/tables.f'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

************************************************************************
** writes the GEN-INFO table                                          **
************************************************************************
      subroutine geninfo(lseg,clu)
      implicit none
      include 'lug.inc'

      line = '  GEN-INFO'
      call ryt(line,uci)

      line = '    #    #      NAME          NBLKS  UCI   IN  OUT ENGL '
     .       //'METR  ***'
      call ryt(line,uci)

      line = '    1     FOREST                  1    1    1    1   26 '
     .       //'   0'
      line(11:30) = ' '//lseg//' '//clu
      call ryt(line,uci)

      line = '  END GEN-INFO'
      call ryt(line,uci)

      line = ' '
      call ryt(line,uci)

      end

************************************************************************
** writes the ACTIVITY and PRINT-INFO tables according to modules     **
************************************************************************
      subroutine PERactivity(modules,nmod)
      implicit none

      include 'lugtables.inc'
      integer nm

      line = '  ACTIVITY'
      call ryt(line,uci)

      line = '    #    # ATMP SNOW PWAT  SED  PST  PWG PQAL MSTL PEST '
     .       //'NITR PHOS TRAC   ***'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0    0    0    0 '
     .       //'   0    0    0'
      do nm = 1,nmod
        if (modules(nm).eq.'ATEMP')  line(15:15) = '1'
        if (modules(nm).eq.'SNOW')   line(20:20) = '1'
        if (modules(nm).eq.'PWATER') line(25:25) = '1'
        if (modules(nm).eq.'SEDMNT') line(30:30) = '1'
        if (modules(nm).eq.'PSTEMP') line(35:35) = '1'
        if (modules(nm).eq.'PWTGAS') line(40:40) = '1'
        if (modules(nm).eq.'PQUAL')  line(45:45) = '1'
        if (modules(nm).eq.'MSTLAY') line(50:50) = '1'
        if (modules(nm).eq.'PEST')   line(55:55) = '1'
        if (modules(nm).eq.'NITR')   line(60:60) = '1'
        if (modules(nm).eq.'PHOS')   line(65:65) = '1'
        if (modules(nm).eq.'TRACER') line(70:70) = '1'
      end do
      call ryt(line,uci)

      line = '  END ACTIVITY'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      line = '  PRINT-INFO'
      call ryt(line,uci)

      line = '    #    # ATMP SNOW PWAT  SED  PST  PWG PQAL MSTL PEST '
     .       //'NITR PHOS TRAC PIVL***PY'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0    0    0    0 '
     .       //'   0    0    0    0   12'
      do nm = 1,nmod
        if (modules(nm).eq.'ATEMP')  line(15:15) = '6'
        if (modules(nm).eq.'SNOW')   line(20:20) = '4'
        if (modules(nm).eq.'PWATER') line(25:25) = '4'
        if (modules(nm).eq.'SEDMNT') line(30:30) = '6'
        if (modules(nm).eq.'PSTEMP') line(35:35) = '6'
        if (modules(nm).eq.'PWTGAS') line(40:40) = '6'
        if (modules(nm).eq.'PQUAL')  line(45:45) = '6'
        if (modules(nm).eq.'MSTLAY') line(50:50) = '6'
        if (modules(nm).eq.'PEST')   line(55:55) = '6'
        if (modules(nm).eq.'NITR')   line(60:60) = '6'
        if (modules(nm).eq.'PHOS')   line(65:65) = '6'
        if (modules(nm).eq.'TRACER') line(70:70) = '6'
      end do
      call ryt(line,uci)

      line = '  END PRINT-INFO'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      end

************************************************************************
** writes the ACTIVITY and PRINT-INFO tables according to modules     **
************************************************************************
      subroutine IMPactivity(modules,nmod)
      implicit none

      include 'lugtables.inc'
      integer nm

      line = '  ACTIVITY'
      call ryt(line,uci)

      line = '    #    # ATMP SNOW IWAT  SLD  IWG IQAL  ***'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0'
      do nm = 1,nmod
        if (modules(nm).eq.'ATEMP')  line(15:15) = '1'
        if (modules(nm).eq.'SNOW')   line(20:20) = '1'
        if (modules(nm).eq.'IWATER') line(25:25) = '1'
        if (modules(nm).eq.'SOLIDS') line(30:30) = '1'
        if (modules(nm).eq.'IWTGAS') line(35:35) = '1'
        if (modules(nm).eq.'IQUAL')  line(40:40) = '1'
      end do
      call ryt(line,uci)

      line = '  END ACTIVITY'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      line = '  PRINT-INFO'
      call ryt(line,uci)

      line = '    #    # ATMP SNOW IWAT  SLD  IWG IQAL PIVL  PYR  ***'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0    0   12'
      do nm = 1,nmod
        if (modules(nm).eq.'ATEMP')  line(15:15) = '6'
        if (modules(nm).eq.'SNOW')   line(20:20) = '4'
        if (modules(nm).eq.'IWATER') line(25:25) = '4'
        if (modules(nm).eq.'SOLIDS') line(30:30) = '6'
        if (modules(nm).eq.'IWTGAS') line(35:35) = '6'
        if (modules(nm).eq.'IQUAL')  line(40:40) = '6'
      end do
      call ryt(line,uci)

      line = '  END PRINT-INFO'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      end

