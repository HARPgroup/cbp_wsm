************************************************************************
** modifies the PQUAL parameters.  This should all be handled with    **
**  iovar files rather than the current hard-coded setup              **
** performs 2 functions, modifies the pqual variables, then writes    **
**  them to special actions over time                                 **
** SQOLIM is modified in the parameter line, but it was not           **
**   implemented in hspf for special actions, so it is constant       **
************************************************************************
**  Agchem sensitivity is slightly greater than 1                     **
** meaning that a 50% drop in inputs is slightly more than a 50% drop **
**  in output.  The PQUAL model will have a sensitivity of 1 assigned **
**  to it.  When a scenario has half of the input of the calibration, **
**  it will have half of the output. check the file                   **
**  wsm/p5/development/PQUAL_sensitivity/P52_sensitivity.xls          **
************************************************************************
      subroutine PQUALactions(
     I                        lseg,clu,lscen,lenlscen,perlnd,implnd,
     I                        startY,endY,modules,nmod,
     I                        table,nTabs,Var,tabformat,
     I                        ScenarioMod,TimeMod,
     M                        iPar,fPar)
      implicit none
      include 'lugtables.inc'
      include 'acts.inc'

      logical perlnd,implnd,found,scompare
      external scompare
      integer nmodPQ,nqual

      integer nm,nt,nh,nv              ! indices

      real twelveval(12)

      character*1 IorF        ! integer or real format

      integer nVarModify,nVarNoMod
      parameter (nVarModify=6,nVarNoMod=2)
      character*10 VarModify(nVarModify),VarNoMod(nVarNoMod),VarNoSpec
      data VarModify /'SQO','POTFW','ACQOP','SQOLIM','IOQC','AOQC'/
      data VarNoMod /'WSQOP','POTFS'/
      data VarNoSpec /'SQOLIM'/

************* END DECLARATIONHS ****************************************
      found = .false.   ! get QUAL module number quit if no piqual
      do nmodPQ = 1,nmod
        if (modules(nmodPQ).eq.'PQUAL' .or.
     I      modules(nmodPQ).eq.'IQUAL') then
          found = .true.
          exit
        end if
      end do
      if (.not.found) return

************* write QUAL action header
      line = '*** Action Lines for PQUAL/IQUAL'
      call ryt(line,uci)

***************** Look for QUAL-INPUT tables 
      do nt = 1,nTabs(nmodPQ)
        if (table(nmodPQ,nt)(:10).ne.'QUAL-INPUT') cycle

**************** read the qual number (index for mods)
        read(table(nmodPQ,nt)(12:12),'(i1)') nqual

************** modify iPar or fPar using ScenarioMod
        do nv = 1,tabformat(nmodPQ,nt,1)
          found = .false.
          do nm = 1,nVarModify
            if (scompare(Var(nmodPQ,nt,nv),VarModify(nm))) then
              found = .true.
              fPar(nmodPQ,nt,nv)=fPar(nmodPQ,nt,nv)*ScenarioMod(nqual)
    
              if (scompare(Var(nmodPQ,nt,nv),VarNoSpec)) cycle
              call writeSpecActQual(
     I                              Var(nmodPQ,nt,nv),nqual,TimeMod,
     I                              startY,endY,fPar(nmodPQ,nt,nv))
            end if
          end do
          do nm = 1,nVarNoMod
            if (scompare(Var(nmodPQ,nt,nv),VarNoMod(nm))) found = .true.
          end do
          if (.not.found)  go to 992
        end do
*********** using TimeMod, write the parameter value for each year to
************ special actions
      end do


      return

************* ERROR SPACE    *******************************************

992   report(1) = 'need more coding for different table formats'
      report(2) = ' file ./pp/lug/src/tables.f'
      report(3) = ' '
      call stopreport(report)

      end


