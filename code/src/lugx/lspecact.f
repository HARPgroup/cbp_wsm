************************************************************************
** main calling routine for the special action writing section        **
************************************************************************
** V = variable cover in sediment                                     **
** P = plowing                                                        **
** F = fertilizer                                                     **
** M = manure                                                         **
** L = fixation for legumes                                           **
** T = variable total annual uptake target                            **
** U = variable monthly fraction of uptake target                     **
** R = removal of refractory organic N to simulate harvest            **
************************************************************************
      subroutine lspecact(lseg,lenlseg,lscen,lenlscen,clu,
     I                    startY,startM,startD,endY,endM,endD,
     I                    perlnd,implnd,pradscen,
     I                    modules,nmod,
     I                    table,nTabs,Var,tabformat,
     M                    iPar,fPar)

      implicit none
      include 'lugtables.inc'
      include 'acts.inc'

      character*2 m1,m2,d1,d2

      integer nm ! index

      integer julian
      external julian

      integer year1,year2
      integer flag

      logical perlnd,implnd

      logical doV, doP, doF, doM, doW, doB, doL, doT, doU, doR, doQ
      data doV, doP, doF, doM, doW, doB, doL, doT, doU, doR, doQ 
     .     /11*.false./

********** END DECLARATION *********************************************
******** find the number of species
      call getSpecSpecies(
     I                    lscen,lenlscen,
     O                    species,nspecies)

      call getspecflags(lseg,lscen,clu,specflags)

************ determine active actions for active modules
      do flag = 1,flaglength
        if (specflags(flag:flag).eq.'F') then
            doF = .true.
        else if (specflags(flag:flag).eq.'M') then
            doM = .true.
        else if (specflags(flag:flag).eq.'W') then
            doW = .true.
        else if (specflags(flag:flag).eq.'B') then
            doB = .true.
        else if (specflags(flag:flag).eq.'V') then
            doV = .true.
        else if (specflags(flag:flag).eq.'P') then
            doP = .true.
        else if (specflags(flag:flag).eq.'L') then
            doL = .true.
        else if (specflags(flag:flag).eq.'T') then
            doT = .true.
        else if (specflags(flag:flag).eq.'U') then
            doU = .true.
        else if (specflags(flag:flag).eq.'R') then
            doR = .true.
        else if (specflags(flag:flag).eq.'Q') then
            doQ = .true.
        else if (specflags(flag:flag).eq.'I') then
            continue
        else if (specflags(flag:flag).eq.' ') then
          continue
        else
          go to 990
        end if
      end do

      doR = .false. ! TODO BHATT forced it as temp fix?? 
      
      if (.not.(doV.or.doP.or.doF.or.doM.or.
     .          doL.or.doT.or.doU.or.doR.or.doQ)) return

***** GET THE START YEAR 
      year1 = startY
      year2 = endY

********** write out the atmospheric deposition
      call writeatdep(
     I                lscen,lenlscen,lseg,modules,nmod,
     I                pradscen,clu,species,nspecies,
     I                startY,startM,startD,endY,endM,endD)

********** OPEN THE SECTION
      call ttyput (' :: Annual/Monthly Application Rates :: ')

      if (doF) call Faction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doM) call Maction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doB) call Baction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doW) call Waction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doV) call Vaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      if (doP) call Paction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      if (doL) call Laction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doT) call Taction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      species,nspecies)
      if (doU) call Uaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      if (doR) call Raction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)

**************** get pqual modifications
      if (doQ) then
        call getScenarioMod(
     I                      lseg,lscen,lenlscen,clu,modules,nmod,
     I                      startY,endY,species,nspecies,
     I                      doV,doP,doF,doM,doL,doT,doU,doR,
     O                      ScenarioMod,TimeMod)

************ write PQUAL
        call PQUALactions(
     I                    lseg,clu,lscen,lenlscen,perlnd,implnd,
     I                    startY,endY,modules,nmod,
     I                    table,nTabs,Var,tabformat,
     I                    ScenarioMod,TimeMod,
     M                    iPar,fPar)
     
      end if
 
      line = 'END SPEC-ACTIONS'
      call ryt(line,uci)

      line = '       '
      call ryt(line,uci)
      call ttyput ('\n')

      return

************************* ERROR SPACE **********************************
990   report(1) = 'special action flag '//specflags(flag:flag)
      report(2) = '  not programmed for modules in '
      report(3) = '  pp/src/lug/lspecact.f'
      go to 999
          
999   call stopreport(report)
          
      end

************************************************************************
** subroutine to write the distribution lines at the beginning of the **
**  special actions section.  These are shared by all special actions **
************************************************************************
**  #1 = 1/10 per day for 10 straight days                            **
**  #2 = 1/10 per day for 10 days spread over 30 days                 **
************************************************************************
      subroutine distribution(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = '***Distribution Line'
      call ryt(line,uci)
      line = '***kwrd><ds<ct> tc  ts <dff>  <frc><frc><frc><frc><frc>'
     .         //'<frc><frc><frc><frc><frc>'
      call ryt(line,uci)
      line = '  DISTRB  1  10 DY   1 SHIFT    .10  .10  .10  .10  .10'
     .        //'  .10  .10  .10  .10  .10'
      call ryt(line,uci)
      line = '  DISTRB  2  10 DY   3 SHIFT    .10  .10  .10  .10  .10'
     .        //'  .10  .10  .10  .10  .10'
      call ryt(line,uci)
      line = '       '
      call ryt(line,uci)
      return
      end

