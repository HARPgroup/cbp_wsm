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
      subroutine lspecact_monthly(lseg,lenlseg,lscen,lenlscen,clu,
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

      logical doV, doP, doF, doM, doL, doT, doU, doR, doQ
      data doV, doP, doF, doM, doL, doT, doU, doR, doQ /9*.false./

********** END DECLARATION *********************************************
******** find the number of species
      call getSpecSpecies(
     I                    lscen,lenlscen,
     O                    species,nspecies)

      call getspecflags(lseg,lscen,clu,specflags)

************ determine active actions for active modules
      do flag = 1,flaglength
        if (specflags(flag:flag).eq.'F') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doF = .true.
            if (modules(nm).eq.'PHOS') doF = .true.
            if (modules(nm).eq.'PQUAL') doF = .true.
            if (modules(nm).eq.'IQUAL') doF = .true.
          end do
        else if (specflags(flag:flag).eq.'M') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doM = .true.
            if (modules(nm).eq.'PHOS') doM = .true.
            if (modules(nm).eq.'PQUAL') doM = .true.
            if (modules(nm).eq.'IQUAL') doM = .true.
          end do
        else if (specflags(flag:flag).eq.'V') then
          do nm = 1,nmod
            if (modules(nm).eq.'SEDMNT') doV = .true.
          end do
        else if (specflags(flag:flag).eq.'P') then
          do nm = 1,nmod
            if (modules(nm).eq.'SEDMNT') doP = .true.
          end do
        else if (specflags(flag:flag).eq.'L') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doL = .true.
            if (modules(nm).eq.'PHOS') doL = .true.
            if (modules(nm).eq.'PQUAL') doL = .true.
            if (modules(nm).eq.'IQUAL') doL = .true.
          end do
        else if (specflags(flag:flag).eq.'T') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doT = .true.
            if (modules(nm).eq.'PHOS') doT = .true.
          end do
        else if (specflags(flag:flag).eq.'U') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doU = .true.
            if (modules(nm).eq.'PHOS') doU = .true.
          end do
        else if (specflags(flag:flag).eq.'R') then
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') doR = .true.
C            if (modules(nm).eq.'PHOS') doR = .true.
          end do
        else if (specflags(flag:flag).eq.'Q') then
          do nm = 1,nmod
            if (modules(nm).eq.'PQUAL') doQ = .true.
            if (modules(nm).eq.'IQUAL') doQ = .true.
          end do
        else if (specflags(flag:flag).eq.' ') then
          continue
        else
          go to 990
        end if
      end do
      
      if (.not.(doV.or.doP.or.doF.or.doM.or.
     .          doL.or.doT.or.doU.or.doR.or.doQ)) return

***** GET THE START YEAR 
      year1 = startY
      year2 = endY

********** write out the atmospheric deposition
      call writeatdep_monthly(
     I                lscen,lenlscen,lseg,modules,nmod,
     I                pradscen,clu,species,nspecies,
     I                startY,startM,startD,endY,endM,endD)

********** OPEN THE SECTION
c      call ttyput ('spec  ')
c      line = 'SPEC-ACTIONS'
c      call ryt(line,uci)
c      line = '       '
c      call ryt(line,uci)

c      call distribution(line,uci) ! distributions are common to all

c      call precquan(line,uci,perlnd)     ! precip is a common UVQUAN
c      if (doF) call Fuvquan(line,uci)
c      if (doM) call Muvquan(line,uci)
c      if (doV) call Vuvquan(line,uci)
c      if (doP) call Puvquan(line,uci)
c      if (doL) call Luvquan(line,uci)
c      if (doT) call Tuvquan(line,uci)
c      if (doU) call Uuvquan(line,uci)
c      if (doR) call Ruvquan(line,uci)

c      call uvnamehead(line,uci)     ! header for uvnames         
c      if (doF) call Fuvname(line,uci)
c      if (doM) call Muvname(line,uci)
c      if (doV) call Vuvname(line,uci)
c      if (doP) call Puvname(line,uci)
c      if (doL) call Luvname(line,uci)
c      if (doT) call Tuvname(line,uci)
c      if (doU) call Uuvname(line,uci)
c      if (doR) call Ruvname(line,uci)
         
      call actionhead(line,uci)     ! header for special actions
      if (doF) call Faction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
      if (doM) call Maction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                      modules,nmod,species,nspecies)
c      if (doV) call Vaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
c      if (doP) call Paction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
c      if (doL) call Laction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
c     I                      modules,nmod,species,nspecies)
c      if (doT) call Taction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
c     I                      species,nspecies)
c      if (doU) call Uaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
c      if (doR) call Raction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)

**************** get pqual modifications
c      if (doQ) then
c        call getScenarioMod(
c     I                      lseg,lscen,lenlscen,clu,modules,nmod,
c     I                      startY,endY,species,nspecies,
c     I                      doV,doP,doF,doM,doL,doT,doU,doR,
c     O                      ScenarioMod,TimeMod)

************ write PQUAL
c        call PQUALactions(
c     I                    lseg,clu,lscen,lenlscen,perlnd,implnd,
c     I                    startY,endY,modules,nmod,
c     I                    table,nTabs,Var,tabformat,
c     I                    ScenarioMod,TimeMod,
c     M                    iPar,fPar)
     
c      end if
 
c      line = 'END SPEC-ACTIONS'
c      call ryt(line,uci)

c      line = '       '
c      call ryt(line,uci)

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

