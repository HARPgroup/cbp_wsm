************************************************************************
** main calling routine for the special action writing section        **
************************************************************************
** V = variable cover in sediment                                     **
** P = plowing                                                        **
** F = fertilizer                                                     **
** M = manure                                                         **
** L = fixation for legumes                                           **
** T = variable total annual uptake target                            **
** U = variable montly fraction of uptake target                      **
************************************************************************
      subroutine lspecwrite(lseg,lenlseg,lscen,lenlscen,clu,
     I                      startY,startM,startD,endY,endM,endD,
     I                      perlnd,modules,nmod,species,nspecies)

      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer nm ! index

      integer julian
      external julian

      integer year1,year2
      integer flag

      logical dospec  ! are there any special actions
      logical perlnd
      logical found

********** END DECLARATION *********************************************
      call getspecflags(lseg,lscen,clu,specflags)

************ modify specflags for active modules
      do flag = 1,flaglength
        if (specflags(flag:flag).eq.'F') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') found = .true.
            if (modules(nm).eq.'PHOS') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'M') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') found = .true.
            if (modules(nm).eq.'PHOS') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'V') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'SEDMNT') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'P') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'SEDMNT') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'L') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') found = .true.
            if (modules(nm).eq.'PHOS') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'T') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') found = .true.
            if (modules(nm).eq.'PHOS') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
        else if (specflags(flag:flag).eq.'U') then
          found = .false.
          do nm = 1,nmod
            if (modules(nm).eq.'NITR') found = .true.
            if (modules(nm).eq.'PHOS') found = .true.
          end do
          if (.not.found) specflags(flag:flag) = ' '
         else if (specflags(flag:flag).eq.'Q') then     !GY
          do nm = 1,nmod                                !GY
            if (modules(nm).eq.'PQUAL') found = .true.  !GY
            if (modules(nm).eq.'IQUAL') found = .true.  !GY
          end do                                        !GY
          if (.not.found) specflags(flag:flag) = ' '    !GY
          else if (specflags(flag:flag).eq.'R') then    !GY
          do nm = 1,nmod                                !GY
            if (modules(nm).eq.'NITR') found = .true.  !GY
c            if (modules(nm).eq.'IQUAL') found = .true.  !GY
          end do                                        !GY
          if (.not.found) specflags(flag:flag) = ' '    !GY

        else if (specflags(flag:flag).eq.' ') then
          continue
        else
          go to 990
        end if
      end do
     

      dospec = .false.        ! check for SPEC ACT.  return if none
      do flag = 1,flaglength
        if (specflags(flag:flag).ne.' ') dospec = .true.
      end do
      if (.not.dospec) return

***** GET THE START YEAR 
      year1 = startY
      year2 = endY


      do flag = 1,flaglength  ! do ACTIONS
        if (specflags(flag:flag).eq.'F') then
          call Fwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                species,nspecies)
        else if (specflags(flag:flag).eq.'M') then
          call Mwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                species,nspecies)
        else if (specflags(flag:flag).eq.'V') then
          continue
        else if (specflags(flag:flag).eq.'P') then
          continue
        else if (specflags(flag:flag).eq.'L') then
          call Lwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                species,nspecies)
        else if (specflags(flag:flag).eq.'T') then
          call Twrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                species,nspecies)
        else if (specflags(flag:flag).eq.'U') then
          continue
        else if (specflags(flag:flag).eq.'Q') then  !GY
          continue                                  !GY
        else if (specflags(flag:flag).eq.'R') then  !GY
          continue                                  !GY
        else if (specflags(flag:flag).eq.' ') then
          continue
        else
          go to 993
        end if
      end do

      return

************************* ERROR SPACE **********************************
990   report(1) = 'special action flag '//specflags(flag:flag)
      report(2) = '  not programmed for modules in '
      report(3) = '  pp/src/lug/lspecact.f'
      go to 999
          
991   report(1) = 'special action flag '//specflags(flag:flag)
      report(2) = '  not programmed for UVQUANS in '
      report(3) = '  pp/src/lug/lspecact.f'
      go to 999
          
992   report(1) = 'special action flag '//specflags(flag:flag)
      report(2) = '  not programmed for UVNAMES in '
      report(3) = '  pp/src/lug/lspecact.f'
      go to 999
          
993   report(1) = 'special action flag '//specflags(flag:flag)
      report(2) = '  not programmed for special actions in '
      report(3) = '  pp/src/lug/lspecact.f'
      go to 999

999   call stopreport(report)
          
      end

