************************************************************************
** subroutines for generating special actions                         **
************************************************************************
** V = variable cover in sediment                                     **
** P = plowing                                                        **
** A = atmospheric deposition                                         **
** F = fertilizer                                                     **
** M = manure                                                         **
** L = fixation for legumes                                           **
** T = variable total annual uptake target                            **
** U = variable montly fraction of uptake target                      **
************************************************************************

      subroutine actionhead(line,uci)
      implicit none
      character*(*) line
      integer uci
      line = ' '
      call ryt(line,uci)
      line = '*** ACTIONS'
      call ryt(line,uci)
      line = '***optyp range dc ds yr  mo da hr mn d t  '
     .       //' vari  s1 s2 s3 ac  value    tc ts num'
      call ryt(line,uci)
      line = '  <****><-><--><><-><--><-><-><-><-><><>  '
     .       //'<----><-><-><-><-><--------> <> <-><->'
      call ryt(line,uci)
      line = ' '
      call ryt(line,uci)
      return
      end

************************************************************************
** variable cover for the sediment simulation                         **
**  this subroutine writes monthly variables for each year on Jan1    **
************************************************************************
      subroutine Vaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real pctcov(maxTimeBreaks,12),lcv  ! land cover

      character*30 LCVfile(maxTimeBreaks)        ! land cover files
      integer nB

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx

********** END DECLARATION *********************************************
***** READ CONTROL FILE AND DATA
      keyword = 'CROP COVER'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,LCVfile)
       
C      print*,"nB = ",nB
      call readlucv(lseg,lenlseg,clu,nB,LCVfile,pctcov)

C      print*,"before rmMissingMonthly ", nB
      call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,pctcov)

C      print*,"after rmMissingMonthly ", nB
      if (nB.lt.2) then  ! try to go for defaults
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,LCVfile)

        call readlucv(lseg,lenlseg,clu,nB,LCVfile,pctcov)

        call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,pctcov)

        if (nB.lt.2) go to 992
      end if

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for Land Cover'
      call ryt(line,uci)

      line = '  PERLND  1         Y1Y1  1  1  1      3 '//
     .       ' COVERM  I       = <<<value>> '

      year = year1 
      indx1 = 1
      indx2 = 2
      lastindx = .false.
      do while (year.le.year2)
        do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)
          do month = 1,12
            lcv = pctcov(indx1,month) + 
     .            (pctcov(indx2,month)-pctcov(indx1,month))/
     .            real(Byear(indx2)-Byear(indx1)) * 
     .               real(year-Byear(indx1))
            lcv = max(lcv,0.0)
            lcv = min(lcv,1.0)
            write(line(21:24),'(i4)') year 
            write(line(50:51),'(i2)') month
            call ritef10(line(61:70),lcv)
            call ryt(line,uci)
          end do
          year = year + 1
        end do
        if (indx2.eq.nB) then
          lastindx = .true.
        else
          indx2 = indx2 + 1
          indx1 = indx1 + 1
        end if
      end do

      line = ' '
      call ryt(line,uci)
      return

******** ERROR SPACE ***************************************************
992   report(1) = ' insufficient crop cover data found to make the run'
      report(2) = ' expected segment/land use '//lseg//' '//clu
      report(3) = ' in files like '//LCVfile(1)//' or in scenario files'
      go to 999

999   call stopreport(report)

      end



************************************************************************
** Plowing actions for the sediment simulation                        **
************************************************************************
      subroutine Paction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year1,year2,indx1,indx2
      integer year,month,day

      character*30 Pfile(maxTimeBreaks)        ! legume file name
      integer nB,nm

      character*30 keyword

      real dets(maxTimeBreaks,12),det

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx

********** END DECLARATION *********************************************
********* READ DETS
      keyword = 'DETS'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                     nB,Jday,Byear,Bmonth,Bday,Pfile)

      call readDets(lseg,lenlseg,clu,nB,Pfile,dets)

      call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,dets)

      if (nB.lt.2) then  ! try to go for defaults
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Pfile)

        call readDets(lseg,lenlseg,clu,nB,Pfile,dets)

        call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                        nB,Jday,Byear,Bmonth,Bday,dets)

      end if

      if (nB.lt.2) return  ! no data, no actions, no writing

******** WRITE SPECIAL ACTION LINES
      line = '***Action Lines for Plowing'
      call ryt(line,uci)
    
      line  = 'IF (prec < 0.1) THEN' 
      call ryt(line,uci)

      line = '  PERLND  1    DY  1YYYY MM DD 12    2 3 '//
     .       ' DETS           += <<<value>> '

      year = year1
      indx1 = 1
      indx2 = 2
      lastindx = .false.
      day = 1

      do while (year.le.year2)

        do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)

          do month = 1,12
            det=dets(indx1,month)+(dets(indx2,month)-dets(indx1,month))/
     .           real(Byear(indx2)-Byear(indx1))*real(year-Byear(indx1))
            det = max(det,0.0)
            write(line(21:24),'(i4)') year
            write(line(26:27),'(i2)') month
            write(line(29:30),'(i2)') day
            call ritef10(line(61:70),det)
            call ryt(line,uci)
          end do

          year = year + 1
        end do

        if (indx2.eq.nB) then
          lastindx = .true.
        else
          indx2 = indx2 + 1
          indx1 = indx1 + 1
        end if
      end do

      line  = 'END IF'
      call ryt(line,uci)
      line = ' '
      call ryt(line,uci)

      return

      end

************************************************************************
** variable fixation for legumes                                      **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Laction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     .                   modules,nmod,species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real legfix(maxTimeBreaks,12),fix   ! legume fixation
      double precision annapp(year1:year2,nspecies) !annual application
      character*6 apptype
      data apptype /'legume'/

      character*30 Lfile(maxTimeBreaks)        ! legume file name
      integer nB,nm

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx,writeact

      integer day3

      save day3
 
********** END DECLARATION *********************************************
      writeact = .false.
      do nm = 1,nmod
        if (modules(nm).eq.'NITR') writeact = .true.
        if (modules(nm).eq.'PHOS') writeact = .true.
      end do

      do sp = 1,nspecies
        do year = year1,year2
          annapp(year,sp) = 0.0
        end do
      end do
      sp = 2 ! ammonia only

***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'LEGUME'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                     nB,Jday,Byear,Bmonth,Bday,Lfile)
       
      call readlegfix(lseg,lenlseg,clu,nB,Lfile,legfix)
       
      call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,legfix)

      if (nB.lt.2) then  ! try for default
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Lfile)

        call readlegfix(lseg,lenlseg,clu,nB,Lfile,legfix)

        call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                        nB,Jday,Byear,Bmonth,Bday,legfix)
      end if

      if (nB.lt.2) then  ! if still no data, write out zeros
        call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I                year1,year2,apptype,annapp)
        return
      end if


***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for legume fixation'
      if (writeact) call ryt(line,uci)

      line = '  PERLND  1         YYYY MM DD 12    2 3 '//
     .       ' LEGUME         += <<<value>> '
      write(line(29:30),'(i2)') day3

      year = year1 
      indx1 = 1
      indx2 = 2
      lastindx = .false.
      do while (year.le.year2)
        do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)
          write(line(21:24),'(i4)') year 
          do month = 1,12
            fix = legfix(indx1,month) + 
     .            (legfix(indx2,month)-legfix(indx1,month))/
     .            real(Byear(indx2)-Byear(indx1)) * 
     .               real(year-Byear(indx1))
            fix = max(fix,0.0)
            annapp(year,sp) = annapp(year,sp) + fix
            write(line(26:27),'(i2)') month
            call ritef10(line(61:70),fix)
            if (writeact) call ryt(line,uci)
          end do
          year = year + 1
        end do
        if (indx2.eq.nB) then
          lastindx = .true.
        else
          indx2 = indx2 + 1
          indx1 = indx1 + 1
        end if
      end do

      line = ' '
      if (writeact) call ryt(line,uci)

      call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I              year1,year2,apptype,annapp)

      return

***************ERROR SPACE**********************************************
999   call stopreport(report)

      end


************************************************************************
** variable manure applications                                       **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Maction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     .                   modules,nmod,species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real manure(maxTimeBreaks,nspecies,12),man   ! manure
      double precision annapp(year1:year2,nspecies) !annual application
      character*6 apptype
      data apptype /'manure'/

      character*30 Mfile(maxTimeBreaks)        ! manure file name
      integer nB, n, nm

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx,writeact

      integer day3

      save day3

      character*4 uvname(maxspecies)
      data uvname /'MNO3','MNH3','MORN','MPO4','MORP'/

********** END DECLARATION *********************************************
      writeact = .false.
      do nm = 1,nmod
        if (modules(nm).eq.'NITR') writeact = .true.
        if (modules(nm).eq.'PHOS') writeact = .true.
      end do

      do sp = 1,nspecies
        do year = year1,year2
          annapp(year,sp) = 0.0
        end do
      end do

***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'MANURE'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Mfile)
       
      call readmanure(lseg,lenlseg,clu,nB,Mfile,manure,
     I                species,nspecies)
       
      call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                        nspecies,
     M                        nB,Jday,Byear,Bmonth,Bday,manure)

      if (nB.lt.2) then
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                         nB,Jday,Byear,Bmonth,Bday,Mfile)

        call readmanure(lseg,lenlseg,clu,nB,Mfile,manure,
     I                  species,nspecies)

        call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                          nspecies,
     M                          nB,Jday,Byear,Bmonth,Bday,manure)
      end if

      if (nB.lt.2) then  ! if still no data, write out zeros
        call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I                year1,year2,apptype,annapp)
        return
      end if

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for manure'
      if (writeact) call ryt(line,uci)
      line  = 'IF (prec < 0.1) THEN'
      if (writeact) call ryt(line,uci)

      line = '  PERLND  1    DY  1YYYY MM DD 12    2 3 '//
     .       ' Mxxx           += <<<value>> '
      write(line(29:30),'(i2)') day3

********* loop over species, create temporary monthly variable
      do sp = 1,nspecies

        line(43:46) = uvname(sp)

        if (nB.gt.0) then  ! if all missing, then don't do this species

          year = year1 
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (year.le.year2)
            do while  ((year.le.Byear(indx2).or.lastindx)
     .                      .and.year.le.year2)
              write(line(21:24),'(i4)') year 
              do month = 1,12
                write(line(26:27),'(i2)') month
                if (manure(indx1,sp,month).gt.-1) then
                  man = manure(indx1,sp,month) + 
     .                  (manure(indx2,sp,month)-manure(indx1,sp,month))/
     .                    real(Byear(indx2)-Byear(indx1)) * 
     .                         real(year-Byear(indx1))
                  man = max(man,0.0)
                  annapp(year,sp) = annapp(year,sp) + man
                  if (writeact) call ritef10(line(61:70),man)
                  if (writeact) call ryt(line,uci)
                end if
              end do
              year = year + 1
            end do
            if (indx2.eq.nB) then
              lastindx = .true.
            else
              indx2 = indx2 + 1
              indx1 = indx1 + 1
            end if
          end do

        end if  ! if this species exists

      end do  ! loop over species

      call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I              year1,year2,apptype,annapp)

      line  = 'END IF'
      if (writeact) call ryt(line,uci)
      line = ' '
      if (writeact) call ryt(line,uci)
      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end


************************************************************************
** variable fertilizer applications                                   **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Faction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     .                   modules,nmod,species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real fert(maxTimeBreaks,nspecies,12),fer   ! fertilizer
      double precision annapp(year1:year2,nspecies) !annual application
      character*4 apptype
      data apptype /'fert'/

      character*30 Ffile(maxTimeBreaks)        ! fertilizer file name
      integer nB, n, nm

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx,writeact

      integer day3

      character*4 uvname(maxspecies)
      data uvname /'FNO3','FNH3','FORN','FPO4','FORP'/

      save day3
 
********** END DECLARATION *********************************************
      writeact = .false.
      do nm = 1,nmod
        if (modules(nm).eq.'NITR') writeact = .true.
        if (modules(nm).eq.'PHOS') writeact = .true.
      end do

      do sp = 1,nspecies
        do year = year1,year2
          annapp(year,sp) = 0.0
        end do
      end do

***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'FERTILIZER'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Ffile)
       
      call readfert(lseg,lenlseg,clu,nB,Ffile,fert,
     I              species,nspecies)
       
      call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                        nspecies,
     M                        nB,Jday,Byear,Bmonth,Bday,fert)

      if (nB.lt.2) then
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                         nB,Jday,Byear,Bmonth,Bday,Ffile)

        call readfert(lseg,lenlseg,clu,nB,Ffile,fert,
     I                species,nspecies)

        call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                          nspecies,
     M                          nB,Jday,Byear,Bmonth,Bday,fert)
      end if

      if (nB.lt.2) then  ! if still no data, write out zeros
        call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I                year1,year2,apptype,annapp)
        return
      end if

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for fertilizer'
      if (writeact) call ryt(line,uci)
      line  = 'IF (prec < 0.1) THEN'
      if (writeact) call ryt(line,uci)

      line = '  PERLND  1    DY  1YYYY MM DD 12    2 3 '//
     .       ' Fxxx           += <<<value>> '
      write(line(29:30),'(i2)') day3

********* loop over species, create temporary monthly variable
      do sp = 1,nspecies

        line(43:46) = uvname(sp)

        if (nB.gt.0) then  ! if all missing, then don't do this species

          year = year1 
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (year.le.year2)
            do while((year.le.Byear(indx2).or.lastindx)
     .                    .and.year.le.year2)
              write(line(21:24),'(i4)') year 
              do month = 1,12
                write(line(26:27),'(i2)') month
                if (fert(indx1,sp,month).gt.-1) then
                  fer = fert(indx1,sp,month) + 
     .                (fert(indx2,sp,month)-fert(indx1,sp,month))/
     .                real(Byear(indx2)-Byear(indx1)) * 
     .                   real(year-Byear(indx1))
                  fer = max(fer,0.0)
                  annapp(year,sp) = annapp(year,sp) + fer
                  call ritef10(line(61:70),fer)
                  if (writeact) call ryt(line,uci)
                end if
              end do
              year = year + 1
            end do
            if (indx2.eq.nB) then
              lastindx = .true.
            else
              indx2 = indx2 + 1
              indx1 = indx1 + 1
            end if
          end do

        end if  ! if this species exists

      end do  ! loop over species

      call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I              year1,year2,apptype,annapp)

      line  = 'END IF'
      if (writeact) call ryt(line,uci)
      line = ' '
      if (writeact) call ryt(line,uci)
      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end

************************************************************************
** variable total uptake for crops                                    **
**  this subroutine writes annual changes to the total uptake         **
************************************************************************
      subroutine Taction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                   species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real nupt(maxTimeBreaks,2),nup   ! annual uptake 1=N, 2=P
      double precision annapp(year1:year2,nspecies) !annual application
      character*6 apptype
      data apptype /'uptake'/

      character*30 Tfile(maxTimeBreaks)        ! file name
      integer nB,n
      character*1 nut(2)
      data nut /'N','P'/

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx

      integer day3

      save day3
 
********** END DECLARATION *********************************************

      do sp = 1,nspecies
        do year = year1,year2
          annapp(year,sp) = 0.0
        end do
      end do

***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'TOTAL UPTAKE'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Tfile)
       
      call readuptake(lseg,lenlseg,clu,nB,Tfile,nupt)
       
      call rmMissing2(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,nupt)

      if (nB.lt.2) then
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                         nB,Jday,Byear,Bmonth,Bday,Tfile)

        call readuptake(lseg,lenlseg,clu,nB,Tfile,nupt)

        call rmMissing2(maxTimeBreaks,lseg,clu,year1,year2,
     M                        nB,Jday,Byear,Bmonth,Bday,nupt)

        if (nB.lt.2) go to 992
      end if

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for variable total annual uptake'
      call ryt(line,uci)

      line = '  PERLND  1         YYYY 01 01 01      3 '//
     .       ' XUPTGT         =  <<<value>> '
      write(line(29:30),'(i2)') day3

      if (nB.gt.0) then
        year = year1 
        indx1 = 1
        indx2 = 2
        lastindx = .false.
        do while (year.le.year2)
          do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)
            write(line(21:24),'(i4)') year 
            do n = 1,2    ! N and P
              sp = 1              ! N is default
              if (n.eq.2) sp = 4  ! P
              nup = nupt(indx1,n) + (nupt(indx2,n)-nupt(indx1,n))/
     .                  real(Byear(indx2)-Byear(indx1))*
     .                      real(year-Byear(indx1))
              line(43:43) = nut(n)
              nup = max(nup,0.0)
              annapp(year,sp) = annapp(year,sp) + nup
              call ritef10(line(61:70),nup)
              call ryt(line,uci)
            end do
            year = year + 1
          end do
          if (indx2.eq.nB) then
            lastindx = .true.
          else
            indx2 = indx2 + 1
            indx1 = indx1 + 1
          end if
        end do
      end if

      call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I              year1,year2,apptype,annapp)

      line = ' '
      call ryt(line,uci)
      return

******** ERROR SPACE ***************************************************
992   report(1) = ' insufficient crop cover data found to make the run'
      report(2) = ' expected segment/land use '//lseg//' '//clu
      report(3) = ' in files like '//Tfile(1)//' or in scenario files'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** variable monthly uptake fractions for crops                        **
**  this subroutine writes monthly variables for each year on Jan1    **
************************************************************************
      subroutine Uaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      integer ntype, nt
      parameter (ntype=2)
      real pctnup(maxTimeBreaks,ntype,12),pct ! percent N and P by month

      character*30 Ufile(maxTimeBreaks)   ! monthly uptake files
      integer nB, n, NB2
      character*1 nut(ntype)
      data nut /'N','P'/

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx

********** END DECLARATION *********************************************
***** READ CONTROL FILE AND DATA
      keyword = 'MONTHLY FRACTION UPTAKE'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Ufile)
       
      call readpctnup(lseg,lenlseg,clu,nB,Ufile,pctnup)
       
      call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                      ntype,
     M                      nB,Jday,Byear,Bmonth,Bday,pctnup)

      if (nB.lt.2) then
        keyword = 'DEFAULT CROPDATA'
        call readcontrol1key(lscen,lenlscen,keyword,
     O                         nB,Jday,Byear,Bmonth,Bday,Ufile)

        call readpctnup(lseg,lenlseg,clu,nB,Ufile,pctnup)

        call rmMissingMonthlySpecies(maxTimeBreaks,lseg,clu,year1,year2,
     I                        ntype,
     M                        nB,Jday,Byear,Bmonth,Bday,pctnup)

        if (nB.lt.2) go to 992
      end if

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for monthly uptake fractions'
      call ryt(line,uci)

      line = '  PERLND  1         Y1Y1  1  1  1      3 '//
     .       ' XUPTFM  I       = <<<value>> '

      do nt = 1,2

        line(43:43) = nut(nt)

        if (nB.gt.0) then  ! if all missing, then don't do this species

          year = year1 
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (year.le.year2)
            do while ((year.le.Byear(indx2).or.lastindx)
     .                     .and.year.le.year2)
              write(line(21:24),'(i4)') year 
              do month = 1,12
                write(line(50:51),'(i2)') month
                  pct = pctnup(indx1,nt,month) + 
     .                (pctnup(indx2,nt,month)-pctnup(indx1,nt,month))/
     .                real(Byear(indx2)-Byear(indx1)) * 
     .                   real(year-Byear(indx1))
                  call ritef10(line(61:70),pct)
                  call ryt(line,uci)
              end do
              year = year + 1
            end do
            if (indx2.eq.nB) then
              lastindx = .true.
            else
              indx2 = indx2 + 1
              indx1 = indx1 + 1
            end if
          end do

        end if

      end do
      line = ' '
      call ryt(line,uci)
      return

******** ERROR SPACE ***************************************************
992   report(1) = ' insufficient crop cover data found to make the run'
      report(2) = ' expected segment/land use '//lseg//' '//clu
      report(3) = ' in files like '//Ufile(1)//' or in scenario files'
      go to 999

999   call stopreport(report)

      end


************************************************************************
** refractory organic storage control                                 **
**  this subroutine writes annual changes to the total storage        **
************************************************************************
      subroutine Raction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year1,year2
      integer nly,rept

      character*5 RORGN(4)
      data RORGN /'SPRON','UPRON','LPRON','APRON'/     ! Refractory Organic Storage at Each Layer

      real lsegron(4)
********** END DECLARATION *********************************************

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for Adjusting Refractory Storage'
      call ryt(line,uci)
    
***** READ ADJUSTED STORAGE DATA
      call readrorgn(
     I               lscen,lenlscen,lseg,lenlseg,clu,
     O               lsegron)
     
      line = '  PERLND  1         Y1Y1  1  1         3 '//
     .       ' RORGN          -= <<value>>  YR   1 RP'

      rept = year2-year1+1
      do nly = 1, 4 
        line(43:47) = RORGN(nly)     
        write(line(21:24),'(i4)') year1         ! first simulation year
        call ritef10(line(61:70),lsegron(nly))
        write (line(79:80),'(i2)') rept         ! put in repeat number
        call ryt(line,uci)
      end do

      line = '       '
      call ryt(line,uci)

      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end


************************************************************************
** INFEXP special action for hydrology simulation                        **
************************************************************************
      subroutine Iaction(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                   fPar)
      implicit none
c      include 'lug.inc'
      include 'lugtables.inc'
      include 'acts.inc'

      integer year1,year2,indx1,indx2
      integer year,month,day

      character*30 Pfile(maxTimeBreaks)        ! legume file name
      integer nB,nm

      character*30 keyword

      integer days
      real    precip, infexp
      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)

      logical lastindx,writeact

      character*50 dline

********** END DECLARATION *********************************************

      writeact = .false.
c      do nm = 1,nmod
c        print*,modules(nm)
c        if (modules(nm).eq.'PWATER') writeact = .true.
c      end do
      writeact = .true.

c      call readcontrol_Lioscen(
c     I                          lscen,lenlscen,
c     O                          ioscen)
c      call lencl(ioscen,lenioscen)
c      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/infexp'

      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'common/'//paramscen(:lenparamscen)//'/infexp'

c      print*,fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      read(dfile,'(a50)',err=992)dline !header
c      print*,dline

c      read(dfile,'(a300)',err=992)dline
      read(dfile,*,err=992)days,precip,infexp

c      write(precip,*) dline(1:6)
c      write(infexp,*) dline(8:13)

c      print*,precip,',',infexp

***** WRITE SPECIAL ACTION for varied land cover IN UCI FILE
      line = '***Action Lines for INFEXP'
      if (writeact) call ryt(line,uci)

      line  = 'IF (precx1 > 0.0000) THEN'
      write(line(14:19),*)precip
      if (writeact) call ryt(line,uci)
      line = '  PERLND  1                            3 '//
     .       ' ??????          = <<<value>> '

c      write(line(43:48),'(A6)')'INFEXP'
c      write(line(61:70),*)infexp
c      if (writeact) call ryt(line,uci)

      write(line(43:48),'(A6)')'INFILT'
      write(line(61:70),*)fPar(3,2,3)/10
      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'INFILD'
c      write(line(61:70),*)1.0
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'INTFW '
c      write(line(61:70),*)fPar(3,4,4)*1.1
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'IRC   '
c      write(line(61:70),*)fPar(3,4,5)*0.8
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'UZSN  '
c      write(line(61:70),*)fPar(3,4,2)*0.25
c      if (writeact) call ryt(line,uci)

      write(line(43:48),'(A6)')'LZSN  '
      write(line(61:70),*)fPar(3,2,2)*0.25
      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'AGWRC '
c      write(line(61:70),*)fPar(3,2,7)*0.93
c      if (writeact) call ryt(line,uci)

      line  = 'ELSE'
      infexp = 2.0 ! TODO: USE DEFAULT PARAM FROM TABLE
      if (writeact) call ryt(line,uci)
      line = '  PERLND  1                            3 '//
     .       ' ??????          = <<<value>> '

c      write(line(43:48),'(A6)')'INFEXP'
c      write(line(61:70),*)infexp
c      if (writeact) call ryt(line,uci)

      write(line(43:48),'(A6)')'INFILT'
      write(line(61:70),*)fPar(3,2,3)
      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'INFILD'
c      write(line(61:70),*)2.0
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'INTFW '
c      write(line(61:70),*)fPar(3,4,4)
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'IRC   '
c      write(line(61:70),*)fPar(3,4,5)
c      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'UZSN  '
c      write(line(61:70),*)fPar(3,4,2)
c      if (writeact) call ryt(line,uci)

      write(line(43:48),'(A6)')'LZSN  '
      write(line(61:70),*)fPar(3,2,2)
      if (writeact) call ryt(line,uci)

c      write(line(43:48),'(A6)')'AGWRC '
c      write(line(61:70),*)fPar(3,2,7)
c      if (writeact) call ryt(line,uci)

      line  = 'END IF'
      if (writeact) call ryt(line,uci)
      line = ' '
      if (writeact) call ryt(line,uci)
      return

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' insufficient data in infexp file '
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
