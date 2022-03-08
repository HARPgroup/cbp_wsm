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

************************************************************************
** variable fixation for legumes                                      **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Lwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                  species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real legfix(maxTimeBreaks,12),fix   ! legume fixation
      double precision annapp(year1:year2,nspecies) ! annual application
      character*6 apptype
      data apptype /'legume'/

      character*20 Lfile(maxTimeBreaks)        ! legume file name
      integer nB

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
      sp = 2 ! ammonia only

***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'LEGUME'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Lfile)
       
      call readlegfix(lseg,lenlseg,clu,nB,Lfile,legfix)
       
      call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                      nB,Jday,Byear,Bmonth,Bday,legfix)

      year = year1 
      indx1 = 1
      indx2 = 2
      lastindx = .false.
      do while (year.le.year2)
        do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)
          do month = 1,12
            fix = legfix(indx1,month) + 
     .            (legfix(indx2,month)-legfix(indx1,month))/
     .            real(Byear(indx2)-Byear(indx1)) * 
     .               real(year-Byear(indx1))
            fix = max(fix,0.0)
            annapp(year,sp) = annapp(year,sp) + fix
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

      call writeapp(lscen,lenlscen,lseg,clu,species,nspecies,
     I              year1,year2,apptype,annapp)

      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end


************************************************************************
** variable manure applications                                       **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Mwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                  species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real manure(maxTimeBreaks,nspecies,12),man   ! manure
      double precision annapp(year1:year2,nspecies) ! annual application
      character*6 apptype
      data apptype /'manure'/

      character*20 Mfile(maxTimeBreaks)        ! manure file name
      integer nB, nB2, n

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)
      integer Jday2(maxTimeBreaks),Byear2(maxTimeBreaks)
      integer Bmonth2(maxTimeBreaks),Bday2(maxTimeBreaks)

      logical lastindx

      integer day3

      character*4 uvname(maxspecies)
      data uvname /'MNO3','MNH3','MORN','MPO4','MORP'/

      real monthly(maxTimeBreaks,12)

      save day3
 
********** END DECLARATION *********************************************
***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'MANURE'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Mfile)
       
      call readmanure(lseg,lenlseg,clu,nB,Mfile,manure,
     I                species,nspecies)
       
********* loop over species, create temporary monthly variable
      do sp = 1,nspecies

        do year = year1,year2
          annapp(year,sp) = 0.0
        end do

        nB2 = nB  ! copy all data into temp variables
        do n = 1,nB2
          do month = 1,12
            monthly(n,month) = manure(n,sp,month)
          end do
          Jday2(n) = Jday(n)
          Byear2(n) = Byear(n)
          Bmonth2(n) = Bmonth(n)
          Bday2(n) = Bday(n)
        end do

        call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                        nB2,Jday2,Byear2,Bmonth2,Bday2,monthly)


        if (nB2.gt.0) then  ! if all missing, then don't do this species

          year = year1 
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (year.le.year2)
            do while  ((year.le.Byear2(indx2).or.lastindx)
     .                      .and.year.le.year2)
              do month = 1,12
                if (monthly(indx1,month).gt.-1) then
                  man = monthly(indx1,month) + 
     .                  (monthly(indx2,month)-monthly(indx1,month))/
     .                    real(Byear2(indx2)-Byear2(indx1)) * 
     .                         real(year-Byear2(indx1))
                  man = max(man,0.0)
                  annapp(year,sp) = annapp(year,sp) + man
                end if
              end do
              year = year + 1
            end do
            if (indx2.eq.nB2) then
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

      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end


************************************************************************
** variable fertilizer applications                                   **
**  this subroutine writes continual monthly applications             **
**    that are distributed over the month                             **
************************************************************************
      subroutine Fwrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                  species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real fert(maxTimeBreaks,nspecies,12),fer   ! fertilizer
      double precision annapp(year1:year2,nspecies) ! annual application
      character*4 apptype
      data apptype /'fert'/

      character*20 Ffile(maxTimeBreaks)        ! fertilizer file name
      integer nB, nB2, n

      character*30 keyword

      integer Jday(maxTimeBreaks),Byear(maxTimeBreaks)
      integer Bmonth(maxTimeBreaks),Bday(maxTimeBreaks)
      integer Jday2(maxTimeBreaks),Byear2(maxTimeBreaks)
      integer Bmonth2(maxTimeBreaks),Bday2(maxTimeBreaks)

      logical lastindx

      integer day3

      character*4 uvname(maxspecies)
      data uvname /'FNO3','FNH3','FORN','FPO4','FORP'/

      real monthly(maxTimeBreaks,12)

      save day3
 
********** END DECLARATION *********************************************
***** READ CONTROL FILE AND DATA
      day3 = day3 + 1
      if (day3.ge.4) day3 = 1
      keyword = 'FERTILIZER'
      call readcontrol1key(lscen,lenlscen,keyword,
     O                       nB,Jday,Byear,Bmonth,Bday,Ffile)
       
      call readfert(lseg,lenlseg,clu,nB,Ffile,fert,
     I              species,nspecies)
       
********* loop over species, create temporary monthly variable
      do sp = 1,nspecies

        do year = year1,year2
          annapp(year,sp) = 0.0
        end do

        nB2 = nB  ! copy all data into temp variables
        do n = 1,nB2
          do month = 1,12
            monthly(n,month) = fert(n,sp,month)
          end do
          Jday2(n) = Jday(n)
          Byear2(n) = Byear(n)
          Bmonth2(n) = Bmonth(n)
          Bday2(n) = Bday(n)
        end do

        call rmMissingMonthly(maxTimeBreaks,lseg,clu,year1,year2,
     M                        nB2,Jday2,Byear2,Bmonth2,Bday2,monthly)


        if (nB2.gt.0) then  ! if all missing, then don't do this species

          year = year1 
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (year.le.year2)
            do while((year.le.Byear2(indx2).or.lastindx)
     .                    .and.year.le.year2)
              do month = 1,12
                if (fert(indx1,sp,month).gt.-1) then
                  fer = fert(indx1,sp,month) + 
     .                (fert(indx2,sp,month)-fert(indx1,sp,month))/
     .                real(Byear2(indx2)-Byear2(indx1)) * 
     .                   real(year-Byear2(indx1))
                  fer = max(fer,0.0)
                  annapp(year,sp) = annapp(year,sp) + fer
                end if
              end do
              year = year + 1
            end do
            if (indx2.eq.nB2) then
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

      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end

************************************************************************
** variable total uptake for crops                                    **
**  this subroutine writes annual changes to the total uptake         **
************************************************************************
      subroutine Twrite(lscen,lenlscen,lseg,lenlseg,clu,year1,year2,
     I                  species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer year,year1,year2,indx1,indx2
      integer month

      real nupt(maxTimeBreaks,2),nup  ! annual uptake 1=N, 2=P
      double precision annapp(year1:year2,nspecies) ! annual application
      character*6 apptype
      data apptype /'uptake'/

      character*20 Tfile(maxTimeBreaks)        ! file name
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

      if (nB.gt.0) then
        year = year1 
        indx1 = 1
        indx2 = 2
        lastindx = .false.
        do while (year.le.year2)
          do while((year.le.Byear(indx2).or.lastindx).and.year.le.year2)
            do n = 1,2    ! N and P
              sp = 1              ! N is default
              if (n.eq.2) sp = 4  ! P
              nup = nupt(indx1,n) + (nupt(indx2,n)-nupt(indx1,n))/
     .                  real(Byear(indx2)-Byear(indx1))*
     .                      real(year-Byear(indx1))
              nup = max(nup,0.0)
              annapp(year,sp) = annapp(year,sp) + nup
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

      return

******** ERROR SPACE ***************************************************

999   call stopreport(report)

      end


