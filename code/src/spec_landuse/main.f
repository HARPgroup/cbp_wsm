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
      include 'loadsim.inc'

      integer asdate(ndate),aedate(ndate)
      logical dothislu

      character*3 clu
      character*6 keyword

      integer startY,startM,startD
      integer endY,endM,endD
      integer year1,year2,oldyear
      integer year,month,hour
      integer ny,nsp,i,nB

      integer indx1,indx2
      logical lastindx

*************** load related variables
      real MNH3(nyearmax),MNO3(nyearmax),MORGN(nyearmax)
      real MPO4(nyearmax),MORGP(nyearmax)

      real SLONload(nyearmax),SNH3load(nyearmax)
      real SNO3load(nyearmax),SPO4load(nyearmax) 

      real SLONcon(nyearmax),SNH3con(nyearmax)
      real SNO3con(nyearmax),SPO4con(nyearmax)

      real NPratio          ! N to P ratio
      parameter (NPratio = 0.01384)

      real manure(maxTimeBreaks,maxspecies,12),man

*************** WDM related variables
      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)

      integer flowdsn,snh3dsn,sno3dsn,slondsn,spo4dsn
      data    flowdsn,snh3dsn,sno3dsn,slondsn,spo4dsn
     .        /111,144,145,146,153/

      real flowvals(ndaymax*24)       ! flow
      real annflow(nyearmax)

      real slon(ndaymax*24),snh3(ndaymax*24)
      real sno3(ndaymax*24),spo4(ndaymax*24)

      integer bsdate(6)
***************** END DECLARATION ************************************

      read*, lseg,clu,lscen
      call lencl(lscen,lenlscen)
      call lencl(lseg,lenlseg)

      call readcontrol_Lioscen(
     I                         lscen,lenlscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call readcontrol_Ltime(
     I                       lscen,lenlscen,
     O                       startY,startM,startD,endY,endM,endD)

***** GET THE START YEAR
      year1 = startY
      year2 = endY

******* GET START AND END DATE IN WDM FORMAT
      sdate(1) = startY
      sdate(2) = startM
      sdate(3) = startD
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = endY
      edate(2) = endM
      edate(3) = endD
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      do i = 1,6
        asdate(i) = sdate(i)
        bsdate(i) = sdate(i)
      end do

***** initilize variable
      do ny = year1,year2
          annflow(ny) =0.0
        do nsp = 1, nspecies
          annapp(year,sp) = 0.0
        end do
      end do
   
********** find if this is a special land use 
      call readspeclu(clu,ioscen,lenioscen,
     O                dothislu,nspecies,species)
      
********** read in loads
      if (dothislu) then     ! create WDM from loads for the special land use
        keyword = 'MANURE'
        call readcontrolkey(lscen,lenlscen,keyword,
     O                      nB,Jday,Byear,Bmonth,Bday,Mfile)
         
        call readloads(lseg,lenlseg,clu,nB,Mfile,
     I                 Byear,year1,year2,species,nspecies,
     O                 manure)

        call rmMissingloads(maxTimeBreaks,lseg,clu,year1,year2,
     I                      nspecies,
     M                      nB,Jday,Byear,Bmonth,Bday,manure)
        
        if (nB.lt.2) then
        keyword = 'DEFAULT CROPDATA'
        call readcontrolkey(lscen,lenlscen,keyword,
     O                      nB,Jday,Byear,Bmonth,Bday,Mfile)

        call readloads(lseg,lenlseg,clu,nB,Mfile,
     I                 Byear,year1,year2,species,nspecies,
     O                 manure)

        call rmMissingloads(maxTimeBreaks,lseg,clu,year1,year2,
     I                      nspecies,
     M                      nB,Jday,Byear,Bmonth,Bday,manure)
        end if
       
********** interperate to get annual loads for each species
      do sp = 1,nspecies

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
     .                 (manure(indx2,sp,month)-manure(indx1,sp,month))/
     .                  real(Byear(indx2)-Byear(indx1))*
     .                  real(year-Byear(indx1))
                  man = max(man,0.0)
                  annapp(year,sp) = annapp(year,sp) + man
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

********** rearrange variables for each species
        do ny = year1, year2
          do nsp = 1,nspecies
            if (species(nsp) .eq. 'no3n') then
              MNO3(ny) = annapp(ny,nsp)
            else if (species(nsp) .eq. 'nh3n') then
              MNH3(ny) = annapp(ny,nsp)
            else if (species(nsp) .eq. 'orgn') then
              MORGN(ny) = annapp(ny,nsp)
            else if (species(nsp) .eq. 'po4p') then
              MPO4(ny) = annapp(ny,nsp)
            else if (species(nsp) .eq. 'orgp') then
              MORGP(ny) = annapp(ny,nsp)
            else
              go to 993
            end if
          end do
        end do

********** create WDM variable from loads
        do ny = year1, year2
          SLONload(ny) = min(MORGN(ny),MORGP(ny)/NPratio)
          SNH3load(ny) = MNH3(ny)+MORGN(ny)-SLONload(ny)
          SNO3load(ny) = MNO3(ny)
          SPO4load(ny) = MPO4(ny)+MORGP(ny)-SLONload(ny)*NPratio
        end do

********** open land WDM to get flow info
        wdmfnam = outwdmdir//'land/'//clu//'/'//lscen(:lenlscen)//
     .            '/'//clu//lseg(:lenlseg)//'.wdm'
        call wdbopnlong(wdmfil,wdmfnam,1,err)     ! open land read only
        if (err .ne. 0) go to 992

        call gethourdsn(wdmfil,sdate,edate,flowdsn,
     O                  nvals,flowvals)
       
********** get annual flow
        hour = 0
        oldyear = bsdate(1)
        do i = 1,nvals
          hour = hour + 1
          annflow(oldyear) = annflow(oldyear) + flowvals(i)
          if (hour.eq.24) then
            hour = 0
            call tomorrow(bsdate(1),bsdate(2),bsdate(3))
          end if
          
          if (bsdate(1).ne.oldyear) then
            SLONcon(oldyear)= SLONload(oldyear)/annflow(oldyear)
            SNH3con(oldyear)= SNH3load(oldyear)/annflow(oldyear)
            SNO3con(oldyear)= SNO3load(oldyear)/annflow(oldyear)
            SPO4con(oldyear)= SPO4load(oldyear)/annflow(oldyear)
            oldyear = bsdate(1)
          end if
        end do
 
********** get hourly loads
        hour = 0
        oldyear = asdate(1)
        do i = 1,nvals
          hour = hour + 1
          slon(i) = SLONcon(oldyear)*flowvals(i)
          snh3(i) = SNH3con(oldyear)*flowvals(i)
          sno3(i) = SNO3con(oldyear)*flowvals(i)
          spo4(i) = SPO4con(oldyear)*flowvals(i)
          if (hour.eq.24) then
            hour = 0
            call tomorrow(asdate(1),asdate(2),asdate(3))
          end if
        
          if (asdate(1).ne.oldyear) then
            oldyear = asdate(1)
          end if
        end do
       
********** write hourly loads into land WDM
        call puthourdsn(wdmfil,sdate,edate,snh3dsn,nvals,snh3)
        call puthourdsn(wdmfil,sdate,edate,sno3dsn,nvals,sno3)
        call puthourdsn(wdmfil,sdate,edate,slondsn,nvals,slon)
        call puthourdsn(wdmfil,sdate,edate,spo4dsn,nvals,spo4)

      end if   ! end if dothislu

      stop

****************** ERROR SPACE ****************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

993   report(1) = 'could not find species'
      report(2) = species(nsp)
      report(3) = 'check load file '
      go to 999

999   call stopreport(report)

      end


