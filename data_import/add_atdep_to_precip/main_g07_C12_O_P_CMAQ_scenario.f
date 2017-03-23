************************************************************************
** gets wet deposition from the PSU ATDEP study from 2007, which has  **
**  deposition from 1984 through 2005                                 **
** Gets the dry deposition from 2002 directly from the                **
**  CMAQ model.  Per Robin Dennis, the time trends for wet and dry    **
**  deposition are very similar, so this program uses the coefficient **
**  on time from the PSU model to forecase and backcast deposition    **
**  The time trends are calculated separately for NO3 and NH3 and     **
**   for each individual lseg using linear regression                 **
** orgp and po4 are based on phase4 concentration                     **
**  orgn is based on the 2002 UVa study                               **
**                                                                    **
**  modified for a scenario based on CMAQ                             **
**   CMAQ scenarios are presented as a percent reduction from the     **
**    emissionyear base case                                          **
**   Similarly to the back-casting of dry deposition, the trend in    **
**     wet is used to center the deposition on the base case          **
**     then a reduction is taken using the CMAQ scenario file         **
************************************************************************
      include 'qstd.inc'
      real hourlyrain(ndaymax*24),dailyrain(ndaymax)   ! rain in inches
      real no3conc(ndaymax),nh3conc(ndaymax)  ! concentration in mg/l
      real wetno3load(ndaymax),wetnh3load(ndaymax) ! load in lbs
      real mglin2lb  ! mg/l * inch conversion to lb/acre
      parameter (mglin2lb=28.317*43560.0/12.0/453590.0)

      integer year,month,day,ndays,nd,hour,nh,ns
      real denom

      integer julian
      external julian

      character*6,seg,Tlseg,dummy
      character*300 dryfnam

************* variables to read in locations of base and scenario files
      character*120 longnamescen,longnamecal,dir2scen,dir2cal
      integer lenlongname,lendir2

********** CMAQ scenario and base year for CMAQ
      character*20 CMAQscen,CMAQcal
      integer lenCMAQ

*********** time variables
      integer minyear,maxyear,vsize,nyears
      parameter (minyear=1980,maxyear = 2010)
      parameter (vsize = maxyear-minyear+1)
      real years(vsize)
      real annconc(vsize)

      integer nmonths,nm,ny
      parameter (nmonths=12)

*********** annual totals
      real anndryno3(minyear:maxyear),anndrynh3(minyear:maxyear)
      real annwetno3(minyear:maxyear),annwetnh3(minyear:maxyear)
      real annwetorn(minyear:maxyear),annwetorp(minyear:maxyear)
      real annwetpo4(minyear:maxyear)
      real annprecip(minyear:maxyear)

*********** monthly averages
      real mondryno3av(12),mondrynh3av(12)
      real monwetno3av(12),monwetnh3av(12)
      real monwetornav(12),monwetorpav(12),monwetpo4av(12)
      real monprecipav(12)

********* variables to read the wet deposition file
      integer iseg
      real Pnarr,Pusgs  ! precip in inches
      real CNH3,DNH3narr,DNH3usgs  ! conc and load of NH3
      real CNO3,DNO3narr,DNO3usgs  ! conc and load of NO3
      real CDIN,DDINnarr,DDINusgs  ! conc and load of DIN

      character*10 date

************* variables for dry deposition
      real dryno3load(ndaymax),drynh3load(ndaymax) ! load in lbs

      real kgha2lbac  ! kg/ha conversion to lb/ac
      parameter (kgha2lbac = 0.4047*2.2046)

      integer CMAQyear1,CMAQyear2,emissionyear
      parameter (CMAQyear1=2002,CMAQyear2=2002,emissionyear=2002)
         ! CMAQyears are the meteorology years for CMAQ
         !  emissionyear is the loading year for CMAQ

*********** variables to process the dry dep input file
      integer ncells,nc,maxcells  ! number of cells in this lseg
      parameter (maxcells=50)
      character*6 cell(maxcells),Tcell
      real cellfactor(maxcells),Tfac
      real Tdryno3,Tdrynh3

*********** scenario modifiers
      real cqdrynh3(nmonths),cqdryno3(nmonths)
      real cqwetnh3(nmonths),cqwetno3(nmonths)
     
********* dry dep storage
      real mondryno3(nmonths),mondrynh3(nmonths)
     
********* year transformation 
      real slope
      external slope
      real no3slope, nh3slope
      real dryno3beta,drynh3beta  ! coefficients on time based on PSU
C      parameter (dryno3beta = -0.00404099)
C      parameter (drynh3beta =  0.00661725)

      integer ndaysinmonth
      external ndaysinmonth

********** PO4 and ORG concentrations, spring and rest of year (roy)
********** spring in this case is April-June
********* units are mg/l
      real wetpo4spr,wetpo4roy
      real wetorpspr,wetorproy
      real wetornspr,wetornroy
      parameter (wetpo4spr = 0.025, wetpo4roy = 0.0125)
      parameter (wetorpspr = 0.074, wetorproy = 0.037)
      parameter (wetornspr = 0.080, wetornroy = 0.040)
      real wetornload(ndaymax),wetorpload(ndaymax),wetpo4load(ndaymax)

********* error checking for daily concentrations
      integer i1,i2,nd2
      real closerain

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,sdate(1),edate(1),CMAQcal,dir2cal,longnamecal,
     .                            CMAQscen,dir2scen,longnamescen
      print*,seg,' ',sdate(1),' ',edate(1),' wet and dry atdep'

C      sdate(1) = 1984
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
C      edate(1) = 2003
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

************* open wdm and read rainfall, convert to daily
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991
      dsn = 2000
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hourlyrain)
      if (ndays*24.ne.nvals) go to 994
      nh = 0
      do nd = 1,ndays
        dailyrain(nd) = 0.0
        do hour = 1,24
          nh = nh + 1
          dailyrain(nd) = dailyrain(nd) + hourlyrain(nh)
        end do
      end do

************* open datafile
      fnam = tree//'input/unformatted/atdep/grimm/model/areastats'
     .       //'/allfips.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

******************** get and check the wetfall data
      read(dfile,*,end=997,err=998) dummy  ! ditch header line
      do 
        read(dfile,*,end=111,err=998) iseg,Tlseg ! check for right seg
        if (Tlseg.eq.seg) then
          backspace dfile
          read(dfile,*,end=997,err=998) iseg,Tlseg,date,
     .                                  Pnarr,Pusgs,
     .                                  CNH3,DNH3narr,DNH3usgs,
     .                                  CNO3,DNO3narr,DNO3usgs,
     .                                  CDIN,DDINnarr,DDINusgs
          if (date(1:1).eq.'T') exit
          read(date(1:4),'(i4)') year
          read(date(5:6),'(i2)') month
          read(date(7:8),'(i2)') day
          nd = julian(sdate(1),sdate(2),sdate(3),
     .                year,month,day)
          if (nd.ge.1.and.nd.le.ndays) then
            nh3conc(nd) = CNH3
            no3conc(nd) = CNO3
          end if
        end if
      end do
111   close (dfile)

*************** multiply rain * conc
********** if no conc on that day, search +- 1 month for close rain
      do nd = 1,ndays
        if (dailyrain(nd).gt.0.01.and.nh3conc(nd).lt.1e-6) then
          i1 = max(1,nd-30)
          i2 = min(ndays,nd+30)
          closerain = -999.
          do nd2 = i1,i2
            if (dailyrain(nd2).gt.0.01.and.nh3conc(nd2).gt.1e-6) then
              if (abs(dailyrain(nd2)-dailyrain(nd)).lt.
     .            abs(closerain-dailyrain(nd))) then
                closerain = dailyrain(nd2)
                nh3conc(nd) = nh3conc(nd2)
              end if
            end if
          end do
          if (closerain.lt.-1) go to 989
        end if
        wetnh3load(nd) = dailyrain(nd) * nh3conc(nd) * mglin2lb
      end do

      do nd = 1,ndays
        if (dailyrain(nd).gt.0.01.and.no3conc(nd).lt.1e-6) then
          i1 = max(1,nd-30)
          i2 = min(ndays,nd+30)
          closerain = -999.
          do nd2 = i1,i2
            if (dailyrain(nd2).gt.0.01.and.no3conc(nd2).gt.1e-6) then
              if (abs(dailyrain(nd2)-dailyrain(nd)).lt.
     .            abs(closerain-dailyrain(nd))) then
                closerain = dailyrain(nd2)
                no3conc(nd) = no3conc(nd2)
              end if
            end if
          end do
          if (closerain.lt.-1) go to 989
        end if
        wetno3load(nd) = dailyrain(nd) * no3conc(nd) * mglin2lb
      end do

*********** ORGANIC AND PO4 LOAD
*************** multiply rain * conc
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        if (month.ge.4.and.month.le.6) then
          wetornload(nd) = dailyrain(nd) * wetornspr * mglin2lb
          wetorpload(nd) = dailyrain(nd) * wetorpspr * mglin2lb
          wetpo4load(nd) = dailyrain(nd) * wetpo4spr * mglin2lb
        else
          wetornload(nd) = dailyrain(nd) * wetornroy * mglin2lb
          wetorpload(nd) = dailyrain(nd) * wetorproy * mglin2lb
          wetpo4load(nd) = dailyrain(nd) * wetpo4roy * mglin2lb
        end if
        call tomorrow(year,month,day)
      end do


************** CALCULATE ANNUAL AND MONTHLY VALUES OF WET LOADS
      do year = sdate(1),edate(1)
        annprecip(year) = 0.0
        annwetno3(year) = 0.0
        annwetnh3(year) = 0.0
        annwetorn(year) = 0.0
        annwetorp(year) = 0.0
        annwetpo4(year) = 0.0
      end do
      do nm = 1,12
        monprecipav(nm) = 0.0
        monwetno3av(nm) = 0.0
        monwetnh3av(nm) = 0.0
        monwetornav(nm) = 0.0
        monwetorpav(nm) = 0.0
        monwetpo4av(nm) = 0.0
      end do
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        annprecip(year) = annprecip(year) + dailyrain(nd) 
        annwetno3(year) = annwetno3(year) + wetno3load(nd) 
        annwetnh3(year) = annwetnh3(year) + wetnh3load(nd)
        annwetorn(year) = annwetorn(year) + wetornload(nd)
        annwetorp(year) = annwetorp(year) + wetorpload(nd)
        annwetpo4(year) = annwetpo4(year) + wetpo4load(nd)
        monprecipav(month) = monprecipav(month) + dailyrain(nd)
        monwetno3av(month) = monwetno3av(month) + wetno3load(nd)
        monwetnh3av(month) = monwetnh3av(month) + wetnh3load(nd)
        monwetornav(month) = monwetornav(month) + wetornload(nd)
        monwetorpav(month) = monwetorpav(month) + wetorpload(nd)
        monwetpo4av(month) = monwetpo4av(month) + wetpo4load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        monprecipav(nm) = monprecipav(nm) / real(edate(1)-sdate(1)+1)
        monwetno3av(nm) = monwetno3av(nm) / real(edate(1)-sdate(1)+1)
        monwetnh3av(nm) = monwetnh3av(nm) / real(edate(1)-sdate(1)+1)
        monwetpo4av(nm) = monwetpo4av(nm) / real(edate(1)-sdate(1)+1)
        monwetornav(nm) = monwetornav(nm) / real(edate(1)-sdate(1)+1)
        monwetorpav(nm) = monwetorpav(nm) / real(edate(1)-sdate(1)+1)
      end do

**************** DRY DEPOSITION
********* read linkage file
      fnam =tree//'input/unformatted/atdep/CMAQ/'//
     .      'cmaq12_to_lsegs.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 970
      ncells = 0
      do 
        read(dfile,*,err=978,end=222) Tcell,Tlseg,Tfac
        if (Tlseg.eq.seg) then
          ncells = ncells + 1
          if (ncells.gt.maxcells) go to 977
          cell(ncells) = Tcell
          cellfactor(ncells) = Tfac
        end if
      end do
222   close(dfile)
      
      Tfac = 0         ! check that factors add up to 1
      do nc = 1,ncells
        Tfac = Tfac + cellfactor(nc)
      end do
      if (abs(Tfac-1.0) .gt. 0.001) go to 972

************* get reductions from the base case from the CMAQ scenario
      do nm = 1,nmonths
        cqdryno3(nm) = 0.0
        cqdrynh3(nm) = 0.0
        cqwetno3(nm) = 0.0
        cqwetnh3(nm) = 0.0
      end do
      call lencl(CMAQscen,lenCMAQ)
      call lencl(dir2scen,lendir2)
      call lencl(longnamescen,lenlongname)
      do nc = 1,ncells
        dryfnam = tree//'input/unformatted/atdep/CMAQ/'//
     .            CMAQscen(:lenCMAQ)//'/'//
     .            dir2scen(:lendir2)//'/'//
     .            longnamescen(:lenlongname)//cell(nc)//'.csv'
        open(dfile,file=dryfnam,status='old',iostat=err)
        if (err.ne.0) go to 976
        read(dfile,*) dummy
        do nm = 1,12
          read(dfile,*,err=973,end=974) year,month,
     .                                  cqdryno3(month),cqdrynh3(month),
     .                                  cqwetno3(month),cqwetnh3(month)
          if (nm.ne.month) go to 975
        end do
        close(dfile)
      end do

************ read base case dry dep cells for this segment
      do nm = 1,nmonths
        mondryno3(nm) = 0.0
        mondrynh3(nm) = 0.0
      end do
      call lencl(CMAQcal,lenCMAQ)
      call lencl(dir2cal,lendir2)
      call lencl(longnamecal,lenlongname)
      do nc = 1,ncells
        dryfnam = tree//'input/unformatted/atdep/CMAQ/'//
     .            CMAQcal(:lenCMAQ)//'/'//
     .            dir2cal(:lendir2)//'/'//
     .            longnamecal(:lenlongname)//cell(nc)//'.csv'
        open(dfile,file=dryfnam,status='old',iostat=err)
        if (err.ne.0) go to 971
        read(dfile,*) dummy
        denom = real(CMAQyear2-CMAQyear1+1)
        do ny = CMAQyear1,CMAQyear2
          do nm = 1,12
            read(dfile,*,err=973,end=974) year,month,Tdryno3,Tdrynh3
            if (ny.ne.year.or.nm.ne.month) go to 975
            mondryno3(nm) = mondryno3(nm) 
     .                    + Tdryno3*cellfactor(nc)/denom
            mondrynh3(nm) = mondrynh3(nm) 
     .                    + Tdrynh3*cellfactor(nc)/denom
          end do
        end do
        close(dfile)
      end do
            
********* calculate time trend for dry deposition variables
********  the assumption is that dry deposition trend is equal to the
*******   wet deposition trend.  (reasonable per Robin Dennis, pc 2006)
******** The true trend is not an explicit part of the wet deposition
*******  regression model as it was in the 2003 version.  In the 2007
******** version, the inputs change with time and the temporal trend
*******  that is in the model should be interpreted as only the portion
*******  of the trend not explained by other variables.
*******  The method for calculating the trend for each lseg follows:
*******    calculate annual precip-weighted average concentration as:
*********    total dep / total precip            
*********  regress precip-weighted concentration against time
*********  assume that the CMAQ dry dep from CMAQyear1 to CMAQyear2 
********   is the average deposition for the emissionyear scenario.
*********  use the slope to estimate dry deposition for all other years

********** calculate wet deposition change per year
********** by finding slope in fraction per year

******** first populate year variable
      nyears = edate(1) - sdate(1) + 1  ! populate year variable
      do year = 1,nyears
        years(year) = real(sdate(1) + year - 1)
      end do

********** nitrate first
      do year = 1,nyears  ! populate concentration variable
        annconc(year) = annwetno3(sdate(1)+year-1)
     .                / annprecip(sdate(1)+year-1)
      end do

      no3slope = slope(years,annconc,nyears,vsize,err) ! M/V/year

      denom = 0.0  ! divide by average concentration over CMAQ period
      do ny = CMAQyear1,CMAQyear2
        denom = denom + annconc(ny-sdate(1)+1)
      end do
      denom = denom / real(CMAQyear2-CMAQyear1+1)

      no3slope = no3slope / denom   !  fraction change per year

********* repeat for nh3
      do year = 1,nyears  ! populate concentration variable
        annconc(year) = annwetnh3(sdate(1)+year-1)
     .                / annprecip(sdate(1)+year-1)
      end do

      nh3slope = slope(years,annconc,nyears,vsize,err) ! M/V/year

      denom = 0.0  ! divide by average concentration over CMAQ period
      do ny = CMAQyear1,CMAQyear2
        denom = denom + annconc(ny-sdate(1)+1)
      end do
      denom = denom / real(CMAQyear2-CMAQyear1+1)

      nh3slope = nh3slope / denom   !  fraction change per year

**************** DETREND WET LOADS to base dry emission year
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        wetno3load(nd) = wetno3load(nd)
     .                 * (1.0 + real(emissionyear-year)*no3slope)
        wetnh3load(nd) = wetnh3load(nd)
     .                 * (1.0 + real(emissionyear-year)*nh3slope)
        call tomorrow(year,month,day)
      end do

************** APPLY SCENARIO FACTOR
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        wetno3load(nd) = wetno3load(nd)
     .                 * (1.0 - cqwetno3(month)/100.0)
        wetnh3load(nd) = wetnh3load(nd)
     .                 * (1.0 - cqwetnh3(month)/100.0)
        call tomorrow(year,month,day)
      end do

************** RECALCULATE ANNUAL AND MONTHLY VALUES OF WET LOADS
      do year = sdate(1),edate(1)
        annprecip(year) = 0.0
        annwetno3(year) = 0.0
        annwetnh3(year) = 0.0
        annwetorn(year) = 0.0
        annwetorp(year) = 0.0
        annwetpo4(year) = 0.0
      end do
      do nm = 1,12
        monprecipav(nm) = 0.0
        monwetno3av(nm) = 0.0
        monwetnh3av(nm) = 0.0
        monwetornav(nm) = 0.0
        monwetorpav(nm) = 0.0
        monwetpo4av(nm) = 0.0
      end do
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        annprecip(year) = annprecip(year) + dailyrain(nd) 
        annwetno3(year) = annwetno3(year) + wetno3load(nd) 
        annwetnh3(year) = annwetnh3(year) + wetnh3load(nd)
        annwetorn(year) = annwetorn(year) + wetornload(nd)
        annwetorp(year) = annwetorp(year) + wetorpload(nd)
        annwetpo4(year) = annwetpo4(year) + wetpo4load(nd)
        monprecipav(month) = monprecipav(month) + dailyrain(nd)
        monwetno3av(month) = monwetno3av(month) + wetno3load(nd)
        monwetnh3av(month) = monwetnh3av(month) + wetnh3load(nd)
        monwetornav(month) = monwetornav(month) + wetornload(nd)
        monwetorpav(month) = monwetorpav(month) + wetorpload(nd)
        monwetpo4av(month) = monwetpo4av(month) + wetpo4load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        monprecipav(nm) = monprecipav(nm) / real(edate(1)-sdate(1)+1)
        monwetno3av(nm) = monwetno3av(nm) / real(edate(1)-sdate(1)+1)
        monwetnh3av(nm) = monwetnh3av(nm) / real(edate(1)-sdate(1)+1)
        monwetpo4av(nm) = monwetpo4av(nm) / real(edate(1)-sdate(1)+1)
        monwetornav(nm) = monwetornav(nm) / real(edate(1)-sdate(1)+1)
        monwetorpav(nm) = monwetorpav(nm) / real(edate(1)-sdate(1)+1)
      end do

************* populate dry variables
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        dryno3load(nd) = mondryno3(month)/real(ndaysinmonth(year,month))
     .                   * (1.0 - cqdryno3(month)/100.0)
     .                   * kgha2lbac
        drynh3load(nd) = mondrynh3(month)/real(ndaysinmonth(year,month))
     .                   * (1.0 - cqdrynh3(month)/100.0)
     .                   * kgha2lbac
        call tomorrow(year,month,day)
      end do

************** CALCULATE ANNUAL AND MONTHLY VALUES OF DRY LOADS
      do year = sdate(1),edate(1)
        anndryno3(year) = 0.0
        anndrynh3(year) = 0.0
      end do
      do nm = 1,12
        mondryno3av(nm) = 0.0
        mondrynh3av(nm) = 0.0
      end do
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        anndryno3(year) = anndryno3(year) + dryno3load(nd) 
        anndrynh3(year) = anndrynh3(year) + drynh3load(nd) 
        mondryno3av(month) = mondryno3av(month) + dryno3load(nd)
        mondrynh3av(month) = mondrynh3av(month) + drynh3load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        mondryno3av(nm) = mondryno3av(nm) / real(edate(1)-sdate(1)+1)
        mondrynh3av(nm) = mondrynh3av(nm) / real(edate(1)-sdate(1)+1)
      end do

************** PRINT ANNUAL AND MONTHLY VALUES OF ALL LOADS
      print*,'seg,year or month,precip,wetno3,wetnh3,dryno3,drynh3,',
     .       'wetorn,wetorp,wetpo4'
      do year = sdate(1),edate(1)
        write(*,1234)seg,year,annprecip(year),
     .                        annwetno3(year),annwetnh3(year),
     .                        anndryno3(year),anndrynh3(year),
     .                        annwetorn(year),annwetorp(year),
     .                        annwetpo4(year)
      end do
      do nm = 1,12
        write(*,1234)seg,nm,monprecipav(nm),
     .                      monwetno3av(nm),monwetnh3av(nm),
     .                      mondryno3av(nm),mondrynh3av(nm),
     .                      monwetornav(nm),monwetorpav(nm),
     .                      monwetpo4av(nm)
      end do
      write(*,*)seg,',',no3slope,',',nh3slope

*************** store in WDM
      dsn = 2001
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,wetno3load)
      dsn = 2002
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,wetnh3load)
      dsn = 2003
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dryno3load)
      dsn = 2004
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,drynh3load)
      dsn = 2005
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,wetornload)
      dsn = 2006
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,wetpo4load)
      dsn = 2007
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,wetorpload)
      call wdflc1(wdmfil,err)
      if (err.ne.0) go to 995

      return

1234  format(a6,',',i4,8(',',f9.4))


********************************* ERROR SPACE **************************
970   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

971   report(1) = 'could not open dry deposition file '//cell(nc)
      report(2) = dryfnam
      report(3) = ' '
      go to 999

972   report(1) = 'problem with file: '
      report(2) = fnam
      write(report(3),*) 'factors for lseg ',seg,' do not add up to 1'
      go to 999

973   report(1) = 'problem with file: near line '//cell(nc)
      report(2) = dryfnam
      write(report(3),*) year,',',month,',',Tdryno3,',',Tdrynh3
      go to 999

974   report(1) = 'problem with file: '//cell(nc)
      report(2) = dryfnam
      report(3) = 'expecting 12 months for 3 years'
      go to 999

975   report(1) = 'problem with file: '//cell(nc)
      report(2) = dryfnam
      report(3) = 'are months and years out of order?'
      go to 999

976   report(1) = 'could not open cmaq deposition factors file'
      report(2) = dryfnam
      report(3) = cell(nc)
      go to 999

977   report(1) = 'programming problem:  too many cells for seg '//seg
      report(2) = 'in file '//fnam
      report(3) = 'change maxcells in ./pp/.../add_atdep_to_precip/'
      go to 999

978   report(1) = 'problem with file: near line'
      report(2) = fnam
      write(report(3),*) Tcell,',',Tlseg,',',Tfac
      go to 999

989   report(1) = 'no concentration on day with rain'
      write(report(2),*) dailyrain(nd),' ',nh3conc(nd)
      write(report(3),*) 'julian day = ',nd
      go to 999

990   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'incorrect number of lines'
      go to 999

991   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'start date does not match expected date'
      go to 999

994   report(1) = 'problem with wdm'
      report(2) = wdmfnam
      write(report(3),*) 'expecting ',ndays*24,' values, got ',nvals
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

997   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'end of file reached unexpectedly '
      go to 999

998   report(1) = 'problem with file: near line'
      report(2) = fnam
      write(report(3),*) year,', ',month,', ',day
      go to 999

999   call stopreport(report)

      end

