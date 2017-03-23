************************************************************************
** gets wet deposition from the PSU ATDEP study from 2007, which has  **
**  deposition from 1984 through 2005                                 **
** Gets the dry deposition from 2001 through 2003 directly from the   **
**  CMAQ model.  Per Robin Dennis, the time trends for wet and dry    **
**  deposition are very similar, so this program uses the coefficient **
**  on time from the PSU model to forecase and backcast deposition    **
**  The time trends are calculated separately for NO3 and NH3 and     **
**   for each individual lseg using linear regression                 **
** orgp and po4 are based on phase4 concentration                     **
**  orgn is based on the 2002 UVa study                               **
**                                                                    **
**  this code is based on                                             **
**  /model/p515/pp/src/calibration_utils/wdm/src/add_atdep_to_precip/ **
**   main_grimm07_CMAQ_ORG_PO4.f                                      **
************************************************************************
      subroutine getatdep(
     I                    cell,year1,year2,
     I                    nRvar,Rdsn,Rname,nAvar,Adsn,Aname,Afactor,
     I                    nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,DivRvar,
     O                    cellwq)
      implicit none
      include 'transfer_wdm.inc'

************ I/O variables
      integer year1,year2
      integer cell   ! Cell ID to calculate atdep for
      real cellwq(366,year1:year2,maxBvar)  ! load

      real dailyrain(ndaymax)   ! rain in inches
      real no3conc(ndaymax),nh3conc(ndaymax)  ! concentration in mg/l
      real wetno3load(ndaymax),wetnh3load(ndaymax) ! load in lbs
      real mglin2lb  ! mg/l * inch conversion to lb/acre
      parameter (mglin2lb=28.317*43560.0/12.0/453590.0)

      integer year,month,day,ndays,nd,hour,nh,ns
      integer month1,day1,month2,day2
      integer nR, nR2B, nB, i, j1, j2, Avar, icell

      integer julian
      external julian

      integer ndaysinyear
      external ndaysinyear

      character*6,seg,Tlseg,dummy

      character*200 dryfnam

      integer sdate(ndate),edate(ndate)

      logical found

*********** time variables
      integer vsize,nyears
      parameter (vsize = 100)
      real years(vsize)
      real y(vsize)
     
*********** annual totals
      real anndryno3(year1:year2),anndrynh3(year1:year2)
      real annwetno3(year1:year2),annwetnh3(year1:year2)
      real annwetorn(year1:year2),annwetorp(year1:year2)
      real annwetpo4(year1:year2)
      real annprecip(year1:year2)

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

      integer nmonths,nm,ny
      parameter (nmonths=12)

*********** variables to process the dry dep input file
      integer ncells,nc,maxcells  ! number of cells in this wqm cell
      parameter (maxcells=10)
      character*6 Acell(maxcells),Tcell  ! atm cells
      real cellfactor(maxcells),Tfac
      real Tdryno3,Tdrynh3

********* dry dep storage
      real mondryno3(nmonths),mondrynh3(nmonths)
     
********* year transformation 
      real slope
      external slope
      real no3slope, nh3slope
      real dryno3beta,drynh3beta  ! coefficients on time based on PSU
***     now calculated in the program
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
      sdate(1) = year1
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(1) = year2
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

************* open wet dep datafile
      print*,' Wet atdep for cell ',cell
      fnam = '/model/p5_big_dataset_storage/atdep/grimm/model/areastats'
     .       //'/allwqm.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

******************** get and check the wetfall data
      read(dfile,*,end=997,err=998) dummy  ! ditch header line
      do 
        read(dfile,*,end=111,err=998) iseg  ! check for right seg
        if (iseg.eq.cell ) then
          backspace dfile
          read(dfile,*,end=997,err=998) iseg,Tlseg,dummy,date,
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
            dailyrain(nd) = Pnarr  ! use NARR dataset as more complete
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
      print*,' Dry atdep for cell ',cell
      fnam ='/model/p5_big_dataset_storage/atdep/CMAQ/'//
     .      'cells_to_wqm57_fake.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 970
      ncells = 0
      do 
        read(dfile,*,err=978,end=222) icell,Tcell,Tfac
        if (icell.eq.cell) then
          ncells = ncells + 1
          if (ncells.gt.maxcells) go to 977
          Acell(ncells) = Tcell
          cellfactor(ncells) = Tfac
        end if
      end do
222   close(dfile)
      
      Tfac = 0         ! check that factors add up to 1
      do nc = 1,ncells
        Tfac = Tfac + cellfactor(nc)
      end do
      if (abs(Tfac-1.0) .gt. 0.001) go to 972

************ read dry dep cells for this segment
      do nm = 1,nmonths
        mondryno3(nm) = 0.0
        mondrynh3(nm) = 0.0
      end do
      do nc = 1,ncells
        dryfnam = '/model/p5_big_dataset_storage/atdep/CMAQ/'//
     .          'dry_2006_04_13/agg.cctmJ4f.e01ah.b313.nh3c1.dry.colrow'
     .          //Acell(nc)//'.2001base.csv'
        open(dfile,file=dryfnam,status='old',iostat=err)
        if (err.ne.0) go to 971
        read(dfile,*) dummy
        do ny = 2001,2003
          do nm = 1,12
            read(dfile,*,err=973,end=974) year,month,Tdryno3,Tdrynh3
            if (ny.ne.year.or.nm.ne.month) go to 975
            mondryno3(nm) = mondryno3(nm) 
     .                    + Tdryno3*cellfactor(nc)/real(2003-2001+1)
            mondrynh3(nm) = mondrynh3(nm) 
     .                    + Tdrynh3*cellfactor(nc)/real(2003-2001+1)
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
*********  assume that the CMAQ dry dep from 2001-2003 is the 2002 dep
*********  use the slope to estimate dry deposition for all other years

********** find slope in change in lbs wet per year
      nyears = edate(1) - sdate(1) + 1
      do year = 1,nyears
        years(year) = real(sdate(1) + year - 1)
      end do

      do year = 1,nyears
        y(year) = annwetno3(sdate(1)+year-1)/annprecip(sdate(1)+year-1)
      end do
      no3slope = slope(years,y,nyears,vsize,err) 
      no3slope = no3slope /
     .  ((y(2001-sdate(1)+1)+y(2002-sdate(1)+1)+y(2003-sdate(1)+1))/3.0)

      do year = 1,nyears
        y(year) = annwetnh3(sdate(1)+year-1)/annprecip(sdate(1)+year-1)
      end do
      nh3slope = slope(years,y,nyears,vsize,err)
      nh3slope = nh3slope /
     .  ((y(2001-sdate(1)+1)+y(2002-sdate(1)+1)+y(2003-sdate(1)+1))/3.0)

********* convert slope to fraction change per year based on 2001
*******   to 2003 load, which matches with the dry information

************* populate dry variables
********** This algorithm has jumps between months and between years
***** you could smooth this by finding the julian day for the time trend
****** and calculating an interpolated monthly transition s.t. the 
******** monthly averages are preserved.
      year = sdate(1)
      month = sdate(2)
      day = sdate(3)
      do nd = 1,ndays
        dryno3load(nd) = mondryno3(month)/real(ndaysinmonth(year,month))
     .               * kgha2lbac * (1.0 + real(year-2002)*no3slope)
        drynh3load(nd) = mondrynh3(month)/real(ndaysinmonth(year,month))
     .               * kgha2lbac * (1.0 + real(year-2002)*nh3slope)
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
        write(*,1234)cell,year,annprecip(year),
     .                        annwetno3(year),annwetnh3(year),
     .                        anndryno3(year),anndrynh3(year),
     .                        annwetorn(year),annwetorp(year),
     .                        annwetpo4(year)
      end do
      do nm = 1,12
        write(*,1234)cell,nm,monprecipav(nm),
     .                      monwetno3av(nm),monwetnh3av(nm),
     .                      mondryno3av(nm),mondrynh3av(nm),
     .                      monwetornav(nm),monwetorpav(nm),
     .                      monwetpo4av(nm)
      end do
      write(*,*)cell,',',no3slope,',',nh3slope



*************** store in wq variable
      do nB = 1,nBvar  ! initialize
        do ny = year1,year2
          do i = 1,366
            cellwq(i,ny,nB) = 0.0
          end do
        end do
      end do

      do nB = 1,nBvar   ! loop over Bvars
        do nR2B = 1,nRv2Bv(nB)  ! loop over Rvars in this Bvar
          do nR = 1,nRvar
            if (Rname(nR).eq.Rname2Bv(nB,nR2B)) then ! match
              do Avar = 1,nAvar(nR)  ! loop over Avars
                found = .false.
                if (Aname(nR,Avar).eq.'NO23') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + wetno3load(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'NO3D') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + dryno3load(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'NH4A') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + wetnh3load(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'NH4D') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + drynh3load(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'ORGN') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + wetornload(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'ORGP') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + wetorpload(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (Aname(nR,Avar).eq.'PO4A') then
                  found = .true.
                  nd = 0
                  do ny = year1,year2
                    j1 = 1
                    j2 = ndaysinyear(ny)
                    if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1)
                    if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
                    do i = j1,j2
                      nd = nd + 1
                      cellwq(i,ny,nB) = cellwq(i,ny,nB)
     .                                + wetpo4load(nd) 
     .                                * Rfactor(nb,nR2B) 
     .                                * Afactor(nR,Avar)
                    end do
                  end do
                end if
                if (.not.found) go to 969
              end do
            end if  
          end do
        end do
      end do


      return

1234  format(a6,',',i4,8(',',f9.4))


********************************* ERROR SPACE **************************
969   report(1) = 'could not find '//Aname(nR,Avar)
      report(2) = 'in code'
      report(3) = 'recoding necessary'
      go to 999

970   report(1) = 'could not open file'
      report(2) = fnam
      report(3) = ' '
      go to 999

971   report(1) = 'could not open dry deposition file'
      report(2) = dryfnam
      report(3) = ' '
      print*,dryfnam
      go to 999

972   report(1) = 'problem with file: '
      report(2) = fnam
      write(report(3),*) 'factors for lseg ',seg,' do not add up to 1'
      go to 999

973   report(1) = 'problem with file: near line'
      report(2) = dryfnam
      write(report(3),*) year,',',month,',',Tdryno3,',',Tdrynh3
      go to 999

974   report(1) = 'problem with file:'
      report(2) = dryfnam
      report(3) = 'expecting 12 months for 3 years'
      go to 999

975   report(1) = 'problem with file:'
      report(2) = dryfnam
      report(3) = 'are months and years out of order?'
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

