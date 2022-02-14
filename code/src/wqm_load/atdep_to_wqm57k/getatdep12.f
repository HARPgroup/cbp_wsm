************************************************************************
** gets wet deposition from the PSU ATDEP study from 2007, which has  **
**  deposition from 1984 through 2005                                 **
**                                                                    **
** Gets the dry deposition from 2002 directly from the                **
**  CMAQ model.  Per Robin Dennis, the time trends for wet and dry    **
**  deposition are very similar, so this program uses the coefficient **
**  on time from the PSU model to forecase and backcast deposition    **
**  The time trends are calculated separately for NO3 and NH3 and     **
**   for each individual lseg using linear regression                 **
**                                                                    **
**  The above applies to dry deposition to the land.  For this        **
**   application, however, the dry deposition is quite a bit lower    **
**  Per Robin Dennis (PC) on 2/11/2008, we should use a dry:wet ratio **
**   of 0.75 over the water on the 36km grid, but use the given       **
**   deposition from the 12km grid, which can resolve the bay         **
**                                                                    **
** orgp and po4 are based on phase4 concentration                     **
**  orgn is based on the 2002 UVa study                               **
**                                                                    **
**  this code is based on                                             **
**  /model/p515/pp/src/calibration_utils/wdm/src/add_atdep_to_precip/ **
**   main_grimm07_CMAQ12_ORG_PO4.f                                    **
**                                                                    **
** outputs are in kg/day for the WQM                                  **
************************************************************************
      subroutine getatdep12(
     I                      fCWNOX,fCWNHX,fLDNOX,fLDNHX,
     I                      fLORGN,fLORGP,fLPO4X,
     I                      CMAQbase,lenCbase,
     I                      CbaseDir2,lenCb2,
     I                      CbaseSuperLong,lenCbLong,
     I                      cell,linkdir,lenlink,
     O                      cellwq)
      implicit none
      include 'inc_atdep.f'

      integer cell   ! Cell ID to calculate atdep for

********* utility variables
      real mih2kg ! mg/l * inch * hectares conversion to kg
           ! 10e4 sqm/ha * 10e3 l/cm / 39.37 in/m / 10e6 mg/kg
      parameter (mih2kg = 10.0 / 39.37)

      integer i,nB
      character*6,dummy

      character*200 dryfnam,wetfnam

********** area
      real cellarea  ! atdep are areal rates, convert to load
                     ! in hectares
      real getcellarea
      external getcellarea

*********** time variables
      integer maxdays
      parameter (maxdays=(year2-year1+1)*366)

      integer month1,day1,month2,day2,ndays
      data month1,day1,month2,day2 /1,1,12,31/
      real sumyears
      real denom

      integer julian
      external julian

      integer vsize,nyears
      parameter (vsize = 100)
      real years(vsize)
      real annconc(vsize)

********** wet variables
      real dailyrain(maxdays)   ! rain in inches
      real wetno3load(maxdays),wetnh3load(maxdays) ! load in lbs
     
*********** annual totals
      real anndryno3(year1:year2),anndrynh3(year1:year2)
      real annwetno3(year1:year2),annwetnh3(year1:year2)
      real annwetorn(year1:year2),annwetorp(year1:year2)
      real annwetpo4(year1:year2)
      real annprecip(year1:year2)

*********** monthly totals
      real mondryno3(year1:year2,12),mondrynh3(year1:year2,12)
      real monwetno3(year1:year2,12),monwetnh3(year1:year2,12)
      real monwetorn(year1:year2,12),monwetorp(year1:year2,12)
      real monwetpo4(year1:year2,12)
      real monprecip(year1:year2,12)

*********** aveann totals
      real totalwetno3,totaldryno3,totalwetnh3,totaldrynh3
      real totalwetorn,totalwetorp,totalwetpo4,totalprecip

*********** monthly averages
      real mondryno3av(12),mondrynh3av(12)
      real monwetno3av(12),monwetnh3av(12)
      real monwetornav(12),monwetorpav(12),monwetpo4av(12)
      real monprecipav(12)

********* variables to read the wet deposition file
      integer icell,hydcell
      real Pnldas,Pusgs  ! precip in inches
      real cWNHX,WNHXnldas,WNHXusgs  ! conc and load of NH3
      real cWNOX,WNOXnldas,WNOXusgs  ! conc and load of NO3

      real DNHX,DNOX

      character*10 date

************* variables for dry deposition
      real dryno3load(maxdays),drynh3load(maxdays) ! load in lbs

      integer CMAQyear1,CMAQyear2,emissionyear
      parameter (CMAQyear1=2002,CMAQyear2=2002,emissionyear=2002)
         ! CMAQyears are the meteorology years for CMAQ
         !  emissionyear is the loading year for CMAQ

*********** variables to process the dry dep input file
      integer ncells,nc,maxcells  ! number of cells in this wqm cell
      parameter (maxcells=10)
      character*6 Acell(maxcells),Tcell  ! atm cells
      real cellfactor(maxcells),Tfac
      real Tdryno3,Tdrynh3

********* dry dep storage
c      real mondryno3(12),mondrynh3(12)
     
********* year transformation 
      real slope
      external slope
      real no3slope, nh3slope
      real no3intercept,nh3intercept

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
      real wetornload(maxdays),wetorpload(maxdays),wetpo4load(maxdays)

*************  END DECLARATIONS  ***************************************

********** get the area of the current cell
      cellarea = getcellarea(cell,linkdir,lenlink)

************** end declarations, get segment and set dates
      ndays = julian(year1,month1,day1,
     .               year2,month2,day2)

************* open wet dep datafile
      wetfnam = '00000.csv'
      write(wetfnam(:5),'(i5)') cell
      do i = 1,5
        if (wetfnam(i:i).eq.' ') wetfnam(i:i) = '0'
      end do
c      wetfnam = tree//'input/unformatted/atdep/grimm/model/'
c     .       //'areastats/separate_wqm_cell_files/'//wetfnam
      wetfnam = tree//'input/unformatted/atdep/WQMDATA/'
     .       //'AD20160919/WQMCELL_V3/WQMCELLID_'//wetfnam
      open(dfile,file=wetfnam,status='old',iostat=err)
      if (err.ne.0) go to 992

******************** get the wetfall data
      read(dfile,*)dummy ! header line
      do 
        read(dfile,*,end=111,err=998) icell,hydcell,date,
     .                                  Pnldas,
     .                                  cWNHX,WNHXnldas,
     .                                  cWNOX,WNOXnldas
        if (icell.ne.cell ) go to 993
        read(date(1:4),'(i4)') year
        read(date(5:6),'(i2)') month
        read(date(7:8),'(i2)') day
        nd = julian(year1,month1,day1,
     .                year,month,day)
        if (nd.ge.1.and.nd.le.ndays) then
          dailyrain(nd) = Pnldas  ! use NARR dataset as more complete
          wetnh3load(nd) = WNHXnldas * cellarea * fCWNHX(year) ! in kg/day
          wetno3load(nd) = WNOXnldas * cellarea * fCWNOX(year) ! in kg/day
        end if
      end do
111   close (dfile)

************* open dry dep datafile
      dryfnam = '00000.csv'
      write(dryfnam(:5),'(i5)') cell
      do i = 1,5
        if (dryfnam(i:i).eq.' ') dryfnam(i:i) = '0'
      end do
      dryfnam = tree//'input/unformatted/atdep/WQMDATA/'
     .       //'AD20160919/DRYDEP_20170731/WQMCELLID_'//dryfnam
      open(dfile,file=dryfnam,status='old',iostat=err)
      if (err.ne.0) go to 971

******************** get the drydep data
      read(dfile,*)dummy ! header line
      do
        read(dfile,*,end=222,err=998) icell,hydcell,date,
     .                                  DNHX,DNOX
        if (icell.ne.cell ) go to 993
        read(date(1:4),'(i4)') year
        read(date(5:6),'(i2)') month
        read(date(7:8),'(i2)') day
        nd = julian(year1,month1,day1,
     .                year,month,day)
        if (nd.ge.1.and.nd.le.ndays) then
c          dailyrain(nd) = Pnldas  ! use NARR dataset as more complete
          drynh3load(nd) = DNHX * cellarea * fLDNHX(year) ! in kg/day
          dryno3load(nd) = DNOX * cellarea * fLDNOX(year) ! in kg/day
        end if
      end do
222   close (dfile)

*********** ORGANIC AND PO4 LOAD
*************** multiply rain * conc
      year = year1
      month = month1
      day = day1
      do nd = 1,ndays
        if (month.ge.4.and.month.le.6) then
          wetornload(nd) = dailyrain(nd) * wetornspr * mih2kg * cellarea
     .                        * fLORGN(year)
          wetorpload(nd) = dailyrain(nd) * wetorpspr * mih2kg * cellarea
     .                        * fLORGP(year)
          wetpo4load(nd) = dailyrain(nd) * wetpo4spr * mih2kg * cellarea
     .                        * fLPO4X(year)
        else
          wetornload(nd) = dailyrain(nd) * wetornroy * mih2kg * cellarea
     .                        * fLORGN(year)
          wetorpload(nd) = dailyrain(nd) * wetorproy * mih2kg * cellarea
     .                        * fLORGP(year)
          wetpo4load(nd) = dailyrain(nd) * wetpo4roy * mih2kg * cellarea
     .                        * fLPO4X(year)
        end if
        call tomorrow(year,month,day)
      end do


************** CALCULATE ANNUAL AND MONTHLY VALUES OF WET LOADS
      do year = year1,year2
        annprecip(year) = 0.0
        annwetno3(year) = 0.0
        annwetnh3(year) = 0.0
        annwetorn(year) = 0.0
        annwetorp(year) = 0.0
        annwetpo4(year) = 0.0
        do nm = 1,12
          monprecip(year,nm) = 0.0
          monwetno3(year,nm) = 0.0
          monwetnh3(year,nm) = 0.0
          monwetorn(year,nm) = 0.0
          monwetorp(year,nm) = 0.0
          monwetpo4(year,nm) = 0.0
        end do
      end do
      do nm = 1,12
        monprecipav(nm) = 0.0
        monwetno3av(nm) = 0.0
        monwetnh3av(nm) = 0.0
        monwetornav(nm) = 0.0
        monwetorpav(nm) = 0.0
        monwetpo4av(nm) = 0.0
      end do
      year = year1
      month = month1
      day = day1
      do nd = 1,ndays
        annprecip(year) = annprecip(year) + dailyrain(nd) 
        annwetno3(year) = annwetno3(year) + wetno3load(nd) 
        annwetnh3(year) = annwetnh3(year) + wetnh3load(nd)
        annwetorn(year) = annwetorn(year) + wetornload(nd)
        annwetorp(year) = annwetorp(year) + wetorpload(nd)
        annwetpo4(year) = annwetpo4(year) + wetpo4load(nd)
        monprecip(year,month) = monprecip(year,month) + dailyrain(nd)
        monwetno3(year,month) = monwetno3(year,month) + wetno3load(nd)
        monwetnh3(year,month) = monwetnh3(year,month) + wetnh3load(nd)
        monwetorn(year,month) = monwetorn(year,month) + wetornload(nd)
        monwetorp(year,month) = monwetorp(year,month) + wetorpload(nd)
        monwetpo4(year,month) = monwetpo4(year,month) + wetpo4load(nd)
        monprecipav(month) = monprecipav(month) + dailyrain(nd)
        monwetno3av(month) = monwetno3av(month) + wetno3load(nd)
        monwetnh3av(month) = monwetnh3av(month) + wetnh3load(nd)
        monwetornav(month) = monwetornav(month) + wetornload(nd)
        monwetorpav(month) = monwetorpav(month) + wetorpload(nd)
        monwetpo4av(month) = monwetpo4av(month) + wetpo4load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        monprecipav(nm) = monprecipav(nm) / real(year2-year1+1)
        monwetno3av(nm) = monwetno3av(nm) / real(year2-year1+1)
        monwetnh3av(nm) = monwetnh3av(nm) / real(year2-year1+1)
        monwetpo4av(nm) = monwetpo4av(nm) / real(year2-year1+1)
        monwetornav(nm) = monwetornav(nm) / real(year2-year1+1)
        monwetorpav(nm) = monwetorpav(nm) / real(year2-year1+1)
      end do


************** CALCULATE ANNUAL AND MONTHLY VALUES OF DRY LOADS
      do year = year1,year2
        anndryno3(year) = 0.0
        anndrynh3(year) = 0.0
        do nm = 1,12
           mondryno3(year,nm) = 0.0
           mondrynh3(year,nm) = 0.0
        end do
      end do
      do nm = 1,12
        mondryno3av(nm) = 0.0
        mondrynh3av(nm) = 0.0
      end do
      year = year1
      month = month1
      day = day1
      do nd = 1,ndays
        anndryno3(year) = anndryno3(year) + dryno3load(nd) 
        anndrynh3(year) = anndrynh3(year) + drynh3load(nd) 
        mondryno3(year,month) = mondryno3(year,month) + dryno3load(nd)
        mondrynh3(year,month) = mondrynh3(year,month) + drynh3load(nd)
        mondryno3av(month) = mondryno3av(month) + dryno3load(nd)
        mondrynh3av(month) = mondrynh3av(month) + drynh3load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        mondryno3av(nm) = mondryno3av(nm) / real(year2-year1+1)
        mondrynh3av(nm) = mondrynh3av(nm) / real(year2-year1+1)
      end do

************** PRINT ANNUAL AND MONTHLY VALUES OF ALL LOADS
      do year = year1,year2
        write(sumfile,1234)cell,year,annprecip(year),
     .                        annwetno3(year),annwetnh3(year),
     .                        anndryno3(year),anndrynh3(year),
     .                        annwetorn(year),annwetpo4(year),
     .                        annwetorp(year),
     .                        cellarea
        do nm = 1,12
           write(sumfile+3,1236)cell,year,nm,monprecip(year,nm),
     .                monwetno3(year,nm),monwetnh3(year,nm),
     .                mondryno3(year,nm),mondrynh3(year,nm),
     .                monwetorn(year,nm),monwetpo4(year,nm),
     .                monwetorp(year,nm),
     .                cellarea
           !print*,year,month,mondryno3(year,nm),mondrynh3(year,nm)
        end do
      end do
      do nm = 1,12
        write(sumfile+1,1234)cell,nm,monprecipav(nm),
     .                      monwetno3av(nm),monwetnh3av(nm),
     .                      mondryno3av(nm),mondrynh3av(nm),
     .                      monwetornav(nm),monwetpo4av(nm),
     .                      monwetorpav(nm),
     .                      cellarea
      end do
      totalwetno3 = 0.0
      totaldryno3 = 0.0
      totalwetnh3 = 0.0
      totaldrynh3 = 0.0
      totalwetorn = 0.0
      totalwetorp = 0.0
      totalwetpo4 = 0.0
      totalprecip = 0.0
      do year = year1,year2
        totalwetno3 = totalwetno3 + annwetno3(year)
        totaldryno3 = totaldryno3 + anndryno3(year)
        totalwetnh3 = totalwetnh3 + annwetnh3(year)
        totaldrynh3 = totaldrynh3 + anndrynh3(year)
        totalwetorn = totalwetorn + annwetorn(year)
        totalwetorp = totalwetorp + annwetorp(year)
        totalwetpo4 = totalwetpo4 + annwetpo4(year)
        totalprecip = totalprecip + annprecip(year)
      end do
      totalwetno3 = totalwetno3 / real(year2-year1+1)
      totaldryno3 = totaldryno3 / real(year2-year1+1)
      totalwetnh3 = totalwetnh3 / real(year2-year1+1)
      totaldrynh3 = totaldrynh3 / real(year2-year1+1)
      totalwetorn = totalwetorn / real(year2-year1+1)
      totalwetorp = totalwetorp / real(year2-year1+1)
      totalwetpo4 = totalwetpo4 / real(year2-year1+1)
      totalprecip = totalprecip / real(year2-year1+1)
      write(sumfile+2,1235)cell,totalprecip,
     .                          totalwetno3,totalwetnh3,
     .                          totaldryno3,totaldrynh3,
     .                          totalwetorn,totalwetpo4,
     .                          totalwetorp,
     .                          cellarea

*************** store in wq variable
      do nB = 1,nBvar  ! initialize
        do ny = year1,year2
          do i = 1,366
            cellwq(i,ny,nB) = 0.0
          end do
        end do
      end do

      year = year1
      month = month1
      nd = 1
      do year = year1,year2
        do day = 1,ndaysinyear(year)
          cellwq(day,year,kno3) = wetno3load(nd) + dryno3load(nd)
          cellwq(day,year,knh3) = wetnh3load(nd) + drynh3load(nd)
          cellwq(day,year,korn) = wetornload(nd)
          cellwq(day,year,korp) = wetorpload(nd)
          cellwq(day,year,kpo4) = wetpo4load(nd)
          nd = nd + 1
        end do
      end do

      return

1234  format(i6,',',i4,9(',',f9.4))
1235  format(i6,9(',',f9.4))
1236  format(i6,',',i4,',',i2,9(',',f9.4))


********************************* ERROR SPACE **************************
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
      write(report(3),*) 'factors for cell ',cell,' do not add up to 1'
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

977   report(1)='programming problem: too many CMAQ cells for wqm cell '
      write(report(2),*) cell,' in file ',fnam
      report(3) = 'change maxcells in ./pp/.../add_atdep_to_precip/'
      go to 999

978   report(1) = 'problem with file: near line'
      report(2) = fnam
      write(report(3),*) icell,',',Tcell,',',Tfac
      go to 999

992   print*,wetfnam
      report(1) = 'could not open file'
      report(2) = wetfnam
      report(3) = ' '
      go to 999

993   print*,wetfnam
      report(1) = 'problem with file'
      report(2) = wetfnam
      report(3) = 'cell does not match'
      go to 999

998   print*,wetfnam
      print*,icell,hydcell,dummy,date
      report(1) = 'problem with file: near line'
      report(2) = wetfnam
      write(report(3),*) year,', ',month,', ',day
      go to 999

999   call stopreport(report)

      end

