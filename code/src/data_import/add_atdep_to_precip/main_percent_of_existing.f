************************************************************************
** calculates deposition for the pristine scenario                    **
**   see spreadsheet in the wdm directory                             **
**   The pristine scenario has a somewhat-arbitrary 1 lb/ac/year      **
**      atmospheric deposition on average.	                      **
**                                        	                      **
** The natural sources would be fires and lightning, which would be   **
**   more evenly distributed geographically than the current at dep   **
** A spatially-constant concentration in the atmosphere would lead to **
**   a spatially-constant dry atdep of oxidized                       **
**   a spatially-constant wet concentration of oxidized               **
**   reduced is ignored                                               **
**                                        	                      **
**  other assumptions	                                              **
**    wet:dry is the same as the current ratio                        **
**                                        	                      **
** Dry dep = current dry fraction * 1 lb/ac/year = .65 lb/ac/year     **
** Wet conc = current wet fraction * 1 lb/ac/year / average rainfall  **
**    = 0.035 mg/l                                                    **
**                                        	                      **
************************************************************************
!      include 'qstd.inc'

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)
      integer sdate(ndate),edate(ndate)

      real hourlyrain(ndaymax*24),dailyrain(ndaymax)   ! rain in inches

      real wetno3load(ndaymax),wetnh3load(ndaymax) ! load in lbs
      real wetornload(ndaymax),wetorpload(ndaymax),wetpo4load(ndaymax)

      integer year,month,day,ndays,nd,hour,nh,ns

      integer julian
      external julian

      character*6,seg

*********** time variables
      integer minyear,maxyear,vsize,nyears
      parameter (minyear=1980,maxyear = 2050)

      integer scenyear  ! year of the scenario

********** change factors
      real facwetno3,facdryno3,facwetnh3,facdrynh3,
     .     facwetorn,facwetorp,facwetpo4
     
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

      character*10 date

************* variables for dry deposition
      real dryno3load(ndaymax),drynh3load(ndaymax) ! load in lbs

      integer nmonths,nm,ny
      parameter (nmonths=12)

********* dry dep storage
      real mondryno3(nmonths),mondrynh3(nmonths)
     
      integer ndaysinmonth
      external ndaysinmonth

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,sdate(1),edate(1),facwetno3,facdryno3,
     .                            facwetnh3,facdrynh3,
     .                            facwetpo4,facwetorn,facwetorp
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

*************** get from WDM
      dsn = 2001
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,wetno3load)
      dsn = 2002
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,wetnh3load)
      dsn = 2003
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,dryno3load)
      dsn = 2004
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,drynh3load)
      dsn = 2005
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,wetornload)
      dsn = 2006
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,wetpo4load)
      dsn = 2007
      call getdailydsn(wdmfil,sdate,edate,dsn,ndays,wetorpload)

      do nd = 1,ndays
        wetno3load(nd) = wetno3load(nd) * facwetno3
        wetnh3load(nd) = wetnh3load(nd) * facwetnh3
        dryno3load(nd) = dryno3load(nd) * facdryno3
        drynh3load(nd) = drynh3load(nd) * facdrynh3
        wetornload(nd) = wetornload(nd) * facwetorn
        wetorpload(nd) = wetorpload(nd) * facwetorp
        wetpo4load(nd) = wetpo4load(nd) * facwetpo4
      end do


************** CALCULATE ANNUAL AND MONTHLY VALUES OF WET LOADS
      do year = sdate(1),edate(1)
        annprecip(year) = 0.0
        annwetno3(year) = 0.0
        annwetnh3(year) = 0.0
        annwetorn(year) = 0.0
        annwetorp(year) = 0.0
        annwetpo4(year) = 0.0
        anndryno3(year) = 0.0
        anndrynh3(year) = 0.0
      end do
      do nm = 1,12
        monprecipav(nm) = 0.0
        monwetno3av(nm) = 0.0
        monwetnh3av(nm) = 0.0
        monwetornav(nm) = 0.0
        monwetorpav(nm) = 0.0
        monwetpo4av(nm) = 0.0
        mondryno3av(nm) = 0.0
        mondrynh3av(nm) = 0.0
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
        anndryno3(year) = anndryno3(year) + dryno3load(nd) 
        anndrynh3(year) = anndrynh3(year) + drynh3load(nd) 
        monprecipav(month) = monprecipav(month) + dailyrain(nd)
        monwetno3av(month) = monwetno3av(month) + wetno3load(nd)
        monwetnh3av(month) = monwetnh3av(month) + wetnh3load(nd)
        monwetornav(month) = monwetornav(month) + wetornload(nd)
        monwetorpav(month) = monwetorpav(month) + wetorpload(nd)
        monwetpo4av(month) = monwetpo4av(month) + wetpo4load(nd)
        mondryno3av(month) = mondryno3av(month) + dryno3load(nd)
        mondrynh3av(month) = mondrynh3av(month) + drynh3load(nd)
        call tomorrow(year,month,day)
      end do
                                
      do nm = 1,12
        monprecipav(nm) = monprecipav(nm) / real(edate(1)-sdate(1)+1)
        monwetno3av(nm) = monwetno3av(nm) / real(edate(1)-sdate(1)+1)
        monwetnh3av(nm) = monwetnh3av(nm) / real(edate(1)-sdate(1)+1)
        monwetpo4av(nm) = monwetpo4av(nm) / real(edate(1)-sdate(1)+1)
        monwetornav(nm) = monwetornav(nm) / real(edate(1)-sdate(1)+1)
        monwetorpav(nm) = monwetorpav(nm) / real(edate(1)-sdate(1)+1)
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
      write(*,*)seg,',',0.0,',',0.0

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

972   report(1) = 'problem with file: '
      report(2) = fnam
      write(report(3),*) 'factors for lseg ',seg,' do not add up to 1'
      go to 999

977   report(1) = 'programming problem:  too many cells for seg '//seg
      report(2) = 'in file '//fnam
      report(3) = 'change maxcells in ./pp/.../add_atdep_to_precip/'
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

