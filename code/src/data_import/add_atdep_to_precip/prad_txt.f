************************************************************************
**                                                                    **
**  TODO: Describe what does this program do?                         **
**  TODO: Briefly describe the algorithm                              **
**                                        	                      **
************************************************************************
!      include 'qstd.inc'

      implicit none 

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)

      integer minyear,maxyear
      parameter (minyear=1980,maxyear=2035)

      integer i,sdate(ndate),edate(ndate),tstep,ndsns,dsns(maxdsns)

*********** time variables
!      integer minyear,maxyear
!      parameter (minyear=1984,maxyear=2014)

      ! NLDAS precipitation data in inches
      real hprc(ndaymax*24),dprc(ndaymax)
      real mprc(minyear:maxyear,12)
      real aprc(minyear:maxyear)

      real dlywetno3(ndaymax),dlywetnh4(ndaymax) ! load in lbs
      real dlydryno3(ndaymax),dlydrynh4(ndaymax)
      real dlywetorn(ndaymax),dlywetorp(ndaymax),dlywetpo4(ndaymax)

      integer nyears,nmonths,ndays,nhours
      integer ny,nm,nd,nh ! use with loops
      integer iy, im, id
      integer iyear,idays !match year
      character*4 cyear

      integer julian
      external julian

      character*6,seg

*********** annual totals
      real annwetno3(minyear:maxyear),annwetnh4(minyear:maxyear)
      real anndryno3(minyear:maxyear),anndrynh4(minyear:maxyear)
      real annwetorn(minyear:maxyear),annwetorp(minyear:maxyear)
      real annwetpo4(minyear:maxyear)

*********** monthly totals
      real monwetno3(minyear:maxyear,12),monwetnh4(minyear:maxyear,12)
      real mondryno3(minyear:maxyear,12),mondrynh4(minyear:maxyear,12)
      real monwetorn(minyear:maxyear,12),monwetorp(minyear:maxyear,12)
      real monwetpo4(minyear:maxyear,12)

*********** monthly averages
      real monprecipav(12)
      real mondryno3av(12),mondrynh4av(12)
      real monwetno3av(12),monwetnh4av(12)
      real monwetornav(12),monwetorpav(12),monwetpo4av(12)

      integer ndaysinmonth
      external ndaysinmonth

      integer WriteDaily, WriteMonthly, WriteAnnual

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,sdate(1),edate(1),
     .     WriteDaily,WriteMonthly,WriteAnnual
      print*,seg,' ',sdate(1),' ',edate(1)

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

************* Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991

************* open wdm and read rainfall and loads
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991
c      print*,'debug: wdm now open'


******* initialize monthly + annual precipitation
      do ny = minyear,maxyear
        aprc(ny) = 0.0
        annwetno3(ny) = 0.0
        annwetnh4(ny) = 0.0
        anndryno3(ny) = 0.0
        anndrynh4(ny) = 0.0
        annwetorn(ny) = 0.0
        annwetpo4(ny) = 0.0
        annwetorp(ny) = 0.0

        do nm = 1,12
          mprc(ny,nm) = 0.0
          monwetno3(ny,nm) = 0.0
          monwetnh4(ny,nm) = 0.0
          mondryno3(ny,nm) = 0.0
          mondrynh4(ny,nm) = 0.0
          monwetorn(ny,nm) = 0.0
          monwetpo4(ny,nm) = 0.0
          monwetorp(ny,nm) = 0.0
        end do
      end do


        dsn = 2000
        call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hprc)
        if (ndays*24.ne.nvals) go to 994
c        print*,'debug: ',dsn,nvals
        dsn = 2001
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlywetno3)
c        print*,'debug: ',dsn,ndays
        dsn = 2002
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlywetnh4)
c        print*,'debug: ',dsn,ndays
        dsn = 2003
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlydryno3)
c        print*,'debug: ',dsn,ndays
        dsn = 2004
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlydrynh4)
c        print*,'debug: ',dsn,ndays
        dsn = 2005
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlywetorn)
c        print*,'debug: ',dsn,ndays
        dsn = 2006
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlywetpo4)
c        print*,'debug: ',dsn,ndays
        dsn = 2007
        call getdailydsn(wdmfil,sdate,edate,dsn,ndays,
     .          dlywetorp)
c        print*,'debug: ',dsn,ndays

        call wdflcl(wdmfil,err)
        if (err.ne.0) go to 993


        iy = sdate(1)
        im = sdate(2)
        id = sdate(3)
        nhours = 0
        do nd = 1,ndays
          ! daily
          dprc(nd) = 0.0
          do nh = 1,24
            nhours = nhours + 1
            dprc(nd) = dprc(nd) + hprc(nhours)
          end do
          ! monthly
          aprc(iy)        = aprc(iy)    + dprc(nd)
          annwetno3(iy) = annwetno3(iy) + dlywetno3(nd)
          annwetnh4(iy) = annwetnh4(iy) + dlywetnh4(nd)
          anndryno3(iy) = anndryno3(iy) + dlydryno3(nd)
          anndrynh4(iy) = anndrynh4(iy) + dlydrynh4(nd)
          annwetorn(iy) = annwetorn(iy) + dlywetorn(nd)
          annwetpo4(iy) = annwetpo4(iy) + dlywetpo4(nd)
          annwetorp(iy) = annwetorp(iy) + dlywetorp(nd)

          mprc(iy,im)     = mprc(iy,im) + dprc(nd)
          monwetno3(iy,im)= monwetno3(iy,im) + dlywetno3(nd)
          monwetnh4(iy,im)= monwetnh4(iy,im) + dlywetnh4(nd)
          mondryno3(iy,im)= mondryno3(iy,im) + dlydryno3(nd)
          mondrynh4(iy,im)= mondrynh4(iy,im) + dlydrynh4(nd)
          monwetorn(iy,im)= monwetorn(iy,im) + dlywetorn(nd)
          monwetpo4(iy,im)= monwetpo4(iy,im) + dlywetpo4(nd)
          monwetorp(iy,im)= monwetorp(iy,im) + dlywetorp(nd)

          call tomorrow(iy,im,id)
        end do



      if (WriteDaily.eq.1) then
       fnam = 'daily_'//seg//'.csv'
       open(dfile,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 992

       write(dfile,'(a,a,a)',err=951) 'seg,year,month,day,precip(in),'//
     .                       'wetno3(lb),wetnh3(lb),dryno3(lb),'//
     .                     'drynh3(lb),wetorn(lb),wetorp(lb),wetpo4(lb)'
       iy=sdate(1)
       im=sdate(2)
       id=sdate(3)
       do nd=1,ndays
         write(dfile,1235,err=951)seg,iy,im,id,dprc(nd),
     .                      dlywetno3(nd),dlywetnh4(nd),
     .                      dlydryno3(nd),dlydrynh4(nd),
     .                      dlywetorn(nd),dlywetorp(nd),
     .                      dlywetpo4(nd)
         call tomorrow(iy,im,id)
       end do
       close(dfile)
      end if

      if (WriteMonthly.eq.1) then
       fnam = 'monthly_'//seg//'.csv'
       open(dfile,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 992

       write(dfile,'(a,a,a)',err=951) 'seg,year,month,precip(in),'//
     .                       'wetno3(lb),wetnh3(lb),dryno3(lb),'//
     .                     'drynh3(lb),wetorn(lb),wetorp(lb),wetpo4(lb)'
       do ny=sdate(1),edate(1)
         do nm=1,12
           write(dfile,1236,err=951)seg,ny,nm,mprc(ny,nm),
     .                      monwetno3(ny,nm),monwetnh4(ny,nm),
     .                      mondryno3(ny,nm),mondrynh4(ny,nm),
     .                      monwetorn(ny,nm),monwetorp(ny,nm),
     .                      monwetpo4(ny,nm)
         end do
       end do
       close(dfile)
      end if

      if (WriteAnnual.eq.1) then
       fnam = 'annual_'//seg//'.csv'
       open(dfile,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 992

       write(dfile,'(a,a,a)',err=951) 'seg,year,precip(in),'//
     .                       'wetno3(lb),wetnh3(lb),dryno3(lb),'//
     .                     'drynh3(lb),wetorn(lb),wetorp(lb),wetpo4(lb)'
       do ny=sdate(1),edate(1)
           write(dfile,1234,err=951)seg,ny,aprc(ny),
     .                      annwetno3(ny),annwetnh4(ny),
     .                      anndryno3(ny),anndrynh4(ny),
     .                      annwetorn(ny),annwetorp(ny),
     .                      annwetpo4(ny)
       end do
       close(dfile)
      end if

      return
     
1234  format(a6,',',i4,8(',',f9.4))
1235  format(a6,3(',',i4),8(',',f9.4))
1236  format(a6,2(',',i4),8(',',f9.4))

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

993   report(1) = 'error closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
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

951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)

      end

