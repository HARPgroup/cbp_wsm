************************************************************************
**                                                                    **
**  TODO: Describe what does this program do?                         **
**  TODO: Briefly describe the algorithm                              **
**                                        	                      **
************************************************************************
c      include 'qstd.inc'

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil,maxdsns
      parameter (wdmfil=dfile+10)
      parameter (maxdsns=20)

c      integer i,sdate(ndate),edate(ndate),tstep,ndsns,dsns(maxdsns)
      integer i,ndsns,dsns(maxdsns)

*********** time variables
      integer minyear,maxyear
      parameter (minyear=1984,maxyear=2030)

      ! NLDAS precipitation data in inches
      real hprc_des(ndaymax*24),dprc_des(ndaymax)
      real mprc_des(minyear:maxyear,12)
      real aprc_des(minyear:maxyear)

      ! XYZ precipitation data in inches
      real hprc_src(ndaymax*24),dprc_src(ndaymax)
      real mprc_src(minyear:maxyear,12)
      real aprc_src(minyear:maxyear)

      real dlywetno3(ndaymax),dlywetnh4(ndaymax) ! load in lbs
      real dlydryno3(ndaymax),dlydrynh4(ndaymax)
      real dlywetorn(ndaymax),dlywetorp(ndaymax),dlywetpo4(ndaymax)

      real dlywetno3_src(ndaymax),dlywetnh4_src(ndaymax) ! load in lbs
      real dlydryno3_src(ndaymax),dlydrynh4_src(ndaymax)
      real dlywetorn_src(ndaymax),dlywetorp_src(ndaymax),
     .     dlywetpo4_src(ndaymax)

      integer nyears,nmonths,nhours
      integer ndays_des
      integer ndays_src
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

      real monwetno3_src(minyear:maxyear,12)
      real monwetnh4_src(minyear:maxyear,12)
      real mondryno3_src(minyear:maxyear,12)
      real mondrynh4_src(minyear:maxyear,12)
      real monwetorn_src(minyear:maxyear,12)
      real monwetorp_src(minyear:maxyear,12)
      real monwetpo4_src(minyear:maxyear,12)

*********** monthly averages
      real monprecipav(12)
      real mondryno3av(12),mondrynh4av(12)
      real monwetno3av(12),monwetnh4av(12)
      real monwetornav(12),monwetorpav(12),monwetpo4av(12)

      integer ndaysinmonth
      external ndaysinmonth

      integer sdate_src(ndate),edate_src(ndate)
      integer sdate_des(ndate),edate_des(ndate)

      real delta

      integer WriteDaily, WriteMonthly, WriteAnnual

      integer HCODE, DCODE ! Hourly, Daily
      parameter (HCODE=3, DCODE=4)
      integer TSTEP, qualfg, dtovwr
      parameter (TSTEP=1, dtran=0, qualfg=0, dtovwr=1)
      integer retcod
      integer sdate_t(ndate),edate_t(ndate)

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,
     .      sdate_src(1),edate_src(1),
     .      sdate_des(1),edate_des(1),
     .      WriteDaily,WriteMonthly,WriteAnnual
      print*,seg,' ',sdate_des(1),' ',edate_des(1)

C      sdate_des(1) = 1984
      sdate_des(2) = 1
      sdate_des(3) = 1
      sdate_des(4) = 0
      sdate_des(5) = 0
      sdate_des(6) = 0
C      edate_des(1) = 2003
      edate_des(2) = 12
      edate_des(3) = 31
      edate_des(4) = 24
      edate_des(5) = 0
      edate_des(6) = 0

C      sdate_src(1) = 1984
      sdate_src(2) = 1
      sdate_src(3) = 1
      sdate_src(4) = 0
      sdate_src(5) = 0
      sdate_src(6) = 0

C      edate_src(1) = 2005
      edate_src(2) = 12
      edate_src(3) = 31
      edate_src(4) = 24 
      edate_src(5) = 0
      edate_src(6) = 0

      ndays_des = julian(sdate_des(1),sdate_des(2),sdate_des(3),
     .               edate_des(1),edate_des(2),edate_des(3))
      ndays_src = julian(sdate_src(1),sdate_src(2),sdate_src(3),
     .               edate_src(1),edate_src(2),edate_src(3))


************* Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991

************* open wdm and read rainfall and loads
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991

      dsn = 2000
      print*,'reading des(destination) prad file'
      call gethourdsn(wdmfil,sdate_des,edate_des,dsn,nvals,hprc_des)
c      call timdif(sdate_des,edate_des,tcode,1,nvals)
c      call wtdate(wdmfil,1,dsn,2,sdate_t,edate_t,err)
c      print*,sdate_t
c      print*,edate_t
c      call wdtget(
c     I            wdmfile,dsn,TSTEP,sdate_des,nvals,
c     I            dtran, qualfg, HCODE,
c     O            hprc_des, retcod)
      if (ndays_des*24.ne.nvals) go to 994

      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993


******* initialize monthly + annual nldas precipitation
      do ny = minyear,maxyear
        aprc_des(ny) = 0.0
        annwetno3(ny) = 0.0
        annwetnh4(ny) = 0.0
        anndryno3(ny) = 0.0
        anndrynh4(ny) = 0.0
        annwetorn(ny) = 0.0
        annwetpo4(ny) = 0.0
        annwetorp(ny) = 0.0

        do nm = 1,12
          mprc_des(ny,nm) = 0.0
          monwetno3(ny,nm) = 0.0
          monwetnh4(ny,nm) = 0.0
          mondryno3(ny,nm) = 0.0
          mondrynh4(ny,nm) = 0.0
          monwetorn(ny,nm) = 0.0
          monwetpo4(ny,nm) = 0.0
          monwetorp(ny,nm) = 0.0
        end do
      end do

******* hourly to daily/monthly/annual precipitation
      iy  = sdate_des(1)
      im  = sdate_des(2)
      id  = sdate_des(3)
      nhours = 0
      do nd = 1,ndays_des
        ! daily
        dprc_des(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dprc_des(nd) = dprc_des(nd) + hprc_des(nhours)
        end do
        ! monthly + annual
        mprc_des(iy,im)=mprc_des(iy,im)+dprc_des(nd)
        aprc_des(iy)=aprc_des(iy)+dprc_des(nd)

        call tomorrow(iy,im,id)
      end do

C?      print*,(aprc_des(ny),ny=sdate_des(1),edate_des(1))


      idays = 1
      do ny = sdate_des(1),edate_des(1)

        !print*,'... processing ',seg,ny

        write(cyear,'(i4)') ny
        wdmfnam = tree//'input/scenario/climate/prad/P602_'//
     .              cyear//'/prad_'//seg//'.wdm'
C?        print*,wdmfnam

        call wdbopnlong(wdmfil,wdmfnam,0,err)
C        call wdbopn(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

        dsn = 2000
        print*,'reading src(source) prad file'
        call gethourdsn(wdmfil,sdate_src,edate_src,dsn,nvals,hprc_src)
        if (ndays_src*24.ne.nvals) go to 994
        dsn = 2001
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlywetno3_src)
        dsn = 2002
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlywetnh4_src)
        dsn = 2003
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlydryno3_src)
        dsn = 2004
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlydrynh4_src)
        dsn = 2005
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlywetorn_src)
        dsn = 2006
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlywetpo4_src)
        dsn = 2007
        call getdailydsn(wdmfil,sdate_src,edate_src,dsn,ndays_src,
     .          dlywetorp_src)

        call wdflcl(wdmfil,err)
        if (err.ne.0) go to 993


        do iy = minyear,maxyear
          aprc_src(iy) = 0.0
          do im = 1,12
            mprc_src(iy,im)    = 0.0
            monwetno3_src(iy,im) = 0.0
            monwetnh4_src(iy,im) = 0.0
            mondryno3_src(iy,im) = 0.0
            mondrynh4_src(iy,im) = 0.0
            monwetorn_src(iy,im) = 0.0
            monwetpo4_src(iy,im) = 0.0
            monwetorp_src(iy,im) = 0.0
          end do
        end do

        iy = sdate_src(1)
        im = sdate_src(2)
        id = sdate_src(3)
        nhours = 0
        do nd = 1,ndays_src
          ! daily
          dprc_src(nd) = 0.0
          do nh = 1,24
            nhours = nhours + 1
            dprc_src(nd) = dprc_src(nd) + hprc_src(nhours)
          end do
          ! monthly
          mprc_src(iy,im)     = mprc_src(iy,im) + dprc_src(nd)
          aprc_src(iy)        = aprc_src(iy)    + dprc_src(nd)
          monwetno3_src(iy,im)= monwetno3_src(iy,im) + dlywetno3_src(nd)
          monwetnh4_src(iy,im)= monwetnh4_src(iy,im) + dlywetnh4_src(nd)
          mondryno3_src(iy,im)= mondryno3_src(iy,im) + dlydryno3_src(nd)
          mondrynh4_src(iy,im)= mondrynh4_src(iy,im) + dlydrynh4_src(nd)
          monwetorn_src(iy,im)= monwetorn_src(iy,im) + dlywetorn_src(nd)
          monwetpo4_src(iy,im)= monwetpo4_src(iy,im) + dlywetpo4_src(nd)
          monwetorp_src(iy,im)= monwetorp_src(iy,im) + dlywetorp_src(nd)

          call tomorrow(iy,im,id)
        end do
C?        print*,sdate_src(1),edate_src(1)
C?        print*,(aprc_src(iy),iy=sdate_src(1),edate_src(1))

        delta = 999
        do im = 1,12
          do iy = sdate_src(1),edate_src(1)
           if(delta.gt.(abs((mprc_src(iy,im)-mprc_des(ny,im)))))then
             delta = abs((mprc_src(iy,im)-mprc_des(ny,im)))
             iyear = iy
           end if
          end do

C?          print*,ny,',',im,',',iyear,',',monwetno3_src(iyear,im),',',
C?     .               mprc_src(iyear,im)

          ! ** do calculation for the days in the month = im
          do nd = 1,ndaysinmonth(ny,im)
             dlywetno3(idays)= dprc_des(idays) * 
     .                        monwetno3_src(iyear,im)/mprc_src(iyear,im)
c             dlywetno3(idays)= 1
             dlywetnh4(idays)= dprc_des(idays) *
     .                        monwetnh4_src(iyear,im)/mprc_src(iyear,im)

             dlydryno3(idays)= mondryno3_src(iyear,im) /
     .                              ndaysinmonth(iyear,im)
             dlydrynh4(idays)= mondrynh4_src(iyear,im) /
     .                              ndaysinmonth(iyear,im)

             dlywetorn(idays)= dprc_des(idays) *
     .                        monwetorn_src(iyear,im)/mprc_src(iyear,im)
             dlywetpo4(idays)= dprc_des(idays) *
     .                        monwetpo4_src(iyear,im)/mprc_src(iyear,im)
             dlywetorp(idays)= dprc_des(idays) *
     .                        monwetorp_src(iyear,im)/mprc_src(iyear,im)

             monwetno3(ny,im) = monwetno3(ny,im) + dlywetno3(idays)
             monwetnh4(ny,im) = monwetnh4(ny,im) + dlywetnh4(idays)
             mondryno3(ny,im) = mondryno3(ny,im) + dlydryno3(idays)
             mondrynh4(ny,im) = mondrynh4(ny,im) + dlydrynh4(idays)
             monwetorn(ny,im) = monwetorn(ny,im) + dlywetorn(idays)
             monwetpo4(ny,im) = monwetpo4(ny,im) + dlywetpo4(idays)
             monwetorp(ny,im) = monwetorp(ny,im) + dlywetorp(idays)

             annwetno3(ny) = annwetno3(ny) + dlywetno3(idays)
             annwetnh4(ny) = annwetnh4(ny) + dlywetnh4(idays)
             anndryno3(ny) = anndryno3(ny) + dlydryno3(idays)
             anndrynh4(ny) = anndrynh4(ny) + dlydrynh4(idays)
             annwetorn(ny) = annwetorn(ny) + dlywetorn(idays)
             annwetpo4(ny) = annwetpo4(ny) + dlywetpo4(idays)
             annwetorp(ny) = annwetorp(ny) + dlywetorp(idays)

             idays = idays + 1
          end do

        end do
C        print*,dlywetno3
C        print*,mprc_src

      end do
      idays = idays-1
C      print*,idays

      ! write/insert deposition data to wdm -- number of daily data = idays
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
c      call wdbopn(wdmfil,wdmfnam,0,err)

C      edate_des(4)=0
C      print*,sdate,',',edate,',',idays,',',(dlywetno3(id),id=1,idays)
      dsn = 2001
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlywetno3)
      dsn = 2002
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlywetnh4)
      dsn = 2003
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlydryno3)
      dsn = 2004
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlydrynh4)
      dsn = 2005
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlywetorn)
      dsn = 2006
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlywetpo4)
      dsn = 2007
      print*,'... writing ',dsn
      call putdailydsn(wdmfil,sdate_des,edate_des,dsn,idays,dlywetorp)


      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993


      if (WriteDaily.eq.1) then
       fnam = 'daily_'//seg//'.csv'
       open(dfile,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 992

       write(dfile,'(a,a,a)',err=951) 'seg,year,month,day,precip(in),'//
     .                       'wetno3(lb),wetnh3(lb),dryno3(lb),'//
     .                     'drynh3(lb),wetorn(lb),wetorp(lb),wetpo4(lb)'
       iy=sdate_des(1)
       im=sdate_des(2)
       id=sdate_des(3)
       do nd=1,idays
         write(dfile,1235,err=951)seg,iy,im,id,dprc_des(nd),
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
       do ny=sdate_des(1),edate_des(1)
         do nm=1,12
           write(dfile,1236,err=951)seg,ny,nm,mprc_des(ny,nm),
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
       do ny=sdate_des(1),edate_des(1)
           write(dfile,1234,err=951)seg,ny,aprc_des(ny),
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

