************************************************************************
**                                                                    **
**  TODO: Describe what does this program do?                         **
**  TODO: Briefly describe the algorithm                              **
**                                        	                      **
************************************************************************
      include 'qstd.inc'

*********** time variables
!      integer minyear,maxyear
!      parameter (minyear=1984,maxyear=2014)

      ! NLDAS precipitation data in inches
      real hprc_nldas(ndaymax*24),dprc_nldas(ndaymax)
      real mprc_nldas(minyear:maxyear,12)
      real aprc_nldas(minyear:maxyear)

      ! XYZ precipitation data in inches
      real hprc_xyz(ndaymax*24),dprc_xyz(ndaymax)
      real mprc_xyz(minyear:maxyear,12)
      real aprc_xyz(minyear:maxyear)

      real dlywetno3(ndaymax),dlywetnh4(ndaymax) ! load in lbs
      real dlydryno3(ndaymax),dlydrynh4(ndaymax)
      real dlywetorn(ndaymax),dlywetorp(ndaymax),dlywetpo4(ndaymax)

      real dlywetno3_xyz(ndaymax),dlywetnh4_xyz(ndaymax) ! load in lbs
      real dlydryno3_xyz(ndaymax),dlydrynh4_xyz(ndaymax)
      real dlywetorn_xyz(ndaymax),dlywetorp_xyz(ndaymax),
     .     dlywetpo4_xyz(ndaymax)

      integer nyears,nmonths,ndays,nhours
      integer ndays_xyz
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

      real monwetno3_xyz(minyear:maxyear,12)
      real monwetnh4_xyz(minyear:maxyear,12)
      real mondryno3_xyz(minyear:maxyear,12)
      real mondrynh4_xyz(minyear:maxyear,12)
      real monwetorn_xyz(minyear:maxyear,12)
      real monwetorp_xyz(minyear:maxyear,12)
      real monwetpo4_xyz(minyear:maxyear,12)

*********** monthly averages
      real monprecipav(12)
      real mondryno3av(12),mondrynh4av(12)
      real monwetno3av(12),monwetnh4av(12)
      real monwetornav(12),monwetorpav(12),monwetpo4av(12)

      integer ndaysinmonth
      external ndaysinmonth

      integer sdate_xyz(ndate),edate_xyz(ndate)

      real delta

      integer WriteDaily, WriteMonthly, WriteAnnual

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,sdate(1),edate(1),WriteDaily,WriteMonthly,WriteAnnual
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

      sdate_xyz(1) = 1985
      sdate_xyz(2) = 1
      sdate_xyz(3) = 1
      sdate_xyz(4) = 0
      sdate_xyz(5) = 0
      sdate_xyz(6) = 0

      edate_xyz(1) = 2000
      edate_xyz(2) = 12
      edate_xyz(3) = 31
      edate_xyz(4) = 24 
      edate_xyz(5) = 0
      edate_xyz(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))
      ndays_xyz = julian(sdate_xyz(1),sdate_xyz(2),sdate_xyz(3),
     .               edate_xyz(1),edate_xyz(2),edate_xyz(3))


************* Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 991

************* open wdm and read rainfall and loads
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991

      dsn = 2000
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hprc_nldas)
      if (ndays*24.ne.nvals) go to 994

      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993


******* initialize monthly + annual nldas precipitation
      do ny = minyear,maxyear
        aprc_nldas(ny) = 0.0
        annwetno3(ny) = 0.0
        annwetnh4(ny) = 0.0
        anndryno3(ny) = 0.0
        anndrynh4(ny) = 0.0
        annwetorn(ny) = 0.0
        annwetpo4(ny) = 0.0
        annwetorp(ny) = 0.0

        do nm = 1,12
          mprc_nldas(ny,nm) = 0.0
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
      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
      nhours = 0
      do nd = 1,ndays
        ! daily
        dprc_nldas(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dprc_nldas(nd) = dprc_nldas(nd) + hprc_nldas(nhours)
        end do
        ! monthly + annual
        mprc_nldas(iy,im)=mprc_nldas(iy,im)+dprc_nldas(nd)
        aprc_nldas(iy)=aprc_nldas(iy)+dprc_nldas(nd)

        call tomorrow(iy,im,id)
      end do

C?      print*,(aprc_nldas(ny),ny=sdate(1),edate(1))


      idays = 1
      do ny = sdate(1),edate(1)

        write(cyear,'(i4)') ny
        wdmfnam = tree//'input/scenario/climate/prad/p532_'//
     .              '20cair'//'/prad_A'//seg(2:6)//'.wdm'
C?        print*,wdmfnam

        call wdbopnlong(wdmfil,wdmfnam,0,err)
C        call wdbopn(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

        dsn = 2000
        call gethourdsn(wdmfil,sdate_xyz,edate_xyz,dsn,nvals,hprc_xyz)
        if (ndays_xyz*24.ne.nvals) go to 994
        dsn = 2001
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlywetno3_xyz)
        dsn = 2002
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlywetnh4_xyz)
        dsn = 2003
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlydryno3_xyz)
        dsn = 2004
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlydrynh4_xyz)
        dsn = 2005
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlywetorn_xyz)
        dsn = 2006
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlywetpo4_xyz)
        dsn = 2007
        call getdailydsn(wdmfil,sdate_xyz,edate_xyz,dsn,ndays_xyz,
     .          dlywetorp_xyz)

        call wdflcl(wdmfil,err)
        if (err.ne.0) go to 993


        do iy = minyear,maxyear
          aprc_xyz(iy) = 0.0
          do im = 1,12
            mprc_xyz(iy,im)    = 0.0
            monwetno3_xyz(iy,im) = 0.0
            monwetnh4_xyz(iy,im) = 0.0
            mondryno3_xyz(iy,im) = 0.0
            mondrynh4_xyz(iy,im) = 0.0
            monwetorn_xyz(iy,im) = 0.0
            monwetpo4_xyz(iy,im) = 0.0
            monwetorp_xyz(iy,im) = 0.0
          end do
        end do

        iy = sdate_xyz(1)
        im = sdate_xyz(2)
        id = sdate_xyz(3)
        nhours = 0
        do nd = 1,ndays_xyz
          ! daily
          dprc_xyz(nd) = 0.0
          do nh = 1,24
            nhours = nhours + 1
            dprc_xyz(nd) = dprc_xyz(nd) + hprc_xyz(nhours)
          end do
          ! monthly
          mprc_xyz(iy,im)     = mprc_xyz(iy,im) + dprc_xyz(nd)
          aprc_xyz(iy)        = aprc_xyz(iy)    + dprc_xyz(nd)
          monwetno3_xyz(iy,im)= monwetno3_xyz(iy,im) + dlywetno3_xyz(nd)
          monwetnh4_xyz(iy,im)= monwetnh4_xyz(iy,im) + dlywetnh4_xyz(nd)
          mondryno3_xyz(iy,im)= mondryno3_xyz(iy,im) + dlydryno3_xyz(nd)
          mondrynh4_xyz(iy,im)= mondrynh4_xyz(iy,im) + dlydrynh4_xyz(nd)
          monwetorn_xyz(iy,im)= monwetorn_xyz(iy,im) + dlywetorn_xyz(nd)
          monwetpo4_xyz(iy,im)= monwetpo4_xyz(iy,im) + dlywetpo4_xyz(nd)
          monwetorp_xyz(iy,im)= monwetorp_xyz(iy,im) + dlywetorp_xyz(nd)

          call tomorrow(iy,im,id)
        end do
C?        print*,sdate_xyz(1),edate_xyz(1)
C?        print*,(aprc_xyz(iy),iy=sdate_xyz(1),edate_xyz(1))

        delta = 999
        do im = 1,12
          do iy = sdate_xyz(1),edate_xyz(1)
           if(delta.gt.(abs((mprc_xyz(iy,im)-mprc_nldas(ny,im)))))then
             delta = abs((mprc_xyz(iy,im)-mprc_nldas(ny,im)))
             iyear = iy
           end if
          end do

C?          print*,ny,',',im,',',iyear,',',monwetno3_xyz(iyear,im),',',
C?     .               mprc_xyz(iyear,im)

          ! ** do calculation for the days in the month = im
          do nd = 1,ndaysinmonth(ny,im)
             dlywetno3(idays)= dprc_nldas(idays) * 
     .                        monwetno3_xyz(iyear,im)/mprc_xyz(iyear,im)
c             dlywetno3(idays)= 1
             dlywetnh4(idays)= dprc_nldas(idays) *
     .                        monwetnh4_xyz(iyear,im)/mprc_xyz(iyear,im)

             dlydryno3(idays)= mondryno3_xyz(iyear,im) /
     .                              ndaysinmonth(iyear,im)
             dlydrynh4(idays)= mondrynh4_xyz(iyear,im) /
     .                              ndaysinmonth(iyear,im)

             dlywetorn(idays)= dprc_nldas(idays) *
     .                        monwetorn_xyz(iyear,im)/mprc_xyz(iyear,im)
             dlywetpo4(idays)= dprc_nldas(idays) *
     .                        monwetpo4_xyz(iyear,im)/mprc_xyz(iyear,im)
             dlywetorp(idays)= dprc_nldas(idays) *
     .                        monwetorp_xyz(iyear,im)/mprc_xyz(iyear,im)

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
C        print*,mprc_xyz

      end do
      idays = idays-1
C      print*,idays

      ! write/insert deposition data to wdm -- number of daily data = idays
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
c      call wdbopn(wdmfil,wdmfnam,0,err)

C      edate(4)=0
C      print*,sdate,',',edate,',',idays,',',(dlywetno3(id),id=1,idays)
      dsn = 2001
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlywetno3)
      dsn = 2002
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlywetnh4)
      dsn = 2003
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlydryno3)
      dsn = 2004
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlydrynh4)
      dsn = 2005
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlywetorn)
      dsn = 2006
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlywetpo4)
      dsn = 2007
      call putdailydsn(wdmfil,sdate,edate,dsn,idays,dlywetorp)


      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993


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
       do nd=1,idays
         write(dfile,1235,err=951)seg,iy,im,id,dprc_nldas(nd),
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
           write(dfile,1236,err=951)seg,ny,nm,mprc_nldas(ny,nm),
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
           write(dfile,1234,err=951)seg,ny,aprc_nldas(ny),
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

