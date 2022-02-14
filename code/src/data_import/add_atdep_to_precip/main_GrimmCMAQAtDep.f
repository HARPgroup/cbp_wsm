************************************************************************
**                                                                    **
**  TODO: Describe what does this program do?                         **
**  TODO: Briefly describe the algorithm                              **
**                                        	                      **
************************************************************************
      include 'qstd.inc'

      !
      integer I_READ_WET, I_READ_DRY, I_CALC_ORG, I_CALC_PO4

      ! precipitation data in inches
      real hprc(ndmax*24),dprc(ndmax)
      real mprc(minyear:maxyear,12)
      real aprc(minyear:maxyear)

      real dlyprecip(ndmax)
      real dlywetno3(ndmax),dlywetnh4(ndmax) ! load in lbs
      real dlydryno3(ndmax),dlydrynh4(ndmax)
      real dlywetorn(ndmax),dlywetorp(ndmax),dlywetpo4(ndmax)
      real tdlywet,tdlydry

      integer nyears,nmonths,ndays,nhours
      integer ny,nm,nd,nh ! use with loops
      integer iy, im, id
      integer iyear,idays !match year
      character*4 cyear

      integer julian
      external julian

      character*6,seg
      character*75 wetfolder
      integer      lenwetfolder
      character*75 dryfolder
      integer      lendryfolder

      character*300 longline
      character*6   tlseg
      integer       tyear,tmonth,tday,thour

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

      real delta

      integer WriteDaily, WriteMonthly, WriteAnnual

      integer spring(2)
      data    spring /4,6/
      real wetpo4spr,wetpo4roy
      real wetorpspr,wetorproy
      real wetornspr,wetornroy
      parameter (wetpo4spr = 0.025, wetpo4roy = 0.0125)
      parameter (wetorpspr = 0.074, wetorproy = 0.037)
      parameter (wetornspr = 0.080, wetornroy = 0.040)
      real mglin2lb  ! mg/l * inch conversion to lb/acre
      parameter (mglin2lb=28.317*43560.0/12.0/453590.0)

************** end declarations, get segment and set dates
C      read(*,'(a)') seg
      read*,seg,wetfolder,dryfolder,sdate(1),edate(1),
     .      I_READ_WET,I_READ_DRY,I_CALC_ORG,I_CALC_PO4,
     .      WriteDaily,WriteMonthly,WriteAnnual
      print*,seg,' ',sdate(1),' ',edate(1)
      call lencl(wetfolder,lenwetfolder)
      call lencl(dryfolder,lendryfolder)
      print*,wetfolder(:lenwetfolder)
      print*,dryfolder(:lendryfolder)

      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

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

      dsn = 2000
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hprc)
      if (ndays*24.ne.nvals) go to 994

      dsn = 2001
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlywetno3)
      if (ndays.ne.nvals) go to 994

      dsn = 2002
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlywetnh4)
      if (ndays.ne.nvals) go to 994

      dsn = 2003
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlydryno3)
      if (ndays.ne.nvals) go to 994

      dsn = 2004
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlydrynh4)
      if (ndays.ne.nvals) go to 994

      dsn = 2005
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlywetorn)
      if (ndays.ne.nvals) go to 994

      dsn = 2006
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlywetpo4)
      if (ndays.ne.nvals) go to 994

      dsn = 2007
      call getdailydsn(wdmfil,sdate,edate,dsn,nvals,dlywetorp)
      if (ndays.ne.nvals) go to 994

      call wdflcl(wdmfil,err)
      if (err.ne.0) go to 993




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


      nhours = 0
      do nd = 1,ndays
        ! daily
        dprc(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dprc(nd) = dprc(nd) + hprc(nhours)
        end do
      end do

******* READ WET DEPOSITION DATA
      if (I_READ_WET .eq. 1) then
       iy  = sdate(1)
       im  = sdate(2)
       id  = sdate(3)
       nd  = 1
       do ny = sdate(1),edate(1)
       !{
        write(cyear,'(I4)') ny 
        fnam = tree//'input/unformatted/atdep/'//
     .         wetfolder(:lenwetfolder)//
     .         '/'//cyear//'/wetdep_daily_'//seg//'_'//cyear//'.csv'

        print*,'reading... '//fnam
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 992

        read (dfile,'(a300)',end=111,err=992) longline ! header line
        do
         read (dfile,'(a300)',end=111,err=992) longline
         call d2x(longline,last)
         read(longline,*,end=994,err=994),tlseg,tyear,tmonth,tday,thour
     .         ,dlyprecip(nd),dlywetno3(nd),dlywetnh4(nd),tdlywet

         if(iy.ne.tyear .or. im.ne.tmonth .or. id.ne.tday) goto 881

         nd = nd + 1
         call tomorrow(iy,im,id)
        end do
111     close(dfile)
       !}  
       end do
      end if

******* READ DRY DEPOSITION DATA
      if (I_READ_DRY .eq. 1) then
       iy  = sdate(1)
       im  = sdate(2)
       id  = sdate(3)
       nd  = 1
       do ny = sdate(1),edate(1)
       !{
        write(cyear,'(I4)') ny
        fnam = tree//'input/unformatted/atdep/'//
     .         dryfolder(:lendryfolder)//
     .         '/'//cyear//'/drydep_daily_'//seg//'_'//cyear//'.csv'

        print*,'reading... '//fnam
        open(dfile+1,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 992

        read (dfile+1,'(a300)',end=112,err=992) longline ! header line
        do
         read (dfile+1,'(a300)',end=112,err=992) longline
         call d2x(longline,last)
         read(longline,*,end=994,err=994),tlseg,tyear,tmonth,tday,thour
     .         ,dlyprecip(nd),dlydryno3(nd),dlydrynh4(nd),tdlydry

         if(iy.ne.tyear .or. im.ne.tmonth .or. id.ne.tday) goto 882

         nd = nd + 1
         call tomorrow(iy,im,id)
        end do
112     close(dfile+1)
       !}  
       end do
      end if

******* CALCULATE ORGANICS DATA
      if (I_CALC_ORG .eq. 1) then
       iy  = sdate(1)
       im  = sdate(2)
       id  = sdate(3)
       nd  = 1
       do ny = sdate(1),edate(1)
       !{
        write(cyear,'(I4)') ny
        do
         if (im.ge.spring(1).and.im.le.spring(2)) then
           dlywetorn(nd) = dprc(nd) * wetornspr * mglin2lb
           dlywetorp(nd) = dprc(nd) * wetorpspr * mglin2lb
         else
           dlywetorn(nd) = dprc(nd) * wetornroy * mglin2lb
           dlywetorp(nd) = dprc(nd) * wetorproy * mglin2lb
         end if
         nd = nd + 1
         call tomorrow(iy,im,id)
        end do
       !}  
       end do
      end if

******* CALCULATE PO4 DATA
      if (I_CALC_PO4 .eq. 1) then
       iy  = sdate(1)
       im  = sdate(2)
       id  = sdate(3)
       nd  = 1
       do ny = sdate(1),edate(1)
       !{
        write(cyear,'(I4)') ny
        do
         if (im.ge.spring(1).and.im.le.spring(2)) then
           dlywetpo4(nd) = dprc(nd) * wetpo4spr * mglin2lb
         else
           dlywetpo4(nd) = dprc(nd) * wetpo4roy * mglin2lb
         end if
         nd = nd + 1
         call tomorrow(iy,im,id)
        end do
       !}  
       end do
      end if

******* hourly to daily/monthly/annual data summary
      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
c      nhours = 0
      do nd = 1,ndays
        ! daily
c        dprc(nd) = 0.0
c        do nh = 1,24
c          nhours = nhours + 1
c          dprc(nd) = dprc(nd) + hprc(nhours)
c        end do
        ! monthly + annual
        mprc(iy,im)     = mprc(iy,im)      + dprc(nd)
        monwetno3(iy,im)= monwetno3(iy,im) + dlywetno3(nd)
        monwetnh4(iy,im)= monwetnh4(iy,im) + dlywetnh4(nd)
        mondryno3(iy,im)= mondryno3(iy,im) + dlydryno3(nd)
        mondrynh4(iy,im)= mondrynh4(iy,im) + dlydrynh4(nd)
        monwetorn(iy,im)= monwetorn(iy,im) + dlywetorn(nd)
        monwetpo4(iy,im)= monwetpo4(iy,im) + dlywetpo4(nd)
        monwetorp(iy,im)= monwetorp(iy,im) + dlywetorp(nd)

        aprc(iy)      = aprc(iy)      + dprc(nd)
        annwetno3(iy) = annwetno3(iy) + dlywetno3(nd)
        annwetnh4(iy) = annwetnh4(iy) + dlywetnh4(nd)
        anndryno3(iy) = anndryno3(iy) + dlydryno3(nd)
        anndrynh4(iy) = anndrynh4(iy) + dlydrynh4(nd)
        annwetorn(iy) = annwetorn(iy) + dlywetorn(nd)
        annwetpo4(iy) = annwetpo4(iy) + dlywetpo4(nd)
        annwetorp(iy) = annwetorp(iy) + dlywetorp(nd)

        call tomorrow(iy,im,id)
      end do




      ! write/insert deposition data to wdm -- number of daily data = idays
      wdmfnam = 'prad_'//seg//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
c      call wdbopn(wdmfil,wdmfnam,0,err)

C      edate(4)=0
C      print*,sdate,',',edate,',',idays,',',(dlywetno3(id),id=1,idays)
      dsn = 2001
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlywetno3)
      dsn = 2002
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlywetnh4)
      dsn = 2003
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlydryno3)
      dsn = 2004
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlydrynh4)
      dsn = 2005
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlywetorn)
      dsn = 2006
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlywetpo4)
      dsn = 2007
      if (debug) print*,'writing... ',dsn
      call putdailydsn(wdmfil,sdate,edate,dsn,ndays,dlywetorp)


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

      print*,seg,' ... Processing Completed Successfully'

      return
     
1234  format(a6,',',i4,8(',',f9.4))
1235  format(a6,3(',',i4),8(',',f9.4))
1236  format(a6,2(',',i4),8(',',f9.4))

********************************* ERROR SPACE **************************
881   report(1) = 'dates does not match in WET deposition file'
      report(2) = fnam
      write(report(3),*) iy,im,id,'vs',tyear,tmonth,tday,'(on file)'
      go to 999

882   report(1) = 'dates does not match in DRY deposition file'
      report(2) = fnam
      write(report(3),*) iy,im,id,'vs',tyear,tmonth,tday,'(on file)'
      go to 999

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

