      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer SYEAR, EYEAR
      integer MAXSYEAR,MAXEYEAR
      parameter (MAXSYEAR=1980,MAXEYEAR=2035)

      integer NumDaysMax
      parameter ( NumDaysMax = (MAXEYEAR - MAXSYEAR + 1) * 366 )

      real htmp(NumDaysMax*24),hpet(NumDaysMax*24)

      real dTMP(NumDaysMax)
      real mTMP(MAXSYEAR:MAXEYEAR,12)
      real yTMP(MAXSYEAR:MAXEYEAR)

      real dPET(NumDaysMax)
      real mPET(MAXSYEAR:MAXEYEAR,12)
      real yPET(MAXSYEAR:MAXEYEAR)

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer i,sdate(ndate),edate(ndate),tstep

      integer year,month,day,hour
      integer time(NumDaysMax*24,4)  ! year,month,day,hour

      integer ny,nm,nd,nh
      integer iy,im,id
      integer nhours

      integer NumDays
      integer ndaysinmonth,  ndaysinyear, julian
      external ndaysinmonth, ndaysinyear, julian

      double precision dacc

      character*100 datasource,version ! location of data
      integer lendatasource,lenversion

      character*200 dfnam

      integer FH,FD,FM,FY

************* open datafile and new wdm
      read(*,*) lseg, datasource, SYEAR, EYEAR, FH,FD,FM,FY
      call lencl(datasource,lendatasource)
      call lencl(version,lenversion)
      print*,lseg

************* Start Stop Time
      sdate(1) = SYEAR
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = EYEAR
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      NumDays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))
************* open wdm
      wdmfnam = tree//'input/scenario/climate/met/'//
     .          datasource(:lendatasource)//'/'//
     .          'met_'//lseg(:6)//'.wdm'
      call wdbopn(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991


      ! ** PROCESS TEMPERATURE
      dsn = 1004   ! air temperature (unit: )
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,htmp)

      do ny = SYEAR,EYEAR
         do nm = 1,12
            mTMP(ny,nm) = 0.0
         end do
         yTMP(ny) = 0.0
      end do

      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
      nhours = 0
      do nd = 1,NumDays
        ! daily
        dTMP(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          htmp(nhours) = (htmp(nhours)-32.0)*(5.0/9.0)
          dTMP(nd) = dTMP(nd) + htmp(nhours) / 24
        end do
        ! monthly + annual
        mTMP(iy,im) = mTMP(iy,im) + dTMP(nd) / ndaysinmonth(iy,im)
        yTMP(iy)    = yTMP(iy) + dTMP(nd) / ndaysinyear(iy)

        call tomorrow(iy,im,id)
      end do

      ! ** PROCESS POTENTIAL EVAPOTRANSPIRATION
      dsn = 1000   ! potential evapotranspiration (unit: )
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hpet)

      do ny = SYEAR,EYEAR
         do nm = 1,12
            mPET(ny,nm) = 0.0
         end do
         yPET(ny) = 0.0
      end do

      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
      nhours = 0
      do nd = 1,NumDays
        ! daily
        dPET(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dPET(nd) = dPET(nd) + hpet(nhours)
        end do
        ! monthly + annual
        mPET(iy,im) = mPET(iy,im) + dPET(nd) ! / ndaysinmonth(iy,im)
        yPET(iy)    = yPET(iy) + dPET(nd) ! / ndaysinyear(iy)

        call tomorrow(iy,im,id)
      end do




      if ( FD .eq. 1 ) then
      fnam = 'daily_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,day,'//
     .                       'temperature(dC),'//
     .                       'pot.et(in)'
      id = 1
      do ny=sdate(1),edate(1)
         do nm=1,12
           do nd=1,ndaysinmonth(ny,nm)
              write(dfile,2000,err=993)lseg(:6),ny,nm,nd,
     .                      dTMP(id),
     .                      dPET(id)
              id = id + 1
           end do
        end do
      end do
      close(dfile)
      end if


      if ( FM .eq. 1 ) then
      fnam = 'monthly_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,'//
     .                       'temperature(dC),'//
     .                       'pot.et(in)'
      do ny=sdate(1),edate(1)
         do nm=1,12
           write(dfile,1000,err=993)lseg(:6),ny,nm,
     .                      mTMP(ny,nm),
     .                      mPET(ny,nm)
        end do
      end do
      close(dfile) 
      end if


      if ( FY .eq. 1 ) then
      fnam = 'annual_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,'//
     .                       'temperature(dC),'//
     .                       'pot.et(in)'
       do ny=sdate(1),edate(1)
           write(dfile,0500,err=993)lseg(:6),ny,
     .                      yTMP(ny),
     .                      yPET(ny)
       end do
      close(dfile)
      end if


      if ( FH .eq. 1 ) then
      fnam = 'hourly_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,day,hour,'//
     .                       'temperature(dC),'//
     .                       'pot.et(in)'
      nhours = 1
      do ny=sdate(1),edate(1)
         do nm=1,12
           do nd=1,ndaysinmonth(ny,nm)
              do nh=1,24
                 write(dfile,3000,err=993)lseg(:6),ny,nm,nd,nh,
     .                      htmp(nhours),
     .                      hpet(nhours)
                 nhours = nhours + 1
              end do
           end do
        end do
      end do
      close(dfile)
      end if


       return


0500  format(a6,1(',',i4),2(',',f9.4))
1000  format(a6,2(',',i4),2(',',f9.4))
2000  format(a6,3(',',i4),2(',',f9.4))
3000  format(a6,1(',',i4),3(',',i2),2(',',E14.8))


991   report(1) = 'WDM file must previously exist'
      report(2) = 'Missing WDM File Name: '
      report(3) = wdmfnam
      go to 999

992   report(1) = 'Could not open file for writing'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'Error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)

      end
