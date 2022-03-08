      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer StartYear,StopYear
      parameter (StartYear=1984,StopYear=2014)

      integer NumDaysMax
      parameter ( NumDaysMax = (StopYear - StartYear + 1) * 366 )

      real hdata(NumDaysMax*24)

      real dPPT(NumDaysMax)
      real mPPT(StartYear:StopYear,12)
      real yPPT(StartYear:StopYear)

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

************* open datafile and new wdm
      read(*,*) lseg, datasource
      call lencl(datasource,lendatasource)
      call lencl(version,lenversion)
      print*,lseg

************* Start Stop Time
      sdate(1) = StartYear
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = StopYear
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      NumDays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))
************* open wdm
      wdmfnam = tree//'input/scenario/climate/prad/'//
     .          datasource(:lendatasource)//'/'//
     .          'prad_'//lseg(:6)//'.wdm'
      print*,wdmfnam
      call wdbopn(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991


      print*,'BHATT 73'
      ! ** PROCESS PRECIP
      dsn = 2000   ! precip (unit: )
      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hdata)

      print*,'BHATT 78'
      do ny = StartYear,StopYear
         do nm = 1,12
            mPPT(ny,nm) = 0.0
         end do
         yPPT(ny) = 0.0
      end do

      iy  = sdate(1)
      im  = sdate(2)
      id  = sdate(3)
      nhours = 0
      do nd = 1,NumDays
        ! daily
        dPPT(nd) = 0.0
        do nh = 1,24
          nhours = nhours + 1
          dPPT(nd) = dPPT(nd) + hdata(nhours)! / 24
        end do
        ! monthly + annual
        mPPT(iy,im) = mPPT(iy,im) + dPPT(nd)! / ndaysinmonth(iy,im)
        yPPT(iy)    = yPPT(iy) + dPPT(nd)! / ndaysinyear(iy)

        call tomorrow(iy,im,id)
      end do
      print*,'BHATT 102'

c      dsn = 1000   ! potential evapotranspiration (unit: )
c          dPET(nd) = dPET(nd) + hdata(nhours)

      fnam = 'daily_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,day,'//
     .                       'precip(in)'
      id = 1
      do ny=sdate(1),edate(1)
         do nm=1,12
           do nd=1,ndaysinmonth(ny,nm)
              write(dfile,2000,err=993)lseg(:6),ny,nm,nd,
     .                      dPPT(id)
              id = id + 1
           end do
        end do
      end do
      close(dfile)


      fnam = 'monthly_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,'//
     .                       'precip(in)'
      do ny=sdate(1),edate(1)
         do nm=1,12
           write(dfile,1000,err=993)lseg(:6),ny,nm,
     .                      mPPT(ny,nm)
        end do
      end do
      close(dfile) 


      fnam = 'annual_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,'//
     .                       'precip(in)'
       do ny=sdate(1),edate(1)
           write(dfile,1000,err=993)lseg(:6),ny,1,
     .                      yPPT(ny)
       end do
       close(dfile)


      fnam = 'hourly_'//lseg(:6)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992

      write(dfile,'(A)',err=993) 'seg,year,month,day,hour,'//
     .                       'precip(in)'
      nhours = 1
      do ny=sdate(1),edate(1)
         do nm=1,12
           do nd=1,ndaysinmonth(ny,nm)
              do nh=1,24
                 write(dfile,3000,err=993)lseg(:6),ny,nm,nd,
     .                     nh,hdata(nhours)
                 nhours = nhours + 1
              end do
           end do
        end do
      end do
      close(dfile)


       return


1000  format(a6,2(',',i4),2(',',f9.4))
2000  format(a6,3(',',i4),2(',',f9.4))
3000  format(a6,1(',',i4),3(',',i2),1(',',E14.8))



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
