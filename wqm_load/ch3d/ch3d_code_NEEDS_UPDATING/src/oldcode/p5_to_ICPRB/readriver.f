************************************************************************
**  gets the flow output for each river                               **
************************************************************************
      subroutine readriver(rscen,trseg,
     I                     year1,month1,day1,year2,month2,day2,
     I                     nRvarOut,RdsnOut,RnameOut,
     O                     pairq)  ! get the data
      implicit none

      include 'transfer_wdm.inc'

      integer year1,month1,day1,year2,month2,day2
      character*13 trseg
      real pairq(-5:366,year1:year2)
      integer ny              ! year of infomation to get

      real hvaltemp(ndaymax*24,maxRvar)   ! hourly values to river

      integer jday,jdaytot,j1,j2       ! julian day determined from hours
      integer jdcorrect  ! correction between start time of etm
                         ! file and time series from wdm
      real fac1,fac2

      integer i, Rvar, nv, hour  ! indices
      integer wdmlnd,etmfil
       
      parameter (etmfil=dfile+1)
      parameter (wdmlnd=etmfil+1)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      integer ndaysinyear,ndays,nd
      external ndaysinyear

      integer julian
      external julian

************ END DECLARATION ******************************************


********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = year1
      sdate(2) = month1
      sdate(3) = day1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = year2
      edate(2) = month2
      edate(3) = day2
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

********** wind back sdate to -5
      do nd = 0,-5,-1
        call yesterday(sdate(1),sdate(2),sdate(3))
        ndays = ndays + 1
      end do


      do Rvar = 1,maxRvar     ! initialize temp variable
        do nv = 1,ndays*24
          hvaltemp(nv,Rvar) = 0.
        end do
      end do

*********  OPEN AND READ ETM FILE
      call lencl(rscen,lenrscen)
      call lencl(trseg,lenrseg)
         
************ READY TO OPEN WDM FILE
      wdmfnam = tree//'wdm/river/'//
     .          rscen(:lenrscen)//'/stream/'//
     .          trseg(:lenrseg)//'.wdm'
      call wdbopn(wdmlnd+nlu+1,wdmfnam,1,err)     ! open land read only
      if (err .ne. 0) go to 998

      do Rvar = 1,nRvarOut  ! loop over river variables

        if (RnameOut(Rvar).ne.'WATR') cycle
        call gethourdsn(wdmlnd+nlu+1,sdate,edate,
     I                  RdsnOut(Rvar),
     O                  nvals,hval)                   

        jdaytot = nvals/24
        if (jdaytot.ne.ndays) go to 992
        do nv = 1,jdaytot*24
          hvaltemp(nv,Rvar)=hval(nv)+hvaltemp(nv,Rvar) 
        end do    

      end do            ! end loop over land uses in segment

      call wdflcl (wdmlnd+nlu+1,err)
      if (err.ne.0) call wdflc1(wdmlnd+nlu+1,err)
      if (err.ne.0) go to 996

      do ny = year1,year2
        do jday = -5,ndaysinyear(ny)
          pairq(jday,ny) = 0.0
        end do
      end do

      do Rvar = 1,nRvarOut  ! store water flows
        if (RnameOut(Rvar).ne.'WATR') cycle
        nv = 0
        do ny = year1,year2  ! populate pairq
          j1 = 1
          j2 = ndaysinyear(ny)
          if (ny.eq.year1) j1 = julian(ny,1,1,ny,month1,day1) - 6
          if (ny.eq.year2) j2 = julian(ny,1,1,ny,month2,day2)
          do jday = j1,j2
            do hour = 1,24
              nv = nv + 1
              pairq(jday,ny) = pairq(jday,ny) + hvaltemp(nv,Rvar) 
            end do
          end do
        end do
        do ny = year1+1,year2  ! go back and get days -5,0
          do jday = -5,0
            pairq(jday,ny) = pairq(ndaysinyear(ny-1)+jday,ny-1)
          end do
        end do

      end do
          
      return

********************************* ERROR SPACE **********************************
992   report(1) = 'problem in wdm, difference between '
      report(2) = 'expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',jdaytot
      go to 999

996   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

998   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

