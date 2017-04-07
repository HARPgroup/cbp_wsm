************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/wdm.inc'
      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer i,j,sdate(ndate),edate(ndate),tstep
      integer time(ndaymax*24,4)  ! year,month,day,hour
      integer ny,oldyear
      integer dapart,Uday
      integer ndays,nd,nd2
      integer nhour,nh,nh2
      real Udval(ndaymax),Uhval(ndaymax*24)       ! daily and hourly USGS data
      real Ndval(ndaymax),Nhval(ndaymax*24)
      real ratio

      double precision dacc

************* open new wdm
      read(*,'(a)') lseg

      print*,lseg

      wdmfnam = 'prad_'//lseg(:6)//'.wdm'
      call wdbopn(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 991

      tcode = 3    ! hourly data

      dsn = 2000   ! precip dsn

************ read in NARR and USGS data
      print*, 'reading USGS data '
      call readUSGS(
     I              lseg,
     O              nvals,Uhval,Udval,sdate,edate)

      print*, 'reading NARR data'
      call readNARR(
     I              lseg,
     O              ndays,Ndval)
      
******* disaggragate daily NARR data to hourly 
********** assign all hourly precip to zero first
      nhour = 0
      do i = 1, ndays
        do j = 1, 24 
          nhour = nhour + 1
          Nhval(nhour) = 0.0
        end do
      end do

********** then for any day with precip, use USGS hourly data signal to disaggragate 
      do nd = 1, ndays
        if (Ndval(nd) .gt. 0.0) then      ! if there is precip this day
        dapart = 0
          do     
            if (abs(Ndval(nd)-Udval(nd-dapart)).lt.1.0 .and.           ! move backward to find a closest date 
     .              Udval(nd-dapart).gt.0.0) then 
              Uday = nd-dapart
              exit
            end if
 
            if (abs(Ndval(nd)-Udval(nd+dapart)).lt.1.0 .and. 
     .          Udval(nd+dapart).gt.0.0 .and. (nd+dapart).le.ndays) then  ! move forward to find a closest date
              Uday = nd+dapart                                           ! when beyond last day, force to move backward
              exit
            end if

            if (Ndval(nd) .gt.3.0 .and. Udval(nd) .gt. 2.0) then
              Uday = nd
              exit
            end if
            dapart = dapart + 1
          end do
  
          nh2 = (Uday-1)*24      ! last hour of previous day,USGS data                          
          nh = (nd-1)*24          ! NARR data
          do j = 1, 24
            nh2 = nh2 + 1                     ! find current hour
            ratio = Uhval(nh2)/Udval(Uday)            ! compute ratio for USGS data
            nh = nh + 1
            Nhval(nh) = ratio * Ndval(nd)             ! use the ratio to disaggragate NARR data for the same day
          end do
      
        end if          
      end do               ! end loop for all days
     
************** write hourly NARR data to wdm
      print*,'writing to ',wdmfnam
      call puthourdsn(wdmfil,sdate,edate,dsn,nvals,Nhval)
   
      call wdflc1(wdmfil,err)
      if (err.ne.0) go to 993

      return

********************************* ERROR SPACE **************************
991   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   report(1) = 'the USGS date outside the total days'
      report(2) = ' '
      write(report(3),*) 'Uday = ',Uday
      go to 999

993   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)


      end

