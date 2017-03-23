************************************************************************
**  The purpose of this program is to find average monthly load totals **
**    The Strategy                                                     **
**      1.  get segment and scenario                                   **
**      2.  get info from catalog file                                 **
**      3.  read water and store it to find concentrations             **
**      4.  read and write concentrations for each variable            **
**********************************************************************************
      subroutine avemon(rscen,rseg,year1,year2,wdmfil,Nexits)

      implicit none
      include 'Ravemon.inc'

      integer asdate(ndate),aedate(ndate)           ! test dates for checking wdm
      integer wdmfil
   
      character*100 pfname
      character*4 Tdsn,cyear1,cyear2

      integer i,np,nc,j,n,Rvar,hour,oldyear,oldmonth         ! indices
      integer year1,year2
      integer ny,nm

      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      real flowvals(ndaymax*24)        ! flow
      real loadvals(ndaymax*24)        ! load
      real tload    ! load accumulator
      real mload(1984:2005,12),avemonth(12)

      integer Nexits,idummy
      logical found,foundall

************* END DECLARATIONS ***************************************************
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      
      print*,' Average Monthly River Load ',rscen(:lenrscen),' ',rseg

      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname)! POPULATE nRvar, Rdsn, Rname 
      call loadinfo(rscen,lenrscen,nRvar,Rname,
     .              nloads,loadname,unit,ncons,con,confactor)     ! POPULATE loading variables
      call readcontrol_time(rscen,lenrscen,sdate,edate)         ! get start and stop

      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
      if (err .ne. 0) go to 991

      call wtdate(wdmfil,1,Rdsn(1),2,asdate,aedate,err)    ! tests the date

      if (err .ne. 0) go to 992

      nWATRdsns = 0
      do Rvar = 1,nRvar                     ! find which dsns corresponds to water
        if (Rname(Rvar).eq.'WATR') then
          nWATRdsns = nWATRdsns + 1
          if (nWATRdsns.gt.maxWATR) go to 993
          WATRdsn(nWATRdsns) = Rdsn(Rvar)
        end if
      end do

      call checkflow(wdmfnam,wdmfil,0,maxWATR,nWATRdsns,WATRdsn,
     .                     sdate,edate)               ! make sure that flow exists

      call gethourdsn(wdmfil,sdate,edate,WATRdsn(1),nvals,flowvals)         ! get nvals from water variable
      do Rvar = 2,nWATRdsns         ! get other flowvals if more than 1 water variables
        call gethourdsn(wdmfil,sdate,edate,WATRdsn(Rvar),nvals,hval)
        do i = 1,nvals
          flowvals(i) = flowvals(i) + hval(i)
        end do
      end do

      do np = 1,nloads

        call ttyput(loadname(np))
        call ttyput(' ')

        do i = 1,nvals                 ! initialize loadvals
          loadvals(i) = 0.
        end do
        foundall = .true.
        do nc = 1,ncons(np)            ! for each constituent
          found = .false.
          do Rvar = 1,nRvar            ! get the right dsn
            if (con(np,nc).eq.Rname(Rvar)) then
              found = .true.
              dsn = Rdsn(Rvar)
              call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)      
              do i = 1,nvals
                if (hval(i).gt.0.)
     .            loadvals(i) = loadvals(i) + hval(i)*confactor(np,nc) 
              end do
            end if
          end do
          if (.not.found) foundall = .false.
        end do

*****************loada
        write(cyear1,'(i4)') year1
        write(cyear2,'(i4)') year2
        
        if (foundall) then
          pfname = outdir//'river/avemon/'//rscen(:lenrscen)//'/'//
     .             rseg(:lenrseg)//'_'//loadname(np)//'_'//
     .             cyear1//'_'//cyear2//'_avemon.prn'
          open (pltfil,file = pfname, status = 'unknown',iostat = err)
          if (err.ne.0) goto 994
 
          write(pltfil,'(a12,a6)',err=951)'CON   MONTH ',unit(np)
      
          do i = 1,6
            asdate(i) = sdate(i)
          end do

          hour = 0
          tload = 0.
          oldyear = asdate(1)
          oldmonth = asdate(2)
          do i = 1,nvals
            hour = hour + 1
            tload = tload + loadvals(i)
            if (hour.eq.24) then
              hour = 0
              call tomorrow(asdate(1),asdate(2),asdate(3))
            end if
            if (asdate(2).ne.oldmonth) then
              mload(oldyear,oldmonth) = tload        ! record monthly loads for each month in each year
              tload = 0.
              oldyear = asdate(1)
              oldmonth = asdate(2)
            end if
          end do
          
          do nm = 1, 12
            avemonth(nm) = 0. 
            do ny = year1, year2
              avemonth(nm) = avemonth(nm) + mload(ny,nm)   ! compute aveage monthly loads
            end do
            avemonth(nm) = avemonth(nm)/real(year2-year1+1)
            write(pltfil,1234,err=951) loadname(np),nm,avemonth(nm)
          end do
          
          close (pltfil)

        end if
        
      end do            ! end loop over all loads in 'rchres_out_to_daily' file

      print*,' '

      return

1234  format(a6,i6,e14.5)

********ERROR SPACE ****************************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

993   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

994   report(1) = 'Problem opening load files for segment '//rseg
      report(2) = pfname
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

