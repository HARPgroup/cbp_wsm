************************************************************************
**  The purpose of this program is to prepare the plot files for the  **
**    SAS routines for calibration                                    **
**    The Strategy                                                    **
**      1.  get segment and scenario                                  **
**      2.  get info from catalog file                                **
**      3.  read water and store it to find concentrations            **
**      4.  read and write concentrations for each variable           **
************************************************************************

      implicit none
      include 'Rdaily.inc'

      integer asdate(ndate),aedate(ndate)           ! test dates for checking wdm

      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      character*64 pfname

      integer i,np,nc,j,Rvar,hour, h         ! indices
      integer year,month,day,ndays,nv,nhours
      character*4 dsnname,suffix

      integer ndaysinmonth
      external ndaysinmonth

      real factor

      integer Nexits,idummy
      integer maxWATR,nWATRdsns    ! number of DSNs that make up the total flow (multiple exits)
      parameter (maxWATR=4)
      integer WATRdsn(maxWATR)      ! dsns for flow variables

      real dailyconc(ndaymax)       ! read from pltgens
      real dailyload(ndaymax)       ! read from pltgens
      real dailyflow(ndaymax)       ! read from pltgens
      real flowvals(ndaymax*24)        ! flow
      real loadvals(ndaymax*24)        ! load
      real normvals(ndaymax*24)        ! normalize by:  (usually flow)
      real:: concvals(24)              ! hourly concentration in a day         

      real dnorm,dload,dconc    ! daily normalization (flow),phytoplankton, and load accumulators

      real:: daymax, daymin     ! daily MAX and MIN values
      
      character*4 Tdsn

      logical founddouble

      data sdate /1984, 1, 1, 1,0,0/
      data edate /1997,12,31,24,0,0/

************* END DECLARATIONS *******************************


      rscen = 'phase4'
      read*,rseg     ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      print*,'creating phase 4 wdms ',rscen(:lenrscen),' ',rseg

      Nexits = 3
      call WDMinfo(rscen,Nexits,nRvar,Rdsn,Rname) 

      wdmfnam = outwdmdir//'river/'//rscen(:lenrscen)//
     .            '/stream/'//rseg(:lenrseg)//'.wdm'
      call wdbopnlong(wdmfil,wdmfnam,0,err)     ! open wdm read/write
      if (err .ne. 0) go to 991

********** get flow
      suffix = 'FLOW'
      dsnname = 'WATR'
      factor = 12.10/24.0
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyflow)

********* make flow hourly from cfs to ac-ft/hour
      nhours = 0
      ndays = 0
      do year = sdate(1),edate(1)
        do month = 1,12
          do day = 1,ndaysinmonth(year,month)
            ndays = ndays + 1
            dailyflow(ndays) = dailyflow(ndays) / factor ! to ac-ft/day
            do hour = 1,24
              nhours = nhours + 1
              flowvals(nhours) = dailyflow(ndays) / 24.0 ! to ac-ft/hr
            end do
          end do
        end do
      end do
      nvals = nhours
      do Rvar = 1,nRvar 
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,flowvals)
        end if
      end do

********* get WTMP
      suffix = 'WTMP'
      dsnname = 'HEAT'
      factor = 2.043E-7
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / factor / 24.0
          end do
        end if
      end do
      do Rvar = 1,nRvar 
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get DOXY
      suffix = 'DOXX'
      dsnname = 'DOXY'
      factor = 0.3667
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / factor / 24.0
          end do
        end if
      end do
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get PHYT
      suffix = 'CHLA'
      dsnname = 'PHYT'
      factor = 2.635
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / factor / 24.0
          end do
        end if
      end do
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get NO3D
      suffix = 'NO3X'
      dsnname = 'NO3D'
      factor = 0.3667
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / factor / 24.0
          end do
        end if
      end do
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get PO4D
      suffix = 'PO4X'
      dsnname = 'PO4D'
      factor = 0.3667
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / factor / 24.0
          end do
        end if
      end do
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get NH3D,NH3A,NH3I,NH3C
      suffix = 'NH3X'
      factor = 0.3667
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv)=dailyconc(day)*dailyflow(day)/factor/24.0/4.0
          end do
        end if
      end do
      dsnname = 'NH3D'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do
      dsnname = 'NH3A'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do
      dsnname = 'NH3I'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do
      dsnname = 'NH3C'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get SAND,SILT,CLAY from TSSX given PHYT
      suffix = 'TSSX'
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / 24.0
          end do
        end if
      end do

      dsnname = 'PHYT'   ! get PHYT to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do

      do nv = 1,nvals   ! get into correct units
        loadvals(nv) = loadvals(nv) / 735.4 / 3.0
      end do
        
      dsnname = 'SAND'   ! load wdm
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do
      dsnname = 'SILT'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do
      dsnname = 'CLAY'
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,loadvals)
        end if
      end do

********* get BODA,TORC from TOCX given PHYT
      suffix = 'TOCX'
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / 24.0
          end do
        end if
      end do

      dsnname = 'PHYT'   ! get PHYT to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.1802)
      end do

      do nv = 1,nvals   ! get into correct units for TORC
        hval(nv) = loadvals(nv) / 0.3677 / 2.0
      end do
      dsnname = 'TORC'   ! load wdm
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do

      do nv = 1,nvals   ! get into correct units for BODA
        hval(nv) = loadvals(nv) / 0.09099 / 2.0
      end do
      dsnname = 'BODA'   ! load wdm
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do

********* get RORN from TOTN given NH3D,NH3A,NH3I,NH3C,NO3D,BODA,PHYT
      suffix = 'TOTN'
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / 24.0
          end do
        end if
      end do

      dsnname = 'NH3D'   ! get NH3D to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'NH3A'   ! get NH3A to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'NH3I'   ! get NH3I to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'NH3C'   ! get NH3C to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'NO3D'   ! get NO3D to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'BODA'   ! get BODA to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.01602)
      end do
      dsnname = 'PHYT'   ! get PHYT to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.03173)
      end do

      do nv = 1,nvals   ! get into correct units for RORN
        hval(nv) = loadvals(nv) / 0.3677 
      end do
      dsnname = 'RORN'   ! load wdm
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do

********* get RORP from TOTP given PO4D,PO4A,PO4I,PO4C,BODA,PHYT
      suffix = 'TOTP'
      call readplt(
     I             sdate(1),edate(1),rscen,lenrscen,rseg,lenrseg,suffix,
     O             dailyconc)
      nv = 0
      do day = 1,ndays
        if (dailyflow(day).lt.0.0001) then
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = 0.0
          end do
        else
          do hour = 1,24
            nv = nv + 1
            loadvals(nv) = dailyconc(day)*dailyflow(day) / 24.0
          end do
        end if
      end do

      dsnname = 'PO4D'   ! get PO4D to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'PO4A'   ! get PO4A to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'PO4I'   ! get PO4I to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'PO4C'   ! get PO4C to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.3677)
      end do
      dsnname = 'BODA'   ! get BODA to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.002218)
      end do
      dsnname = 'PHYT'   ! get PHYT to subtract
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call gethourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do
      do nv = 1,nvals
        loadvals(nv) = loadvals(nv) - (hval(nv) * 0.004391)
      end do

      do nv = 1,nvals   ! get into correct units for RORP
        hval(nv) = loadvals(nv) / 0.3677 
      end do
      dsnname = 'RORP'   ! load wdm
      do Rvar = 1,nRvar
        if (Rname(Rvar).eq.dsnname) then
          call puthourdsn(wdmfil,sdate,edate,Rdsn(Rvar),nvals,hval)
        end if
      end do


      stop

************************ error reporting
991   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

