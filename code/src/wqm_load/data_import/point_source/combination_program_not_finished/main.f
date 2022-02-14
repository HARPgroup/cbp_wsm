************************************************************************
**  point source wdm creation                                         **
**    This code is used for both the calibration and scenarios        **
**    there is a flag to determine whether the inputs are repeating   **
**    annually as in a scenario or annually variable as in the calib  **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'

      integer y1,y2  ! start and end year
      parameter (y1=EarliestYear,y2=LatestYear)
      integer jday

************ data scenario and raw data location
      character*35 psscen            ! ps data scenario for wdm location
      character*60 dataversion,nonCSOfnam,CSOfnam ! location of raw data
      integer lenpsscen,lendv,lennonCSOfnam,lenCSOfnam
      character*60 CSOfacfnam
      integer lenCSOfacfnam
*************** WDM related variables
      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)

      integer flowdsn,heatdsn,bodxdsn,tssxdsn,nh3xdsn,no3xdsn,
     .        ornxdsn,po4xdsn,orpxdsn,doxxdsn,orcxdsn
      data    flowdsn,heatdsn,bodxdsn,tssxdsn,nh3xdsn,no3xdsn,
     .        ornxdsn,po4xdsn,orpxdsn,doxxdsn,orcxdsn
     .        /3000,3001,3021,3022,3002,3003,3004,3005,3006,3023,3024/

************** conversion factors
      real mg2acft    ! output is daily time series ac-ft/day
                    ! divided in UCI to ac-ft/hour
      parameter (mg2acft=1000000./7.479/43560.)

      real acftC2heat  ! multiply acft*temp (c) to get heat
      parameter (acftC2heat = 4.895e6)

      real bod2lorn, bod2lorp ! labile fraction of orn and orp
                          ! which is interpreted in HSPF as refractory
      real rorn2rorc  ! ratio of refractory ORC to refractory ORN
      parameter (bod2lorn=0.0436,bod2lorp=0.00603,rorn2rorc=5.678)

      real bod2tss  ! bod is particulate and counted in VSS of TSS
      parameter (bod2tss=0.505)

      real tempC(12) ! assumed temperature in degrees C
      data tempC /10.,10.,14.,16.,20.,23.,25.,25.,23.,20.,16.,14./

*************** facility information
      character*10 Tfac
      integer TdisPoint
      integer Tfips

*************** daily variables for each data type
      real dailyflow(ndaymax)
      real dailyheat(ndaymax)
      real dailybodx(ndaymax)
      real dailytssx(ndaymax)
      real dailynh3x(ndaymax)
      real dailyno3x(ndaymax)
      real dailyornx(ndaymax)
      real dailypo4x(ndaymax)
      real dailyorpx(ndaymax)
      real dailydoxx(ndaymax)
      real dailyorcx(ndaymax)

**************** variables related to LRsegs
      integer maxlrsegs,nlrsegs,nlrseg,ns ! land to water combinations
      parameter (maxlrsegs = 1300)
      character*19 lrseg(maxlrsegs)
     
             ! indexed monthly
      real lrflow(maxlrsegs,y1:y2,12),Tflow
      real lrheat(maxlrsegs,y1:y2,12)
      real lrbodx(maxlrsegs,y1:y2,12),Tbod
      real lrtssx(maxlrsegs,y1:y2,12),Ttss
      real lrnh3x(maxlrsegs,y1:y2,12),Tnh3
      real lrno3x(maxlrsegs,y1:y2,12),Tno3
      real lrornx(maxlrsegs,y1:y2,12),Torn
      real lrpo4x(maxlrsegs,y1:y2,12),Tpo4
      real lrorpx(maxlrsegs,y1:y2,12),Torp
      real lrdoxx(maxlrsegs,y1:y2,12),Tdox
      real lrorcx(maxlrsegs,y1:y2,12)
      real Ndum,Pdum  ! dummies for TN and TP

      character*10, SigInsig, FacType
**************** variables related to wqm or hydro cells
      integer maxcells
      parameter (maxcells = 600)
      character*6 w57cell(maxcells),cell
      integer n57cells  ! total # of cells
      integer n57cell   ! index of current cell
      integer lencell

             ! indexed monthly
      real flow57(maxcells,y1:y2,12)
      real heat57(maxcells,y1:y2,12)
      real bodx57(maxcells,y1:y2,12)
      real tssx57(maxcells,y1:y2,12)
      real nh3x57(maxcells,y1:y2,12)
      real no3x57(maxcells,y1:y2,12)
      real ornx57(maxcells,y1:y2,12)
      real po4x57(maxcells,y1:y2,12)
      real orpx57(maxcells,y1:y2,12)
      real doxx57(maxcells,y1:y2,12)
      real orcx57(maxcells,y1:y2,12)

*************** variables for CSOs
      integer maxCSOlrsegs,nCSOlrsegs,nCSOlrseg ! land to water combinations
      parameter (maxCSOlrsegs = 120)
      character*19 CSOlrseg(maxlrsegs)

             ! indexed daily
      real CSOlrflow(maxCSOlrsegs,ndaymax)
      real CSOlrheat(maxCSOlrsegs,ndaymax)
      real CSOlrbodx(maxCSOlrsegs,ndaymax)
      real CSOlrtssx(maxCSOlrsegs,ndaymax)
      real CSOlrnh3x(maxCSOlrsegs,ndaymax)
      real CSOlrno3x(maxCSOlrsegs,ndaymax)
      real CSOlrornx(maxCSOlrsegs,ndaymax)
      real CSOlrpo4x(maxCSOlrsegs,ndaymax)
      real CSOlrorpx(maxCSOlrsegs,ndaymax)
      real CSOlrdoxx(maxCSOlrsegs,ndaymax)
      real CSOlrorcx(maxCSOlrsegs,ndaymax)

************* variables for the CSOs for the wqmcells
      integer maxCSOcells,nCSOcells,nCSOcell ! CSO-cell combination
      parameter (maxCSOcells = 100)
      character*19 CSOcell(maxCSOcells)

      real CSOcellflow(maxCSOcells,ndaymax)
      real CSOcellheat(maxCSOcells,ndaymax)
      real CSOcellbodx(maxCSOcells,ndaymax)
      real CSOcelltssx(maxCSOcells,ndaymax)
      real CSOcellnh3x(maxCSOcells,ndaymax)
      real CSOcellno3x(maxCSOcells,ndaymax)
      real CSOcellornx(maxCSOcells,ndaymax)
      real CSOcellpo4x(maxCSOcells,ndaymax)
      real CSOcellorpx(maxCSOcells,ndaymax)
      real CSOcelldoxx(maxCSOcells,ndaymax)
      real CSOcellorcx(maxCSOcells,ndaymax)

**************** variable for calib or scenario
********** calib = 0 is a scenario, repeat year
********** calib <> 0 is the calibration, use the year in the line
      logical calib   
      integer intcalib

************** utility variables
      character*400 longline,longfnam,command
      character*2 state 

      integer Tyear,ny,nm,nd,nday,nd2

      integer julian
      external julian

      logical scompcase ! function to compare strings
      external scompcase

      integer ndaysinmonth
      external ndaysinmonth

      data sdate /y1,1,1,0,0,0/
      data edate /y2,12,31,24,0,0/
      integer tnerr,tperr
      integer CSOtnerr,CSOtperr

      logical found,foundCSO,foundPS

********** NPDES number, CSO BMP fatcors
      integer maxfacs,nfacs,nf
      parameter (maxfacs = 100)
      character*10 facility(maxfacs)
      real TNfac(maxfacs),TPfac(maxfacs),TSSfac(maxfacs)

**************** END DECLARATIONS **************************************

******** get input data
      read*, psscen,dataversion,nonCSOfnam,CSOfnam,CSOfacfnam,intcalib
      call lencl(psscen,lenpsscen)
      call lencl(dataversion,lendv)
      call lencl(nonCSOfnam,lennonCSOfnam)
      call lencl(CSOfnam,lenCSOfnam)
      calib = .false.
      if (intcalib.ne.0) calib = .true.

**************  intitialize all variables
      do ns = 1,maxlrsegs  
        do ny = y1,y2
          do nm = 1,12
            lrflow(ns,ny,nm) = 0.0
            lrheat(ns,ny,nm) = 0.0
            lrbodx(ns,ny,nm) = 0.0
            lrtssx(ns,ny,nm) = 0.0
            lrnh3x(ns,ny,nm) = 0.0
            lrno3x(ns,ny,nm) = 0.0
            lrornx(ns,ny,nm) = 0.0
            lrpo4x(ns,ny,nm) = 0.0
            lrorpx(ns,ny,nm) = 0.0
            lrdoxx(ns,ny,nm) = 0.0
            lrorcx(ns,ny,nm) = 0.0
          end do
        end do
      end do

      do ns = 1,maxCSOlrsegs
        do nd = 1,ndaymax
          CSOlrflow(ns,nd) = 0.0
          CSOlrheat(ns,nd) = 0.0
          CSOlrbodx(ns,nd) = 0.0
          CSOlrtssx(ns,nd) = 0.0
          CSOlrnh3x(ns,nd) = 0.0
          CSOlrno3x(ns,nd) = 0.0
          CSOlrornx(ns,nd) = 0.0
          CSOlrpo4x(ns,nd) = 0.0
          CSOlrorpx(ns,nd) = 0.0
          CSOlrdoxx(ns,nd) = 0.0
          CSOlrorcx(ns,nd) = 0.0
        end do
      end do

      do ns = 1,maxcells
        do ny = y1,y2
          do nm = 1,12
            flow57(ns,ny,nm) = 0.0
            heat57(ns,ny,nm) = 0.0
            bodx57(ns,ny,nm) = 0.0
            tssx57(ns,ny,nm) = 0.0
            nh3x57(ns,ny,nm) = 0.0
            no3x57(ns,ny,nm) = 0.0
            ornx57(ns,ny,nm) = 0.0
            po4x57(ns,ny,nm) = 0.0
            orpx57(ns,ny,nm) = 0.0
            doxx57(ns,ny,nm) = 0.0
            orcx57(ns,ny,nm) = 0.0
          end do
        end do
      end do

      do ns = 1,maxCSOcells
        do nd = 1,ndaymax
          CSOcellflow(ns,nd) = 0.0
          CSOcellheat(ns,nd) = 0.0
          CSOcellbodx(ns,nd) = 0.0
          CSOcelltssx(ns,nd) = 0.0
          CSOcellnh3x(ns,nd) = 0.0
          CSOcellno3x(ns,nd) = 0.0
          CSOcellornx(ns,nd) = 0.0
          CSOcellpo4x(ns,nd) = 0.0
          CSOcellorpx(ns,nd) = 0.0
          CSOcelldoxx(ns,nd) = 0.0
          CSOcellorcx(ns,nd) = 0.0
        end do
      end do
      
************ open the data file - nonCSO
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//nonCSOfnam(:lennonCSOfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 992
      print*,'reading point source data from'
      print*,longfnam
      
      read(dfile,*)longline  ! ditch header row

      tnerr=0
      tperr=0

************ loop over all lines and populate variables
      nlrsegs = 0
      n57cells = 0
      do
        read(dfile,'(a400)',end=111,err=996) longline
        call d2x(longline,last)
        read(longline,*,end=990,err=990)
     .                                  SigInsig,FacType,
     .                                  rseg,lseg,cell,
     .                                  Tfac,TdisPoint,Tfips,
     .                                  Tyear,nm,nd,
     .                                  Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                                  Ndum,Tpo4,Torp,Pdum,Ttss

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) tperr=tperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) tnerr=tnerr+1

**************** get total number of lrsegs represented
        found = .false.
        do nlrseg = 1,nlrsegs
          if (lrseg(nlrseg).eq.lseg//rseg) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nlrsegs = nlrsegs + 1
          if (mod(nlrsegs,100).eq.0) print*,'read ',nlrsegs,' lrsegs'
          if (nlrsegs.gt.maxlrsegs) go to 993
          nlrseg = nlrsegs
          lrseg(nlrseg) = lseg//rseg
        end if

**************** get total number of wqm57 cells represented
        found = .false.
        do n57cell = 1,n57cells
          if (w57cell(n57cell).eq.cell) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          n57cells = n57cells + 1
          if (n57cells.gt.maxcells) go to 9931
          n57cell = n57cells
          w57cell(n57cell) = cell
        end if

************** add this line to lrseg variables
************ if calibration, only copy to the year for which the
*********** line was read.  If not calib, copy to all years
        do ny = y1,y2  

          if (calib.and.ny.ne.Tyear) cycle

          lrflow(nlrseg,ny,nm) = lrflow(nlrseg,ny,nm) + Tflow*mg2acft
          lrheat(nlrseg,ny,nm) = lrheat(nlrseg,ny,nm) 
     .                         + Tflow*mg2acft*tempC(nm)*acftC2heat
          lrdoxx(nlrseg,ny,nm) = lrdoxx(nlrseg,ny,nm) + Tdox
          lrnh3x(nlrseg,ny,nm) = lrnh3x(nlrseg,ny,nm) + Tnh3
          lrno3x(nlrseg,ny,nm) = lrno3x(nlrseg,ny,nm) + Tno3
          lrpo4x(nlrseg,ny,nm) = lrpo4x(nlrseg,ny,nm) + Tpo4

         ! for organics, calculate the labile portion and compare it
         !  to the total organic, if labile is greater than total,
         !  reset BOD so that labile is equal to total for the most
         !  limiting nutrient
          Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          lrornx(nlrseg,ny,nm) = lrornx(nlrseg,ny,nm)+Torn-Tbod*bod2lorn
          lrorpx(nlrseg,ny,nm) = lrorpx(nlrseg,ny,nm)+Torp-Tbod*bod2lorp
          lrorcx(nlrseg,ny,nm) = lrorcx(nlrseg,ny,nm) 
     .                         + (Torn-Tbod*bod2lorn) * rorn2rorc
          lrbodx(nlrseg,ny,nm) = lrbodx(nlrseg,ny,nm) + Tbod
          lrtssx(nlrseg,ny,nm) = lrtssx(nlrseg,ny,nm) 
     .                         + max(0.0,Ttss-Tbod*bod2tss)

************** add this line to the 57k bay model variables
          flow57(n57cell,ny,nm) = flow57(n57cell,ny,nm) + Tflow*mg2acft
          heat57(n57cell,ny,nm) = heat57(n57cell,ny,nm) 
     .                          + Tflow*mg2acft*tempC(nm)*acftC2heat
          doxx57(n57cell,ny,nm) = doxx57(n57cell,ny,nm) + Tdox
          nh3x57(n57cell,ny,nm) = nh3x57(n57cell,ny,nm) + Tnh3
          no3x57(n57cell,ny,nm) = no3x57(n57cell,ny,nm) + Tno3
          po4x57(n57cell,ny,nm) = po4x57(n57cell,ny,nm) + Tpo4

          Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          ornx57(n57cell,ny,nm) = ornx57(n57cell,ny,nm) 
     .                          + Torn-Tbod*bod2lorn
          orpx57(n57cell,ny,nm) = orpx57(n57cell,ny,nm) 
     .                          + Torp-Tbod*bod2lorp
          orcx57(n57cell,ny,nm) = orcx57(n57cell,ny,nm) 
     .                          + (Torn-Tbod*bod2lorn) * rorn2rorc
          bodx57(n57cell,ny,nm) = bodx57(n57cell,ny,nm) + Tbod
          tssx57(n57cell,ny,nm) = tssx57(n57cell,ny,nm) 
     .                          + max(0.0,Ttss-Tbod*bod2tss)

        end do  ! end loop that copies to all years
      end do
111   close(dfile)
       
*********** done with all data
      print*,'got all non CSO data into memory'
      print*, '  '

***************CSO data ****************************************
************* read in BMP factors for CSO
      call getCSOfacs(
     I                maxfacs,dataversion,lendv,CSOfacfnam,
     O                facility,TNfac,TPfac,TSSfac,nfacs)
 
************ open the CSO data file
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//CSOfnam(:lenCSOfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 992
      print*,'reading CSO data from'
      print*,longfnam

      read(dfile,*)longline  ! ditch header row

      CSOtnerr=0
      CSOtperr=0

************ loop over all lines and populate variables
      nCSOlrsegs = 0
      nCSOcells  = 0
      do
        read(dfile,'(a400)',end = 222,err=996)longline
        call d2x(longline,last)
        read(longline,*,end=990,err=990)
     .                                  rseg,lseg,cell,
     .                                  Tfac,TdisPoint,Tfips,
     .                                  ny,nm,nd,
     .                                  Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                                  Ndum,Tpo4,Torp,Pdum,Ttss

        if (ny.lt.y1.or.ny.gt.y2) cycle ! only process within years

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) CSOtperr=CSOtperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) CSOtnerr=CSOtnerr+1

************* check for all zeros
        if (Tflow+Tbod+Tdox+Tnh3+Tno3+Torn+Tpo4+Torp+Ttss.lt.0.01) cycle
        
************* get BMP factors for CSO
         found = .false.
         do nf = 1,nfacs
           if (Tfac.eq.facility(nf)) then
             found = .true.
************* Multiply all consitutens by BMP factors
             Tflow = Tflow*(1.0-TNfac(nf))
             Tbod  = Tbod*(1.0-TNfac(nf))
             Tdox  = Tdox*(1.0-TNfac(nf))
             Tnh3  = Tnh3*(1.0-TNfac(nf))
             Tno3  = Tno3*(1.0-TNfac(nf))
             Torn  = Torn*(1.0-TNfac(nf))
             Tpo4  = Tpo4*(1.0-TPfac(nf))
             Torp  = Torp*(1.0-TPfac(nf))
             Ttss  = Ttss*(1.0-TSSfac(nf))
             exit
           end if
        end do
        if (.not.found) go to 998
        
*************** find lrseg index if already exists, otherwise create new
        found = .false.
        do nCSOlrseg = 1,nCSOlrsegs
          if (CSOlrseg(nCSOlrseg).eq.lseg//rseg) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nCSOlrsegs = nCSOlrsegs + 1
          if (mod(nCSOlrsegs,10).eq.0) then
            print*,'read ',nCSOlrsegs,' CSO lrsegs'
          end if
          if (nCSOlrsegs.gt.maxCSOlrsegs) go to 9933
          nCSOlrseg = nCSOlrsegs
          CSOlrseg(nCSOlrseg) = lseg//rseg
        end if

*************** find w57cell index if already exists, otherwise create new
        found = .false.
        do nCSOcell = 1,nCSOcells
          if (CSOcell(nCSOcell).eq.cell) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nCSOcells = nCSOcells + 1
          if (nCSOcells.gt.maxCSOcells) go to 9933
          nCSOcell = nCSOcells
          CSOcell(nCSOcell) = cell
        end if

************ get day
        jday = julian(sdate(1),sdate(2),sdate(3),ny,nm,nd)

************** add this line to lrseg variables
        CSOlrflow(nCSOlrseg,jday) = CSOlrflow(nCSOlrseg,jday)
     .                            + Tflow*mg2acft
        CSOlrheat(nCSOlrseg,jday) = CSOlrheat(nCSOlrseg,jday)
     .                            + Tflow*mg2acft*tempC(nm)*acftC2heat
        CSOlrdoxx(nCSOlrseg,jday) = CSOlrdoxx(nCSOlrseg,jday) + Tdox
        CSOlrnh3x(nCSOlrseg,jday) = CSOlrnh3x(nCSOlrseg,jday) + Tnh3
        CSOlrno3x(nCSOlrseg,jday) = CSOlrno3x(nCSOlrseg,jday) + Tno3
        CSOlrpo4x(nCSOlrseg,jday) = CSOlrpo4x(nCSOlrseg,jday) + Tpo4

         ! for organics, calculate the labile portion and compare it
         !  to the total organic, if labile is greater than total,
         !  reset BOD so that labile is equal to total for the most
         !  limiting nutrient
        Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
        CSOlrornx(nCSOlrseg,jday) = CSOlrornx(nCSOlrseg,jday)
     .                            + Torn-Tbod*bod2lorn
        CSOlrorpx(nCSOlrseg,jday) = CSOlrorpx(nCSOlrseg,jday)
     .                            + Torp-Tbod*bod2lorp
        CSOlrorcx(nCSOlrseg,jday) = CSOlrorcx(nCSOlrseg,jday)
     .                            + (Torn-Tbod*bod2lorn) * rorn2rorc
        CSOlrbodx(nCSOlrseg,jday) = CSOlrbodx(nCSOlrseg,jday) + Tbod
        CSOlrtssx(nCSOlrseg,jday) = CSOlrtssx(nCSOlrseg,jday)
     .                            + max(0.0,Ttss-Tbod*bod2tss)

************** add this line to cell variables
        CSOcellflow(nCSOcell,jday) = CSOcellflow(nCSOcell,jday)
     .                             + Tflow*mg2acft
        CSOcellheat(nCSOcell,jday) = CSOcellheat(nCSOcell,jday)
     .                             + Tflow*mg2acft*tempC(nm)*acftC2heat
        CSOcelldoxx(nCSOcell,jday) = CSOcelldoxx(nCSOcell,jday) + Tdox
        CSOcellnh3x(nCSOcell,jday) = CSOcellnh3x(nCSOcell,jday) + Tnh3
        CSOcellno3x(nCSOcell,jday) = CSOcellno3x(nCSOcell,jday) + Tno3
        CSOcellpo4x(nCSOcell,jday) = CSOcellpo4x(nCSOcell,jday) + Tpo4

         ! for organics, calculate the labile portion and compare it
         !  to the total organic, if labile is greater than total,
         !  reset BOD so that labile is equal to total for the most
         !  limiting nutrient
        Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
        CSOcellornx(nCSOcell,jday) = CSOcellornx(nCSOcell,jday)
     .                             + Torn-Tbod*bod2lorn
        CSOcellorpx(nCSOcell,jday) = CSOcellorpx(nCSOcell,jday)
     .                             + Torp-Tbod*bod2lorp
        CSOcellorcx(nCSOcell,jday) = CSOcellorcx(nCSOcell,jday)
     .                             + (Torn-Tbod*bod2lorn) * rorn2rorc
        CSOcellbodx(nCSOcell,jday) = CSOcellbodx(nCSOcell,jday) + Tbod
        CSOcelltssx(nCSOcell,jday) = CSOcelltssx(nCSOcell,jday)
     .                             + max(0.0,Ttss-Tbod*bod2tss)
      
      end do
222   close(dfile)
      print*,'got all CSO data into memory'

*********** open wdms and write

********* open the dummy wdm to appease the HSPF gremlins
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)  ! open junk wdm
      if (err.ne.0) go to 991

*********** open all of the lrseg wdms and populate them
      do nlrseg = 1,nlrsegs   ! loop over all Rseg/Lseg pairs

        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/ps_'//
     .            lrseg(nlrseg)(:6)//'_to_'//lrseg(nlrseg)(7:19)//'.wdm'
        print*,nlrseg,' of ',nlrsegs,' opening ',wdmfnam(:79)
        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

*********** create the time series
        nvals = 0
        do ny = y1,y2
          do nm = 1,12
            do nday = 1,ndaysinmonth(ny,nm)
              nvals = nvals + 1
              dailyflow(nvals) = lrflow(nlrseg,ny,nm)
              dailyheat(nvals) = lrheat(nlrseg,ny,nm)
              dailybodx(nvals) = lrbodx(nlrseg,ny,nm)
              dailytssx(nvals) = lrtssx(nlrseg,ny,nm)
              dailynh3x(nvals) = lrnh3x(nlrseg,ny,nm)
              dailyno3x(nvals) = lrno3x(nlrseg,ny,nm)
              dailyornx(nvals) = lrornx(nlrseg,ny,nm)
              dailypo4x(nvals) = lrpo4x(nlrseg,ny,nm)
              dailyorpx(nvals) = lrorpx(nlrseg,ny,nm)
              dailydoxx(nvals) = lrdoxx(nlrseg,ny,nm)
              dailyorcx(nvals) = lrorcx(nlrseg,ny,nm)
            end do
          end do
        end do

*********** if CSO, add the CSO time series
        foundCSO = .false.
        do nCSOlrseg = 1,nCSOlrsegs
          if (lrseg(nlrseg).eq.CSOlrseg(nCSOlrseg)) then
            foundCSO = .true.
            exit                 ! nCSOlrseg set
          end if
        end do
        if (foundCSO) then
          do nd = 1,nvals
            dailyflow(nd) = dailyflow(nd) + CSOlrflow(nCSOlrseg,nd)
            dailyheat(nd) = dailyheat(nd) + CSOlrheat(nCSOlrseg,nd)
            dailybodx(nd) = dailybodx(nd) + CSOlrbodx(nCSOlrseg,nd)
            dailytssx(nd) = dailytssx(nd) + CSOlrtssx(nCSOlrseg,nd)
            dailynh3x(nd) = dailynh3x(nd) + CSOlrnh3x(nCSOlrseg,nd)
            dailyno3x(nd) = dailyno3x(nd) + CSOlrno3x(nCSOlrseg,nd)
            dailyornx(nd) = dailyornx(nd) + CSOlrornx(nCSOlrseg,nd)
            dailypo4x(nd) = dailypo4x(nd) + CSOlrpo4x(nCSOlrseg,nd)
            dailyorpx(nd) = dailyorpx(nd) + CSOlrorpx(nCSOlrseg,nd)
            dailydoxx(nd) = dailydoxx(nd) + CSOlrdoxx(nCSOlrseg,nd)
            dailyorcx(nd) = dailyorcx(nd) + CSOlrorcx(nCSOlrseg,nd)
          end do
        end if

************* put in the wdm
        call putdailydsn(wdmfil,sdate,edate,flowdsn,nvals,dailyflow)
        call putdailydsn(wdmfil,sdate,edate,heatdsn,nvals,dailyheat)
        call putdailydsn(wdmfil,sdate,edate,bodxdsn,nvals,dailybodx)
        call putdailydsn(wdmfil,sdate,edate,tssxdsn,nvals,dailytssx)
        call putdailydsn(wdmfil,sdate,edate,nh3xdsn,nvals,dailynh3x)
        call putdailydsn(wdmfil,sdate,edate,no3xdsn,nvals,dailyno3x)
        call putdailydsn(wdmfil,sdate,edate,ornxdsn,nvals,dailyornx)
        call putdailydsn(wdmfil,sdate,edate,po4xdsn,nvals,dailypo4x)
        call putdailydsn(wdmfil,sdate,edate,orpxdsn,nvals,dailyorpx)
        call putdailydsn(wdmfil,sdate,edate,doxxdsn,nvals,dailydoxx)
        call putdailydsn(wdmfil,sdate,edate,orcxdsn,nvals,dailyorcx)

        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

********** echo out lrseg point sources
C      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
C     .           '/lrseg_ps_loads.csv'
C      open(dfile,file=longfnam,status='unknown',iostat=err)
C      if (err.ne.0) go to 992

C      write(dfile,'(a,a)',err=951) 'lseg,rseg,year,month,',
C     .                   'flow,heat,bod,do,nh3,no3,po4,tss,orn,orp,orc'
C      do nlrseg = 1,nlrsegs
C        ny = year
C          do nm = 1,12
C            nday = ndaysinmonth(ny,nm)
C            write(dfile,1234,err=951) lrseg(nlrseg)(1:6),',',
C     .                        lrseg(nlrseg)(7:19),',',ny,',',nm,',',
C     .                        dailyflow(nday),',',
C     .                        dailyheat(nday),',',
C     .                        dailybodx(nday),',',
C     .                        dailydoxx(nday),',',
C     .                        dailynh3x(nday),',',
C     .                        dailyno3x(nday),',',
C     .                        dailypo4x(nday),',',
C     .                        dailytssx(nday),',',
C     .                        dailyornx(nday),',',
C     .                        dailyorpx(nday),',',
C     .                        dailyorcx(nday)
C          end do
C      end do
C      close(dfile)
C1234  format(a6,a1,a13,a1,i4,a1,i2,11(a1,e10.3))



      nvals = julian(sdate(1),sdate(2),sdate(3),    ! set nvals
     .               edate(1),edate(2),edate(3))   ! in case no lrsegs
*********** open all of the CSO only lrseg wdms and populate them
      do nCSOlrseg = 1,nCSOlrsegs   ! loop over all Rseg/Lseg pairs

*********** if lrseg exists, cycle
        foundPS = .false.
        do nlrseg = 1,nlrsegs
          if (lrseg(nlrseg).eq.CSOlrseg(nCSOlrseg)) then
            foundPS = .true.
            exit
          end if
        end do
        if (foundPS) cycle  ! CSO already processed in regular wdm

        do nd = 1,nvals
          dailyflow(nd) = CSOlrflow(nCSOlrseg,nd)
          dailyheat(nd) = CSOlrheat(nCSOlrseg,nd)
          dailybodx(nd) = CSOlrbodx(nCSOlrseg,nd)
          dailytssx(nd) = CSOlrtssx(nCSOlrseg,nd)
          dailynh3x(nd) = CSOlrnh3x(nCSOlrseg,nd)
          dailyno3x(nd) = CSOlrno3x(nCSOlrseg,nd)
          dailyornx(nd) = CSOlrornx(nCSOlrseg,nd)
          dailypo4x(nd) = CSOlrpo4x(nCSOlrseg,nd)
          dailyorpx(nd) = CSOlrorpx(nCSOlrseg,nd)
          dailydoxx(nd) = CSOlrdoxx(nCSOlrseg,nd)
          dailyorcx(nd) = CSOlrorcx(nCSOlrseg,nd)
        end do

************ open the WDM
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/ps_'//
     .            CSOlrseg(nCSOlrseg)(:6)//'_to_'//
     .            CSOlrseg(nCSOlrseg)(7:19)//'.wdm'
        print*,nCSOlrseg,' of ',nCSOlrsegs,' CSO-only segs'
        print*,'opening ',wdmfnam(:79)
        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

************* put in the wdm
        call putdailydsn(wdmfil,sdate,edate,flowdsn,nvals,dailyflow)
        call putdailydsn(wdmfil,sdate,edate,heatdsn,nvals,dailyheat)
        call putdailydsn(wdmfil,sdate,edate,bodxdsn,nvals,dailybodx)
        call putdailydsn(wdmfil,sdate,edate,tssxdsn,nvals,dailytssx)
        call putdailydsn(wdmfil,sdate,edate,nh3xdsn,nvals,dailynh3x)
        call putdailydsn(wdmfil,sdate,edate,no3xdsn,nvals,dailyno3x)
        call putdailydsn(wdmfil,sdate,edate,ornxdsn,nvals,dailyornx)
        call putdailydsn(wdmfil,sdate,edate,po4xdsn,nvals,dailypo4x)
        call putdailydsn(wdmfil,sdate,edate,orpxdsn,nvals,dailyorpx)
        call putdailydsn(wdmfil,sdate,edate,doxxdsn,nvals,dailydoxx)
        call putdailydsn(wdmfil,sdate,edate,orcxdsn,nvals,dailyorcx)

        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

*********** open all of the bay wdms and populate them
      do n57cell = 1,n57cells   ! loop over all cells

        call lencl(w57cell(n57cell),lencell)
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/bay_models/ps_wqm57k_'//w57cell(n57cell)(:lencell)//
     .            '.wdm'
        command = 'cp '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        print*,n57cell,' of ',n57cells,' opening ',wdmfnam(:79)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

*********** create the time series
        nvals = 0
        do ny = y1,y2
          do nm = 1,12
            do nday = 1,ndaysinmonth(ny,nm)
              nvals = nvals + 1
              dailyflow(nvals) = flow57(n57cell,ny,nm)
              dailyheat(nvals) = heat57(n57cell,ny,nm)
              dailybodx(nvals) = bodx57(n57cell,ny,nm)
              dailytssx(nvals) = tssx57(n57cell,ny,nm)
              dailynh3x(nvals) = nh3x57(n57cell,ny,nm)
              dailyno3x(nvals) = no3x57(n57cell,ny,nm)
              dailyornx(nvals) = ornx57(n57cell,ny,nm)
              dailypo4x(nvals) = po4x57(n57cell,ny,nm)
              dailyorpx(nvals) = orpx57(n57cell,ny,nm)
              dailydoxx(nvals) = doxx57(n57cell,ny,nm)
              dailyorcx(nvals) = orcx57(n57cell,ny,nm)
            end do
          end do
        end do

*********** if CSO, add the CSO time series
        foundCSO = .false.
        do nCSOcell = 1,nCSOcells
          if (w57cell(n57cell).eq.CSOcell(nCSOcell)) then
            foundCSO = .true.
            exit                 ! nCSOcell set
          end if
        end do
        if (foundCSO) then
          do nd = 1,nvals
            dailyflow(nd) = dailyflow(nd) + CSOcellflow(nCSOcell,nd)
            dailyheat(nd) = dailyheat(nd) + CSOcellheat(nCSOcell,nd)
            dailybodx(nd) = dailybodx(nd) + CSOcellbodx(nCSOcell,nd)
            dailytssx(nd) = dailytssx(nd) + CSOcelltssx(nCSOcell,nd)
            dailynh3x(nd) = dailynh3x(nd) + CSOcellnh3x(nCSOcell,nd)
            dailyno3x(nd) = dailyno3x(nd) + CSOcellno3x(nCSOcell,nd)
            dailyornx(nd) = dailyornx(nd) + CSOcellornx(nCSOcell,nd)
            dailypo4x(nd) = dailypo4x(nd) + CSOcellpo4x(nCSOcell,nd)
            dailyorpx(nd) = dailyorpx(nd) + CSOcellorpx(nCSOcell,nd)
            dailydoxx(nd) = dailydoxx(nd) + CSOcelldoxx(nCSOcell,nd)
            dailyorcx(nd) = dailyorcx(nd) + CSOcellorcx(nCSOcell,nd)
          end do
        end if

************* put in the wdm
        call putdailydsn(wdmfil,sdate,edate,flowdsn,nvals,dailyflow)
        call putdailydsn(wdmfil,sdate,edate,heatdsn,nvals,dailyheat)
        call putdailydsn(wdmfil,sdate,edate,bodxdsn,nvals,dailybodx)
        call putdailydsn(wdmfil,sdate,edate,tssxdsn,nvals,dailytssx)
        call putdailydsn(wdmfil,sdate,edate,nh3xdsn,nvals,dailynh3x)
        call putdailydsn(wdmfil,sdate,edate,no3xdsn,nvals,dailyno3x)
        call putdailydsn(wdmfil,sdate,edate,ornxdsn,nvals,dailyornx)
        call putdailydsn(wdmfil,sdate,edate,po4xdsn,nvals,dailypo4x)
        call putdailydsn(wdmfil,sdate,edate,orpxdsn,nvals,dailyorpx)
        call putdailydsn(wdmfil,sdate,edate,doxxdsn,nvals,dailydoxx)
        call putdailydsn(wdmfil,sdate,edate,orcxdsn,nvals,dailyorcx)

        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

      nvals = julian(sdate(1),sdate(2),sdate(3),    ! set nvals
     .               edate(1),edate(2),edate(3))   ! in case no lrsegs
*********** open all of the CSO only cell wdms and populate them
      do nCSOcell = 1,nCSOcells   ! loop over all Rseg/Lseg pairs

*********** if cell exists, cycle
        foundPS = .false.
        do n57cell = 1,n57cells
          if (w57cell(n57cell).eq.CSOcell(nCSOcell)) then
            foundPS = .true.
            exit
          end if
        end do
        if (foundPS) cycle  ! CSO already processed in regular wdm

        do nd = 1,nvals
          dailyflow(nd) = CSOcellflow(nCSOcell,nd)
          dailyheat(nd) = CSOcellheat(nCSOcell,nd)
          dailybodx(nd) = CSOcellbodx(nCSOcell,nd)
          dailytssx(nd) = CSOcelltssx(nCSOcell,nd)
          dailynh3x(nd) = CSOcellnh3x(nCSOcell,nd)
          dailyno3x(nd) = CSOcellno3x(nCSOcell,nd)
          dailyornx(nd) = CSOcellornx(nCSOcell,nd)
          dailypo4x(nd) = CSOcellpo4x(nCSOcell,nd)
          dailyorpx(nd) = CSOcellorpx(nCSOcell,nd)
          dailydoxx(nd) = CSOcelldoxx(nCSOcell,nd)
          dailyorcx(nd) = CSOcellorcx(nCSOcell,nd)
        end do

************ open the WDM
        call lencl(CSOcell(nCSOcell),lencell)
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/bay_models/ps_wqm57k_'//CSOcell(nCSOcell)(:lencell)
     .            //'.wdm'
        print*,nCSOcell,' of ',nCSOcells,' CSO-only segs'
        print*,'opening ',wdmfnam(:79)
        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 991

************* put in the wdm
        call putdailydsn(wdmfil,sdate,edate,flowdsn,nvals,dailyflow)
        call putdailydsn(wdmfil,sdate,edate,heatdsn,nvals,dailyheat)
        call putdailydsn(wdmfil,sdate,edate,bodxdsn,nvals,dailybodx)
        call putdailydsn(wdmfil,sdate,edate,tssxdsn,nvals,dailytssx)
        call putdailydsn(wdmfil,sdate,edate,nh3xdsn,nvals,dailynh3x)
        call putdailydsn(wdmfil,sdate,edate,no3xdsn,nvals,dailyno3x)
        call putdailydsn(wdmfil,sdate,edate,ornxdsn,nvals,dailyornx)
        call putdailydsn(wdmfil,sdate,edate,po4xdsn,nvals,dailypo4x)
        call putdailydsn(wdmfil,sdate,edate,orpxdsn,nvals,dailyorpx)
        call putdailydsn(wdmfil,sdate,edate,doxxdsn,nvals,dailydoxx)
        call putdailydsn(wdmfil,sdate,edate,orcxdsn,nvals,dailyorcx)

        call wdflcl(wdmfil,err)   ! close wdm file
        if (err.ne.0) go to 995

      end do

      call wdflc1(wdmfil+1,err)   ! close junk wdm file
      print*,'done making all wdms'
      print*,'there were ',tnerr,' TN errors and ',tperr,' TP errors'

************ Write out list of LRsegs to update GEO file
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//wwtp(:lenwwtp)//'_lrsegs.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing out lrsegs'
      print*,longfnam

      write(dfile,*) 'point source,', 'river,', 'land'
      do nlrseg = 1,nlrsegs
        call trims(lrseg(nlrseg),last)
        write(dfile,*) 'wwtp,',lrseg(nlrseg)(7:last),',',
     .                 lrseg(nlrseg)(:6)
      end do

      do nCSOlrseg = 1,nCSOlrsegs   ! loop over all Rseg/Lseg pairs

        foundPS = .false.   ! if lrseg exists, cycle
        do nlrseg = 1,nlrsegs
          if (lrseg(nlrseg).eq.CSOlrseg(nCSOlrseg)) then
            foundPS = .true.
            exit
          end if
        end do
        if (foundPS) cycle  ! CSO already processed in regular wdm

        call trims(CSOlrseg(nCSOlrseg),last)
        write(dfile,*) 'wwtp,',CSOlrseg(nCSOlrseg)(7:last),',',
     .                 CSOlrseg(nCSOlrseg)(:6)
      end do

      close(dfile)

      stop

************************* ERROR SPACE **********************************
990   report(1) = 'problem reading file:  near line: could not parse'
      report(2) = longfnam
      report(3) = longline 
      print*, Tfac,nm,nd,ny,Tflow,Tbod,Tdox,Tnh3,
     .                    Tno3,Torn,Ndum,Tpo4,Torp,Pdum,'tss',Ttss
      go to 999

991   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

992   report(1) = 'could not open file'
      report(2) = longfnam
      report(3) = ' '
      print*,longfnam
      go to 999

993   report(1) = 'more rseg lseg pairs than anticipated'
      report(2) = 'increase maxlrsegs in ./pp/src/'
      report(3) = 'calibration_utils/wdm/src/make_point_source_wdms'
      go to 999

9931  report(1) = 'more 57k cells than anticipated'
      report(2) = 'increase maxcells in ./pp/src/'
      report(3) = 'calibration_utils/wdm/src/make_point_source_wdms'
      go to 999

9933  report(1) = 'more rseg lseg pairs than anticipated in CSO file'
      report(2) = 'fix maxCSOlrsegs in ./code/src/'
      report(3) = 'data_import/point_source/'
      go to 999

995   report(1) = 'Error: closing wdm = '
      write(report(1)(28:30),'(i3)')err
      report(2) = ' wdm name =  '
      report(3) = wdmfnam
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = longline 
      go to 999

997   report(1) = 'TP mass balance problem in file:  on line:'
      report(2) = fnam
      report(3) = longline 
      go to 999

998   report(1) = 'facility not found in reduction file:'
      report(2) = Tfac
      report(3) = 'check reduction file' 
      go to 999

999   call stopreport(report)

      end
