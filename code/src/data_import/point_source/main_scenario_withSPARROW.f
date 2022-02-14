************************************************************************
**  reads point sources for a scenario year within calibration period **
**   creates wdms for the watershed and estuarine models              **
************************************************************************
** For phase 5.3, read 3 separate files:                              **
**     CSO file: daily                                                **
**     WWTP and INDUSTRIAL files: monthly                             ** 
************************************************************************
**  Make three individual WDMS for the watershed model:               **
**    WWTP, CSO, INDUSTRIAL                                           **
**  Make one combined WDM for WQSTM                                   **
************************************************************************
      implicit none
      include 'ps.inc'
      include 'SPARROW.inc'
      include 'CSOPassThru.inc'

      integer year  ! scenario year
      integer nlrsegs,nlrseg,ns ! land to water combinations

**************** variables related to LRsegs
      real lrflow(maxlrsegs,y1:y2,12)
      real lrheat(maxlrsegs,y1:y2,12)
      real lrbodx(maxlrsegs,y1:y2,12)
      real lrtssx(maxlrsegs,y1:y2,12)
      real lrnh3x(maxlrsegs,y1:y2,12)
      real lrno3x(maxlrsegs,y1:y2,12)
      real lrornx(maxlrsegs,y1:y2,12)
      real lrpo4x(maxlrsegs,y1:y2,12)
      real lrorpx(maxlrsegs,y1:y2,12)
      real lrdoxx(maxlrsegs,y1:y2,12)
      real lrorcx(maxlrsegs,y1:y2,12)

      real facQ,facN,facP,facS

      real olrflow(maxlrsegs,y1:y2,12)
      real olrheat(maxlrsegs,y1:y2,12)
      real olrbodx(maxlrsegs,y1:y2,12)
      real olrtssx(maxlrsegs,y1:y2,12)
      real olrnh3x(maxlrsegs,y1:y2,12)
      real olrno3x(maxlrsegs,y1:y2,12)
      real olrornx(maxlrsegs,y1:y2,12)
      real olrpo4x(maxlrsegs,y1:y2,12)
      real olrorpx(maxlrsegs,y1:y2,12)
      real olrdoxx(maxlrsegs,y1:y2,12)
      real olrorcx(maxlrsegs,y1:y2,12)

*************** variables for CSOs
      integer nCSOlrsegs,nCSOlrseg   !land to water combinations
             ! indexed daily
      real CSOflow(maxCSOlrsegs,ndaymax)
      real CSOheat(maxCSOlrsegs,ndaymax)
      real CSObodx(maxCSOlrsegs,ndaymax)
      real CSOtssx(maxCSOlrsegs,ndaymax)
      real CSOnh3x(maxCSOlrsegs,ndaymax)
      real CSOno3x(maxCSOlrsegs,ndaymax)
      real CSOornx(maxCSOlrsegs,ndaymax)
      real CSOpo4x(maxCSOlrsegs,ndaymax)
      real CSOorpx(maxCSOlrsegs,ndaymax)
      real CSOdoxx(maxCSOlrsegs,ndaymax)
      real CSOorcx(maxCSOlrsegs,ndaymax)

      real oCSOflow(maxCSOlrsegs,ndaymax)
      real oCSOheat(maxCSOlrsegs,ndaymax)
      real oCSObodx(maxCSOlrsegs,ndaymax)
      real oCSOtssx(maxCSOlrsegs,ndaymax)
      real oCSOnh3x(maxCSOlrsegs,ndaymax)
      real oCSOno3x(maxCSOlrsegs,ndaymax)
      real oCSOornx(maxCSOlrsegs,ndaymax)
      real oCSOpo4x(maxCSOlrsegs,ndaymax)
      real oCSOorpx(maxCSOlrsegs,ndaymax)
      real oCSOdoxx(maxCSOlrsegs,ndaymax)
      real oCSOorcx(maxCSOlrsegs,ndaymax)

**************** variables related to wqm or hydro cells
      integer n57cells  ! total # of cells
      integer n57cell   ! index of current cell
      integer lencell

      real flow57(maxcells,y1:y2,12)   !indexed monthly
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

************* variables for the CSOs for the wqmcells
      integer nCSOcells,nCSOcell 
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

************** utility variables
      integer ny,nm,nd,nday
      integer nc 
      integer WWTPtnerr,WWTPtperr
      integer INDtnerr,INDtperr
      integer CSOtnerr,CSOtperr
      logical found,foundCSO,foundPS

**************** END DECLARATIONS **************************************

******** get input data
      read*, psscen,dataversion,WWTPfnam,INDfnam,CSOfnam
     .       ,SPARROW_WWTPfnam,SPARROW_INDfnam,SPARROW_CSOfnam
     .       ,CSOPassThrufnam
      call lencl(psscen,lenpsscen)
      call lencl(dataversion,lendv)
      call lencl(WWTPfnam,lenWWTPfnam)
      call lencl(INDfnam,lenINDfnam)
      call lencl(CSOfnam,lenCSOfnam)
      call lencl(wwtp,lenwwtp)
      call lencl(indus,lenindus)
      call lencl(cso,lencso)
      
**************  intitialize cell variables **************
      do nc = 1,maxcells
        do ny = y1,y2
          do nm = 1,12
            flow57(nc,ny,nm) = 0.0
            heat57(nc,ny,nm) = 0.0
            bodx57(nc,ny,nm) = 0.0
            tssx57(nc,ny,nm) = 0.0
            nh3x57(nc,ny,nm) = 0.0
            no3x57(nc,ny,nm) = 0.0
            ornx57(nc,ny,nm) = 0.0
            po4x57(nc,ny,nm) = 0.0
            orpx57(nc,ny,nm) = 0.0
            doxx57(nc,ny,nm) = 0.0
            orcx57(nc,ny,nm) = 0.0
          end do
        end do
      end do

      do nc = 1,maxCSOcells
        do nd = 1,ndaymax
          CSOcellflow(nc,nd) = 0.0
          CSOcellheat(nc,nd) = 0.0
          CSOcellbodx(nc,nd) = 0.0
          CSOcelltssx(nc,nd) = 0.0
          CSOcellnh3x(nc,nd) = 0.0
          CSOcellno3x(nc,nd) = 0.0
          CSOcellornx(nc,nd) = 0.0
          CSOcellpo4x(nc,nd) = 0.0
          CSOcellorpx(nc,nd) = 0.0
          CSOcelldoxx(nc,nd) = 0.0
          CSOcellorcx(nc,nd) = 0.0
        end do
      end do

********* open the dummy wdm to appease the HSPF gremlins
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)  ! open junk wdm
      if (err.ne.0) go to 990

******************** WWTP DATA *************************************
**************  intitialize variables
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

            olrflow(ns,ny,nm) = 0.0
            olrheat(ns,ny,nm) = 0.0
            olrbodx(ns,ny,nm) = 0.0
            olrtssx(ns,ny,nm) = 0.0
            olrnh3x(ns,ny,nm) = 0.0
            olrno3x(ns,ny,nm) = 0.0
            olrornx(ns,ny,nm) = 0.0
            olrpo4x(ns,ny,nm) = 0.0
            olrorpx(ns,ny,nm) = 0.0
            olrdoxx(ns,ny,nm) = 0.0
            olrorcx(ns,ny,nm) = 0.0
          end do
        end do
      end do

************* open rile to read in data
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//WWTPfnam(:lenWWTPfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      print*,'reading point source data from'
      print*,longfnam
      if (err.ne.0) go to 991

      read(dfile,*)longline  ! ditch header row

      WWTPtnerr=0
      WWTPtperr=0

************ loop over all lines and populate variables
      call getSPARROWfacCOMID(SPARROW_WWTPfnam,SPARROW_WWTPfactors)
      nlrsegs = 0
      n57cells = 0
      do
        read(dfile,'(a500)',end = 111,err=996)longline
        call d2x(longline,last)
        read(longline,*,end=792,err=792)
     .                             SigInsig, SourceType,
     .                             rseg,lseg,comid,comidcb,
     .                             Tcell,Tfac,TdisPoint,Tfips,
     .                             ny,nm,nd,
     .                             Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                             Ndum,Tpo4,Torp,Pdum,Ttss
        if(rseg(9:13).eq.'_0000' .and. Tcell.eq.'nocell') goto 9933
        if(rseg(9:13).ne.'_0000' .and. Tcell.ne.'nocell') goto 9934

************* get SPARROW stream to river factors
         facQ = SPARROW_WWTPfactors(comidcb,1)
         facN = SPARROW_WWTPfactors(comidcb,2)
         facP = SPARROW_WWTPfactors(comidcb,3)
         facS = SPARROW_WWTPfactors(comidcb,4)

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) WWTPtperr=WWTPtperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) 
     .      WWTPtnerr=WWTPtnerr+1

*************** find lrseg index if already exists, otherwise create new
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
          if (w57cell(n57cell) .eq. Tcell) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          n57cells = n57cells + 1
          if (n57cells .gt. maxcells) go to 9931
          n57cell = n57cells
          w57cell(n57cell) = Tcell
        end if

************** add this line to lrseg variables
        do ny = y1,y2  ! start loop that copies to all years
          lrflow(nlrseg,ny,nm) = lrflow(nlrseg,ny,nm)+facQ*Tflow*mg2acft
          lrheat(nlrseg,ny,nm) = lrheat(nlrseg,ny,nm)
     .                         + facQ*Tflow*mg2acft*tempC(nm)*acftC2heat
          lrdoxx(nlrseg,ny,nm) = lrdoxx(nlrseg,ny,nm) + Tdox
          lrnh3x(nlrseg,ny,nm) = lrnh3x(nlrseg,ny,nm) + facN*Tnh3
          lrno3x(nlrseg,ny,nm) = lrno3x(nlrseg,ny,nm) + facN*Tno3
          lrpo4x(nlrseg,ny,nm) = lrpo4x(nlrseg,ny,nm) + facP*Tpo4

          ! for organics, calculate the labile portion and compare it
          !  to the total organic, if labile is greater than total,
          !  reset BOD so that labile is equal to total for the most limiting nutrient
          F_FAC = min(facN,facP)
          F_BOD = min(facN*Torn/bod2lorn,facP*Torp/bod2lorp,F_FAC*Tbod)
          lrornx(nlrseg,ny,nm) = lrornx(nlrseg,ny,nm)
     .                           + facN*Torn-F_BOD*bod2lorn
          lrorpx(nlrseg,ny,nm) = lrorpx(nlrseg,ny,nm)
     .                           + facP*Torp-F_BOD*bod2lorp
          lrorcx(nlrseg,ny,nm) = lrorcx(nlrseg,ny,nm)
     .                         + (facN*Torn-F_BOD*bod2lorn)*rorn2rorc
          lrbodx(nlrseg,ny,nm) = lrbodx(nlrseg,ny,nm) + F_BOD
          lrtssx(nlrseg,ny,nm) = lrtssx(nlrseg,ny,nm)
C     .                         + max(0.0,Ttss-Tbod*bod2tss)
     .                         + facS*Ttss

          olrflow(nlrseg,ny,nm) = olrflow(nlrseg,ny,nm)+Tflow*mg2acft
          olrheat(nlrseg,ny,nm) = olrheat(nlrseg,ny,nm)
     .                         + Tflow*mg2acft*tempC(nm)*acftC2heat
          olrdoxx(nlrseg,ny,nm) = olrdoxx(nlrseg,ny,nm) + Tdox
          olrnh3x(nlrseg,ny,nm) = olrnh3x(nlrseg,ny,nm) + Tnh3
          olrno3x(nlrseg,ny,nm) = olrno3x(nlrseg,ny,nm) + Tno3
          olrpo4x(nlrseg,ny,nm) = olrpo4x(nlrseg,ny,nm) + Tpo4

          F_BOD = min(Torn/bod2lorn,Torp/bod2lorp,F_FAC*Tbod)
          olrornx(nlrseg,ny,nm) = olrornx(nlrseg,ny,nm)
     .                           + Torn-F_BOD*bod2lorn
          olrorpx(nlrseg,ny,nm) = olrorpx(nlrseg,ny,nm)
     .                           + Torp-F_BOD*bod2lorp
          olrorcx(nlrseg,ny,nm) = olrorcx(nlrseg,ny,nm)
     .                         + (Torn-F_BOD*bod2lorn)*rorn2rorc
          olrbodx(nlrseg,ny,nm) = olrbodx(nlrseg,ny,nm) + F_BOD
          olrtssx(nlrseg,ny,nm) = olrtssx(nlrseg,ny,nm)
C     .                         + max(0.0,Ttss-Tbod*bod2tss)
     .                         + Ttss

************** add this line to the 57k bay model variables
          flow57(n57cell,ny,nm) = flow57(n57cell,ny,nm) 
     .                            + facQ*Tflow*mg2acft
          heat57(n57cell,ny,nm) = heat57(n57cell,ny,nm)
     .                         + facQ*Tflow*mg2acft*tempC(nm)*acftC2heat
          doxx57(n57cell,ny,nm) = doxx57(n57cell,ny,nm) + Tdox
          nh3x57(n57cell,ny,nm) = nh3x57(n57cell,ny,nm) + facN*Tnh3
          no3x57(n57cell,ny,nm) = no3x57(n57cell,ny,nm) + facN*Tno3
          po4x57(n57cell,ny,nm) = po4x57(n57cell,ny,nm) + facP*Tpo4

          F_FAC = min(facN,facP)
          F_BOD = min(facN*Torn/bod2lorn,facP*Torp/bod2lorp,F_FAC*Tbod)
          ornx57(n57cell,ny,nm) = ornx57(n57cell,ny,nm)
     .                          + facN*Torn-F_BOD*bod2lorn
          orpx57(n57cell,ny,nm) = orpx57(n57cell,ny,nm)
     .                          + facP*Torp-F_BOD*bod2lorp
          orcx57(n57cell,ny,nm) = orcx57(n57cell,ny,nm)
     .                          + (facN*Torn-F_BOD*bod2lorn) * rorn2rorc
          bodx57(n57cell,ny,nm) = bodx57(n57cell,ny,nm) + F_BOD
          tssx57(n57cell,ny,nm) = tssx57(n57cell,ny,nm)
C     .                          + max(0.0,Ttss-Tbod*bod2tss)
     .                         + facS*Ttss

        end do  ! end loop that copies to all years
      end do
111   close(dfile)
      
************ Write out list of LRsegs to update GEO file
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//wwtp(:lenwwtp)//'_lrsegs.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing out lrsegs'
      print*,longfnam

      write(dfile,*) 'point source,', 'river,', 'land'
      do nlrseg = 1,nlrsegs
        call trims (lrseg(nlrseg),last)
        write(dfile,*) 'wwtp,',lrseg(nlrseg)(7:last),',',
     .                 lrseg(nlrseg)(:6)
      end do

      close(dfile)

      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//wwtp(:lenwwtp)//'_lrsegs_ann.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing annual lrsegs'
      print*,longfnam
      write(dfile,1230)'psr,','river_segment',',','lndseg',',','year',
     .     'flow',
     .     'heat','tssx','nh3x','no3x','ornx','po4x',
     .     'orpx','bodx','doxx','orcx'

*********** open all of the lrseg wdms and populate them
      do nlrseg = 1,nlrsegs   ! loop over all Rseg/Lseg pairs

************ open the WDM
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/'//wwtp(:lenwwtp)//'_'//lrseg(nlrseg)(:6)//'_to_'//
     .            lrseg(nlrseg)(7:19)//'.wdm'
        print*,nlrseg,' of ',nlrsegs,' opening ',wdmfnam(:79)

        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 994

*********** create the time series
        nvals = 0
        do ny = y1,y2

          annflow = 0.0
          annheat = 0.0
          anntssx = 0.0
          annnh3x = 0.0
          annno3x = 0.0
          annorgn = 0.0
          annpo4x = 0.0
          annorpx = 0.0
          annbodx = 0.0
          anndoxx = 0.0
          annorcx = 0.0

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

              annflow = annflow + dailyflow(nvals)
              annheat = annheat + dailyheat(nvals)
              anntssx = anntssx + dailytssx(nvals)
              annnh3x = annnh3x + dailynh3x(nvals)
              annno3x = annno3x + dailyno3x(nvals)
              annorgn = annorgn + dailyornx(nvals)
              annpo4x = annpo4x + dailypo4x(nvals)
              annorpx = annorpx + dailyorpx(nvals)
              annbodx = annbodx + dailybodx(nvals)
              anndoxx = anndoxx + dailydoxx(nvals)
              annorcx = annorcx + dailyorcx(nvals)
            end do
          end do
          write(dfile,1234),'mun,',lrseg(nlrseg)(7:last),',',
     .     lrseg(nlrseg)(:6),',',ny,annflow,annheat,
     .     anntssx,annnh3x,annno3x,annorgn,annpo4x,
     .     annorpx,annbodx,anndoxx,annorcx
        end do

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
      close(dfile)

********************************************************************
******************** INDUSTRIAL DATA *******************************
**************  intitialize variables
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

            olrflow(ns,ny,nm) = 0.0
            olrheat(ns,ny,nm) = 0.0
            olrbodx(ns,ny,nm) = 0.0
            olrtssx(ns,ny,nm) = 0.0
            olrnh3x(ns,ny,nm) = 0.0
            olrno3x(ns,ny,nm) = 0.0
            olrornx(ns,ny,nm) = 0.0
            olrpo4x(ns,ny,nm) = 0.0
            olrorpx(ns,ny,nm) = 0.0
            olrdoxx(ns,ny,nm) = 0.0
            olrorcx(ns,ny,nm) = 0.0
          end do
        end do
      end do

************** open file to read in INDUSTIRAL data, following WWTP
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//INDfnam(:lenINDfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading point source data from'
      print*,longfnam

      read(dfile,*) longline  ! ditch header row

      INDtnerr=0
      INDtperr=0

************ loop over all lines and populate variables
      call getSPARROWfacCOMID(SPARROW_INDfnam,SPARROW_INDfactors)
      nlrsegs = 0
      do
        read(dfile,'(a500)',end=222,err=996) longline
        call d2x(longline,last)
        read(longline,*,end=892,err=892)
     .                                  SigInsig, SourceType,
     .                                  rseg,lseg,comid,comidcb,Tcell,
     .                                  Tfac,TdisPoint,Tfips,
     .                                  ny,nm,nd,
     .                                  Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                                  Ndum,Tpo4,Torp,Pdum,Ttss
        if(rseg(9:13).eq.'_0000' .and. Tcell.eq.'nocell') goto 9933
        if(rseg(9:13).ne.'_0000' .and. Tcell.ne.'nocell') goto 9934

************* get SPARROW stream to river factors
         facQ = SPARROW_INDfactors(comidcb,1)
         facN = SPARROW_INDfactors(comidcb,2)
         facP = SPARROW_INDfactors(comidcb,3)
         facS = SPARROW_INDfactors(comidcb,4)

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) then
            INDtperr=INDtperr+1
            print*,'ERR',Tfac,TdisPoint,Tfips,ny,nm,nd
        end if
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) then
            INDtnerr=INDtnerr+1
            print*,'ERR',Tfac,TdisPoint,Tfips,ny,nm,nd
        end if

*************** find lrseg index if already exists, otherwise create new
        found = .false.                     ! DON'T reset nlrsegs
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
        do n57cell = 1,n57cells                   ! DON'T reset n57cells
          if (w57cell(n57cell) .eq. Tcell) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          n57cells = n57cells + 1
          if (n57cells.gt.maxcells) go to 9931
          n57cell = n57cells
          w57cell(n57cell) = Tcell
        end if

************** add this line to lrseg variables
        do ny = y1,y2  ! start loop that copies to all years
          lrflow(nlrseg,ny,nm) = lrflow(nlrseg,ny,nm) 
     .                          + facQ*Tflow*mg2acft
          lrheat(nlrseg,ny,nm) = lrheat(nlrseg,ny,nm)
     .                         + facQ*Tflow*mg2acft*tempC(nm)*acftC2heat
          lrdoxx(nlrseg,ny,nm) = lrdoxx(nlrseg,ny,nm) + Tdox
          lrnh3x(nlrseg,ny,nm) = lrnh3x(nlrseg,ny,nm) + facN*Tnh3
          lrno3x(nlrseg,ny,nm) = lrno3x(nlrseg,ny,nm) + facN*Tno3
          lrpo4x(nlrseg,ny,nm) = lrpo4x(nlrseg,ny,nm) + facP*Tpo4

          ! for organics, calculate the labile portion and compare it
          !  to the total organic, if labile is greater than total,
          !  reset BOD so that labile is equal to total for the most limiting nutrient
          F_FAC = min(facN,facP)
          F_BOD = min(facN*Torn/bod2lorn,facP*Torp/bod2lorp,F_FAC*Tbod)
          lrornx(nlrseg,ny,nm) = lrornx(nlrseg,ny,nm)
     .                          + facN*Torn-F_BOD*bod2lorn
          lrorpx(nlrseg,ny,nm) = lrorpx(nlrseg,ny,nm)
     .                          + facP*Torp-F_BOD*bod2lorp
          lrorcx(nlrseg,ny,nm) = lrorcx(nlrseg,ny,nm)
     .                         + (facN*Torn-F_BOD*bod2lorn)*rorn2rorc
          lrbodx(nlrseg,ny,nm) = lrbodx(nlrseg,ny,nm) + F_BOD
          lrtssx(nlrseg,ny,nm) = lrtssx(nlrseg,ny,nm)
C     .                         + max(0.0,Ttss-Tbod*bod2tss)
     .                         + facS*Ttss

          olrflow(nlrseg,ny,nm) = olrflow(nlrseg,ny,nm) + Tflow*mg2acft
          olrheat(nlrseg,ny,nm) = olrheat(nlrseg,ny,nm)
     .                         + Tflow*mg2acft*tempC(nm)*acftC2heat
          olrdoxx(nlrseg,ny,nm) = olrdoxx(nlrseg,ny,nm) + Tdox
          olrnh3x(nlrseg,ny,nm) = olrnh3x(nlrseg,ny,nm) + Tnh3
          olrno3x(nlrseg,ny,nm) = olrno3x(nlrseg,ny,nm) + Tno3
          olrpo4x(nlrseg,ny,nm) = olrpo4x(nlrseg,ny,nm) + Tpo4

          F_BOD = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          olrornx(nlrseg,ny,nm) = olrornx(nlrseg,ny,nm)
     .                          + Torn-F_BOD*bod2lorn
          olrorpx(nlrseg,ny,nm) = olrorpx(nlrseg,ny,nm)
     .                          + Torp-F_BOD*bod2lorp
          olrorcx(nlrseg,ny,nm) = olrorcx(nlrseg,ny,nm)
     .                          + (Torn-F_BOD*bod2lorn)*rorn2rorc
          olrbodx(nlrseg,ny,nm) = olrbodx(nlrseg,ny,nm) + F_BOD
          olrtssx(nlrseg,ny,nm) = olrtssx(nlrseg,ny,nm)
C     .                         + max(0.0,Ttss-Tbod*bod2tss)
     .                         + Ttss

************** add this line to the 57k bay model variables, WWTP+IND
          flow57(n57cell,ny,nm) = flow57(n57cell,ny,nm) 
     .                           + facQ*Tflow*mg2acft   ! add WWTP,IND for the same cell
          heat57(n57cell,ny,nm) = heat57(n57cell,ny,nm)
     .                          +facQ*Tflow*mg2acft*tempC(nm)*acftC2heat
          doxx57(n57cell,ny,nm) = doxx57(n57cell,ny,nm) + Tdox
          nh3x57(n57cell,ny,nm) = nh3x57(n57cell,ny,nm) + facN*Tnh3
          no3x57(n57cell,ny,nm) = no3x57(n57cell,ny,nm) + facN*Tno3
          po4x57(n57cell,ny,nm) = po4x57(n57cell,ny,nm) + facP*Tpo4

          F_FAC = min(facN,facP)
          F_BOD = min(facN*Torn/bod2lorn,facP*Torp/bod2lorp,F_FAC*F_BOD)
          ornx57(n57cell,ny,nm) = ornx57(n57cell,ny,nm)
     .                          + facN*Torn-F_BOD*bod2lorn
          orpx57(n57cell,ny,nm) = orpx57(n57cell,ny,nm)
     .                          + facP*Torp-F_BOD*bod2lorp
          orcx57(n57cell,ny,nm) = orcx57(n57cell,ny,nm)
     .                          + (facN*Torn-F_BOD*bod2lorn) * rorn2rorc
          bodx57(n57cell,ny,nm) = bodx57(n57cell,ny,nm) + F_BOD
          tssx57(n57cell,ny,nm) = tssx57(n57cell,ny,nm)
C     .                          + max(0.0,Ttss-Tbod*bod2tss)
     .                         + facS*Ttss

        end do     ! end loop copy to all years

      end do       ! end loop all segments 

222   close(dfile)

************ Write out list of LRsegs to update GEO file 
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//indus(:lenindus)//'_lrsegs.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing out lrsegs'
      print*,longfnam

      write(dfile,*) 'point source,', 'river,', 'land'
      do nlrseg = 1,nlrsegs
        call trims (lrseg(nlrseg),last)
        write(dfile,*) 'indus,',lrseg(nlrseg)(7:last),',',
     .                 lrseg(nlrseg)(:6)
      end do

      close(dfile)

      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//indus(:lenindus)//'_lrsegs_ann.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing annual lrsegs'
      print*,longfnam
      write(dfile,1230)'psr,','river_segment',',','lndseg',',','year',
     .     'flow',
     .     'heat','tssx','nh3x','no3x','ornx','po4x',
     .     'orpx','bodx','doxx','orcx'

*********** open all of the lrseg wdms and populate them
      do nlrseg = 1,nlrsegs   ! loop over all Rseg/Lseg pairs

************ open the WDM
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/'//indus(:lenindus)//'_'//lrseg(nlrseg)(:6)//
     .            '_to_'//lrseg(nlrseg)(7:19)//'.wdm'
        print*,nlrseg,' of ',nlrsegs,' opening ',wdmfnam(:79)

        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 994

*********** create the time series
        nvals = 0
        do ny = y1,y2

          annflow = 0.0
          annheat = 0.0
          anntssx = 0.0
          annnh3x = 0.0
          annno3x = 0.0
          annorgn = 0.0
          annpo4x = 0.0
          annorpx = 0.0
          annbodx = 0.0
          anndoxx = 0.0
          annorcx = 0.0

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

              annflow = annflow + dailyflow(nvals)
              annheat = annheat + dailyheat(nvals)
              anntssx = anntssx + dailytssx(nvals)
              annnh3x = annnh3x + dailynh3x(nvals)
              annno3x = annno3x + dailyno3x(nvals)
              annorgn = annorgn + dailyornx(nvals)
              annpo4x = annpo4x + dailypo4x(nvals)
              annorpx = annorpx + dailyorpx(nvals)
              annbodx = annbodx + dailybodx(nvals)
              anndoxx = anndoxx + dailydoxx(nvals)
              annorcx = annorcx + dailyorcx(nvals)
            end do
          end do

          write(dfile,1234),'ind,',lrseg(nlrseg)(7:last),',',
     .     lrseg(nlrseg)(:6),',',ny,annflow,annheat,
     .     anntssx,annnh3x,annno3x,annorgn,annpo4x,
     .     annorpx,annbodx,anndoxx,annorcx

        end do

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
      close(dfile)

************************************************************************
********************  CSO DATA *****************************************
*************** initilize variables
      do ns = 1,maxCSOlrsegs
        do nd = 1,ndaymax
          CSOflow(ns,nd) = 0.0
          CSOheat(ns,nd) = 0.0
          CSObodx(ns,nd) = 0.0
          CSOtssx(ns,nd) = 0.0
          CSOnh3x(ns,nd) = 0.0
          CSOno3x(ns,nd) = 0.0
          CSOornx(ns,nd) = 0.0
          CSOpo4x(ns,nd) = 0.0
          CSOorpx(ns,nd) = 0.0
          CSOdoxx(ns,nd) = 0.0
          CSOorcx(ns,nd) = 0.0

          oCSOflow(ns,nd) = 0.0
          oCSOheat(ns,nd) = 0.0
          oCSObodx(ns,nd) = 0.0
          oCSOtssx(ns,nd) = 0.0
          oCSOnh3x(ns,nd) = 0.0
          oCSOno3x(ns,nd) = 0.0
          oCSOornx(ns,nd) = 0.0
          oCSOpo4x(ns,nd) = 0.0
          oCSOorpx(ns,nd) = 0.0
          oCSOdoxx(ns,nd) = 0.0
          oCSOorcx(ns,nd) = 0.0
        end do
      end do

************* read in CSO data
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//CSOfnam(:lenCSOfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading CSO data from'
      print*,longfnam

      read(dfile,*)longline  ! ditch header row

      CSOtnerr=0
      CSOtperr=0
************ loop over all lines and populate variables
      call getSPARROWfacCOMID(SPARROW_CSOfnam,SPARROW_CSOfactors)
      call getCSOPassThru(CSOPassThrufnam,CSOPassThru)
      nCSOlrsegs = 0
      nCSOcells = 0
      do
        read(dfile,'(a500)',end=333,err=996)longline
        call d2x(longline,last)
        read(longline,*,end=992,err=992)
     .                             rseg,lseg,cbcsoid,
     .                             Tcell,Tfac,TdisPoint,Tfips,
     .                             ny,nm,nd,
     .                             Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                             Ndum,Tpo4,Torp,Pdum,Ttss
        if (ny.lt.y1.or.ny.gt.y2) cycle  ! only process within years
        if(rseg(9:13).eq.'_0000' .and. Tcell.eq.'nocell') goto 9933
        if(rseg(9:13).ne.'_0000' .and. Tcell.ne.'nocell') goto 9934
c        if(rseg(9:13).eq.'_0000' .and. Tcell.eq.'nocell') 
c     .        print*,longline

************* get SPARROW stream to river factors
c         facQ = SPARROW_CSOfactors(comidcb,1)
c         facN = SPARROW_CSOfactors(comidcb,2)
c         facP = SPARROW_CSOfactors(comidcb,3)
c         facS = SPARROW_CSOfactors(comidcb,4)

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) CSOtperr=CSOtperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) CSOtnerr=CSOtnerr+1

************* check for all zeros
        if (Tflow+Tbod+Tdox+Tnh3+Tno3+Torn+Tpo4+Torp+Ttss.lt.0.01) cycle


************ Apply CSO Pass Thru #2 or Holding Threshold
        F_holdfac = 1.0
        if ( CSOPassThru(cbcsoid,4) .gt. 0 ) then
           F_holdfac = (Tflow - CSOPassThru(cbcsoid,4)) / Tflow
           if ( F_holdfac .lt. 0 ) F_holdfac = 0.0
        end if
************ Apply CSO Pass Thru #1
        Tflow = Tflow * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Tbod  = Tbod  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Tdox  = Tdox  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Tnh3  = Tnh3  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Tno3  = Tno3  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Torn  = Torn  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Tpo4  = Tpo4  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Torp  = Torp  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac
        Ttss  = Ttss  * CSOPassThru(cbcsoid,1) * CSOPassThru(cbcsoid,3)
     .                * F_holdfac

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
          if (nCSOlrsegs.gt.maxCSOlrsegs) go to 993
          nCSOlrseg = nCSOlrsegs
          CSOlrseg(nCSOlrseg) = lseg//rseg
        end if

*************** find w57cell index if already exists, otherwise create new
        found = .false.
        do nCSOcell = 1,nCSOcells
          if (CSOcell(nCSOcell).eq.Tcell) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nCSOcells = nCSOcells + 1
          if (nCSOcells.gt.maxCSOcells) go to 9932
          nCSOcell = nCSOcells
          CSOcell(nCSOcell) = Tcell
        end if

************ get day
c        do ny = y1,y2    ! copy the scanrio data to all years
        !{
          jday = julian(sdate(1),sdate(2),sdate(3),ny,nm,nd)

************** add this line to lrseg variables
          CSOflow(nCSOlrseg,jday) = CSOflow(nCSOlrseg,jday)
     .                              + Tflow*mg2acft
          CSOheat(nCSOlrseg,jday) = CSOheat(nCSOlrseg,jday)
     .                              + Tflow*mg2acft*tempC(nm)*acftC2heat
          CSOdoxx(nCSOlrseg,jday) = CSOdoxx(nCSOlrseg,jday) + Tdox
          CSOnh3x(nCSOlrseg,jday) = CSOnh3x(nCSOlrseg,jday) + Tnh3
          CSOno3x(nCSOlrseg,jday) = CSOno3x(nCSOlrseg,jday) + Tno3
          CSOpo4x(nCSOlrseg,jday) = CSOpo4x(nCSOlrseg,jday) + Tpo4

         ! for organics, calculate the labile portion and compare it
         !  to the total organic, if labile is greater than total,
         !  reset BOD so that labile is equal to total for the most limiting nutrient
          Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          CSOornx(nCSOlrseg,jday) = CSOornx(nCSOlrseg,jday)
     .                            + Torn-Tbod*bod2lorn
          CSOorpx(nCSOlrseg,jday) = CSOorpx(nCSOlrseg,jday)
     .                            + Torp-Tbod*bod2lorp
          CSOorcx(nCSOlrseg,jday) = CSOorcx(nCSOlrseg,jday)
     .                            + (Torn-Tbod*bod2lorn) * rorn2rorc
          CSObodx(nCSOlrseg,jday) = CSObodx(nCSOlrseg,jday) + Tbod
          CSOtssx(nCSOlrseg,jday) = CSOtssx(nCSOlrseg,jday)
C     .                            + max(0.0,Ttss-Tbod*bod2tss)
     .                         + Ttss

************** add this line to cell variables
          CSOcellflow(nCSOcell,jday) = CSOcellflow(nCSOcell,jday)
     .                               + Tflow*mg2acft
          CSOcellheat(nCSOcell,jday) = CSOcellheat(nCSOcell,jday)
     .                              + Tflow*mg2acft*tempC(nm)*acftC2heat
          CSOcelldoxx(nCSOcell,jday) = CSOcelldoxx(nCSOcell,jday) + Tdox
          CSOcellnh3x(nCSOcell,jday) = CSOcellnh3x(nCSOcell,jday) + Tnh3
          CSOcellno3x(nCSOcell,jday) = CSOcellno3x(nCSOcell,jday) + Tno3
          CSOcellpo4x(nCSOcell,jday) = CSOcellpo4x(nCSOcell,jday) + Tpo4

          Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          CSOcellornx(nCSOcell,jday) = CSOcellornx(nCSOcell,jday)
     .                               + Torn-Tbod*bod2lorn
          CSOcellorpx(nCSOcell,jday) = CSOcellorpx(nCSOcell,jday)
     .                               + Torp-Tbod*bod2lorp
          CSOcellorcx(nCSOcell,jday) = CSOcellorcx(nCSOcell,jday)
     .                               + (Torn-Tbod*bod2lorn) * rorn2rorc
          CSOcellbodx(nCSOcell,jday) = CSOcellbodx(nCSOcell,jday) + Tbod
          CSOcelltssx(nCSOcell,jday) = CSOcelltssx(nCSOcell,jday)
C     .                               + max(0.0,Ttss-Tbod*bod2tss)
     .                         + Ttss
        !}
c        end do  ! end loop that copies to all years

      end do    ! end loop over all segments

333   close(dfile)

************ Write out list of LRsegs to update GEO file
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//cso(:lencso)//'_lrsegs.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing out lrsegs'
      print*,longfnam

      write(dfile,*) 'point source,', 'river,', 'land'
      do nCSOlrseg = 1,nCSOlrsegs
        call trims (CSOlrseg(nCSOlrseg),last)
        write(dfile,*) 'cso,',CSOlrseg(nCSOlrseg)(7:last),',',
     .                 CSOlrseg(nCSOlrseg)(:6)
      end do

      close(dfile)

      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/'//cso(:lencso)//'_lrsegs_ann.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing annual lrsegs'
      print*,longfnam
      write(dfile,1230)'psr,','river_segment',',','lndseg',',','year',
     .     'flow',
     .     'heat','tssx','nh3x','no3x','ornx','po4x',
     .     'orpx','bodx','doxx','orcx'


*********** open CSO wdms and populate them
      do nCSOlrseg = 1,nCSOlrsegs                             ! loop over all Rseg/Lseg pairs
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/'//cso(:lencso)//'_'//CSOlrseg(nCSOlrseg)(:6)//
     .            '_to_'//CSOlrseg(nCSOlrseg)(7:19)//'.wdm'
        print*,nCSOlrseg,' of ',nCSOlrsegs,' CSO segs'
        print*,'opening ',wdmfnam(:79)

        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 994

        nvals = julian(sdate(1),sdate(2),sdate(3),    ! set nvals
     .               edate(1),edate(2),edate(3))   ! in case no lrsegs

        do nd = 1,nvals
          dailyflow(nd) = CSOflow(nCSOlrseg,nd)
          dailyheat(nd) = CSOheat(nCSOlrseg,nd)
          dailybodx(nd) = CSObodx(nCSOlrseg,nd)
          dailytssx(nd) = CSOtssx(nCSOlrseg,nd)
          dailynh3x(nd) = CSOnh3x(nCSOlrseg,nd)
          dailyno3x(nd) = CSOno3x(nCSOlrseg,nd)
          dailyornx(nd) = CSOornx(nCSOlrseg,nd)
          dailypo4x(nd) = CSOpo4x(nCSOlrseg,nd)
          dailyorpx(nd) = CSOorpx(nCSOlrseg,nd)
          dailydoxx(nd) = CSOdoxx(nCSOlrseg,nd)
          dailyorcx(nd) = CSOorcx(nCSOlrseg,nd)
        end do

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


      do nCSOlrseg = 1,nCSOlrsegs
         nvals = 0 
         do ny = y1,y2
     
           annflow = 0.0
           annheat = 0.0
           anntssx = 0.0
           annnh3x = 0.0
           annno3x = 0.0
           annorgn = 0.0
           annpo4x = 0.0
           annorpx = 0.0
           annbodx = 0.0
           anndoxx = 0.0
           annorcx = 0.0
     
          do nm = 1,12
            do nday = 1,ndaysinmonth(ny,nm)
              nvals = nvals + 1
              dailyflow(nvals) = CSOflow(nCSOlrseg,nvals)
              dailyheat(nvals) = CSOheat(nCSOlrseg,nvals)
              dailybodx(nvals) = CSObodx(nCSOlrseg,nvals)
              dailytssx(nvals) = CSOtssx(nCSOlrseg,nvals)
              dailynh3x(nvals) = CSOnh3x(nCSOlrseg,nvals)
              dailyno3x(nvals) = CSOno3x(nCSOlrseg,nvals)
              dailyornx(nvals) = CSOornx(nCSOlrseg,nvals)
              dailypo4x(nvals) = CSOpo4x(nCSOlrseg,nvals)
              dailyorpx(nvals) = CSOorpx(nCSOlrseg,nvals)
              dailydoxx(nvals) = CSOdoxx(nCSOlrseg,nvals)
              dailyorcx(nvals) = CSOorcx(nCSOlrseg,nvals)

              annflow = annflow + dailyflow(nvals)
              annheat = annheat + dailyheat(nvals)
              anntssx = anntssx + dailytssx(nvals)
              annnh3x = annnh3x + dailynh3x(nvals)
              annno3x = annno3x + dailyno3x(nvals)
              annorgn = annorgn + dailyornx(nvals)
              annpo4x = annpo4x + dailypo4x(nvals)
              annorpx = annorpx + dailyorpx(nvals)
              annbodx = annbodx + dailybodx(nvals)
              anndoxx = anndoxx + dailydoxx(nvals)
              annorcx = annorcx + dailyorcx(nvals)
            end do
          end do

          write(dfile,1234),'cso,',lrseg(nCSOlrseg)(7:last),',',
     .     lrseg(nCSOlrseg)(:6),',',ny,annflow,annheat,
     .     anntssx,annnh3x,annno3x,annorgn,annpo4x,
     .     annorpx,annbodx,anndoxx,annorcx
        
        end do
      end do
      close(dfile)

1230  FORMAT(A4,A13,A,A6,A,A4,11(',',A14))
1234  FORMAT(A4,A13,A,A6,A,I4,11(',',E14.8))

************************************************************************
******************** CREAT WATER QUALITY MODEL WDMS ********************
************ write out list of water quality model cells
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/bay_models/WQMcells.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      print*,'writing out wqm cells'
      print*,longfnam

      do n57cell = 1,n57cells
        write(dfile,*)'point source cell,',w57cell(n57cell)
      end do

      do nCSOcell = 1,nCSOcells
        write(dfile,*)'CSO cell,',CSOcell(nCSOcell)
      end do

      close(dfile)

*********** open all of the bay wdms and populate them
      do n57cell = 1,n57cells   ! loop over all cells

        call lencl(w57cell(n57cell),lencell)
        wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .            '/bay_models/ps_wqm57k_'//w57cell(n57cell)(:lencell)//
     .            '.wdm'

        command = 'cp -v '//tree//
     .            'config/blank_wdm/blank_pointsource.wdm '//wdmfnam
        call system(command)
        print*,n57cell,' of ',n57cells,' opening ',wdmfnam(:79)
        call wdbopnlong(wdmfil,wdmfnam,0,err)
        if (err.ne.0) go to 994

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
        if (err.ne.0) go to 994

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
      if ( WWTPtnerr+WWTPtperr .gt. 0 )
     .  print*,'there were ',WWTPtnerr,' TN errors and '
     .        ,WWTPtperr,' TP errors in the WWTP file'
      if ( INDtnerr+INDtperr .gt. 0 )
     .  print*,'there were ',INDtnerr,' TN errors and ',
     .       INDtperr,' TP errors in the IND file'
      if ( CSOtnerr+CSOtperr .gt. 0 )
     .  print*,'there were ',CSOtnerr,' TN errors and ',
     .       CSOtperr,' TP errors in the CSO file'
      if (WWTPtnerr+WWTPtperr+INDtnerr+INDtperr+CSOtnerr+CSOtperr.eq.0)
     .    print*,'success!'
      print*,'  '

      stop

************************* ERROR SPACE **********************************
951   report(1) = 'error writing to file'
      report(2) = longfnam
      report(3) = 'possible permission problem'
      go to 999

990   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

991   report(1) = 'could not open file'
      report(2) = longfnam
      report(3) = ' '
      print*,longfnam
      go to 999
792   report(1) = 'problem reading file:  near line: could not parse'
      report(2) = longfnam
      report(3) = longline
      print*, Tfac,nm,nd,ny,Tflow,Tbod,Tdox,Tnh3,
     .                    Tno3,Torn,Ndum,Tpo4,Torp,Pdum,'tss',Ttss
      go to 999

892   report(1) = 'problem reading file:  near line: could not parse'
      report(2) = longfnam
      report(3) = longline
      print*, Tfac,nm,nd,ny,Tflow,Tbod,Tdox,Tnh3,
     .                    Tno3,Torn,Ndum,Tpo4,Torp,Pdum,'tss',Ttss
      go to 999

992   report(1) = 'problem reading file:  near line: could not parse'
      report(2) = longfnam
      report(3) = longline
      print*, 
     .                             rseg,lseg,cbcsoid,
     .                             Tcell,Tfac,TdisPoint,Tfips,
     .                             ny,nm,nd,
     .                             Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                             Ndum,Tpo4,Torp,Pdum,Ttss
      go to 999

993   report(1) = 'more rseg lseg pairs than anticipated'
      report(2) = 'increase maxlrsegs in ./pp/src/'
      report(3) = 'calibration_utils/wdm/src/make_point_source_wdms'
      go to 999

9931  report(1) = 'more 57k cells than anticipated'
      report(2) = 'increase maxcells in ./pp/src/'
      report(3) = 'calibration_utils/wdm/src/make_point_source_wdms'
      go to 999

9932  report(1) = 'more rseg lseg pairs than anticipated in CSO file'
      report(2) = 'fix maxCSOlrsegs in ./code/src/'
      report(3) = 'data_import/point_source/'
      go to 999

9933  report(1) = 'wqm cell not defined in file'
      report(2) = longfnam
      report(3) = longline
      go to 999

9934  report(1) = 'wqm cell defined in file for non-0000 segment'
      report(2) = longfnam
      report(3) = longline
      go to 999

994   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
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

999   call stopreport(report)

      end





