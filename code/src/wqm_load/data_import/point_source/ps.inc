************************************************************************
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/ps_septic_atdep.inc' 

      character*35 psscen            ! ps data scenario for wdm location
      character*40 dataversion       ! location of raw data
      integer lenpsscen,lendv
      character*50 WWTPfnam,CSOfnam,INDfnam
      integer lenWWTPfnam,lenCSOfnam,lenINDfnam

      integer y1,y2  ! start and end year
      parameter (y1=EarliestYear, y2=LatestYear)
      integer jday

*************** WDM related variables
      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer sdate(ndate),edate(ndate)
      data sdate /y1,1,1,0,0,0/
      data edate /y2,12,31,24,0,0/

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

*************** variables for land-river segmwnts
      integer maxlrsegs
      parameter (maxlrsegs = 1200)
      character*19 lrseg(maxlrsegs)

      integer maxCSOlrsegs
C BHATT     parameter (maxCSOlrsegs = 120) 
      parameter (maxCSOlrsegs = 200)
      character*19 CSOlrseg(maxCSOlrsegs)

**************** variables related to wqm or hydro cells
      integer maxcells
      parameter (maxcells = 800)
      character*6 w57cell(maxcells)

************* variables for the CSOs for the wqmcells
      integer maxCSOcells
      parameter (maxCSOcells = 100)
      character*19 CSOcell(maxCSOcells)

*************** daily variables for each data type
      character*10 Tfac
      integer TdisPoint
      integer Tfips
      character*6 Tcell
      real dailyflow(ndaymax),Tflow
      real dailyheat(ndaymax)
      real dailybodx(ndaymax),Tbod
      real dailytssx(ndaymax),Ttss
      real dailynh3x(ndaymax),Tnh3
      real dailyno3x(ndaymax),Tno3
      real dailyornx(ndaymax),Torn
      real dailypo4x(ndaymax),Tpo4
      real dailyorpx(ndaymax),Torp
      real dailydoxx(ndaymax),Tdox
      real dailyorcx(ndaymax)
      real Ndum,Pdum  ! dummies for TN and TP
      real annflow,annheat,anntssx
      real annnh3x,annno3x,annorgn
      real annpo4x,annorpx
      real annbodx
      real anndoxx,annorcx

************** utility variables
      character*500 longline,longfnam,command
      character*2 state
      character*20 SigInsig      ! sig/insig
      character*20 SourceType    ! municipal/industrial/cso

      integer julian
      external julian

      integer ndaysinmonth
      external ndaysinmonth

      character*10 iTfac
      integer      iTdisPoint
      character*6  ilseg
      character*13 irseg

      real F_FAC
      real F_BOD

      integer cbcsoid
      integer comid,comidcb
