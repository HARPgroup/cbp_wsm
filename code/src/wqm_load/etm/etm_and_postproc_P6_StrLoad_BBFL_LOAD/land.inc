      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/modules.inc'
      include '../../lib/inc/masslinks.inc'
      
      integer RdsnOut(maxRvar)   ! dsn for each output river variable

      integer maxcons
      parameter (maxcons = 10)     ! max constituents in a load
      integer nloadmax             !
      parameter (nloadmax = 20)    ! maximum number of loads
      integer nloads               ! number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')
      character*4 unit(nloadmax)      ! units for load (e.g. mg/l cfs )
      integer ncons(nloadmax)     ! number of constituents to each load
      character*4 con(nloadmax,maxcons) ! concentration variable to load
      real confactor(nloadmax,maxcons)  ! factor for the variable

      integer numsegs                   ! number of segments in a river

      character*25 LandScen(nlu)   ! scenario for land wdms
      integer lenls

      integer T1year,T1month,T1day    ! start time
      integer T2year,T2month,T2day    ! end time

      integer maxLv2Rv                ! maximum number of land vars
      parameter (maxLv2Rv = 4)        ! linked to a single river var

      integer nLvar(maxRvar,nlu) ! number of land vars for each river
                                 !    variable and land use
      integer Ldsn(maxRvar,nlu,maxLv2Rv)  ! dsn for each land var linked
                                          !   with a river variable 
                                          !   and land use
      character*4 Lname(maxRvar,nlu,maxLv2Rv)  ! variable name for Ldsn
      real Lfactor(maxRvar,nlu,maxLv2Rv)  ! conversion factor Ldsn

      character*3 RvarBMP(maxRvar)  ! bmp type the affects this Rvar


      integer icstr,ncstr
      parameter ( ncstr = 4 )
      character*4 ncstrname(ncstr)
      data ncstrname /'RORN','RORP','CLAY','SILT'/
      integer sYRbb,eYRbb,sYRfl,eYRfl
      real lrbedbank(maxL2R,ncstr), rbedbank(ncstr)
      real lrfloodplain(maxL2R,ncstr), rfloodplain(ncstr)
      real rbedbankload(ncstr)
      real rfloodplainload(ncstr)
      integer iPARA(ncstr)
      real hPARA(ndaymax*24,ncstr)         ! hourly values
      real rPARAload(ncstr)   ! total for river seg
      real pfac,nfac,adjfac
      real facPARAbb(ncstr),facPARAfl(ncstr)
      logical allgood

      character*4 nps(3)
      data nps /'TOTN','TOTP','TSED'/
