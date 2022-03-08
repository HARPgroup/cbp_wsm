      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      integer LBJday(maxTimeBreaks)             ! land use julian day
      character*20 LBfile(maxTimeBreaks)        ! land use files
      real LBdata(maxTimeBreaks,maxL2R,nlu)     ! land use

      character*25 LandScen(nlu)                ! scenario for land wdms

      integer T1year,T1month,T1day              ! start time
      integer T2year,T2month,T2day              ! end time

      real imppercent(ndaymax)     ! daily impervious percent

      real hrain(ndaymax*24)   ! hourly rainfall

      real hRO(ndaymax*24)    ! hourly total runoff



