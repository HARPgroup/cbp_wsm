      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/land_use.inc'
      include '../lib/inc/bmp_constituents.inc'
      include '../lib/inc/land_constituents.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/modules.inc'
      include '../lib/inc/masslinks.inc'

      character*25 LandScen(nlu)                ! scenario for land wdms
      integer lenls

      integer T1year,T1month,T1day              ! start time
      integer T2year,T2month,T2day              ! end time

      integer maxLv2Rv                          ! maximum number of land vars
      parameter (maxLv2Rv = 4)                  ! linked to a single river var

      integer nLvar(maxRvar,nlu) ! number of land variables for each river
                                 !    variable and land use
      integer Ldsn(maxRvar,nlu,maxLv2Rv)  ! dsn for each land variable linked
                                          !   with a river variable and land use
      character*4 Lname(maxRvar,nlu,maxLv2Rv)  ! variable name for Ldsn
      real Lfactor(maxRvar,nlu,maxLv2Rv)  ! conversion factor Ldsn
     
 
