      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/upstream.inc'
      include '../../lib/inc/modules.inc'
      include '../../lib/inc/masslinks.inc'

      integer upNexits,uplakeflag   ! upstream exits and lake flag
      character*1 resflag

      integer timestep

      integer nRvarIn,nRvarOut              ! number of river variables
      integer RdsnIn(maxRvar)    ! dsn for each input river variable
      integer RdsnOut(maxRvar)   ! dsn for each output river variable
      character*4 RnameIn(maxRvar) ! variable name for RdsnIn
      character*4 RnameOut(maxRvar) ! variable name for RdsnOut

