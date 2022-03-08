      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/modules.inc'
      include '../lib/inc/masslinks.inc'

      integer nRvarOut          ! number of output river variables
      integer RdsnOut(maxRvar)   ! dsn for each output river variable
      character*4 RnameOut(maxRvar) ! variable name for RdsnOut
 
