************************************************************************
**  Include file for the land uci generator                           **
************************************************************************

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/pltgen.inc'
      include '../../lib/inc/modules.inc'
     
      character*3 clu             ! land use
      integer uci                 ! file numbers
      parameter (uci=12)

      character*25 metscen,pradscen

      integer startY,startM,startD
      integer endY,endM,endD

