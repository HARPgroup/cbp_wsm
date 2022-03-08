************************************************************************
************************************************************************
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/land_use.inc'
      include '../lib/inc/wdm.inc'

      integer maxTimeBreaks   ! break points for time-varying special actions
      parameter (maxTimeBreaks = 30)
      integer Byear(maxTimeBreaks)             ! year of break
      integer Bmonth(maxTimeBreaks)            ! month
      integer Bday(maxTimeBreaks)              ! day
      integer Jday(maxTimeBreaks)    ! julian day since start of sim

      integer maxspecies,nspecies,sp
      parameter (maxspecies=5)
      character*4 species(maxspecies)

      double precision annapp(1984:2005,maxspecies) !annual application

      character*30 Mfile(maxTimeBreaks)        ! manure file name
      
      
