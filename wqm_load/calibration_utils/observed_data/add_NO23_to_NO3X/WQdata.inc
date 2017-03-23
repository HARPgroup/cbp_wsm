      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'
      include '../../../src/lib/inc/rsegs.inc'

      integer nconcmax,nconcs,nc     ! number of concs
      parameter (nconcmax = 20)      ! max number of concentrations
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')


