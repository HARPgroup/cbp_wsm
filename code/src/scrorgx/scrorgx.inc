      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/masslinks.inc'
      include '../lib/inc/rsegs.inc'

      integer nparmax,npar,np  ! number of parameters to test
      parameter (nparmax = 100)

      integer parLKflag(nparmax)  ! lake flag
      character*6 parModule(nparmax)  ! module of this parameter
      character*14 parTable(nparmax)  ! table name
      character*10 parName(nparmax)   ! name
      character*1 parAorM(nparmax)    ! either add or multiply
      logical parUsed(nparmax)    ! does this parameter change?

      integer lenmod
      integer lakeflags(maxrsegs)

      real parval(nparmax,maxrsegs)             ! parameter value


      character*200 ioline
      integer idsn, odsn

      integer ival

      integer sdate(ndate), edate(ndate)

      real tval
      real hscr(ndaymax*24)
      real horg(ndaymax*24)

      integer iy, im, id, ih
      integer irow,nrow,maxrow
      parameter (maxrow=5)
      character*10 parscr(maxrow)
      real tscr
      real annscr(1980:2020,maxrow)

      real anntssx(1980:2020)

      character*50 calscen
      integer lencalscen
      character*4 C4_scenmod
      real R_scenmod, R_tssxmod
