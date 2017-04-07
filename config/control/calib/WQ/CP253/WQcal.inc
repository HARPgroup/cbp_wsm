************************************************************************
** variables used frequently in the calibration parameter change prog **
************************************************************************
      character*25 calscen    ! set of specifications for this
      integer lencalscen      ! calibration run

      character*12 module  ! variable specifiers
      character*12 lmodule ! variable specifiers
      integer lenmod
      integer lenlmod

      real misval
      parameter (misval=-999.0)
      logical missing
      external missing

      integer maxitnum,itnum
      parameter (maxitnum=100)

      character*25 basin
      integer lenbasin

      integer nbinmax,nbins,nb  ! number of bins in CFD
      parameter (nbinmax=5)  ! 1 is low conc, 5 is high conc

      integer year1,year2  ! start and end years for calculation
      character*4 cy1,cy2  ! character versions

      integer lakeflags(maxrsegs)  ! lakeflag for each river

      integer goodobs(maxcsegs,nconcmax) ! number of obs above LOD

*********** starting value matrix
      real sim(maxcsegs,nconcmax,nbinmax,maxitnum)
      real obs(maxcsegs,nconcmax,nbinmax,maxitnum)
      real noObsSim(maxrsegs,nconcmax,nbinmax,maxitnum)

************* Kolmogorov-Smirnov stat
      real ksKstat(maxcsegs,nconcmax,maxitnum)

*********** parameter variables
      integer nparmax,npar,np  ! number of parameters to test
      parameter (nparmax = 100)

      integer parLKflag(nparmax)  ! lake flag
      character*6 parModule(nparmax)  ! module of this parameter
      character*14 parTable(nparmax)  ! table name
      character*10 parName(nparmax)   ! name
      character*1 parAorM(nparmax)    ! either add or multiply
      logical parUsed(nparmax)    ! does this parameter change?

      real parstart(nparmax)        ! default value
      real parmin(nparmax)          ! min value
      real parmax(nparmax)          ! max value

      real parval(nparmax,maxrsegs)             ! parameter value
      real oldparval(nparmax,maxrsegs)             ! parameter value
      


