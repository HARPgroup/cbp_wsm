************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This is the include file                                           **
************************************************************************
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/rsegs.inc'

      integer nparmax,npar,np  ! number of parameters to test
      parameter (nparmax = 100)      

      integer nval,nv   ! number of values for each parameter
      parameter (nval = 2)      

      integer nconcmax,nconcs,nc     ! number of concentrations
      parameter (nconcmax = 20)      
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')

      integer nbinmax,nbins,nb  ! number of bins in CFD
      parameter (nbinmax=5)

      integer year1,year2  ! start and end years for calculation
      character*4 cy1,cy2  ! character versions 

      integer parLKflag(nparmax)  ! lake flag
      character*6 parModule(nparmax)  ! module of this parameter
      character*14 parTable(nparmax)  ! table name
      character*10 parName(nparmax)   ! name
      character*1 parAorM(nparmax)    ! either add or multiply

      real parstart(nparmax)          ! starting value
      real parmin(nparmax)          ! min value
      real parmax(nparmax)          ! max value

      real parval(nparmax,nval)    ! parameter value

      integer lakeflags(maxrsegs)  ! lakeflag for each river

      character*100 header  ! header of param_list.csv file

