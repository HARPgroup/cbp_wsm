      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/land_use.inc'
      include '../../../../lib/inc/wdm.inc'
      include '../../../../lib/inc/masslinks.inc'
      include '../../../../lib/inc/modules.inc'


      integer sdate(ndate), edate(ndate)  ! start and end dates 

************** load variables, made of Rvars
      integer maxcons
      parameter (maxcons = 10)     ! max constituents in a load
      integer nloadmax             !
      parameter (nloadmax = 20)    ! maximum number of loads
      integer nloads               ! number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')
      character*4 unit(nloadmax)      ! units for load (e.g. mg/l cfs )
      integer ncons(nloadmax)     ! number of constituents to each load
      character*4 con(nloadmax,maxcons) ! concentration variable to load
      real confactor(nloadmax,maxcons)  ! factor for the variable


      integer maxDat2Rv     ! number of data types for each river var
      parameter (maxDat2Rv = 10)

************ atdep vars
      integer nATDEPvar(maxRvar)  ! number of ATDEPvars for each Rvar
      integer ATDEPdsn(maxRvar,maxDat2Rv)  ! dsns of ATDEPvars
      character*4 ATDEPname(maxRvar,maxDat2Rv) ! name of ATDEPvars
      real ATDEPfac(maxRvar,maxDat2Rv)      ! conversion factor



