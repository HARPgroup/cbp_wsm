      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'
      include '../../../lib/inc/masslinks.inc'

      integer:: nptsmax
      parameter (nptsmax = ndaymax*24)


      integer:: sdate(ndate), edate(ndate)              ! start and end dates in wdm file

********* load variables
      integer:: maxcons
      parameter (maxcons = 10)            ! maximum number of constituents in a load
      integer:: nloadmax                    !
      parameter (nloadmax = 20)           ! maximum number of loads
      integer:: nloads                      ! number of loads
      character(4):: loadname(nloadmax)      ! name of each load (e.g. 'totn', 'flow')
      character(4):: unit(nloadmax)          ! units for load (e.g. mg/l cfs )
      integer:: ncons(nloadmax)             ! number of constituents to each load
      character(4):: con(nloadmax,maxcons)   ! concentration variable to load
      real:: confactor(nloadmax,maxcons)    ! factor for the variable

******* seperation variables
      integer:: ndays       
      integer:: nasdays, ndsdays                        ! number of simulated and observed ascending flow
      integer:: npts,naspts, ndspts, nbspts                  ! number of data points
      integer:: asmatch(ndaymax), dsmatch(ndaymax)

*******  regression variables
      integer:: maxobsinhour 
      parameter (maxobsinhour = 10)
      real:: obflow(ndaymax),simflow(ndaymax)           ! daily flow
      real:: bobs(ndaymax),bsim(ndaymax)                ! base flow
      real:: qobs(ndaymax),qsim(ndaymax)                ! quick flow

      real:: simfl(EarliestYear:LatestYear,12,31,24)    ! hourly flow                    
      real:: simcon(EarliestYear:LatestYear,12,31,24)   ! hourly concentration
      real:: simdayfl(EarliestYear:LatestYear,12,31) ! daily flow

      real:: obfl(EarliestYear:LatestYear,12,31)     ! daily
      real:: obcon(EarliestYear:LatestYear,12,31,24,maxobsinhour)  ! instantous concentration 

      real:: simallfl(nptsmax),simasfl(nptsmax)
      real:: simdsfl(nptsmax),simbsfl(nptsmax)        ! simulated flow
      real:: simallcon(nptsmax),simascon(nptsmax)
      real:: simdscon(nptsmax),simbscon(nptsmax)     ! simulated concentrations
      real:: oballfl(nptsmax),obasfl(nptsmax)
      real:: obdsfl(nptsmax),obbsfl(nptsmax)           ! observed flow
      real:: oballcon(nptsmax),obascon(nptsmax)
      real:: obdscon(nptsmax),obbscon(nptsmax)        ! observed concentrations

******** date variables
      integer:: obyear(ndaymax), simyear(ndaymax)
      integer:: obmonth(ndaymax), simmonth(ndaymax)
      integer:: obday(ndaymax), simday(ndaymax)
    
******** rating curve  variables
      real:: obslp,obint,obr2
      real:: simslp,simint,simr2
      real:: obaslp,obasint,obasr2
      real:: simaslp,simasint,simasr2
      real:: obdslp,obdsint,obdsr2
      real:: simdslp,simdsint,simdsr2
      real:: obbslp,obbsint,obbsr2
      real:: simbslp,simbsint,simbsr2
      real:: limit,aslimit, dslimit, bslimit


