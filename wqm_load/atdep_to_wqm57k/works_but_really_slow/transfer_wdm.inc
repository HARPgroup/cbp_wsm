      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/modules.inc'
      include '../lib/inc/masslinks.inc'
      include '../lib/inc/rsegs.inc'

*********** definition of bay variables
      integer maxBvar        ! max number of bay variables
      parameter (maxBvar = 18)
      integer nBvar      ! actual number of bay variables
      character*4 Bname(maxBvar)  ! names of bay variables
      character*4 DivRvar(maxBvar)  ! name of variable to divide by
                                  ! used for water in concentrations

**************** names of output Rvars
      integer nRvarOut          ! number of output river variables
      integer RdsnOut(maxRvar)   ! dsn for each output river variable
      character*4 RnameOut(maxRvar) ! variable name for RdsnOut
 
************* translation between bay variables and output river vars
      integer maxRv2Bv                  ! maximum number of river vars
      parameter (maxRv2Bv = 10)         ! linked to a single bay var

      integer nRv2Bv(maxBvar) ! number of river vars for each bay var

      character*4 Rname2Bv(maxBvar,maxRv2Bv)  ! variable name for Rvar 

      real Rfactor(maxBvar,maxRv2Bv)  ! conversion for each river var

************** translation between river variables and atdep variables
      integer maxAv2Rv                  ! maximum number of atdep vars
      parameter (maxAv2Rv = 2)          ! linked to a single river var

      integer nAvar(maxRvar) ! number of atdep vars for each river
                                 !    variable and atdep 

      integer Adsn(maxRvar,maxAv2Rv)! dsn for each atdep var linked
                                        !  with a river var and atdep 

      character*4 Aname(maxRvar,maxAv2Rv)  ! variable name for Adsn

      real Afactor(maxRvar,maxAv2Rv)       ! conversion factor Adsn

