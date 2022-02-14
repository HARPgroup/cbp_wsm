      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'
      include '../../lib/inc/wdm.inc'
      include '../../lib/inc/modules.inc'
      include '../../lib/inc/masslinks.inc'
      include '../../lib/inc/rsegs.inc'

      character*50 LandScen(nlu)        ! scenario for land wdms
      integer lenls

*********** definition of bay variables
      integer maxBvar        ! max number of bay variables
      parameter (maxBvar = 28)
      integer nBvar      ! actual number of bay variables
      character*4 Bname(maxBvar)  ! names of bay variables
      character*4 Uname(maxBvar)  ! units of bay variables
      character*4 DivBvar(maxBvar)  ! name of variable to divide by
                                  ! used for water in concentrations

************* translation between bay variables and output river vars
      integer maxRv2Bv                  ! maximum number of river vars
      parameter (maxRv2Bv = 10)         ! linked to a single bay var

      integer nRv2Bv(maxBvar) ! number of river vars for each bay var

      character*4 Rname2Bv(maxBvar,maxRv2Bv)  ! variable name for Rvar

      real Rfactor(maxBvar,maxRv2Bv)  ! conversion for each river var

      character*25 rbfnam  ! file that connects Rvars to Bvars

**************** names of output Rvars
      integer nRvarOut          ! number of output river variables
      integer RdsnOut(maxRvar)   ! dsn for each output river variable
      character*4 RnameOut(maxRvar) ! variable name for RdsnOut
 
************** translation between river variables and land variables
      integer maxLv2Rv                  ! maximum number of land vars
      parameter (maxLv2Rv = 4)          ! linked to a single river var

      integer nLvar(maxRvar,nlu) ! number of land vars for each river
                                 !    variable and land use

      integer Ldsn(maxRvar,nlu,maxLv2Rv)! dsn for each land var linked
                                        !  with a river var and land use

      character*4 Lname(maxRvar,nlu,maxLv2Rv)  ! variable name for Ldsn
      real Lfactor(maxRvar,nlu,maxLv2Rv)       ! conversion factor Ldsn

      character*3 RvarBMP(maxRvar)  ! bmp type the affects this Rvar

************ data vars
      integer maxDat2Rv     ! number of data types for each river var
      parameter (maxDat2Rv = 10)

************ ps vars
      integer nPSvar(maxRvar)           ! number of PSvars for each Rvar
      integer PSdsn(maxRvar,maxDat2Rv)  ! dsns of PSvars
      character*4 PSname(maxRvar,maxDat2Rv) ! name of PSvars
      real PSfac(maxRvar,maxDat2Rv)      ! conversion factor

************ sep vars
      integer nSEPvar(maxRvar)        ! number of SEPvars for each Rvar
      integer SEPdsn(maxRvar,maxDat2Rv)  ! dsns of SEPvars
      character*4 SEPname(maxRvar,maxDat2Rv) ! name of SEPvars
      real SEPfac(maxRvar,maxDat2Rv)      ! conversion factor

************ atdep vars
      integer nATDEPvar(maxRvar)  ! number of ATDEPvars for each Rvar
      integer ATDEPdsn(maxRvar,maxDat2Rv)  ! dsns of ATDEPvars
      character*4 ATDEPname(maxRvar,maxDat2Rv) ! name of ATDEPvars
      real ATDEPfac(maxRvar,maxDat2Rv)      ! conversion factor


