      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/land_use.inc'
      include '../lib/inc/bmp_constituents.inc'
      include '../lib/inc/land_constituents.inc'
      include '../lib/inc/wdm.inc'
      include '../lib/inc/modules.inc'
      include '../lib/inc/masslinks.inc'

      character*25 LandScen(nlu)     ! scenario for land wdms
      integer lenls

************* river vars
      integer nRvarOut          ! number of output river variables
      integer RdsnOut(maxRvar)   ! dsn for each output river variable
      character*4 RnameOut(maxRvar) ! variable name for RdsnOut


************* land vars linked to river vars
      integer maxLv2Rv               ! maximum number of land vars
      parameter (maxLv2Rv = 4)       ! linked to a single river var

      integer nLvar(maxRvar,nlu) ! number of land variables for each 
                                 !    river variable and land use
      integer Ldsn(maxRvar,nlu,maxLv2Rv)  ! dsn for each land variable 
                                          !   linked with a river 
                                          !   variable and land use
      character*4 Lname(maxRvar,nlu,maxLv2Rv)  ! variable name for Ldsn
      real Lfactor(maxRvar,nlu,maxLv2Rv)  ! conversion factor Ldsn

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
 
