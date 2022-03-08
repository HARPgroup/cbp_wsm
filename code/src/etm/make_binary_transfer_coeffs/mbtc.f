      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'
      include '../../lib/inc/masslinks.inc'

      character*25 LandScen(nlu)                ! scenario for land wdms

      integer StartY,StartM,StartD              ! start time
      integer EndY,EndM,EndD                    ! end time

      character*25 tranfile                     ! transport factor file
      real trandata(maxL2R,maxBMPcon,nlu)            ! transport factors

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      integer LBJday(maxTimeBreaks)             ! land use julian day
      character*40 LBfile(maxTimeBreaks)        ! land use files
      real LBdata(maxTimeBreaks,maxL2R,nlu)     ! land use

      integer BBJday(maxTimeBreaks)             ! bmp factor julian day
      character*40 BBfile(maxTimeBreaks)        ! bmp factor files

      integer PBJday(MaxTimeBreaks)             ! pound bmp factor jday
      character*40 PBfile(MaxTimeBreaks)        ! pound bmp factor files

*********** the following variables are to read in bmps by acres
************   applied rather than by aggregate efficiency
      character*25 BmpTypeScen   ! refers to files of efficiencies
                                 ! and distribution specs

      integer MaxBmpTypes,nBmpTypes,nBmp    ! number of bmp types
      parameter (MaxBmpTypes=171) ! GY increased 100 to 111
                                  ! BHATT increased to 129 on Aug 22 2013
                                  ! BHATT increased to 171 on Oct 28 2013

*************** Acres for each BMP
      real BmpAcres(MaxBmpTypes,MaxTimeBreaks,MaxL2R,nlu)  ! acres

*************** logical variable if constrained to MaxImplement
*      logical constrained(MaxBmpTypes,MaxTimeBreaks,MaxL2R,nlu)

************** Region for each segment
      integer maxHGMR,nHGMR, nHG
      parameter (maxHGMR=20)
      character*4 allHGMRs(maxHGMR)  ! list of all HGMRs
      character*4 THGMR         ! temporary reading variable
      integer indHGMR(MaxL2R) ! HGMR index for each lrseg

********** variables to describe default behaviour of the BMPs
      character*60 LongBmpName(MaxBmpTypes)  ! full name for each bmp
      character*60 TLongBmpName ! temp reading var
      character*20 BmpName(MaxBmpTypes)  ! abbreviation for each bmp
      character*20 TBmpName,TBmpVector(MaxBmpTypes) ! temp reading vars
*      real MaxImplement(MaxBmpTypes,maxHGMR,nlu)  ! maximum fraction of
*                                          ! acres that can receive bmps
      real BmpEff(MaxBmpTypes,maxBMPcon,maxHGMR,nlu)  
            ! single-value efficiency, separate for each constituent,
            !   HGMR, and land use

      logical BmpDefined(MaxBmpTypes,maxHGMR,nlu)

      logical AdditiveBmp(MaxBmpTypes,MaxBmpTypes)  ! matrix of 
                 ! showing additive relationships between BMPs
                 ! BMPs are additive if they cannot be applied to 
                 ! the same ground

************* variables to deal with hydrologic variability
************ bmps are reduced in efficiency for a storm with a high
************ return period. efficiency  = BmpEff * HydEff
      integer nparms,np  ! number of parameters for dist functions
      parameter (nparms=3)
      real BmpHydEff  ! temporary variable
      integer BmpHydEffType(MaxBmpTypes,maxBMPcon,maxHGMR,nlu) 
                ! distribution type
                ! 0 = no reduction
                ! 1 = Michaelis-Menten
                   ! (1-min) * (RetFreq-Start) 
                   !    /  (RetFreq-Start + HalfSat-Start)
                ! other = error
      real BmpHydEffParm(MaxBmpTypes,maxBMPcon,maxHGMR,nlu,nparms) 
           ! distribution descriptors
            ! if BmpHydEffType = 1
                ! 1 = Return Freq at which decrement starts
                ! 2 = 1/2 sat const
                ! 3 = Minimum efficiency no matter what storm

********* variables to deal with BMPs that reduce by a certain number
********** of pounds rather than by a percentage
      real PoundsReduced(MaxTimeBreaks,maxBMPcon,MaxL2R,nlu)

******* variables to deal with the randomization given a particular
********  distribution.  specific code will have to be written to 
********  interpret these variables.  Daily distributions are evaluated
******** on a daily basis, single value distributinos are evaluated once
********* and applied for the entire time period.
      integer BmpEffDistType(MaxBmpTypes,maxBMPcon,maxHGMR,nlu)  
             ! distribution type
                 ! 0 = no distribution
                 ! 1 = daily uniform, fixed max min
                 ! 2 = daily uniform, fixed max/min ratio
                 ! 3 = daily normal   
                 ! 4 = single value uniform, fixed max min
                 ! 5 = single value uniform, fixed max/min ratio
                 ! 6 = single value normal   
      real BmpEffDistParm(MaxBmpTypes,maxBMPcon,maxHGMR,nlu,nparms)
             ! distribution descriptors, 
                 !  if BmpEffDistType = 1
                   !  1 = min
                   !  2 = max
                 !  if BmpEffDistType = 2
                   !  1 = ratio of min:value and value:max
                   !  2 = {0=allow out of bounds, 1=constrain to 0&1}
                 !  if BmpEffDistType = 3
                   !  1 = mean
                   !  2 = variance
                   !  3 = 0=allow, 1 = truncated
      
********** variables to help with input.  Inputs can be given as 
********  categories rather than individual land uses, for example
********  hitil = (hom, hwm, nho, nhi )
      integer maxCats,nCats,nc    ! number of categories
      parameter (maxCats=20)
      character*20 CatName(maxCats)  ! name of category
      character*20 TCat             ! temporary reading variable
      integer CatLU(maxCats,nlu)  ! land uses in category
      integer nLUinCat(maxCats) ! number of land uses in each category

