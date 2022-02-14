************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
************************************************************************

      integer maxL2R,ndaymax,nyearmax,EarliestYear,LatestYear
********* commonly used parameters 
      parameter (maxL2R=15) ! max number of land segs flowing to 1 reach
      parameter (ndaymax = 10227) ! max number of days in simulation
      parameter (EarliestYear=1984)  ! first year simulated
      parameter (LatestYear=2011)    ! last year simulated
      parameter (nyearmax=LatestYear-EarliestYear+1 )    ! max number of years

      integer lenseg
      parameter (lenseg = 13)      ! length of the segment variables,
                                   !   need to change below also

********* commonly used variables
      character*25 rscen,lscen     ! name of the current scenario
      character*200 fnam      ! file name variable 
      character*100 line      ! variable to read one line
      character*1 c           ! single character reading variable

      character*13 rseg  ! river segment name
      character*6 lseg  ! land segment name
      character*6 l2r(maxL2R)  ! land segments draining to a river
      character*13 Tseg                     ! temp reading variable
      integer lenrscen,lenlscen,last  ! length of character variables
      integer lenrseg,lenlseg         ! length of character variables
      integer err                   ! error in read and open statements

      character*64 report(3)    ! string to report errors in execution

********* parameter to use when one datafile is open at a time
      integer dfile
      parameter(dfile=11)

      character*3 checkend,testend  ! test if binary file read correctly
      data checkend /'end'/

