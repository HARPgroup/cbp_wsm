************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************

      implicit none

      include 'psu.inc'

      integer i, nLB, nBB, numsegs,l

      print*,'Penn State Urban Runoff Model'
      print*,'  enter a LAND segment and a RIVER scenario'
      
      read*,rscen,lseg     ! variables supplied by 

      call lencl(rscen,lenrscen)

*** for each land segment, open the correct land use and bmp files then
******* make a time series file.  One for each land-river pair.
******** need to use the control file.

******** FIRST GET THE CONTROL FILE INTO MEMORY
      call readcontrol(rscen,lenrscen,
     .                 LBJday,LBfile,
     .                 T1year,T1month,T1day,
     .                 T2year,T2month,T2day,
     .                 LandScen,nLB)

******** GET THE DATA AT THE BREAKPOINTS
      call getlandbreak(lseg,LBfile,LBdata,nLB)

******** get the percent of impervious as a daily time series
      call getimppercent(lseg,
     .                 LBJday,LBdata,
     .                 T1year,T1month,T1day,
     .                 T2year,T2month,T2day,
     .                 nLB,
     .                 imppercent)

********* get the hourly precipitation
      call gethrain(lseg,LandScen(lpur),
     .              T1year,T1month,T1day,
     .              T2year,T2month,T2day,
     .              hrain)

********** get the hourly runoff
      call getRO(lseg,LandScen(lpur),
     .              T1year,T1month,T1day,
     .              T2year,T2month,T2day,
     .              imppercent,
     .              hRO)

*********** run the model
      call runPSUsed(imppercent,hrain,hRO)

*********** ERROR SPACE

      end
