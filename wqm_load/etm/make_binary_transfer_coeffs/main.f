************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************

      implicit none

      include 'mbtc.f'

      integer i, nLB, nBB, nPB, numsegs,l
      integer seed  ! random number seed

********** END DECLARATIONS ********************************************
      
      read*,rscen,rseg,seed ! variables supplied by pp/run/run_etm.com 

      call lencl(rscen,lenrscen)

      print*,'Binary ETM file for ',rseg,' ',rscen(:lenrscen)
      print*,'  reading data files'
       
*** find number of segments going into the river
      call getl2r(rseg,rscen,lenrscen,
     O            numsegs,l2r)
     
*** for each land segment, open the correct land use and bmp files then
******* make a time series file.  One for each land-river pair.
******** need to use the control file.

******** GET THE CONTROL FILE INTO MEMORY
      call readcontrol(
     I                 rscen,lenrscen,
     O                 LBJday,LBfile,nLB,LandScen,
     O                 BBJday,BBfile,nBB,BmpTypeScen,
     O                 PBJday,PBfile,nPB,
     O                 tranfile,
     O                 StartY,StartM,StartD,
     O                 EndY,EndM,EndD)

************ get the BMP constituents
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)
      call getBMPcons(
     I                ioscen,lenioscen,
     O                BMPconname,nbcon)

*********** GET BMP SPECIFICATION
      call getBmpSpecs(
     I                 BmpTypeScen,BMPconname,nbcon,
     O                 nCats,CatName,CatLU,nLUinCat,
     O                 allHGMRs,nHGMR,
     O                 LongBmpName,BmpName,BmpEff,nBmpTypes,
     O                 AdditiveBmp,BmpDefined,
     O                 BmpHydEffType,BmpHydEffParm,
     O                 BmpEffDistType,BmpEffDistParm)

********* GET HGMR
      call gethgmr(
     I             BmpTypeScen,
     I             rseg,numsegs,l2r,
     I             allHGMRs,nHGMR,
     O             indHGMR)
   
******** GET THE DATA AT THE BREAKPOINTS
      call getlandbreaks(
     I                   rseg,numsegs,l2r,LBfile,
     O                   LBdata,nLB)

      call gettransport(
     I                  rseg,numsegs,l2r,tranfile,
     I                  BMPconname,nbcon,
     O                  trandata)

      call getBmpBreaks(
     I                  rseg,numsegs,l2r,BBfile,nBB,
     I                  BmpName,nBmpTypes,
     O                  BmpAcres)
      
      call getPoundBreaks(
     I                    rseg,numsegs,l2r,PBfile,nPB,
     I                    BMPconname,nbcon,
     O                    PoundsReduced)
     
      print*,'  all data read, make binary etm file'
******** MAKE THE FINAL FACTOR FILES
      call maketransfile(
     I                   rseg,numsegs,l2r,
     I                   rscen,lenrscen,LandScen,
     I                   StartY,StartM,StartD,
     I                   EndY,EndM,EndD,
     I                   trandata,
     I                   LBJday,nLB,LBdata,
     I                   BMPconname,nbcon,
     I                   BmpName,nBmpTypes,
     I                   BmpEff,AdditiveBmp,BmpDefined,
     I                   BmpHydEffType,BmpHydEffParm,
     I                   BmpEffDistType,BmpEffDistParm,seed,
     I                   BBJday,nBB,
     I                   PBJday,nPB,PoundsReduced,
     I                   indHGMR,
     I                   BmpAcres)

*********** ERROR SPACE ************************************************

      end
