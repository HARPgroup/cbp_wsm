************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************

      implicit none

      include 'mbtc.f'

      integer i, nLB, nBB, nPB, nPBEOT,numsegs,l
      integer seed  ! random number seed

********** END DECLARATIONS ********************************************
      
      read*,rscen,rseg,seed ! variables supplied by pp/run/run_etm.com 

      call lencl(rscen,lenrscen)

      print*,'Binary ETM file for ',rseg,' ',rscen(:lenrscen)
      print*,'  reading data files'
       
*** find number of segments going into the river
      call getl2r(rseg,rscen,lenrscen,
     O            numsegs,l2r)
      print*,rseg,' -> ',(l2r(i),' ',i=1,numsegs)
     
*** for each land segment, open the correct land use and bmp files then
******* make a time series file.  One for each land-river pair.
******** need to use the control file.

******** GET THE CONTROL FILE INTO MEMORY
      call readcontrol(
     I                 rscen,lenrscen,
     O                 LBJday,LBfile,nLB,LandScen,
     O                 BBJday,BBfile,nBB,BmpTypeScen,
     O                 PBJday,PBfile,nPB,
     O                 PBEOTJDAY,PBEOTFILE,nPBEOT,
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
     O                BMPconname,nBmpCon)
      print*,'BMP Con -> ',(BmpConName(i),' ',i=1,nBmpCon)

      call getBMPconsEOT(
     I                ioscen,lenioscen,
     I                BMPconname,nBmpCon,
     O                C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT,
     O                C_EOTrvars,I_EOTrvars)

*********** GET BMP SPECIFICATION
C      call getBmpSpecs(
C     I                 BmpTypeScen,BMPconname,nBmpCon,
C     O                 nCats,CatName,CatLU,nLUinCat,
C     O                 allHGMRs,nHGMR,
C     O                 LongBmpName,BmpName,BmpEff,nBmpTypes,
C     O                 AdditiveBmp,BmpDefined,
C     O                 BmpHydEffType,BmpHydEffParm,
C     O                 BmpEffDistType,BmpEffDistParm)

********* GET HGMR
C      call gethgmr(
C     I             BmpTypeScen,
C     I             rseg,numsegs,l2r,
C     I             allHGMRs,nHGMR,
C     O             indHGMR)
   
******** GET THE DATA AT THE BREAKPOINTS
      call getlandbreaks(
     I                   rseg,numsegs,l2r,LBfile,
     O                   LBdata,nLB)
      print*,'Land-uses',' -> ',(LBfile(i),' ',i=1,nLB)

      call gettransport_l2w(
     I                  rseg,numsegs,l2r,tranfile,
     I                  BMPconname,nBmpCon,
     O                  trandataL2W)

      call gettransport_s2r(
     I                  rseg,numsegs,l2r,tranfile,
     I                  BMPconname,nBmpCon,
     O                  trandataS2R)

      call getBmpBreaks(
     I                  rseg,numsegs,l2r,BBfile,nBB,
     I                  BMPconname,nBmpCon,
c     I                  BmpName,nBmpTypes,
     O                  BmpPassThru)
      print*,'BMP PassThrus', ' -> ',(BBfile(i),' ',i=1,nBB)

      call getPoundBreaksEOS(
     I                    rseg,numsegs,l2r,PBfile,nPB,
     I                    BMPconname,nBmpCon,
     O                    PoundsReduced)

      call getPoundBreaksEOT(
     I                    rseg,numsegs,l2r,PBEOTfile,nPBEOT,
     I                    BMPconname,nBmpCon,
     I                    C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT,
     O                    PoundsReducedEOT)

     
      print*,'  all data read, make binary etm file'
******** MAKE THE FINAL FACTOR FILES
C      call maketransfile(
C     I                   rseg,numsegs,l2r,
C     I                   rscen,lenrscen,LandScen,
C     I                   StartY,StartM,StartD,
C     I                   EndY,EndM,EndD,
C     I                   trandata,
C     I                   LBJday,nLB,LBdata,
C     I                   BMPconname,nBmpCon,
C     I                   BmpName,nBmpTypes,
C     I                   BmpEff,AdditiveBmp,BmpDefined,
C     I                   BmpHydEffType,BmpHydEffParm,
C     I                   BmpEffDistType,BmpEffDistParm,seed,
C     I                   BBJday,nBB,
C     I                   PBJday,nPB,PoundsReduced,
C     I                   indHGMR,
C     I                   BmpAcres)

       call maketransfile(
     I                   rseg,numsegs,l2r,
     I                   rscen,lenrscen,LandScen,
     I                   StartY,StartM,StartD,
     I                   EndY,EndM,EndD,
     I                   trandataL2W,trandataS2R,
     I                   LBJday,nLB,LBdata,
     I                   BMPconname,nBmpCon,
     I                   C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT,
     I                   BBJday,nBB,
     I                   PBJday,nPB,PoundsReduced,
     I                   PBEOTJday,nPBEOT,PoundsReducedEOT,
     I                   BmpPassThru)

*********** ERROR SPACE ************************************************

      end
