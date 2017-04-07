************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine printall(
     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
     O      monthlyUZSNfac,SUROtargets)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'calib.inc'

      integer i,j

        print*,(lufacLZSN(i) ,i=1,nlu)
        print*,(lufacINFILT(i) ,i=1,nlu)
        print*,(lufacIRC(i) ,i=1,nlu)
        print*,(lufacAGWR(i) ,i=1,nlu)
        print*,(lufacINTFW(i) ,i=1,nlu)
        print*,(lufacAGWETP(i) ,i=1,nlu)
        print*,(lufacKVARY(i) ,i=1,nlu)
        print*,(columnorder(i) ,i=1,nlu)
        print*,(UZSNfac(i) ,i=1,nlu)
        print*,(monthlyUZSN(i) ,i=1,nlu)
        do j = 1,12
          print*,(monthlyUZSNfac(i,j) ,i=1,nlu)
        end do


        print*,(limitsLandEvap(i) ,i=1,4)
        print*,(limitsLZSN(i) ,i=1,4)
        print*,(limitsINFILT(i) ,i=1,4)
        print*,(limitsIRC(i) ,i=1,4)
        print*,(limitsAGWR(i) ,i=1,4)
        print*,(limitsINTFW(i) ,i=1,4)
        print*,(limitsAGWETP(i) ,i=1,4)
        print*,(limitsKVARY(i) ,i=1,4)

        print*,(SUROtargets(i) ,i=1,2)

      return
      end

