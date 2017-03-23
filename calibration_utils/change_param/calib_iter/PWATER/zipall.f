************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine zipall(
     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
     O      monthlyUZSNfac,SUROtargets)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'calib.inc'

      integer i,j

      do i = 1,nlu
        lufacLZSN(i) = 1.0
        lufacINFILT(i) = 1.0
        lufacIRC(i) = 1.0
        lufacAGWR(i) = 1.0
        lufacINTFW(i) = 1.0
        lufacAGWETP(i) = 1.0
        lufacKVARY(i) = 1.0
        columnorder(i) = -9
        UZSNfac(i) = 1.0
        monthlyUZSN(i) = .false.
        do j = 1,12
          monthlyUZSNfac(i,j) = 1.0
        end do
      end do


      do i = 1,4
        limitsLandEvap(i) = -9.0
        limitsLZSN(i) = -9.0
        limitsINFILT(i) = -9.0
        limitsIRC(i) = -9.0
        limitsAGWR(i) = -9.0
        limitsINTFW(i) = -9.0
        limitsAGWETP(i) = -9.0
        limitsKVARY(i) = -9.0
      end do

      do i = 1,2
        SUROtargets(i) = -9.0
      end do

      return
      end

