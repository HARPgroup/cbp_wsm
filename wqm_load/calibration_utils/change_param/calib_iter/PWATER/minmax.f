************************************************************************
**  subroutine to keep variables within the min and max allowable     **
**    values                                                          **
************************************************************************
      subroutine  minmax (
     M               LZSN,AGWR,IRC,INTFW,
     M               INFILT,AGWETP,KVARY,
     I               limitsLZSN, limitsINFILT, limitsIRC, limitsAGWR,
     I               limitsINTFW, limitsAGWETP, limitsKVARY)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'calib.inc'
      real maxparam,minparam,correctfactor

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (LZSN(lus(nl)).gt.maxparam) then
          maxparam = LZSN(lus(nl))
        end if
        if (LZSN(lus(nl)).lt.minparam) then
          minparam = LZSN(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsLZSN(2)) then
        correctfactor = limitsLZSN(2)/maxparam
        do nl = 1,numlu
          LZSN(lus(nl)) = LZSN(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsLZSN(1)) then
        correctfactor = limitsLZSN(1)/minparam
        do nl = 1,numlu
          LZSN(lus(nl)) = LZSN(lus(nl)) * correctfactor
        end do
      end if

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (AGWR(lus(nl)).gt.maxparam) then
          maxparam = AGWR(lus(nl))
        end if
        if (AGWR(lus(nl)).lt.minparam) then
          minparam = AGWR(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsAGWR(2)) then
        correctfactor = limitsAGWR(2)/maxparam
        do nl = 1,numlu
          AGWR(lus(nl)) = AGWR(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsAGWR(1)) then
        correctfactor = limitsAGWR(1)/minparam
        do nl = 1,numlu
          AGWR(lus(nl)) = AGWR(lus(nl)) * correctfactor
        end do
      end if

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (IRC(lus(nl)).gt.maxparam) then
          maxparam = IRC(lus(nl))
        end if
        if (IRC(lus(nl)).lt.minparam) then
          minparam = IRC(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsIRC(2)) then
        correctfactor = limitsIRC(2)/maxparam
        do nl = 1,numlu
          IRC(lus(nl)) = IRC(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsIRC(1)) then
        correctfactor = limitsIRC(1)/minparam
        do nl = 1,numlu
          IRC(lus(nl)) = IRC(lus(nl)) * correctfactor
        end do
      end if

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (INTFW(lus(nl)).gt.maxparam) then
          maxparam = INTFW(lus(nl))
        end if
        if (INTFW(lus(nl)).lt.minparam) then
          minparam = INTFW(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsINTFW(2)) then
        correctfactor = limitsINTFW(2)/maxparam
        do nl = 1,numlu
          INTFW(lus(nl)) = INTFW(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsINTFW(1)) then
        correctfactor = limitsINTFW(1)/minparam
        do nl = 1,numlu
          INTFW(lus(nl)) = INTFW(lus(nl)) * correctfactor
        end do
      end if

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (INFILT(lus(nl)).gt.maxparam) then
          maxparam = INFILT(lus(nl))
        end if
        if (INFILT(lus(nl)).lt.minparam) then
          minparam = INFILT(lus(nl))
        end if
      end do
      
      if (maxparam.gt.limitsINFILT(2)) then
        correctfactor = limitsINFILT(2)/maxparam
        do nl = 1,numlu
          INFILT(lus(nl)) = INFILT(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsINFILT(1)) then
        correctfactor = limitsINFILT(1)/minparam
        do nl = 1,numlu
          INFILT(lus(nl)) = INFILT(lus(nl)) * correctfactor
        end do
      end if
     
      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (AGWETP(lus(nl)).gt.maxparam) then
          maxparam = AGWETP(lus(nl))
        end if
        if (AGWETP(lus(nl)).lt.minparam) then
          minparam = AGWETP(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsAGWETP(2)) then
        correctfactor = limitsAGWETP(2)/maxparam
        do nl = 1,numlu
          AGWETP(lus(nl)) = AGWETP(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsAGWETP(1)) then
        correctfactor = limitsAGWETP(1)/minparam
        do nl = 1,numlu
          AGWETP(lus(nl)) = AGWETP(lus(nl)) * correctfactor
        end do
      end if

      maxparam = 0.0
      minparam = 99999
      do nl = 1,numlu
        if (KVARY(lus(nl)).gt.maxparam) then
          maxparam = KVARY(lus(nl))
        end if
        if (KVARY(lus(nl)).lt.minparam) then
          minparam = KVARY(lus(nl))
        end if
      end do
      if (maxparam.gt.limitsKVARY(2)) then
        correctfactor = limitsKVARY(2)/maxparam
        do nl = 1,numlu
          KVARY(lus(nl)) = KVARY(lus(nl)) * correctfactor
        end do
      end if
      if (minparam.lt.limitsKVARY(1)) then
        correctfactor = limitsKVARY(1)/minparam
        do nl = 1,numlu
          KVARY(lus(nl)) = KVARY(lus(nl)) * correctfactor
        end do
      end if

      end
