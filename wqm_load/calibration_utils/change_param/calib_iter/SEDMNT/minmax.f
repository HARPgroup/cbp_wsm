************************************************************************
**  subroutine to keep variables within the min and max allowable     **
**    values                                                          **
************************************************************************
      subroutine  minmax (
     M               KRER,KSER,
     I               limitsKRER, limitsKSER,nlsegs)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'calib_sed.inc'

      real maxparam,minparam,correctfactor
      integer ns

************** END DECLARATION ******************************************
      maxparam = 0.0
      minparam = 99999
       do nl = 1,numlu
        if (KSER(lus(nl)).gt.maxparam) then
          maxparam = KSER(lus(nl))
        end if
        if (KSER(lus(nl)).lt.minparam) then
          minparam = KSER(lus(nl))
        end if
       end do
      
       if (maxparam.gt.limitsKSER(2)) then
        correctfactor = limitsKSER(2)/maxparam
        do nl = 1,numlu
          KSER(lus(nl)) = KSER(lus(nl)) * correctfactor
        end do
       end if
       
       if (minparam.lt.limitsKSER(1)) then
         correctfactor = limitsKSER(1)/minparam
        do nl = 1,numlu
         KSER(lus(nl)) = KSER(lus(nl)) * correctfactor
        end do
       end if

***********for KRER
       maxparam = 0.0
       minparam = 99999
       do nl = 1,numlu
        if (KRER(lus(nl)).gt.maxparam) then
          maxparam = KRER(lus(nl))
        end if
        if (KRER(lus(nl)).lt.minparam) then
          minparam = KRER(lus(nl))
        end if
       end do

       if (maxparam.gt.limitsKRER(2)) then
        correctfactor = limitsKRER(2)/maxparam
        do nl = 1,numlu
          KRER(lus(nl)) = KRER(lus(nl)) * correctfactor
        end do
       end if
      
       if (minparam.lt.limitsKRER(1)) then
        correctfactor = limitsKRER(1)/minparam
        do nl = 1,numlu
         KRER(lus(nl)) = KRER(lus(nl)) * correctfactor
        end do
       end if
 
      end
