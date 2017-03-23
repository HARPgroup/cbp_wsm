************************************************************************
**  subroutine to keep variables within the min and max allowable     **
**    values                                                          **
************************************************************************
      subroutine minmax(
     M                  KIMNI,KAM,KDNI,KNI,KIMAM,KLON,KRON,
     I                  limitKIMNI,limitKAM,limitKDNI,limitKNI,
     I                  limitKIMAM,limitKLON,limitKRON)

      implicit none
      include 'calib_nitr.inc'

      integer nly
      real correctfactor

************** END DECLARATION ******************************************

      do nly = 1, 4
        if (KIMNI(nly).gt.limitKIMNI(2)) then
          correctfactor = limitKIMNI(2)/KIMNI(nly)
          KIMNI(nly) = KIMNI(nly) * correctfactor
        else if (KIMNI(nly).lt.limitKIMNI(1)) then
          KIMNI(nly) = limitKIMNI(1)
        end if
         
        if (KAM(nly).gt.limitKAM(2)) then
          correctfactor = limitKAM(2)/KAM(nly)
          KAM(nly) = KAM(nly) * correctfactor
        else if (KAM(nly).lt.limitKAM(1)) then
          KAM(nly) = limitKAM(1)
        end if

        if (KNI(nly).gt.limitKNI(2)) then
          correctfactor = limitKNI(2)/KNI(nly)
          KNI(nly) = KNI(nly) * correctfactor
        else if (KNI(nly).lt.limitKNI(1)) then
          KNI(nly) = limitKNI(1)
        end if

        if (KIMAM(nly).gt.limitKIMAM(2)) then
          correctfactor = limitKIMAM(2)/KIMAM(nly)
          KIMAM(nly) = KIMAM(nly) * correctfactor
        else if (KIMAM(nly).lt.limitKIMAM(1)) then
          KIMAM(nly) = limitKIMAM(1)
        end if

        if (KLON(nly).gt.limitKLON(2)) then
          correctfactor = limitKLON(2)/KLON(nly)
          KLON(nly) = KLON(nly) * correctfactor
        else if (KLON(nly).lt.limitKLON(1)) then
          KLON(nly) = limitKLON(1)
        end if

        if (KRON(nly).gt.limitKRON(2)) then
          correctfactor = limitKRON(2)/KRON(nly)
          KRON(nly) = KRON(nly) * correctfactor
        else if (KRON(nly).lt.limitKRON(1)) then
          KRON(nly) = limitKRON(1)
        end if

      end do   ! end all layers
  
**********  for denitrification rate, only at last 2 layers
      do nly = 3, 4
        if (KDNI(nly).gt.limitKDNI(2)) then
          correctfactor = limitKDNI(2)/KDNI(nly)
          KDNI(nly) = KDNI(nly) * correctfactor
        else if (KDNI(nly).lt.limitKDNI(1)) then
          KDNI(nly) = limitKDNI(1)
        end if
      end do

*********************************************************************
      end
