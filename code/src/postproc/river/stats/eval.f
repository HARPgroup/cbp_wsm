************************************************************************
** Subroutine to calculate evaluation statistics                      **
** Nash-Sutcliffe effciency (NSE), BIAS, RMSE-observations standard   **
** deviation ratio (RSR) statistics                                   **
************************************************************************

      subroutine eval(Ob,Si,n,vsize,
     O                   NSE,Bias,RSR,Nbias,Nsd,r,sdd,NRMSD,err)

      implicit none
      integer n,vsize   ! number of points, total vector size
      real Ob(vsize),Si(vsize)  ! vectors
      real NSE,Bias,RSR        
      real Nbias,Nsd,r,sdd,NRMSD
      integer err   ! 0=OK, 1=divide by zero
      double precision realn,sumOb,sumSi,sum1,sum2,sum3,sum4,sum5
      double precision meanOb,meanSi,sdOb,sdSi
      integer i    ! index

******************** END DECLARATION ****************************************

******Nash-Sutcliffe effciency (NSE) statistic*******************************

      realn = real(n)
      sumOb = 0.
      sumSi = 0.
      sum1 = 0.
      sum2 = 0.
      sum3 = 0.
      sum4 = 0.
      sum5 = 0.

      do i = 1,n
        sumOb  = sumOb  + Ob(i)
        sumSi  = sumSi  + Si(i)
      end do

        meanOb = sumOb/real(n)
        meanSi = sumSi/real(n)

      do i = 1,n
        sum1 = sum1 + (Si(i)-Ob(i))
        sum2 = sum2 + (Si(i)-Ob(i))**2
        sum3 = sum3 + (Ob(i)-meanOb)**2
        sum4 = sum4 + (Si(i)-meanSi)**2
        sum5 = sum5 + (Ob(i)-meanOb)*(Si(i)-meanSi)

c      end do

        if (sum3.le.-0)then!1.0e-3) or zero?
           err = 1
           Bias = -999.0
           NSE = -999.0
           RSR = -999.0
        else
           err = 0
           sdOb = sqrt(sum3/real(n)) 
           sdSi = sqrt(sum4/real(n))

******Nash-Sutcliffe effciency (NSE) statistic*******************************
           NSE = 1- sum2/sum3

******Bias statistic*********************************************************
           Bias = sum1/sumOb

******Standard deviation ratio statistic*************************************
           RSR = sqrt(sum2)/sqrt(sum3)

******Normalized Bias statistic**********************************************
           Nbias = (meanSi - meanOb)/sdOb

******Normalized Standard Deviation******************************************
           Nsd = sdSi/SdOb

******Linear Correlation Coefficient*****************************************
           r = (sum5/real(n))/(sdOb*sdSi)

******Sign*******************************************************************
           sdd = sdSi-sdOb   

******Normalized RMSD statistic**********************************************
           if (sdd.lt.0) then
              NRMSD = -1*sqrt(1+Nsd**2-2*Nsd*R)
              else
              NRMSD = sqrt(1+Nsd**2-2*Nsd*R)

           end if

        end if

      end do

      return

      end

