************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine fixneglu(
     I                    ndaymax,nlu,ndays,
     M                    lufac)
      implicit none
      integer ndaymax,nlu,ndays
      real lufac(ndaymax,nlu)
      integer l,j

      real sumneg ! sum of negatives for that day
      real sumpos ! sum of posatives for that day
      logical neglu ! do negatives exist this day

********** END OF DECLARATION ******************************************

      do j = 1,ndays
        neglu = .false.

        do l = 1,nlu  ! check for negatives 
          if (lufac(j,l).lt.0.0) neglu = .true.
        end do

        if (neglu) then  ! if found negatives, then adjust all   
          sumneg = 0.0
          sumpos = 0.0
          do l = 1,nlu
            if (lufac(j,l).lt.0.0) then
              sumneg = sumneg + lufac(j,l)
            else
              sumpos = sumpos + lufac(j,l)
            end if
          end do
          do l = 1,nlu
            if (lufac(j,l).lt.0.0) then
              lufac(j,l) = 0.0
            else
              lufac(j,l) = lufac(j,l) * (sumpos + sumneg) / sumpos
            end if
          end do
        end if

      end do
      end
