************************************************************************
**  calling subroutine to cycle through min/max variables             **
************************************************************************
      subroutine minmax (
     M                   ASLT,ULTP1,LGTP1,
     I                   limitsASLT,limitsULTP1,limitsLGTP1)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'tempcal.inc'

      call mm12(ASLT,limitsASLT)
      call mm12(ULTP1,limitsULTP1)
      call mm12(LGTP1,limitsLGTP1)

      end


************************************************************************
**  subroutine to keep variables within the min and max allowable     **
**    values                                                          **
**  no special averaging is called for since these parameters are     **
**  independent and not ratioed to each other.                        **
**    see the PWATER calibration for samples of ratioed values        **
************************************************************************
      subroutine mm12(values,limits)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include 'tempcal.inc'

      real values(nlu,12)
      real limits(4)
      real maxparam,minparam,correctfactor
      integer nm

      do nl = 1,numlu
        do nm = 1,12
          if (values(lus(nl),nm).gt.limits(2)) then
            values(lus(nl),nm) = limits(2)
          end if
          if (values(lus(nl),nm).lt.limits(1)) then
            values(lus(nl),nm) = limits(1)
          end if
        end do
      end do

      end

