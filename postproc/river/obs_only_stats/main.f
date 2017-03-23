************************************************************************
**  The purpose of this program is to summarize the loads from a      **
**    river segment and put out relevent statistics                   **
************************************************************************
      implicit none
      include 'Rstats.inc'

      integer year1,year2             ! first and last year to average

      logical founddouble

******************* END DECLARATIONS ***********************************

      read*,obscen,rseg,year1,year2,rscen
                         ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)

      call getconseg(rseg,rscen,lenrscen,
     O                    conseg,founddouble)   
                  !look for rseg in doubles.csv file

      if (founddouble) then
        call single(obscen,conseg,year1,year2,rscen,lenrscen)
      end if

      call riverstop(rseg)     ! stop if not a simulated river

      call single(obscen,rseg,year1,year2,rscen,lenrscen)

      end

