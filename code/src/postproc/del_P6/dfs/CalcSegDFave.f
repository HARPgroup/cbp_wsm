************************************************************************
** Program to find the delivery factors by multiplying all downstream **
**   transport factors for each delivered constituent.                **
**                                                                    **
**   General strategy is to start with the current segment transport  **
**     factors, then work downstream to the next segment and make it  **
**     the current segment.  When the pour point is reached, stop     **
************************************************************************
      subroutine CalcSegDFave(
     I                        rscen,lenrscen,
     I                        rseg,rsegs,uniqid,dsid,
     I                        uniqindex,nrsegs,
     I                        year1,year2,ndel,delname,
     O                        segDFave)
      implicit none
      include 'dfs.inc'

      integer ndl

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

*********** initialize to 1 and multiply through
      do ndl = 1,ndel
        segDFave(ndl) = 1.
      end do

      Tseg = rseg  ! set to temp variable

*********** loop over downstream segments and multiply
      do
        call avemulttfs(Tseg,rscen,lenrscen,
     I                  year1,year2,ndel,delname,
     M                  segDFave)
        if (pourpoint(Tseg)) exit
        call downstream(
     I                  Tseg,rsegs,uniqindex,nrsegs,
     O                  downseg)
        Tseg = downseg
      end do

*********** if pourpoint is a black hole, set to zero
      if (Tseg(10:13).eq.'0004') then
        do ndl = 1,ndel
          segDFave(ndl) = 0.
        end do
      end if

      return

************************ ERROR SPACE *************************

      end
