************************************************************************
** Program to find the delivery factors by multiplying all downstream **
**   transport factors for each delivered constituent.                **
**                                                                    **
**   General strategy is to start with the current segment transport  **
**     factors, then work downstream to the next segment and make it  **
**     the current segment.  When the pour point is reached, stop     **
************************************************************************
      subroutine CalcSegDFmon(
     I                        rscen,lenrscen,
     I                        rseg,rsegs,uniqid,dsid,
     I                        uniqindex,nrsegs,
     I                        sdate,edate,ndel,delname,
     O                        segDFmon)
      implicit none
      include 'dfs.inc'

      integer ndl,ny,nm

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

*********** initialize to 1 and multiply through
      do nm = 1,12
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            segDFmon(ndl,ny,nm) = 1.
          end do
        end do
      end do

      Tseg = rseg  ! set to temp variable

*********** loop over downstream segments and multiply
      do
        call monmulttfs(Tseg,rscen,lenrscen,
     I                  sdate,edate,ndel,delname,
     M                  segDFmon)
        if (pourpoint(Tseg)) exit
        call downstream(
     I                  Tseg,rsegs,uniqindex,nrsegs,
     O                  downseg)
        Tseg = downseg
      end do

*********** if pourpoint is a black hole, set to zero
      if (Tseg(10:13).eq.'0004') then
        do nm = 1,12
          do ny = sdate(1),edate(1)
            do ndl = 1,ndel
              segDFmon(ndl,ny,nm) = 0.
            end do
          end do
        end do
      end if

      return

************************ ERROR SPACE *************************

      end
