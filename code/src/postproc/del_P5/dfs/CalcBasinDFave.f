************************************************************************
** Program to find the delivery factors for an entire basin.  Works   **
**   on individual segments, however.                                 **
**                                                                    **
**   Find the pourpoint of any segment, then get the list of segments **
**     upstream of that pourpoint.  Calculate the aggregate DF        **
**                                                                    **
************************************************************************
      subroutine CalcBasinDFave(
     I                          rscen,lenrscen,
     I                          rseg,rsegs,uniqid,dsid,
     I                          uniqindex,nrsegs,
     I                          year1,year2,ndel,delname,
     O                          basinDFave)
      implicit none
      include 'dfs.inc'

      integer ndl,ns

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

      integer Tuniqid
      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup,nup  ! number of upstream segments

      real BasinInAve(ndelmax)

*********** find the pour point
      Tseg = rseg
      do
        if (pourpoint(Tseg)) exit
        call downstream(
     I                  Tseg,rsegs,uniqindex,nrsegs,
     O                  downseg)
        Tseg = downseg
      end do

*********** if pourpoint is a black hole, set to zero and quit
      if (Tseg(10:13).eq.'0004') then
        do ndl = 1,ndel
          basinDFave(ndl) = 0.
        end do
        return
      end if

******** find list of upstream segments
      read (Tseg(5:8),'(i4)') Tuniqid
      ns = uniqindex(Tuniqid)
      call FindUpstreamIndices(
     I                         ns,dsid,uniqid,nrsegs,
     O                         allup,nallup)

********** loop over all upsteam and sum the EOS loads
      do ndl = 1,ndel
        BasinInAve(ndl) = 0.0
      end do
      do nup = 1,nallup
        call avereadTFIO(
     I                   rsegs(allup(nup)),rscen,lenrscen,
     I                   year1,year2,ndel,delname,
     O                   tfave,inave,outave)
        do ndl = 1,ndel
          BasinInAve(ndl) = BasinInAve(ndl) + inave(ndl)
        end do
      end do


********** get the output of the pourpoint
      call avereadTFIO(
     I                 rsegs(allup(1)),rscen,lenrscen,
     I                 year1,year2,ndel,delname,
     O                 tfave,inave,outave)

********* calculate the DFs
      do ndl = 1,ndel
        if (BasinInAve(ndl).gt.0.0001) then
          basinDFave(ndl) = outave(ndl) / BasinInAve(ndl)
        else
          basinDFave(ndl) = 1.0
        end if
      end do

      return

************************ ERROR SPACE *************************

      end
