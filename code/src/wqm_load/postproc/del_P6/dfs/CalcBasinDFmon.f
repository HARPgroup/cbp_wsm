************************************************************************
** Program to find the delivery factors for an entire basin.  Works   **
**   on individual segments, however.                                 **
**                                                                    **
**   Find the pourpoint of any segment, then get the list of segments **
**     upstream of that pourpoint.  Calculate the aggregate DF        **
**                                                                    **
************************************************************************
      subroutine CalcBasinDFmon(
     I                          rscen,lenrscen,
     I                          rseg,rsegs,uniqid,dsid,
     I                          uniqindex,nrsegs,
     I                          sdate,edate,ndel,delname,
     O                          basinDFmon)
      implicit none
      include 'dfs.inc'

      integer ndl,ns,ny,nm

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

      integer Tuniqid
      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup,nup  ! number of upstream segments

      real BasinInMon(ndelmax,sdate(1):edate(1),12)

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
        do nm = 1,12
          do ny = sdate(1),edate(1)
            do ndl = 1,ndel
              basinDFmon(ndl,ny,nm) = 0.
            end do
          end do
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
      do nm = 1,12
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            BasinInMon(ndl,ny,nm) = 0.0
          end do
        end do
      end do
      do nup = 1,nallup
        call monreadTFIO(
     I                   rsegs(allup(nup)),rscen,lenrscen,
     I                   sdate,edate,ndel,delname,
     O                   tfmon,inmon,outmon)
        do nm = 1,12
          do ny = sdate(1),edate(1)
            do ndl = 1,ndel
              BasinInMon(ndl,ny,nm) = BasinInMon(ndl,ny,nm) 
     .                              + inmon(ndl,ny,nm)
            end do
          end do
        end do
      end do


********** get the output of the pourpoint
      call monreadTFIO(
     I                 rsegs(allup(1)),rscen,lenrscen,
     I                 sdate,edate,ndel,delname,
     O                 tfmon,inmon,outmon)

********* calculate the DFs
      do nm = 1,12
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            if (BasinInMon(ndl,ny,nm).gt.0.0001) then
              basinDFmon(ndl,ny,nm) = outmon(ndl,ny,nm) 
     .                              / BasinInMon(ndl,ny,nm)
            else
              basinDFmon(ndl,ny,nm) = 1.0
            end if
          end do
        end do
      end do

      return

************************ ERROR SPACE *************************

      end
