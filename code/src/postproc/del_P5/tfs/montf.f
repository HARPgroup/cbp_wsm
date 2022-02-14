************************************************************************
**  This program finds transport factors for each river segment.  The **
***    transport factor is a property of a river segment, not of a    **
***    land segment. It is equal to the total output divided by the   **
***    total input.  By multiplying transport factors downstream, a   **
**     total delivery factor can be found.                            **
**                                                                    **
**  The strategy for this program is to loop over all lsegs in a seg  **
**    adding the associated eos and data inputs, then add the upstream**
**    river segment loads.  All calculations are on load variables in **
**    pp/catalog/iovars/rchres_out_to_load.  This calculation is done **
**    for collective variables, (TN, TP, etc) and then applied to     **
**    constituents.  The file pp/catalog/iovars/delivery_ratio_calc   **
**    contains the relationships between collective and constituents  **
************************************************************************
      subroutine montf(rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I                 doatdep,dops,dosep,sdate,edate,
     I                 ndel,delname,nloads)
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm,nm1,nm2

      integer numsegs        ! number of land segs for this river

      logical doatdep,dops,dosep,IamRiver

*************************** END DECLARATIONS ***************************

      do ndl = 1,ndel           ! initialize
        do ny = sdate(1),edate(1)
          do nm = 1,12
            outmon(ndl,ny,nm) = 0.
            inmon(ndl,ny,nm) = 0.
            upmon(ndl,ny,nm) = 0.
          end do
        end do
      end do

***************** get out loads
      if (IamRiver) then
        call getrsegmon(rseg,lenrseg,rscen,lenrscen,
     I                  ndel,delname,sdate,edate,
     O                  outmon)
      end if

*************** get upstream loads
      if (IamRiver) then
        call getupstreammon(rseg,lenrseg,rscen,lenrscen,
     I                      ndel,delname,sdate,edate,
     M                      upmon)
      end if

***************** loop over lsegs in rseg, get EOS, PS, SEP, ATDEP
      do i = 1,numsegs

        call lsegmon(rseg,lenrseg,l2r(i),rscen,lenrscen,
     I               ndel,delname,sdate,edate,nloads,
     M               inmon)

        call datmon(
     I              rseg,lenrseg,l2r(i),rscen,lenrscen,
     I              ndel,delname,sdate,edate,nloads,
     I              dops,doatdep,dosep,
     M              inmon)

      end do

*********** if not a river, set output = input
      if (.not.IamRiver) then
        do ny = sdate(1),edate(1)
          nm1 = 1
          if (ny.eq.sdate(1)) nm1 = sdate(2)
          nm2 = 12
          if (ny.eq.edate(1)) nm2 = edate(2)
          do nm = nm1,nm2
            do ndl = 1,ndel
              outmon(ndl,ny,nm) = inmon(ndl,ny,nm)
            end do
          end do
        end do
      end if

******** calculate factors
      do ndl = 1,ndel    
        do ny = sdate(1),edate(1)
          nm1 = 1
          if (ny.eq.sdate(1)) nm1 = sdate(2)
          nm2 = 12
          if (ny.eq.edate(1)) nm2 = edate(2)
          do nm = nm1,nm2
            totalin = inmon(ndl,ny,nm) + upmon(ndl,ny,nm)
            if (totalin.gt.0.01) then
              tfmon(ndl,ny,nm) = outmon(ndl,ny,nm) / totalin
            else
              tfmon(ndl,ny,nm) = 1.
            end if
          end do
        end do
      end do

      call writemontfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,sdate,edate,
     I                 inmon,outmon,tfmon)

      return
      end 
