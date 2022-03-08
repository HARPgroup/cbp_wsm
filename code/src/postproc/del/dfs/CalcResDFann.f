************************************************************************
** Program to find the delivery factors for a basin above a reseroir. **
**   Works on individual segments, however.                           **
**                                                                    **
**   Find the downtream pourpoint.  Calculate an aggregate TF for all **
**     segments upstream of that point but downstream of any upstream **
**     reservoir.  Continue calculating TFS for reservoirs along the  **
**     path until the basin contains the passed rseg. Multiply TFs    **
**                                                                    **
************************************************************************
      subroutine CalcResDFann(
     I                        rscen,lenrscen,
     I                        rseg,rsegs,uniqid,dsid,
     I                        uniqindex,nrsegs,
     I                        sdate,edate,ndel,delname,
     O                        resDFann)
      implicit none
      include 'dfs.inc'

      integer ndl,ny

      logical found

      integer path(maxrsegs),NumInPath ! index to path downstream
      integer npRespoint,np  ! index of current respoint in the path

      integer respoint       ! reservoir segment index

      integer rsegindex      ! index of passed segment
      integer downsegindex   ! next downstream segment

      logical pourpoint             ! true if this is the pourpoint
      external pourpoint

      integer Tuniqid
      integer allup(maxrsegs) ! indices of upstream segments
      integer nallup,nup  ! number of upstream segments
      integer resup(maxrsegs) ! indices of upstream reservoirs
      integer nresup,nres  ! number of upstream reservoirs

      real BasinInAnn(ndelmax,sdate(1):edate(1))

      integer Nexitss(maxrsegs),lakeflags(maxrsegs),timesteps(maxrsegs)
      character*1 resflags(maxrsegs)

********* initialize dfs
      do ny = sdate(1),edate(1)
        do ndl = 1,ndel
          resDFann(ndl,ny) = 1.
        end do
      end do

******** get the index for the passed segment
      read(rseg(5:8),'(i4)') Tuniqid
      rsegindex = uniqindex(Tuniqid)

*********** find the pour point, store the path
      NumInPath = 1
      path(NumInPath) = rsegindex
      respoint = path(NumInPath)
      do
        if (pourpoint(rsegs(respoint))) exit
        call DownstreamIndex(
     I                       respoint,rsegs,uniqindex,nrsegs,
     O                       downsegindex )
        respoint = downsegindex 
        NumInPath = NumInPath + 1
        path(NumInPath) = respoint
      end do

*********** if pourpoint is a black hole, set to zero and quit
      if (rsegs(respoint)(10:13).eq.'0004') then
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            resDFann(ndl,ny) = 0.
          end do
        end do
        return
      end if

************* get the lakeflags for all segments
      call readcontrol_Rparamscen(
     I                           rscen,lenrscen,
     O                           paramscen)
      call lencl(paramscen,lenparamscen)
      call getallrflags(
     I                  paramscen,lenparamscen,rsegs,nrsegs,
     O                  Nexitss,lakeflags,resflags,timesteps)


*************** start loop over reservoirs, starting from most
*************** downstream.  find reservoir basin, calc tf, and mult df
      npRespoint = NumInPath
      do

******** find list of upstream segments
        call FindResBasin(
     I                    respoint,dsid,uniqid,lakeflags,nrsegs,
     O                    allup,nallup,resup,nresup)

********* initialize inputs
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            BasinInAnn(ndl,ny) = 0.0
          end do
        end do

********** loop over all upsteam and sum the EOS loads
        do nup = 1,nallup
          call annreadTFIO(
     I                     rsegs(allup(nup)),rscen,lenrscen,
     I                     sdate,edate,ndel,delname,
     O                     tfann,inann,outann)
          do ny = sdate(1),edate(1)
            do ndl = 1,ndel
              BasinInAnn(ndl,ny) = BasinInAnn(ndl,ny) + inann(ndl,ny)
            end do
          end do
        end do

************* loop over upstream reservoirs and sum the out loads
        do nres = 1,nresup
          call annreadTFIO(
     I                     rsegs(resup(nres)),rscen,lenrscen,
     I                     sdate,edate,ndel,delname,
     O                     tfann,inann,outann)
          do ny = sdate(1),edate(1)
            do ndl = 1,ndel
              BasinInAnn(ndl,ny) = BasinInAnn(ndl,ny) + outann(ndl,ny)
            end do
          end do
        end do
      
********** get the output of the pourpoint
        call annreadTFIO(
     I                   rsegs(respoint),rscen,lenrscen,
     I                   sdate,edate,ndel,delname,
     O                   tfann,inann,outann)

********* calculate the TFs for this basin
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            if (BasinInAnn(ndl,ny).gt.0.0001) then
              basinDFann(ndl,ny) = outann(ndl,ny) / BasinInAnn(ndl,ny)
            else
              basinDFann(ndl,ny) = 1.0
            end if
          end do
        end do

************ multiply through DF
        do ny = sdate(1),edate(1)
          do ndl = 1,ndel
            ResDFann(ndl,ny) = ResDFann(ndl,ny) * basinDFann(ndl,ny)
          end do
        end do

************* check to see if passed segment is in this basin
        do nup = 1,nallup
          if (allup(nup).eq.rsegindex) return 
        end do

********* passed segment is upstream, follow path up to next reservoir
        found = .false.
        do np = npRespoint-1,1,-1    ! go upstream
          if (lakeflags(path(np)).ne.0) then
            found = .true.
            npRespoint = np
            respoint = path(np)
          end if
        end do
        if (.not.found) go to 992

      end do

************************ ERROR SPACE *************************
992   report(1) = 'error in program logic'
      report(2) = 'this point should never be reached'
      report(3) = './pp/src/postproc/del/dfs/'
      go to 999

999   call stopreport(report)

      end
