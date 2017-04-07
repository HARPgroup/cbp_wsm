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
      subroutine CalcResDFave(
     I                        rscen,lenrscen,
     I                        rseg,rsegs,uniqid,dsid,
     I                        uniqindex,nrsegs,
     I                        year1,year2,ndel,delname,
     O                        resDFave)
      implicit none
      include 'dfs.inc'

      integer ndl

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

      real BasinInAve(ndelmax)

      integer Nexitss(maxrsegs),lakeflags(maxrsegs),timesteps(maxrsegs)
      character*1 resflags(maxrsegs)

********* initialize dfs
      do ndl = 1,ndel
        resDFave(ndl) = 1.
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
        do ndl = 1,ndel
          resDFave(ndl) = 0.
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
C        print*,'path ',(rsegs(path(nup)),' ',nup=1,NumInPath)
C        print*,'allup ',(rsegs(allup(nup)),' ',nup=1,nallup)
C        print*,'upres ',(rsegs(resup(nup)),' ',nup=1,nresup)

********* initialize inputs
        do ndl = 1,ndel
          BasinInAve(ndl) = 0.0
        end do

********** loop over all upsteam and sum the EOS loads
        do nup = 1,nallup
          call avereadTFIO(
     I                     rsegs(allup(nup)),rscen,lenrscen,
     I                     year1,year2,ndel,delname,
     O                     tfave,inave,outave)
          do ndl = 1,ndel
            BasinInAve(ndl) = BasinInAve(ndl) + inave(ndl)
          end do
        end do

************* loop over upstream reservoirs and sum the out loads
        do nres = 1,nresup
          call avereadTFIO(
     I                     rsegs(resup(nres)),rscen,lenrscen,
     I                     year1,year2,ndel,delname,
     O                     tfave,inave,outave)
          do ndl = 1,ndel
            BasinInAve(ndl) = BasinInAve(ndl) + outave(ndl)
          end do
        end do
      
********** get the output of the pourpoint
        call avereadTFIO(
     I                   rsegs(respoint),rscen,lenrscen,
     I                   year1,year2,ndel,delname,
     O                   tfave,inave,outave)

********* calculate the TFs for this basin
        do ndl = 1,ndel
          if (BasinInAve(ndl).gt.0.0001) then
            basinDFave(ndl) = outave(ndl) / BasinInAve(ndl)
          else
            basinDFave(ndl) = 1.0
          end if
        end do

************ multiply through DF
        do ndl = 1,ndel
          ResDFave(ndl) = ResDFave(ndl) * basinDFave(ndl)
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
            exit
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
