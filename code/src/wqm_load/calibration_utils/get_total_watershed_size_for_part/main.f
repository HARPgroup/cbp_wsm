************************************************************************
** fortran version of the matlab basingen program.  Also gets the     **
**  relative size of counties in the watershed and watershed in the   **
**   counties,  and other calibration sites that have this county     **
**  STRATEGY:                                                         **
**   first, get all calibration sites and couties in them             **
************************************************************************
      include 'basingen.inc'
      character*50 prnline(maxrsegs)  ! preprint variable
      integer nprnline

      real basinsize
      integer nsoneseg  ! index of base input segment
      integer nb,nc  ! indices

      read*,rscen
      call lencl(rscen,lenrscen)

******** read in all necessary data
      call initialize
      call getriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getland(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getlandtoriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990
      call getcalibsites(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990

C      nsoneseg = uniqindex(oneseg)
C      if (nsoneseg.eq.0) go to 997

******** find list of all upstream segments for each segment
      print*,'processing river data'
      do ns = 1,nrsegs  ! ns is index for row of allup
        call findupstream(ns)
      end do  ! loop over all rsegs


************* write out base watershed

C      print*,' '
C      print*,' river segments'
C      do ns1 = 1,nallup(nsoneseg)
C        print*,rsegs(allup(nsoneseg,ns1))
C      end do

************ get lsegs and area of base watershed
C      call getlseglist(nsoneseg,
C     O                 blseglist,blarea,nblseglist)

********** write out total area
C      print*,' '
C      print*,'land segments in river'
C      print*,'river, land, acrage/1000, % of lseg in basin, ',
C     .       '% of basin that is lseg'
C
C      basinsize = 0.0
C      do nl = 1,nblseglist
C        basinsize = basinsize + blarea(nl)
C      end do
C
C      do nl = 1,nblseglist
C        print 1234,' ',lsegs(blseglist(nl)),blarea(nl)/1000.0,
C     .             blarea(nl)/lacres(blseglist(nl))*100.0,
C     .             blarea(nl)/basinsize*100.0
C      end do

********* loop over all watersheds, find overlapping segs, and print
C      print*,' '
C      print*,'other watersheds that share couties'
C      print*,'river, land, acrage/1000, % of lseg in basin, ',
C     .       '% of basin that is lseg'
C      nprnline = 0
      do ns = 1,nrsegs

C        if (calsite(uniqid(ns))) then  ! only do this if a calib site

C          found = .false.  ! check for upstream segment
C          do n1 = 1,nallup(nsoneseg)
C            if (allup(nsoneseg,n1).eq.ns) found = .true.
C          end do
C
C          if (.not.found) then    ! not an upstream segment

            call getlseglist(ns,
     O                       clseglist,clarea,nclseglist)

            basinsize = 0.0
            do nl = 1,nclseglist
              basinsize = basinsize + clarea(nl)
            end do

            print*,rsegs(ns),',',basinsize

C            do nc = 1,nclseglist
C              do nb = 1,nblseglist
C                if (clseglist(nc).eq.blseglist(nb)) then  ! match
C                  nprnline = nprnline + 1
C                  write(prnline(nprnline),1234)
C     .               rsegs(ns),lsegs(clseglist(nc)),clarea(nc)/1000.0,
C     .               clarea(nc)/lacres(clseglist(nc))*100.0,
C     .               clarea(nc)/basinsize*100.0
C                end if
C              end do
C            end do

C          end if

C        end if
              
      end do
C      do nl = 1,nblseglist
C        do ns = 1,nprnline
C          if (lsegs(blseglist(nl)).eq.prnline(ns)(15:20)) then
C            print*,prnline(ns)
C          end if
C        end do
C      end do

      stop
1234  format(A13,',',A6,',',f8.1,',',f5.0,',',f5.1)
************* ERROR SPACE ****************
990   if (err.eq.991) go to 991
      if (err.eq.992) go to 992
      if (err.eq.993) go to 993
      if (err.eq.994) go to 994
      if (err.eq.996) go to 996
      report(1) = 'unspecified error'
      report(2) = 'check code'
      report(3) = ' '
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'river segment must have exactly 13 characters'
      go to 999

993   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'land segment must have exactly 6 characters'
      go to 999

994   report(1) = 'segment '//Tseg//' exists in file:'
      report(2) = fnam
      report(3) = 'but not in file catdir//landnames.csv'
      go to 999

996   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'trouble reading line'
      go to 999

997   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

