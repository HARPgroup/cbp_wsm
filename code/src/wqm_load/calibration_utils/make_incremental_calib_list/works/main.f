************************************************************************
** fortran version of the matlab basingen program.  Also gets the     **
**  relative size of counties in the watershed and watershed in the   **
**   counties,  and other calibration sites that have this county     **
**  STRATEGY:                                                         **
**   first, get all calibration sites and couties in them             **
************************************************************************
      include 'basingen.inc'

      real basinsize
      integer nsoneseg  ! index of base input segment
      integer nb,nc  ! indices

      read*,oneseg   ! get starting segment

******** read in all necessary data
      call initialize
      call getriver(fnam,err)
      if (err.ne.0) go to 990
      call getland(fnam,err)
      if (err.ne.0) go to 990
      call getlandtoriver(fnam,err)
      if (err.ne.0) go to 990
C      call getcalibsites(fnam,err)
C      if (err.ne.0) go to 990

      nsoneseg = uniqindex(oneseg)
      if (nsoneseg.eq.0) go to 996

******** find list of all upstream segments for each segment
      print*,'processing river data'
      do ns = 1,nrsegs  ! ns is index for row of allup
        call findupstream(ns)
      end do  ! loop over all rsegs


************* write out base watershed

      print*,uniqid(nsoneseg),' ',rsegs(nsoneseg),' ',
     .        nsoneseg,' ',uniqindex(6030)
      print*,'nallup(nsoneseg) = ',nallup(nsoneseg)

      do ns1 = 1,nallup(nsoneseg)
        print*,uniqid(allup(nsoneseg,ns1)),' ',
     .         rsegs(allup(nsoneseg,ns1))
      end do

************ get lsegs and area of base watershed
      call getlseglist(nsoneseg,
     O                 blseglist,blarea,nblseglist)

********** write out total area
      print*,'land segments in river'
      print*,'land segment, acrage/1000, % of lseg in basin, ',
     .       '% of basin that is lseg'

      basinsize = 0.0
      do nl = 1,nblseglist
        basinsize = basinsize + blarea(nl)
      end do

      do nl = 1,nblseglist
        print 1234,' ',lsegs(blseglist(nl)),blarea(nl)/1000.0,
     .             blarea(nl)/lacres(blseglist(nl))*100.0,
     .             blarea(nl)/basinsize*100.0
      end do

********* loop over all watersheds, find overlapping segs, and print
      print*,' '
      print*,'looking for other watersheds that share couties'
      print*,'land segment, acrage/1000, % of lseg in basin, ',
     .       '% of basin that is lseg'
      do ns = 1,nrsegs

C        if (calsite(uniqid(ns))) then  ! only do this if a calib site

          found = .false.  ! check for upstream segment
          do n1 = 1,nallup(nsoneseg)
            if (allup(nsoneseg,n1).eq.ns) found = .true.
          end do

          if (.not.found) then    ! not an upstream segment

            call getlseglist(ns,
     O                       clseglist,clarea,nclseglist)
            do nc = 1,nclseglist
              do nb = 1,nblseglist
                if (clseglist(nc).eq.blseglist(nb)) then  ! match
                  print 1234,
     .               rsegs(ns),lsegs(clseglist(nc)),clarea(nc)/1000.0,
     .               clarea(nc)/lacres(clseglist(nc))*100.0,
     .               clarea(nc)/basinsize*100.0
                end if
              end do
            end do

C          end if

        end if
              
      end do

      stop
1234  format(A13,',',A6,',',f8.1,',',f5.0,',',f5.1)
************* ERROR SPACE ****************
990   if (err.eq.991) go to 991
      if (err.eq.992) go to 992
      if (err.eq.993) go to 992
      if (err.eq.994) go to 992
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

996   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

