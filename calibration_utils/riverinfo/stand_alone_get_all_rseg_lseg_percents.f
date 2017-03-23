************************************************************************
** river info program.  reads in a uniqid and gives the upstream      **
**  rivers, the size of the counties in the entire upstream watershed **
**   and the other calibration sites that are affected by these       **
**   counties                                                         **
************************************************************************
      include 'basingen.inc'
      character*50 prnline(maxrsegs)  ! preprint variable
      integer nprnline

      real basinsize
      integer nb,nc  ! indices

      character*1 updownflag ! = U-upstream, D-down, ' '-neither

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

******** find list of all upstream segments for each segment
      print*,'processing river data'
      do ns = 1,nrsegs  ! ns is index for row of allup
        call findupstream(ns)
      end do  ! loop over all rsegs

********* loop over all watersheds, find overlapping segs, and print
      print*,' '
      print*,'river, land, acrage/1000, % of lseg in basin, ',
     .       '% of basin that is lseg'
      nprnline = 0
      do ns = 1,nrsegs

C        if (calsite(uniqid(ns))) then  ! only do this if a calib site

          call getlseglist(ns,
     O                     clseglist,clarea,nclseglist)

          basinsize = 0.0
          do nl = 1,nclseglist
            basinsize = basinsize + clarea(nl)
          end do

          do nc = 1,nclseglist
            print 1234, 
     .         rsegs(ns),lsegs(clseglist(nc)),clarea(nc)/1000.0,
     .         clarea(nc)/lacres(clseglist(nc))*100.0,
     .         clarea(nc)/basinsize*100.0
          end do

C        end if

      end do

      stop
1234  format(A13,',',A6,',',f8.1,',',f5.0,',',f5.1)
************* ERROR SPACE ****************
990   if (err.eq.991) go to 991
      if (err.eq.992) go to 992
      if (err.eq.993) go to 993
      if (err.eq.994) go to 994
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

999   call stopreport(report)

      end

