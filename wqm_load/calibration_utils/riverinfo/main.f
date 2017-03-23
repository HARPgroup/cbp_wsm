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
      integer nsoneseg  ! index of base input segment
      integer nb,nc  ! indices

      character*1 updownflag ! = U-upstream, D-down, ' '-neither

      read*,oneseg,rscen   ! get starting segment
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

      nsoneseg = uniqindex(oneseg)
      if (nsoneseg.eq.0) go to 997

******** find list of all upstream segments for each segment
      print*,'processing river data'
      do ns = 1,nrsegs  ! ns is index for row of allup
        call findupstream(ns)
      end do  ! loop over all rsegs


************* write out base watershed
      print*,' '
      print*,' river segments'
      do ns1 = 1,nallup(nsoneseg)
        print*,rsegs(allup(nsoneseg,ns1))
      end do

************ get lsegs and area of base watershed
      call getlseglist(nsoneseg,
     O                 blseglist,blarea,nblseglist)

********** write out total area
      print*,' '
      print*,'land segments in river'
      print*,'river, land, acrage/1000, % of lseg in basin, ',
     .       '% of basin that is lseg'

      basinsize = 0.0
      do nl = 1,nblseglist
        basinsize = basinsize + blarea(nl)
      end do

      do nl = 1,nblseglist
        print 1234,' ',' ',lsegs(blseglist(nl)),blarea(nl)/1000.0,
     .             blarea(nl)/lacres(blseglist(nl))*100.0,
     .             blarea(nl)/basinsize*100.0
      end do

********* loop over all watersheds, find overlapping segs, and print
      print*,' '
      print*,'other watersheds that share couties'
      print*,'river, land, acrage/1000, % of lseg in basin, ',
     .       '% of basin that is lseg'
      nprnline = 0
      do ns = 1,nrsegs

        if (ns.ne.nsoneseg) then
          if (calsite(uniqid(ns))) then  ! only do this if a calib site

            updownflag = ' '
            do n1 = 1,nallup(nsoneseg)
              if (allup(nsoneseg,n1).eq.ns) updownflag = 'U'
            end do
            do n1 = 1,nallup(ns)
              if (allup(ns,n1).eq.nsoneseg) updownflag = 'D'
            end do

            call getlseglist(ns,
     O                       clseglist,clarea,nclseglist)

            basinsize = 0.0
            do nl = 1,nclseglist
              basinsize = basinsize + clarea(nl)
            end do

            do nc = 1,nclseglist
              do nb = 1,nblseglist
                if (clseglist(nc).eq.blseglist(nb)) then  ! match
                  nprnline = nprnline + 1
                  write(prnline(nprnline),1234) updownflag,
     .               rsegs(ns),lsegs(clseglist(nc)),clarea(nc)/1000.0,
     .               clarea(nc)/lacres(clseglist(nc))*100.0,
     .               clarea(nc)/basinsize*100.0
                end if
              end do
            end do

          end if

        end if
              
      end do
      do ns = 1,nprnline
        if (prnline(ns)(1:1).eq.'U') print*,prnline(ns)
      end do
      do ns = 1,nprnline
        if (prnline(ns)(1:1).eq.'D') print*,prnline(ns)
      end do
      do nl = 1,nblseglist
        do ns = 1,nprnline
          if (prnline(ns)(1:1).eq.' ') then
            if (lsegs(blseglist(nl)).eq.prnline(ns)(17:22)) then
              print*,prnline(ns)
            end if
          end if
        end do
      end do

      stop
1234  format(a1,',',A13,',',A6,',',f8.1,',',f5.0,',',f5.1)
************* ERROR SPACE ****************
990   if (err.eq.991) go to 991
      if (err.eq.992) go to 992
      if (err.eq.993) go to 993
      if (err.eq.994) go to 994
      if (err.eq.995) go to 995
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

995   report(1) = ' river segments found in '//catdir//
     .              'geo/rivernames.csv'
      report(2) = '  but not found in '
      report(3) = '    '//catdir//'geo/land_water_area.csv'
      go to 999

996   report(1) = 'Problem in file '
      report(2) = fnam
      report(3) = 'error reading file'
      go to 999

997   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

