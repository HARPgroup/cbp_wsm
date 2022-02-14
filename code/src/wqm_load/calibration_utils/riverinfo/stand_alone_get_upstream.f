************************************************************************
** river info program.  reads in a uniqid and gives the upstream      **
**  rivers, the size of the counties in the entire upstream watershed **
**   and the other calibration sites that are affected by these       **
**   counties                                                         **
************************************************************************
      include 'basingen.inc'

      integer nb  ! indices

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


********* loop over all watersheds, find upstream segs and print
      print*,' '
      print*,'river, upstream seg'
      do ns = 1,nrsegs

        if (calsite(uniqid(ns))) then  ! only do this if a calib site

          call findupstream(ns)
          do nb = 1,nallup(ns)
            print*,rsegs(ns),',',rsegs(allup(ns,nb))
          end do

        end if

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

