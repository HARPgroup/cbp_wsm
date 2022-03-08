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

      character*13 dscalsite

      integer nstemp

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

********* loop over all rseg-lseg pairs, give each a calib label
      print*,' '
      print*,'river, land, acres, calibsite'
      nprnline = 0
      do ns = 1,nrsegs

        found = .false.
        dscalsite = ' '

        nstemp = ns
        do while (.not.found)    ! find downstream gage
          if (calsite(uniqid(nstemp))) then
            found = .true.
            dscalsite = rsegs(nstemp)
          else if (dsid(nstemp).eq.0) then
            found = .true.
            dscalsite = '0000'
          else if (dsid(nstemp).eq.1) then
            found = .true.
            dscalsite = '0001'
          end if  

          nstemp = uniqindex(dsid(nstemp))
        end do
          

        do nl = 1,nallland(ns)

          print 1234, 
     .       rsegs(ns),lsegs(allland(ns,nl)),acres(ns,nl),dscalsite
        end do


      end do

      stop
1234  format(A13,',',A6,',',f9.2,',',a13)
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

