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
      integer ns2,ns3,ns4  ! indices

      character*100 fnam2

      character*1 updownflag ! = U-upstream, D-down, ' '-neither

      logical controlled(maxrsegs)  ! is this seg controlled by the
                                    !  current segment

      character*25 calscen,caltype
      integer lencalscen,lencaltype

      integer ncsegs,nc1,nc2,nr1,nr2,itemp
      character*13 csegs(maxrsegs)  ! calibration segments
                               ! can include 0003 segs

      call initialize

      read*,calscen,caltype,rscen
      call lencl(calscen,lencalscen)
      call lencl(rscen,lenrscen)
      call lencl(caltype,lencaltype)

      fnam = controldir//'calib/'//caltype(:lencaltype)//'/'//
     .       calscen(:lencalscen)//'/'//calscen(:lencalscen)//
     .       '_river_land.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      ncsegs = 0
      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.13) go to 992
          found = .false.
          do ns = 1,ncsegs
            if (Tseg.eq.csegs(ns)) found = .true.
          end do
          if (.not.found) then
            ncsegs = ncsegs + 1
            csegs(ncsegs) = Tseg
          end if
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do
      close (dfile)

************* got all csegs, ncsegs

******** read in all necessary data
      call getriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990

******** find list of all upstream segments for each segment
      print*,'processing river data'
      do ns = 1,nrsegs  ! ns is index for row of allup
        call findupstream(ns)
      end do  ! loop over all rsegs

*************** sort the list of csegs
      sorted = .false.
      do while (.not.sorted)
        sorted = .true.
        do nc1 = 1,ncsegs-1
          read(csegs(nc1)(5:8),'(i4)') itemp
          nr1 = uniqindex(itemp)
          do nc2 = nc1+1,ncsegs
            read(csegs(nc2)(5:8),'(i4)') itemp
            nr2 = uniqindex(itemp)
            do n1 = 1,nallup(nr1)-1
              if (allup(nr1,n1).eq.nr2) then
                Tseg = csegs(nc1)
                csegs(nc1) = csegs(nc2)
                csegs(nc2) = Tseg
                sorted = .false.
              end if
            end do
          end do
        end do
      end do

      fnam = seglistdir//calscen(:lencalscen)//'_'//
     .       caltype(:lencaltype)//'.calib'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (csegs(ns),' ',ns=1,ncsegs),')'
      close(dfile)

1233  format(2000a)
      stop
1234  format(A13,',',A13)
************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

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

995   report(1) = 'code problem: fnam should not equal fnam2'
      report(2) = fnam
      report(3) = fnam2
      go to 999

994   report(1) = 'segment '//Tseg//' exists in file:'
      report(2) = fnam
      report(3) = 'but not in file catdir//landnames.csv'
      go to 999

996   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = 'Problem opening file:'
      report(2) = fnam2
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

