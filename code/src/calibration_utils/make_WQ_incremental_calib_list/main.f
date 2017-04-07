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
                                    ! current segment. indexed to allup.

      character*25 calscen,caltype,basin
      integer lencalscen,lencaltype,lenbasin

      integer nc,nr1,nr2,itemp

      integer ncsegs ! calibration segments. Can include 0003 segs
      character*13 csegs(maxrsegs)  ! name
      integer csegindex(maxrsegs)   ! index for rsegs
      logical csegcon(maxrsegs)     ! is this a confluence?

      logical foundoneseg  ! did I find this seg in the land_river.csv?
      integer onesegindex  ! index for oneseg in ncsegs

      character*13 up(maxrsegs)  ! controlled segments

      character*12000 segline  ! read segment list

*********************** END DECLARATIONS ******************************

      call initialize

      read*,calscen,caltype,basin,oneseg,rscen
      call lencl(calscen,lencalscen)
      call lencl(rscen,lenrscen)
      call lencl(caltype,lencaltype)
      call lencl(basin,lenbasin)

******** read in rsegs
      call getriver(rscen,lenrscen,fnam,err)
      if (err.ne.0) go to 990

*********** read in calibration stations

      fnam = seglistdir//calscen(:lencalscen)//'_'//
     .       basin(:lenbasin)//'_'//caltype(:lencaltype)//'.calib'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      read(dfile,'(a12000)',err=993) segline

      foundoneseg = .false.
      ncsegs = 0
      call d2x(segline,last)
      if (last.gt.12000-2) go to 995
      last = 1
      do while (segline(last:last).ne.'(')
        last = last + 1
      end do
      segline = segline(last:)
      call spaceshift(segline,last)
      do while (segline(:1).ne.')')
        ncsegs = ncsegs + 1
        if (ncsegs.ge.maxrsegs) go to 994
        csegs(ncsegs) = segline(:13)
        if (csegs(ncsegs).eq.oneseg) then
          foundoneseg = .true.
          onesegindex = ncsegs
        end if
        call spaceshift(segline,last)
      end do
      close (dfile)
	print*,(csegs(nc),nc=1,ncsegs)
      if (.not.foundoneseg) go to 996

********* process each cseg
      do nc = 1,ncsegs
        found = .false.  ! get segindex
        do ns = 1,nrsegs
          if (csegs(nc)(5:8).eq.rsegs(ns)(5:8)) then
            found = .true.
            csegindex(nc) = ns
            exit
          end if      
        end do
        if (.not.found) go to 997

        csegcon(nc) = .false.
        if (csegs(nc)(10:13).eq.'0003') csegcon(nc) = .true.

      end do

********* populate nallup and allup for each cseg
      do nc = 1,ncsegs
        call findupstream(csegindex(nc))
      end do

************ all upstream of one seg are set as controlled
      do ns = 1,nallup(csegindex(onesegindex))  ! initially set all 
        controlled(ns) = .true.       !  upstream as controlled
        print*,ns,' ',rsegs(allup(csegindex(onesegindex),ns))
      end do

        print*,'above segments are controlled'
     

************* got all csegs, ncsegs
******** NOW FIND ALL OF THE RSEGS THAT ARE ABOVE ONESEG, 
*******     BUT NOT ABOVE UPSTREAM CSEGS
******** loop over all segments upstream of oneseg
******** if they are csegs, then uncontrol everything upstream
      do ns = 1,nallup(csegindex(onesegindex))-1  ! loop over upstream
        do nc = 1,ncsegs               ! loop over calib

                      !  if calib station is upstream of oneseg
          if (allup(csegindex(onesegindex),ns).eq.csegindex(nc)) then

            do nr = 1,nallup(csegindex(nc))-1  ! loop upstream of calib
                    ! find matches upstream of oneseg and delete them
              do nr2 = 1,nallup(csegindex(onesegindex))-1
                if (allup(csegindex(nc),nr).eq.
     .              allup(csegindex(onesegindex),nr2)) then 
                  controlled(nr2) = .false.
                  exit
                end if
              end do
            end do

            if (.not.csegcon(nc)) then ! if not 0003 station
              do nr2 = 1,nallup(csegindex(onesegindex))-1 ! delete 
                if (csegindex(nc).eq.
     .              allup(csegindex(onesegindex),nr2)) then
                  controlled(nr2) = .false.
                end if
              end do
            end if
            
          end if   ! end if calib station upstream of oneseg

        end do
      end do

********** write to final variable
      nup = 0
      do ns = 1,nallup(csegindex(onesegindex))
        if (controlled(ns)) then
          nup = nup + 1
          up(nup) = rsegs(allup(csegindex(onesegindex),ns))
        end if
      end do

      fnam = seglistdir//calscen(:lencalscen)//'_'//
     .       caltype(:lencaltype)//'_'//oneseg//'.riv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,1233,err=951) 'set segments = ( ',
     .     (up(ns),' ',ns=1,nup),')'
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
      report(3) = 'problem reading single line in file'
      go to 999

994   report(1) = 'code error, did not find '//char(39)//')'//char(39)
      report(2) = ' in seglists file:'
      report(3) = fnam
      go to 999

995   report(1) = 'Problem reading seglists file'
      report(2) = fnam
      report(3) = ' increase the size of variable segline'
      go to 999

996   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' in file  '
      report(3) = fnam
      go to 999

997   report(1) = 'Problem: calib segment'//csegs(nc)//'not found in'
      report(2) = ' rivernames.csv file'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

