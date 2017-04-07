************************************************************************
** process geography, determine river segments to act on              **
************************************************************************
      subroutine procgeo(geography,maxrsegs,numrsegs,rsegs)
      
      implicit none

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      
      character(*):: geography
      integer:: maxrsegs,numrsegs
      character(13):: rsegs(maxrsegs)

      character(3000):: riverline                             ! single line in data file

      integer:: ns,i,j,shifti

101   call lencl(geography,last)
      
111   fnam = seglistdir//geography(:last)//'.riv'  ! must be seglist
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a3000)',err=992,end=992) riverline
      call lencl(riverline,last)
      do while (last.lt.4)                                   ! get rid of possible blank lines
        read(dfile,'(a3000)',err=992,end=992) riverline
        call lencl(riverline,last)
      end do

      if (riverline(3000-5:3000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1                                           ! i is an index for the space along riverline
      do while (riverline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      i = i + 1
      
      do while (riverline(i:i).ne.')')
        riverline(i-shifti:i-shifti) = riverline(i:i)
        i = i + 1
      end do
      riverline(i-shifti:i) = ' '

******** riverline is now just the segments, read
      do ns = 1,maxrsegs
        rsegs(ns)='      '
      end do
      numrsegs = 0
      last = 1
      do while (last.ne.0)
        numrsegs = numrsegs + 1
        read(riverline,*) rsegs(numrsegs)
        call spaceshift(riverline,last)
      end do

******** check for duplicates
      do i = 1,numrsegs-1
        do j = i+1,numrsegs
          if (rsegs(i).eq.rsegs(j)) go to 994
        end do
      end do
        

      return
************ ERROR SPACE ***********************************************
991   print*, 'could not open file'
      print*, fnam
      print*,'enter river segment or seglist .riv file name'
      read*,geography
      go to 101

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'file has line longer than 3000 characters'
      report(2) = fnam
      report(3) = 'fix pp/src/calibration_utils/change_param/rproc.f'
      go to 999

994   report(1) = 'file has a duplicate river segment'
      report(2) = fnam
      report(3) = rsegs(i)
      go to 999

999   print*,report(1)
      print*,report(2)
      print*,report(3)
      stop

      end


************************************************************************
** reads values from a file                                           **
************************************************************************
      subroutine readvalues(typeflag,paramscen,variable,maxrsegs,
     I                      numrsegs,rsegs,
     O                      rvalues,ivalues)
      
      implicit none
      
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character(1):: typeflag
      character(1):: answer
      character(*):: variable

      integer:: maxrsegs,numrsegs          ! number of river segments

      character(*):: rsegs(maxrsegs)       ! names of river segments

      real:: rvalues(maxrsegs),Trvalue     ! real values for all rsegs
      integer:: ivalues(maxrsegs),Tivalue  ! intervalues of all river segs

      logical:: found(maxrsegs)            ! true if this segment found in file

      logical:: scompare                   ! exterior comparison function
      external scompare

      integer:: lenpscen,lenvar            ! lengths of character variables

      integer:: ns                         ! index

**********  END DECLARATIONS    ****************************************

********* INITIALIZATIONS
      call lencl(paramscen,lenpscen)
      call lencl(variable,lenvar)
      
      do ns = 1,numrsegs
        found(ns) = .false.
      end do

********** OPEN FILE
      fnam = pardir//'common/'//paramscen(:lenpscen)//'/'//
     .       variable(:lenvar)//'_factors.csv'

      open(dfile,file=fnam,status='old',iostat=err)
      do while (err.ne.0)
        write(*,*) 'could not open file ',fnam
        write(*,*) ' [T]ry again, [N]ew file, or [Q]uit?'
        read(*,*) answer
        if (answer.eq.'n'.or.answer.eq.'N') then
          write(*,*) ' Enter new file name:'
          read(*,'(a)') fnam
        else if (answer.eq.'q'.or.answer.eq.'Q') then
          stop
        end if
        open(dfile,file=fnam,status='old',iostat=err)
      end do


***********  READ IN VALUES FOR ALL LSEGS
      if (typeflag.eq.'i'.or.typeflag.eq.'I') then

        do
111       read(dfile,'(a100)',end=333) line
          call d2x(line,last)
          read(line,*,err=111) Tseg,Tivalue

          call trims(Tseg,last)
          do ns = 1,numrsegs
            if (scompare(Tseg,rsegs(ns))) then
              found(ns) = .true.
              ivalues(ns) = Tivalue
            end if
          end do
        end do

      else

        do
222       read(dfile,'(a100)',end=333) line
          call d2x(line,last)
          read(line,*,err=222) Tseg,Trvalue

          call trims(Tseg,last)
          do ns = 1,numrsegs
            if (scompare(Tseg,rsegs(ns))) then
              found(ns) = .true.
              rvalues(ns) = Trvalue
            end if
          end do
        end do

      end if

333   close(dfile)

      do ns = 1,numrsegs
        if (.not.found(ns)) then
          write(*,*) ' Did not find segment ',rsegs(ns),' in file:'
          write(*,*) fnam
          write(*,*) ' parameters not changed'
          stop
        end if
      end do

      write(*,*) ' all values successfully read from ',fnam

      end
