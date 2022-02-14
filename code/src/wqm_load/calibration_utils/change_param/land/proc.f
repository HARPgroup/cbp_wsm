************************************************************************
** process geography, determine land segments to act on               **
************************************************************************
      subroutine procgeo(geography, maxlsegs,numlsegs,lsegs)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      character*(*) geography
      integer maxlsegs,numlsegs
      character*6 lsegs(maxlsegs)

      character*3000 landline  ! single line in data file

      integer ns,i,j,shifti

101   call lencl(geography,last)
      if (last.ne.6) go to 111
      read(geography(2:6),'(i5)',err=111) numlsegs  !if error, not a single seg
      if (numlsegs.lt.10000) go to 111  ! if nothing, not a single seg
      if (.not.(geography(1:1).eq.'A' .or.
     .          geography(1:1).eq.'B' .or.
     .          geography(1:1).eq.'C')) go to 111

      numlsegs = 1               ! passed all tests
      lsegs(1) = geography(:6)
      return

111   fnam = seglistdir//geography(:last)//'.land'  ! must be seglist
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a3000)',err=992,end=992) landline
      call lencl(landline,last)
      do while (last.lt.4)   ! get rid of possible blank lines
        read(dfile,'(a3000)',err=992,end=992) landline
        call lencl(landline,last)
      end do

      if (landline(3000-5:3000).ne.'      ') go to 993

************ get rid of all non-segment characters
      i = 1   ! i is an index for the space along landline
      do while (landline(i:i).ne.'(')
        i = i + 1
      end do
      shifti = i
      i = i + 1
      
      do while (landline(i:i).ne.')')
        landline(i-shifti:i-shifti) = landline(i:i)
        i = i + 1
      end do
      landline(i-shifti:i) = ' '

******** landline is now just the segments, read
      do ns = 1,maxlsegs
        lsegs(ns)='      '
      end do
      numlsegs = 0
      last = 1
      do while (last.ne.0)
        numlsegs = numlsegs + 1
        read(landline,*) lsegs(numlsegs)
        call spaceshift(landline,last)
      end do

******** check for duplicates
      do i = 1,numlsegs-1
        do j = i+1,numlsegs
          if (lsegs(i).eq.lsegs(j)) go to 994
        end do
      end do
        

      return
************ ERROR SPACE ***********************************************
991   print*, 'could not open file'
      print*, fnam
      print*,'enter land segment or seglist .land file name'
      read*,geography
      go to 101

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'could not find non-blank line'
      go to 999

993   report(1) = 'file has line longer than 3000 characters'
      report(2) = fnam
      report(3) = 'fix pp/src/calibration_utils/change_param/proc.f'
      go to 999

994   report(1) = 'file has a duplicate land segment'
      report(2) = fnam
      report(3) = lsegs(i)
      go to 999

999   print*,report(1)
      print*,report(2)
      print*,report(3)
      stop

      end


************************************************************************
** process geography, determine land uses to act on                   **
************************************************************************
      subroutine procluflag(lscen,luflag, numlu,lus,err)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/locations.inc'

      character*1 luflag  ! [a,p,i,s] = [all,perlnds,implnds,single]
      character*3 clu  ! character land use
      integer lus(nlu),numlu

      logical perlnd,implnd

      character*3000 landline  ! single line in data file

      integer nl,i,shifti,nl2

************ END DECLARATION ********************************************
      do nl = 1,nlu
        lus(nl) = 0
      end do

      err = 0
      if (luflag.eq.'A'.or.luflag.eq.'a') then  ! do all land uses
        numlu = nlu-1                           !  except water
        do nl = 1,numlu
          lus(nl) = nl
        end do

      else if (luflag.eq.'S'.or.luflag.eq.'s') then  ! do 1 land use
        print*,' Which number land use?'
        do nl = 1,nlu-1
          print*,'  ',nl,'  ',luname(nl)
        end do
        read*,lus(1)
        numlu = 1
        if (lus(1).ge.nlu) then
          print*,'land use ',lus(1),' not allowed, try again'
          err = 1
        end if

      else if (luflag.eq.'M'.or.luflag.eq.'m') then !user spec land uses
        print*,' Which numbers land use? Enter 0 when done'
        do nl = 1,nlu-1
          print*,'  ',nl,'  ',luname(nl)
        end do
        numlu = 1
        read*,lus(numlu)
        do while (lus(numlu).ne.0)
          numlu = numlu + 1
          read*,lus(numlu)
        end do
        numlu = numlu - 1
        do nl = 1,numlu
          if (lus(nl).ge.nlu) then
            print*,'land use ',lus(nl),' not allowed'
            err = 1
          end if
        end do
        do nl = 1,numlu-1
          do nl2 = nl+1,numlu
            if (lus(nl).eq.lus(nl2)) then
              print*,'You entered land use ',luname(lus(nl)),' twice.'
              err = 1
            end if
          end do
        end do

      else if (luflag.eq.'P'.or.luflag.eq.'p') then ! all perlnds
        call lencl(lscen,lenlscen)
        numlu = 0
        do nl = 1,nlu-1
          clu = luname(nl)
          call getlutype(lscen,lenlscen,clu,perlnd,implnd)
          if (perlnd) then
            numlu = numlu + 1
            lus(numlu) = nl
          end if
        end do

      else if (luflag.eq.'I'.or.luflag.eq.'i') then ! all implnds
        call lencl(lscen,lenlscen)
        numlu = 0
        do nl = 1,nlu-1
          clu = luname(nl)
          call getlutype(lscen,lenlscen,clu,perlnd,implnd)
          if (implnd) then
            numlu = numlu + 1
            lus(numlu) = nl
          end if
        end do

      else

        err = 1  ! did not correctly enter luflag

      end if
      end


************************************************************************
** process monthly data, determine months to act on                   **
************************************************************************
      subroutine procmonflag(lscen,monflag,nmonth,monthidx,err)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character*1 monflag  ! [a,s,m] = [all,single,multiple]
      character*3 cmon  ! character month
      integer monthidx(12),nmonth

      character*3000 landline  ! single line in data file

      integer nm,i,shifti,nm2

      character*3 monname(12)

      data monname /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     .             'SEP','OCT','NOV','DEC'/

************ END DECLARATION ********************************************

      do nm = 1,12
        monthidx(nm) = 0
      end do

      err = 0
      if (monflag.eq.'A'.or.monflag.eq.'a') then  ! do all months
        nmonth = 12
        do nm = 1,nmonth
          monthidx(nm) = nm
        end do

      else if (monflag.eq.'S'.or.monflag.eq.'s') then  ! do 1 month
        print*,' Which month?'
        do nm = 1,12
          print*,'  ',nm,'  ',monname(nm)
        end do
        read*,monthidx(1)
        nmonth = 1
        if (monthidx(1).gt.12) then
          print*,'month ',monthidx(1),' not valid, try again'
          err = 1
        end if

      else if (monflag.eq.'M'.or.monflag.eq.'m') then !user spec months
        print*,' Which numbers months? Enter 0 when done'
        do nm = 1,12
          print*,'  ',nm,'  ',monname(nm)
        end do
        nmonth = 1
        read*,monthidx(nmonth)
        do while (monthidx(nmonth).ne.0)
          nmonth = nmonth + 1
          read*,monthidx(nmonth)
        end do
        nmonth = nmonth - 1
 
        do nm = 1,nmonth
          if (monthidx(nm).gt.12) then
            print*,'month ',monthidx(nm),' not valid'
            err = 1
          end if
        end do
        do nm = 1,nmonth-1
          do nm2 = nm+1,nmonth
            if (monthidx(nm) .eq. monthidx(nm2)) then
              print*,'You entered month ',monname(monthidx(nm)),'twice'
              err = 1
            end if
          end do
        end do

      else

        err = 1  ! did not correctly enter luflag

      end if
      end


************************************************************************
** reads values from a file                                           **
************************************************************************
      subroutine readvalues(typeflag,paramscen,variable,maxlsegs,
     I                      numlsegs,lsegs,
     O                      rvalues,ivalues)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character*1 typeflag
      character*1 answer
      character*(*) variable

      integer maxlsegs,numlsegs  ! number of land segments

      character*(*) lsegs(maxlsegs)  ! names of land segments

      real rvalues(maxlsegs),Trvalue     ! real values for all lsegs
      integer ivalues(maxlsegs),Tivalue  ! intervalues of all land segs

      logical found(maxlsegs)   ! true if this segment found in file

      logical scompare  ! exterior comparison function
      external scompare

      integer lenpscen,lenvar  ! lengths of character variables

      integer ns  ! index

**********  END DECLARATIONS    ****************************************

********* INITIALIZATIONS
      call lencl(paramscen,lenpscen)
      call lencl(variable,lenvar)
      do ns = 1,numlsegs
        found(ns) = .false.
      end do

********** OPEN FILE
      fnam = pardir//'common/'//paramscen(:lenpscen)//'/'//
     .       variable(:lenvar)//'_factors.csv'

      open(dfile,file=fnam,status='old',iostat=err)
      do while (err.ne.0)
        print*,'could not open file ',fnam
        print*,' [T]ry again, [N]ew file, or [Q]uit?'
        read*,answer
        if (answer.eq.'n'.or.answer.eq.'N') then
          print*,' Enter new file name:'
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
          do ns = 1,numlsegs
            if (scompare(Tseg,lsegs(ns))) then
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
          do ns = 1,numlsegs
            if (scompare(Tseg,lsegs(ns))) then
              found(ns) = .true.
              rvalues(ns) = Trvalue
            end if
          end do
        end do

      end if

333   close(dfile)

      do ns = 1,numlsegs
        if (.not.found(ns)) then
          print*,' Did not find segment ',lsegs(ns),' in file:'
          print*,fnam
          print*,' parameters not changed'
          stop
        end if
      end do

      print*,' all values successfully read from ',fnam

      end
