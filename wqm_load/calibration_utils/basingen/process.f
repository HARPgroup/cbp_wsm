************************************************************************
** subroutines to read all of the data, share common blocks with main **
************************************************************************

************************************************************************
** subroutine to initialize all important variables                   **
************************************************************************
      subroutine initialize
      include 'basingen.inc'

C      print*,'initializing'    ! initialize
      do ns = 1,maxrsegs
        nallup(ns) = 0
        dsid(ns) = 0
        uniqid(ns) = 0
        rsegs(ns) = ' '
        do ns1 = 1,maxrsegs
          allup(ns,ns1) = 0
        end do
        do ns1 = 1,maxL2R
          allland(ns,ns1) = 0
          acres(ns,ns1) = 0.0
        end do
        nallland(ns) = 0
      end do
      do nl = 1,maxlsegs
        lacres(nl) = 0.0
      end do
      do ns = 1,supermax
        uniqindex(ns) = 0
C        calsite(ns) = .false.
      end do
        

      end

************************************************************************
** subroutine organize land to river data and get areas               **
************************************************************************
      subroutine getlseglist(
     I                       rproc,nrproc,
     O                       blseglist,nblseglist)
      include 'basingen.inc'
      integer nsup  ! index to upstream segment

      do nl = 1,maxlsegs
        blseglist(nl) = 0
      end do

      nblseglist = 0

      do ns = 1,nrproc
        do nr = 1,nallup(rproc(ns)) ! loop over all upstream segments

          nsup = allup(rproc(ns),nr) ! get index of upstream segment

          do nl = 1,nallland(nsup)  ! loop over all land in up seg

            found = .false.
            do n2 = 1,nblseglist   ! loop over already found lsegs
              if (allland(nsup,nl).eq.blseglist(n2)) then   ! if found
                found = .true.
                exit
              end if
            end do

            if (.not.found) then  ! if not found, add new land segment
              nblseglist = nblseglist + 1
              blseglist(nblseglist) = allland(nsup,nl)
            end if

          end do
        end do
      end do

      end


************************************************************************
** subroutine to find the upstream basins from a single point         **
************************************************************************
      subroutine findupstream(ns)
      include 'basingen.inc'

******** find list of all upstream segments for each segment
      nallup(ns) = 1  ! segment ns counts as first segment
      allup(ns,nallup(ns)) = ns
      foundnew = .true.

      do while (foundnew)

        foundnew = .false.

        n1end = nallup(ns)
        do n1 = 1,n1end   ! n1 is index for segments in upstream list
          do n2 = 1,nrsegs  ! n2 is index for all segments to search
            if (dsid(n2).eq.uniqid(allup(ns,n1))) then !check and add
              found = .false.  ! found = already in list
              do n3 = 1,n1end
                if (n2.eq.allup(ns,n3)) found = .true.
              end do
              if (.not.found) then
                nallup(ns) = nallup(ns) + 1
                allup(ns,nallup(ns)) = n2
                foundnew = .true.
              end if
            end if
          end do
        end do

      end do

      sorted = .false.
      do while (.not.sorted)
        sorted = .true.
        do n1 = 1,nallup(ns)-1
          do n2 = n1+1,nallup(ns)
            if (dsid(allup(ns,n2)).eq.uniqid(allup(ns,n1))) then
              n3 = allup(ns,n1)
              allup(ns,n1) = allup(ns,n2)
              allup(ns,n2) = n3
              sorted = .false.
            end if
          end do
        end do
      end do

      end

************************************************************************
** subroutine to determine the basins to process from the input       **
************************************************************************
      subroutine procinput(
     I                     segORbasin,
     O                     singleseg,nrproc,rproc)

      include 'basingen.inc'

      character*4 segORbasin,tempsb
      integer lenbasin
      logical singleseg


      read(segORbasin,'(i4)',iostat=err) oneseg
      if (err.eq.0) then  ! single segment
        singleseg = .true.
        nrproc = 1
        rproc(1) = uniqindex(oneseg)
        if (rproc(1).eq.0) go to 996
      else           ! multiple segments (e.g. 'PS' or 'Y' or 'all')
        singleseg = .false.
        tempsb = segORbasin
        call lowercase(tempsb)
        if (tempsb(:3).eq.'all') then  ! all segments
          nrproc = nrsegs
          do ns = 1,nrsegs
            rproc(ns) = ns
          end do
        else         ! a certain basin
          call lencl(segORbasin,lenbasin)
          nrproc = 0
          do ns = 1,nrsegs
            if (rsegs(ns)(:lenbasin).eq.segORbasin(:lenbasin)) then
              nrproc = nrproc + 1
              rproc(nrproc) = ns
            end if
          end do
          if (nrproc.eq.0) go to 997
        end if
      end if

      return

************* ERROR SPACE ****************
996   write(report(1),*) 'segment ',oneseg,' not found'
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = 'No segments found that start with '//
     .             segORbasin(:lenbasin)
      report(2) = ' '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end




