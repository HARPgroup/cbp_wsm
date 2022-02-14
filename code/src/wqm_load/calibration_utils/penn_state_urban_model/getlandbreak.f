************************************************************************
******** GET THE DATA AT THE BREAKPOINTS                              **
**** programming strategy:  for each breakpoint (n in nLB), open the  **
**       file and read the header line to find the column order.      **
**       Look down the file to find the river segment, then look      **
**       across to put the data line with a land segment.  Use the    **
**       'order' variable to correctly populate the variable 'LBdata' **
************************************************************************

      subroutine getlandbreak(lseg,LBfile,LBdata,nLB)
      implicit none
      include 'mbtc.inc'
      character*300 dline
      character*13 ctemp
      integer order(nlu)                ! indexed to the column, not land use
      logical found
      integer i,k,l,ll,j,n
      integer nLB,lastLB,ifound
      integer nl2r,numsegs
      character*13 Trseg,Tl2r
      character*5 river
      character*4 land
      real x
      logical scompcase   ! string compare subroutine
      logical foundl(maxL2R)

      river = 'river'
      land = 'land'

      do n = 1, maxTimeBreaks           ! initialize LBdata
        do i = 1, maxL2R
          do l = 1,nlu
            LBdata(n,i,l) = -999.0
          end do
        end do
      end do

      do n = 1,nLB

        call trims(LBfile(n),lastLB)              ! open land use file
        fnam = tree//'pp/data/land_use/land_use_'
     .         //LBfile(n)(:lastLB)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=996)dline            ! read header line

        call findcomma(dline,i)                    ! check that first column is river
        ctemp = dline(:i-1)
        call trims(ctemp,i)
        if (.not.scompcase(ctemp(:i),river)) go to 992
        call shift(dline)

        call findcomma(dline,i)                    ! check that second column is land
        ctemp = dline(:i-1)
        call trims(ctemp,i)
        if (.not.scompcase(ctemp(:i),land)) go to 993
        call shift(dline)

        do l = 1,nlu                               ! find the order of the land uses
          order(l) = 0
          call findcomma(dline,i)
          ctemp = dline(:i-1)
          call trims(ctemp,i)
          if (ctemp(:i).ne.' ') then
            found = .false.
            do ll = 1,nlu
              if (ctemp(:i).eq.luname(ll)) then
                order(l) = ll  
                found = .true.
              end if
            end do
            if (.not.found) go to 994
          end if
          call shift(dline)
        end do

        ifound = 0
        do i = 1,numsegs
          foundl(i) = .false.
        end do
        do while (ifound.lt.numsegs)          ! read down until all land segs found
          read(dfile,'(a300)',err=997)dline
          if (dline(:3).eq.'end') go to 998
          call findcomma(dline,last)
          Trseg = dline(:last-1)
          if (Trseg.eq.rseg) then
            ifound = ifound + 1
            call shift(dline)
            call findcomma(dline,last)
            Tl2r = dline(:last-1)
            found = .false.
            do i = 1,numsegs
              if (scompcase(Tl2r,l2r(i))) then
                foundl(i) = .true.
                found = .true.
                nl2r = i
              end if
            end do
            if (.not.found) go to 995
            do l = 1,nlu                       ! we have land seg, read across
              call shift(dline)                ! according to order found above
              call fread(dline,x)
              LBdata(n,nl2r,order(l)) = x
            end do
          end if

        end do

        close (dfile)

      end do
        

      return
*********** ERROR SPACE
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'first column must be the River segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

993   report(1) = 'second column must be the land segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

994   report(1) = 'land use type '//ctemp(:i)//' not valid type'
      report(2) = 'check the file'
      report(3) = fnam
      go to 999

995   report(1) = 'found unexpected land segment '//Tl2r
      report(2) = ' associated with '//rseg//' in file '
      report(3) = fnam
      go to 999

996   report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

998   report(1) = 'did not find all land segs for river segment'
      report(2) = fnam
      report(3) = 'river seg '//Rseg//' land seg(s)'
      do i = 1,numsegs
        if (.not.foundl(i))then
          call lencl(report(3),last)
          report(3) = report(3)(:last)//' '//l2r(i)
        end if
      end do
      go to 999


999   call stopreport(report)

      end


