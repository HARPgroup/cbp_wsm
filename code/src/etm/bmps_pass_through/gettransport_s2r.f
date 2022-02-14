************************************************************************
******** GET load transport factor                                    **
******** programming strategy:                                        **
**    open the file and read the header line to find the column order **
**     Look down the file to find the river segment, then look across **
**       to put the data line with a land segment.                    **
**     Use the 'order' variable to correctly populate 'Trandata'      **
************************************************************************
      subroutine gettransport_s2r(
     I                        rseg,numsegs,l2r,tranfile,
     I                        BMPconname,nBmpCon,
     O                        trandataS2R)
      implicit none

      include 'mbtc.f'

      character(2000):: dline !BHATT 900 to 2000
      character(13):: ctemp,colname
      integer:: order(maxBMPcon*nlu,2)    ! first index is column
                                      ! second is  1:trancon, 2:lu
      logical:: found
      integer:: i,k,l,j,n,kl,kk,ll
      integer:: ifound
      integer:: numsegs, nl2r
      integer:: ntranps, lastran

      character(13) Trseg,Tl2r
      character(5) river
      character(4) land
      real:: x

      logical:: scompcase             ! string comparison routines
      logical:: foundl(maxL2R)


************* some constituents are a combination of N and P
********* these have the BMPconstituent of "bod"
********** they need to have the average of N and P
       integer nb
       logical foundBOD,foundTN,foundTP
       integer nBOD,nTN,nTP

**************END DECLARATION ******************************************

      river = 'river'
      land = 'land'

      do i = 1, maxL2R             ! initialize Trandata
        do k = 1,nBmpCon
          do l = 1,nlu
            trandataS2R(i,k,l) = 1.0
          end do
        end do
      end do

      call trims(tranfile,lastran)              ! open transport file
      fnam = pardir//'transport/'//tranfile(:lastran)//'_s2r.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a2000)',err=996)dline            ! read header line !BHATT 900 to 2000
      call d2x(dline,i)
      if (dline(2000-3:2000).ne.'    ') go to 988 !BHATT 900 to 2000

      call findcomma(dline,i) 
      call shift(dline)

      call findcomma(dline,i)   
      call shift(dline)

      ntranps = 0
      foundBOD = .false.
      do kl = 1,nBmpCon*nlu       ! find the order of the bmps
        order(kl,1) = 0
        order(kl,2) = 0
        call findcomma(dline,i)
        ctemp = dline(:i-1)
        call trims(ctemp,i)
        if (ctemp(:i).ne.' ') then
          found = .false.
          do kk = 1,nBmpCon
            do ll = 1,nlu
              colname = BMPconname(kk)//luname(ll)
              if (ctemp(:i).eq.colname) then
                order(kl,1) = kk
                order(kl,2) = ll
                found = .true.
                ntranps = ntranps + 1
                if (BMPconname(kk).eq.'bod') foundBOD = .true.
              end if
            end do
          end do
          if (.not.found) go to 994
        end if
        call shift(dline)
      end do

      ifound = 0
      do i = 1,numsegs
        foundl(i) = .false.
      end do
      do while (ifound.lt.numsegs)  ! file until all land segs are found
        read(dfile,'(a2000)',err=997,end=998)dline !BHATT 900 to 2000
        call d2x(dline,last)
        if (dline(2000-5:2000).ne.'      ') go to 990 !BHATT 900 to 2000
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
          do kl = 1,ntranps                            ! now that we have the land segment, read across
            call shift(dline)                        ! the file according to the order found above
            call fread(dline,x)
            trandataS2R(nl2r,order(kl,1),order(kl,2)) = x
          end do
        end if
      end do

      close (dfile)

*********** correct for BOD if missing
      if (.not.foundBOD) then

        found = .false.  ! check to see if BOD is a constituent
        do nb = 1,nBmpCon
          if (BMPconname(nb).eq.'bod') then
            found = .true.
            nBOD = nb
          end if
        end do

        if (found) then   ! inherit N or P or average

          foundTN = .false.  ! find tn and tp indices
          foundTP = .false.
          do nb = 1,nBmpCon
            if (BMPconname(nb).eq.'tnx') then
              foundTN = .true.
              nTN = nb
            end if
            if (BMPconname(nb).eq.'tpx') then
              foundTP = .true.
              nTP = nb
            end if
          end do

          if (foundTN.and.foundTP) then ! take average
            do i = 1, maxL2R           
              do l = 1,nlu
                trandataS2R(i,nBOD,l) = (trandataS2R(i,nTN,l)
     .                             +  trandataS2R(i,nTP,l)) / 2.0
              end do
            end do
          else if (foundTN) then      ! set to TN
            do i = 1, maxL2R            
              do l = 1,nlu
                trandataS2R(i,nBOD,l) = trandataS2R(i,nTN,l)
              end do
            end do
          else if (foundTP) then      ! set to TP
            do i = 1, maxL2R             
              do l = 1,nlu
                trandataS2R(i,nBOD,l) = trandataS2R(i,nTP,l)
              end do
            end do
          else
            go to 989
          end if

        end if    ! end if BOD is a consituent in this IO scenario
      end if    ! end if BOD was not found in the transfile

      return

*********** ERROR SPACE ******************************************
988   report(1) = 'error reading file:  line too long:'
      report(2) = fnam
      report(3) = dline
      go to 999

989   report(1) = 'problem with iovars specifications. '//
     .            'BOD is specified as a BMP '
      report(2) = 'constituent, but not given a transport factor.'
     .            //'  this is OK if '
      report(3) = 'tn and/or tp are BMP consituents and have'//
     .            ' transport factors'
      go to 999

990   report(1) = 'problem in ETM:  character variable not long '
      report(2) = 'enough to read entire file, modify:'
      report(3) = 
     .     './code/src/etm/make_binary_transfer_coeffs/gettransport.f'
      go to 999

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
      report(2) = ' associated with '//Trseg//' in file '
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

998   report(1) = 'Could not find all land segs for river seg, file'
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


