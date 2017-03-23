************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine getacres(
     I                    rscen,lseg,rseg,year1,year2,
     O                    acres)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      integer LBJday(maxTimeBreaks)             ! land use julian day
      character*20 LBfile(maxTimeBreaks)        ! land use files
      real LBdata(maxTimeBreaks,nlu)     ! land use

      integer year1,year2

      integer i, nLB, nBB, numsegs,l

      real acres(nlu)
      
************* END DECLARATIONS *****************************************

      call lencl(rscen,lenrscen)

      call readcontrol(rscen,lenrscen,year1,
     O                 LBJday,LBfile,nLB)
     
******** GET THE DATA AT THE BREAKPOINTS
      call getlandbreaks(rseg,lseg,nLB,LBfile,
     O                   LBdata)
    
******** MAKE THE FINAL FACTOR FILES
      call calcacres(LBJday,LBdata,nLB,year1,year2,
     O               acres)

*********** ERROR SPACE

      end
************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol(rscen,lenrscen,year1,
     O                       LBJday,LBfile,nLB)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      integer LBJday(maxTimeBreaks)             ! land use julian day
      character*20 LBfile(maxTimeBreaks)        ! land use files
      integer itemp
      character*20 tempfile

      integer LByear(maxTimeBreaks)    ! land use year
      integer LBmonth(maxTimeBreaks)   ! land use month
      integer LBday(maxTimeBreaks)     ! land use factor application day

      character*100 templine(maxTimeBreaks)

      integer year1

      logical comment

      integer nLB,l,n,i,j

      integer julian
      external julian

      logical findlu

      data findlu /.false./

*************** END OF DECLARATION *************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:8).eq.'LAND USE') then
            findlu = .true.
            nLB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:12).ne.'END LAND USE')
              if (.not.comment(line)) then
                nLB = nLB + 1
                templine(nLB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nLB
              read(templine(n),1234)
     .              LByear(n),LBmonth(n),LBday(n),LBfile(n)
            end do

          end if
        end if
      end do

      close (dfile)
      if (.not.findlu) go to 991

      do n = 1, maxTimeBreaks  
        LBJday(n) = -ndaymax-9   ! initialize low since could be neg
      end do
      do n = 1, nLB
        LBJday(n) = julian(year1,1,1,
     .                        LByear(n),LBmonth(n),LBday(n))
      end do

      do i = 1,nLB   ! sort
        do j = 1, nLB-i
          if (LBJday(j).gt.LBJday(j+1)) then
            itemp = LBJday(j)
            LBJday(j) = LBJday(j+1)
            LBJday(j+1) = itemp
            tempfile = LBfile(j)
            LBfile(j) = LBfile(j+1)
            LBfile(j+1) = tempfile
          end if
        end do
      end do

      return

1234  format(i4,i3,i3,1x,a20)

*********** ERROR SPACE
991   report(1) = 'Error reading file'
      report(2) = fnam
      report(3) = 'did not find LAND USE section'
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end

************************************************************************
******** GET THE DATA AT THE BREAKPOINTS                              **
**** programming strategy:  for each breakpoint (n in nLB), open the  **
**       file and read the header line to find the column order.      **
**       Look down the file to find the river segment, then look      **
**       across to put the data line with a land segment.  Use the    **
**       'order' variable to correctly populate the variable 'LBdata' **
************************************************************************

      subroutine getlandbreaks(rseg,lseg,nLB,LBfile,
     O                         LBdata)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      character*20 LBfile(maxTimeBreaks)        ! land use files
      real LBdata(maxTimeBreaks,nlu)     ! land use

      character*600 dline
      character*13 ctemp
      integer order(nlu)  ! indexed to the column, not land use
      integer i,k,l,ll,j,n
      integer nLB,lastLB
      integer nl2r,numsegs
      character*13 Trseg,Tl2r
      character*5 river
      character*4 land,segr,segl
      character*8 riverseg
      character*7 landseg
      real x
      logical scompcase   ! string compare subroutine
      logical found

*************** END OF DECLARATION *************************************
      river = 'river'
      segr = 'rseg'
      riverseg = 'riverseg'
      land = 'land'
      landseg = 'landseg'
      segl = 'lseg'

      do n = 1, maxTimeBreaks           ! initialize LBdata
        do i = 1, maxL2R
          do l = 1,nlu
            LBdata(n,l) = -999.0
          end do
        end do
      end do

      do n = 1,nLB

        call trims(LBfile(n),lastLB)              ! open land use file
        fnam = ScenDatDir//'river/land_use/land_use_'
     .         //LBfile(n)(:lastLB)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a600)',err=996)dline            ! read header line
        if (dline(600-2:600).ne.'   ') go to 990

        call findcomma(dline,i)   ! check that first column is river
        ctemp = dline(:i-1)
        call trims(ctemp,i)
        if (.not.(scompcase(ctemp(:i),river)
     .        .or.scompcase(ctemp(:i),segr)  
     .        .or.scompcase(ctemp(:i),riverseg))) go to 992
        call shift(dline)

        call findcomma(dline,i)   ! check that second column is land
        ctemp = dline(:i-1)
        call trims(ctemp,i)
        if (.not.(scompcase(ctemp(:i),land)
     .        .or.scompcase(ctemp(:i),segl)  
     .        .or.scompcase(ctemp(:i),landseg))) go to 993
        call shift(dline)

        do l = 1,nlu              ! find the order of the land uses
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

        do 
          read(dfile,'(a600)',err=997)dline
          if (dline(600-2:600).ne.'   ') go to 990
          if (dline(:3).eq.'end') go to 998
          call findcomma(dline,last)
          Trseg = dline(:last-1)
          if (Trseg.eq.rseg) then
            call shift(dline)
            call findcomma(dline,last)
            Tl2r = dline(:last-1)
            if (scompcase(Tl2r,lseg)) then
              do l = 1,nlu    ! we have land seg, read across
                call shift(dline)  ! according to order found above
                call fread(dline,x)
                LBdata(n,order(l)) = x
              end do
              exit
            end if
          end if

        end do

        close (dfile)

      end do
        
      return

*********** ERROR SPACE ************************************************
990   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = ' line too long, change character size in program '
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
      go to 999


999   call stopreport(report)

      end


************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine calcacres(LBJday,LBdata,nLB,year1,year2,
     O                     acres)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/land_use.inc'

      integer maxTimeBreaks
      parameter (maxTimeBreaks = 30)

      integer LBJday(maxTimeBreaks)             ! land use julian day
      real LBdata(maxTimeBreaks,nlu)     ! land use

      integer year1,year2

      real lufac(ndaymax,nlu)
      real acres(nlu)
      logical lastindx
      integer i,k,l,j,indx1,indx2
      integer nLB,ndays
      real denom,lfactor(nlu)

      integer julian
      external julian

********** END OF DECLARATION ******************************************
      do j = 1,nLB
        do l = 1,nlu
          if (LBdata(j,l).lt.-0.1) go to 990
        end do
      end do

      ndays = julian(year1,1,1,year2,12,31)

      if (nLB.eq.1) then  ! create second point for interpolation
        nLB = 2
        LBJday(2) = ndays
        if (ndays-LBJday(1).le.300) LBJday(2) = LBJday(1) + 1000
        do l = 1,nlu
          LBdata(2,l) = LBdata(1,l)
        end do
      end if

********** formula for interpolation is break1 + (break2-break1)
************   /(day2-day1) * (currentday - day1)
***********   this holds true up to day2

********* interpolate land use
      j = 1                        
      indx1 = 1
      indx2 = 2
      lastindx = .false.
        
      do while (j.le.ndays)
        denom = real(LBJday(indx2) - LBJday(indx1))
        do l = 1,nlu
          lfactor(l) = (LBdata(indx2,l)-LBdata(indx1,l))/denom
        end do

        do while ((j.le.LBJday(indx2).or.lastindx).and.j.le.ndays)
          do l = 1,nlu
            lufac(j,l) = LBdata(indx1,l) + 
     .                   lfactor(l)*real(j-LBJday(indx1))
          end do
          j = j + 1
        end do

        if (indx2.eq.nLB) then
          lastindx = .true.
        else
          indx2 = indx2+1
          indx1 = indx1+1
        end if

      end do

********** check for negatives
      call fixneglu(
     I              ndaymax,nlu,ndays,
     M              lufac)

********** average
      do l = 1,nlu
        acres(l) = 0.0
        do j = 1,ndays
          acres(l) = acres(l) + lufac(j,l)
        end do
        acres(l) = acres(l) / real(ndays)
      end do 

      return

**************** ERROR SPACE *****************************************
990   report(1) = 'problem with land use data, land segment '//l2r(i)//
     .            ' River '//rseg
      report(2) = '  landuse    value'
      write(report(3),'(a3,2x,f10.4)') 
     .                luname(l),LBdata(j,l)
      go to 999

991   report(1) = 'problem with Land use data, '
      report(2) = '   land segment  River '//rseg
      report(3) = '   day 99999, land use '//luname(l)//'  value '
      write(report(3)(8:12),'(i5)') j
      write(report(3)(35:44),'(f10.1)') lufac(j,l)
      go to 999

992   report(1) = 'problem with BMP data, land segment '//
     .            ' River '//rseg
      report(2) = '  day  landuse    con        value'
      go to 999

999   call stopreport(report)

      end
************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine fixneglu(
     I                    ndaymax,nlu,ndays,
     M                    lufac)
      implicit none
      integer ndaymax,nlu,ndays
      real lufac(ndaymax,nlu)
      integer l,j

      real sumneg ! sum of negatives for that day
      real sumpos ! sum of posatives for that day
      logical neglu ! do negatives exist this day

********** END OF DECLARATION ******************************************

      do j = 1,ndays
        neglu = .false.

        do l = 1,nlu  ! check for negatives 
          if (lufac(j,l).lt.0.0) neglu = .true.
        end do

        if (neglu) then  ! if found negatives, then adjust all   
          sumneg = 0.0
          sumpos = 0.0
          do l = 1,nlu
            if (lufac(j,l).lt.0.0) then
              sumneg = sumneg + lufac(j,l)
            else
              sumpos = sumpos + lufac(j,l)
            end if
          end do
          do l = 1,nlu
            if (lufac(j,l).lt.0.0) then
              lufac(j,l) = 0.0
            else
              lufac(j,l) = lufac(j,l) * (sumpos + sumneg) / sumpos
            end if
          end do
        end if

      end do
      end
