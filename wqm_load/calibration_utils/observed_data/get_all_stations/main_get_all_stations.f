************************************************************************
** Subroutine to split flow data into: Calibration and Verfication    **
**                                                                    **
** Validation = last 25% of stations with more than 210 observations  **
**    if validation overlaps critical period of 1991-2000, use data   **
**    starting in 1985 to make up the remaining 25% of the obs        **
**                                                                    **
** Calibration = all data minus validation                            **
**                                                                    **
************************************************************************
      implicit none
      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'
      include '../../../src/lib/inc/rsegs.inc'

      integer nconcmax,nconcs,nc     ! number of concs
      parameter (nconcmax = 20)      ! max number of concentrations
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')

      character*110 dline
  
      integer nr,n
      integer nobs,no,nsta

********* variables to read and store data
      integer year
      integer month
      integer day
      integer hour
      integer mint
      real val
      character*1 Q
      character*10 station(ndaymax),sta
      character*13 staseg(ndaymax)

      logical found,firstcon

************ handle confluence (0003) stations
      integer ntype,nt  ! 2 types, regular and confluence
      parameter (ntype = 2)

************************* END DECLARATIONS *****************************
           
********** get river segs
      call getrsegs(nrsegs,rsegs)

***********get constituent names
      call getconcname(nconcs,concname) 

******* Loop over all segments          
      nsta = 0
      print*, 'looking for stations in '
      do nr = 1, nrsegs

********* loop over segment types
        do nt = 1,ntype
                                     
          if (nt.eq.1) tseg = rsegs(nr)  ! set segment names
          if (nt.eq.2) tseg = rsegs(nr)(:9)//'0003'  ! confluence seg

          call ttyput(tseg)
          call ttyput(' ')

******* loop over all constituents
          firstcon = .true.
          do nc = 1, nconcs 

***********Open existing files to read data
            fnam = tree//'pp/observed/alldata/'//concname(nc)//
     .              '/'//tseg//'.O'//concname(nc)
            open (unit=dfile,file=fnam,status='old',iostat=err)

            if (err.eq.2) cycle  ! file doesn't exist
            if (err.ne.0) go to 990

            if (firstcon) then
              firstcon = .false.
              print*,' '
              call ttyput(tseg)
            end if
            call ttyput(' ')
            call ttyput(concname(nc))

************** file exists, read into memory
            do 
              read(dfile,'(a110)',err=994,end=222) dline
              read(dline,*,err=995,end=111)
     .              year,month,day,hour,mint,val,Q,sta
              found = .false.
              do n = 1,nsta
                if (sta.eq.station(n)) then
                  found = .true.
                  if (tseg.ne.staseg(n)) then
                    print*,'problem with ',station(n),' ',tseg,' ',
     .                                     staseg(n),' ',concname(nc)
                    stop
                  end if
                  exit
                end if
              end do
              if (.not.found) then
                nsta = nsta + 1
                station(nsta) = sta
                staseg(nsta) = tseg
              end if
111           continue
            end do 

222         close(dfile)

************* close loops
          end do        ! loop over concentration types
          print*,' '
        end do        !loop over segment types
      end do        !loop over segments

      fnam = tree//'pp/observed/allstations.csv'
      open (unit=dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 990
      write(dfile,*) 'station,segment'
      do n = 1,nsta
        write(dfile,*) station(n),',',staseg(n)
      end do
      close(dfile)

      stop 

1001  format(i4,',',i2,',',i2,',',i2,',',i2,',',e12.5,',',a1,',',a10)  

************************ ERROR SPACE
990   report(1) = 'Problem opening alldata file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = fnam
      go to 999

991   report(1) = 'the total numbers greater than max specified'
      report(2) = ' need to check array boundary     '
      report(3) = ' '
      go to 999


994   report(1) = 'Problem reading alldata file: near line:'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,mint,val,Q,sta
      go to 999

995   report(1) = 'Problem reading alldata line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end 
***************************************************************************
** Subroutine to get the unique ID of each rive segs                     **
**                                                                       **
***************************************************************************
      subroutine getrsegs(
     O                     nr,rsegs)

      implicit none
      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'
      include '../../../src/lib/inc/rsegs.inc'

      integer nconcmax,nconcs,nc     ! number of concs
      parameter (nconcmax = 20)      ! max number of concentrations
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')



      character*110 cline
      character*13 catnam          ! reiver segment
     
      integer nr,ic 

      logical comment
      external comment
************************* END DECLARATIONS ***********************************

      fnam = tree//'pp/catalog/connect/rivernames.csv'
      open (unit=dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nr = 0
      do 
        read(dfile,'(a110)',err=992,end=993) cline                ! read line
        call d2x(cline,last)
        if (comment(cline)) cycle  ! ditch header

        if (cline(:3).eq.'end') exit

***********loop over all river segments
        nr = nr + 1
        call findcomma(cline,ic)              
        catnam = cline(:ic-1)                 ! get the name of river segment
        call trims(catnam,ic)
        rsegs(nr)=catnam(:ic)
      end do                                 !loop over segments
 
993   close(dfile)

     
      return 

*********** ERROR SPACE *******************************************************

991   report(1) = 'Problem with opening gage file'//fnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999


999   call stopreport(report)

      end 
************************************************************************
******* Routine to populate the variables that describe the           **
**        constituents of each water quality variable                 **
********** using the file  ./pp/lib/catalogs/rchres_out_to_conc       **
************************************************************************
      subroutine getconcname(nconcs,concname)

      implicit none
      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'

      integer nconcmax,nconcs,nc     ! number of concs
      parameter (nconcmax = 20)      ! max number of concentrations
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')


      character*200 pline    ! line long enough to read concfile line

      integer Tnccons         ! temporary variable for reading a line
********************* END DECLARATIONS ********************************** 

      fnam = catdir//'iovars/rchres_out_to_conc'  
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991 

      nconcs = 0
      read(dfile,'(a200)') pline
      do while (pline(:3).ne.'end')
        read(pline,'(16x,i4)',iostat=err) Tnccons
        if (err.eq.0.and.Tnccons.ne.0) then
          nconcs = nconcs + 1
          concname(nconcs) = pline(:4)
        end if
        read(dfile,'(a200)') pline
      end do

      close (dfile)

      return

*************** ERROR SPACE ********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end 
