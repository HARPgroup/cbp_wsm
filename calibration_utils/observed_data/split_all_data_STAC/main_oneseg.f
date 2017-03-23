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
      include 'WQdata.inc'

      character*110 dline
      character*100 cfnam,vfnam  ! calibration and validation file names
  
      integer nmaxrmpts
      parameter (nmaxrmpts=1000)

      integer year1, year2
      data year1,year2 /1985,2005/

      integer Calyear1, Calyear2   
      data Calyear1,Calyear2 /1991,2000/     ! calibration period

      integer nr,n
      integer nobs,no

********* variables to read and store data
      integer obyear(ndaymax),year
      integer obmonth(ndaymax),month
      integer obday(ndaymax),day
      integer obhour(ndaymax),hour
      integer obmint(ndaymax),mint
      real obdata(ndaymax),val
      character*1 Qflag(ndaymax),Q
      character*10 station(ndaymax),sta

******** validation segments and variables
      character*13 vsegs(maxrsegs)  ! validation segments
      integer nvsegs,nv
      logical vseg  ! is the current seg a validation segment

      real STACfrac  ! fraction reserved for validation
      parameter (STACfrac=0.25)  ! recommeded by STAC

      integer lastCalOb  ! index of last calibration observation

************ handle confluence (0003) stations
      integer ntype,nt  ! 2 types, regular and confluence
      parameter (ntype = 2)

************************* END DECLARATIONS *****************************
           
********** get river segs
      call getrsegs(nrsegs,rsegs)
      nrsegs = 1
      rsegs(1) = 'SJ6_2130_2050'

********** get validation stations
      call getvsegs(nvsegs,vsegs)

***********get constituent names
      call getconcname(nconcs,concname) 

******* Loop over all segments          
      do nr = 1, nrsegs

********* loop over segment types
        do nt = 1,ntype
                                     
          if (nt.eq.1) tseg = rsegs(nr)  ! set segment names
          if (nt.eq.2) tseg = rsegs(nr)(:9)//'0003'  ! confluence seg

**************** determine if this is a validation segment
          vseg = .false.
          do nv = 1,nvsegs
            if (tseg.eq.vsegs(nv)) vseg = .true.
          end do 

          print*, 'create calibration data set for river ',tseg
          if (vseg) print*,' splitting out validation data set'
          call ttyput('   ')

******* loop over all constituents
          do nc = 1, nconcs 

***********Open existing files to read data
            fnam = tree//'pp/observed/alldata/'//concname(nc)//
     .              '/'//tseg//'.O'//concname(nc)
            open (unit=dfile,file=fnam,status='old',iostat=err)

            if (err.eq.2) cycle  ! file doesn't exist
            if (err.ne.0) go to 990

            call ttyput(' ')
            call ttyput(concname(nc))

************** file exists, read into memory
            nobs = 0
            
            do 
              read(dfile,'(a110)',err=994,end=222) dline
              read(dline,*,err=995,end=111)
     .              year,month,day,hour,mint,val,Q,sta
111           continue
              if (abs(val+9).lt.0.01) cycle  ! missing value
              if (val.lt.0.001) then  ! cal routine can not handle negs
                val = 0.001
                Q = '<'
              end if
              if (year.ge.year1 .and. year.le.year2) then
                nobs = nobs + 1
                if (nobs .ge. ndaymax) go to 991
                obyear(nobs) = year
                obmonth(nobs)= month
                obday(nobs)  = day
                obhour(nobs) = hour
                obmint(nobs) = mint
                obdata(nobs) = val
                Qflag(nobs)  = Q
                station(nobs)= sta
              end if
              val = -9
              Q = ''
              sta = ''
            end do 

222         close(dfile)

            if (nobs.eq.0) cycle  ! if no data in date range

************* write calibration files for non-validation sites
            if(.not. vseg) then
              lastCalOb = nobs
              cfnam =tree//'pp/observed/calib/'//concname(nc)//
     .               '/'//tseg//'.O'//concname(nc)
              open (unit=dfile+1,file=cfnam,status='unknown',iostat=err)
              if (err.ne.0) go to 992

              do n = 1, lastCalOb
                write(dfile+1,1001) obyear(n),obmonth(n),obday(n),
     .                              obhour(n),obmint(n),obdata(n),
     .                              Qflag(n),station(n)
              end do

              close(dfile+1)
            end if 

************* determine last calibration observation
************ STAC recommended 25% of the observations be reserved
************ for validation at those sites
            if (vseg) then
              lastCalOb = int((1.0-STACfrac)*real(nobs))

              cfnam =tree//'pp/observed/calib/'//concname(nc)//
     .               '/'//tseg//'.O'//concname(nc)
              open (unit=dfile+1,file=cfnam,status='unknown',iostat=err)
              if (err.ne.0) go to 992

              vfnam =tree//'pp/observed/valid/'//concname(nc)//
     .                '/'//tseg//'.O'//concname(nc)
              open (dfile+2,file=vfnam,status='unknown',iostat=err)
              if (err.ne.0) go to 993
              
              no = 0 
              do n = lastCalOb+1,nobs
                if(obyear(n).ge.Calyear1.and.obyear(n).le.Calyear2) then ! if validate data contains 1991-2000 data
                  no = no + 1                                            ! find number of overlap data points
                end if
              end do

************ write calibration data set
              do n = 1+no,lastCalOb+no 
                write(dfile+1,1001) obyear(n),obmonth(n),obday(n),
     .                              obhour(n),obmint(n),obdata(n),
     .                              Qflag(n),station(n)
              end do  
              close(dfile+1)

************ write validation data set
              if (no .ge. 1) then                   ! first write data moved from calibration data set            
                do n = 1,no
                  write(dfile+2,1001) obyear(n),obmonth(n),obday(n),
     .                                obhour(n),obmint(n),obdata(n),
     .                                Qflag(n),station(n)
                end do
              end if

              do n = lastCalOb+no+1,nobs            ! then write rest validation data
                write(dfile+2,1001) obyear(n),obmonth(n),obday(n),
     .                              obhour(n),obmint(n),obdata(n),
     .                              Qflag(n),station(n)
              end do
              close(dfile+2)
            end if

************* close loops
          end do        ! loop over concentration types
          print*,' '
        end do        !loop over segment types
      end do        !loop over segments

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

992   report(1) = 'Problem opening calibration data file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = cfnam
      go to 999

993   report(1) = 'Problem opening validation data file, '
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = vfnam
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
