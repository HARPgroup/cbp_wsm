***************************************************************************
** Subroutine to split flow data into: Calibration and Verfication       **
**                                                                       **
** Verification = middle 20% of all data                                 **
** Calibration = all data - Verification                                 **
***************************************************************************
      program main

      implicit none
      include 'WQdata.inc'

      character*110 dline
      character*100 cafnam,vefnam                    ! DATA file name
  
      integer nmaxrmpts
      parameter (nmaxrmpts=1000)

      integer year1, year2
      integer year,month,day,hour,mint
      integer nd,nc,nr

      integer npts,n,n1,n2       
      integer nobs,ntot
      integer nrand(nmaxrmpts),ncomp,tnum

      integer obyear(ndaymax)
      integer obmonth(ndaymax)
      integer obday(ndaymax)
      integer obhour(ndaymax)
      integer obmint(ndaymax)
      real obdata(ndaymax)
      real val
  
      character*1 Qflag(ndaymax),Q
      character*10 station(ndaymax),sta

      logical found(ndaymax)
************************* END DECLARATIONS ***********************************
           
      read*, year1, year2                                 ! read in the user defined time period  

********** get river segs
      call getrsegs(nrsegs,rsegs)

***********get constituent names
      call getconcname(nconcs,concname) 

******* loop over all constituents
      do nc = 1, nconcs 
        if (concname(nc) .eq. 'FLOW' ) cycle      ! flow is handled differently
        if (concname(nc) .eq. 'WTMP' ) cycle      ! no need to split temperature

******* Loop over all segments          
        do nr = 1, nrsegs
                                     
***********Open existing files to read data
          fnam = tree//'pp/observed/extended_data/'//concname(nc)//
     .              '/'//rsegs(nr)//'.O'//concname(nc)
          open (unit=dfile,file=fnam,status='old',iostat=err)

          if (err.eq.0) then       ! when flow data file exists
            print*, 'split ', concname(nc),' data for river ',rsegs(nr)

            nobs = 0
            do 
              read(dfile,*,err=992,end=222) 
     .               year,month,day,hour,mint,val,Q,sta
              if (year.ge.year1 .and. year.le.year2) then       ! only count days in specified time period
                nobs = nobs + 1
                if (nobs .ge. ndaymax) go to 993
                obyear(nobs) = year
                obmonth(nobs)= month
                obday(nobs)  = day
                obhour(nobs) = hour
                obmint(nobs) = mint
                obdata(nobs) = val
                Qflag(nobs)  = Q
                station(nobs)= sta
              end if
            end do 

222         close(dfile)

            if (nobs.eq.0) cycle  ! if no data in date range
           
********** generate random number
            ntot = int((nobs*0.2)+ 0.5)         ! total random numbers=20% 0f observed pts, 0.5 ued to deal with round number
            nrand(1) =int(rand()*nobs)+ 1       ! generate a random number
            npts = 1
            do while (npts .le. ntot)
              tnum = int(rand()*nobs)+ 1
              ncomp = 0
              do n = 1, npts
                if (tnum .eq. nrand(n)) exit  
                ncomp = ncomp + 1
              end do
              if (ncomp .eq. npts) then
                npts = npts + 1
                if (npts .ge. nmaxrmpts) go to 993
                nrand(npts) = tnum
              end if
            end do

********** Find data pts to be removed
            do n2 = 1, nobs
              found(n2) = .false.
            end do

            do n1 = 1, npts
             do n2 = 1, nobs
               if (n2 .eq. nrand(n1)) then      ! if found a pt to be removed
                 found(n2)=.true.
                 exit
               end if
              end do
            end do 

*********** write splited data into different files 
            vefnam =tree//'pp/observed/verify/'//concname(nc)//
     .              '/'//rsegs(nr)//'.O'//concname(nc)
            open (unit=dfile+1,file=vefnam,status='unknown',iostat=err)
            if (err.ne.0) go to 991

            cafnam =tree//'pp/observed/calib/'//concname(nc)//
     .              '/'//rsegs(nr)//'.O'//concname(nc)
            open (unit=dfile+2,file=cafnam,status='unknown',iostat=err)
            if (err.ne.0) go to 992

            do n = 1, nobs
              if (found(n)) then         ! if this year needs to be romved, put into verify data set
                write(dfile+1,1001) obyear(n),obmonth(n),obday(n),
     .                              obhour(n),obmint(n),obdata(n),
     .                              Qflag(n),station(n)
              else                          ! otherwise, put into calibration data set          
                write(dfile+2,1001) obyear(n),obmonth(n),obday(n),
     .                              obhour(n),obmint(n),obdata(n),
     .                              Qflag(n),station(n)
              end if
            end do                                !loop over records for a segment

            close(dfile+2)
            close(dfile+1)   
     
          end if
        end do                                 !loop over segments
      end do                                 ! loop over constituents

1001   format(i4,',',i2,',',i2,',',i2,',',i2,',',e12.5,',',a1,',',a10)  

      stop 

************************ ERROR SPACE
991   report(1) = 'Problem opening data file, '//vefnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening data file, '//cafnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

993   report(1) = 'the total numbers greater than max specified'
      report(2) = ' need to check array boundary     '
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end 
