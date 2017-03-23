***************************************************************************
** Subroutine to split flow data into: Calibration and Verfication       **
**                                                                       **
** Verification = middle 20% of all data                                 **
** Calibration = all data - Verification                                 **
***************************************************************************
      program main
      implicit none

      include '../../../src/lib/inc/standard.inc'
      include '../../../src/lib/inc/locations.inc'
      include '../../../src/lib/inc/rsegs.inc'

      character*13 riverseg(maxrsegs)          ! reiver segment
      character*7  riverid(maxrsegs)           ! river segmeent ID

      integer nrseg,nr

      character*110 dline
      character*100 cafnam,vefnam                    ! DATA file name
  
      integer year1, year2, oldyear
      integer year, month, day,hour,zero
      integer i,j,ic
      integer ny,ny2,nm,nm2,nd

      integer nrmv,nyrmove(nyearmax)       
      integer nyears,ndays
      integer nobday(nyearmax)

      integer yrindex(nyearmax),yrtemp,tempx
      integer obyear(ndaymax)
      integer obmonth(nyearmax,366)
      integer obday(nyearmax,366)
      real obflow(nyearmax,366),dflow(ndaymax)
      real flow,ftemp
      real flowvar(nyearmax),variance

      logical found,foundyr(nyearmax)
************************* END DECLARATIONS ***********************************
           
      read*, year1, year2                                 ! read in the user defined time period  

      fnam = tree//'pp/observed/gage_riverseg_notes.csv'  ! get pairs of rsegs and ID
      open (unit=dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nrseg = 0
      read(dfile,'(a110)',err=992,end=111) dline          ! get rid of the header 
      do
        read(dfile,'(a110)',err=992,end=111) dline        ! read line
***********loop over all river segments
        nrseg = nrseg + 1
        call findcomma(dline,ic)
        riverseg(nrseg)= dline(:ic-1)         ! get the name of river segment
        call shift(dline)
        call findcomma(dline,last)           ! get the segment ID
        riverid(nrseg) = dline(:last-1)
        if (dline(:last).ne.' ') then
         call shift(dline)
        end if
      end do                                 !loop over segments

111   close(dfile)
      
************ Loop over all segments          

      do nr = 1, nrseg
                                     
***********Open existing filw to read data
        fnam = tree//'pp/observed/alldata/FLOW/'//riverseg(nr)//'.OFLOW'
        open (unit=dfile,file=fnam,status='old',iostat=err)

        if (err.eq.0) then       ! when flow data file exists
          print*, 'split data for river segment ',riverseg(nr)
 
          do i = 1, nyearmax
            nobday(i) = 0
          end do

          nyears = 0
          oldyear = 1000

          do 
           read(dfile,*,err=992,end=222) year,month,day,hour,zero,flow
           if (year.ge.year1 .and. year.le.year2) then       ! only count days in specified time period
             if(year .ne. oldyear) then
               nyears = nyears + 1
               oldyear = year
               backspace(dfile)
             else
               nobday(nyears) = nobday(nyears) + 1 
               if (nobday(nyears) .ge. ndaymax) go to 994
               yrindex(nyears) = year 
               obyear(nyears) = year
               obmonth(nyears,nobday(nyears))= month
               obday(nyears,nobday(nyears))  = day
               obflow(nyears,nobday(nyears)) = flow
             end if 
           end if
          end do 

222   close(dfile)
           
************ find the variance of flow data of each year
          do ny = 1, nyears
            ndays = nobday(ny)
            do nd = 1, ndays
              dflow(nd) = obflow(ny,nd)           ! get daily flow for year ny
            end do
            flowvar(ny) = variance(dflow,ndays,ndaymax,err)
          end do

************ sort the variance 
          do ny = 1, nyears
            do ny2 = 1, nyears-ny
              if (flowvar(ny2) .gt. flowvar(ny2+1)) then
                ftemp = flowvar(ny2)
                flowvar(ny2) = flowvar(ny2+1)
                flowvar(ny2+1) = ftemp
                yrtemp = yrindex(ny2)
                yrindex(ny2) = yrindex(ny2+1)
                yrindex(ny2+1) = yrtemp
              end if
            end do
          end do

*********** Split the flow data based on the total years of record
          if (nyears .ge. 10) then                       ! remove 25th,50th, and 75th percentile variability
            nrmv = 3 
          else if (nyears.ge.5 .and. nyears.le.9) then   ! remove 33th and 67th percentile variability
            nrmv = 2
          else if (nyears .le. 4) then                   ! remove 50th percentile variability
            nrmv = 1
          end if

          do nm = 1, nrmv                                ! find which year to remove
         nyrmove(nm) = int(nm*real(nyears-1)/(real(nrmv)+ 1)+ 1.5) ! deal with round problem
          end do

********** Find which years to remove
          do ny = 1, nyears
            foundyr(ny) = .false.
            do nm = 1, nrmv
              if (obyear(ny) .eq. yrindex(nyrmove(nm))) then      ! if found the year to be removed
                foundyr(ny)=.true.
                exit
              end if
            end do
          end do

*********** write splited data into different files 
        vefnam =tree//'pp/observed/verify/FLOW/'//riverseg(nr)//'.OFLOW'
        open (unit=dfile+1,file=vefnam,status='unknown',iostat=err)
        if (err.ne.0) go to 992

        cafnam =tree//'pp/observed/calib/FLOW/'//riverseg(nr)//'.OFLOW'
        open (unit=dfile+2,file=cafnam,status='unknown',iostat=err)
        if (err.ne.0) go to 993

         do ny = 1, nyears
          ndays = nobday(ny)
          do nd = 1, ndays
           if (foundyr(ny)) then         ! if this year needs to be romved, put into verify data set
             write(dfile+1,1001) obyear(ny),obmonth(ny,nd),obday(ny,nd),
     .                            12,0,obflow(ny,nd)
           else                          ! otherwise, put into calibration data set          
             write(dfile+2,1001) obyear(ny),obmonth(ny,nd),obday(ny,nd),  
     .                            12,0,obflow(ny,nd)
           end if
          end do                                !loop over records for a segment
         end do

        close(dfile+2)
        close(dfile+1)   
     
       end if
      end do                                 !loop over segments

1001   format(i4,',',i2,',',i2,',',i2,',',i2,',',e12.5)  

      stop 

************************ ERROR SPACE
991   report(1) = 'Problem opening output file'//fnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

992   report(1) = 'Problem opening data file'//cafnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

993   report(1) = 'Problem opening data file'//vefnam
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = ' '
      go to 999

994   report(1) = 'the total days greater than max days specified'
      report(2) = ' need to check array boundary     '
      report(3) = ' '
      go to 999


999   call stopreport(report)

      end 
