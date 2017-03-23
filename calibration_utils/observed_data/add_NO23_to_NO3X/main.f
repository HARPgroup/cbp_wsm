************************************************************************
** Subroutine to add NO23 to NO3X                                     **
**                                                                    **
** reads the NO23 file, and the old and new versions of NO3X to       **
**   create the newest version of NO3X                                **
** if NO3 exists in any file with a particular day and time, keep it  **
**                                                                    **
************************************************************************
      implicit none
      include 'WQdata.inc'

      character*110 dline
      character*100 cfnam,vfnam  ! calibration and validation file names
      character*200 dfnams(3)
  
      integer nr,n,i
      integer nobs(3),no,n1,n3,oldnobs3

      logical anyexist,match

********* variables to read and store data
      integer obyear(ndaymax,3),year
      integer obmonth(ndaymax,3),month
      integer obday(ndaymax,3),day
      integer obhour(ndaymax,3),hour
      integer obmint(ndaymax,3),mint
      real obdata(ndaymax,3),val
      character*1 Qflag(ndaymax,3),Q
      character*10 station(ndaymax,3),sta

************ handle confluence (0003) stations
      integer ntype,nt  ! 2 types, regular and confluence
      parameter (ntype = 2)

************************* END DECLARATIONS *****************************
           
********** get river segs
      call getrsegs(nrsegs,rsegs)

******* Loop over all segments          
      do nr = 1, nrsegs

********* loop over segment types
        do nt = 1,ntype
                                     
          if (nt.eq.1) tseg = rsegs(nr)  ! set segment names
          if (nt.eq.2) tseg = rsegs(nr)(:9)//'0003'  ! confluence seg

          print*, tseg

***********Open existing files to read data
          dfnams(1) = tree//'pp/observed/'//
     .                'old_files_from_observed_directory/NO23/'//tseg//
     .                '.ONO23'
          dfnams(2) = tree//'pp/observed/'//
     .                'old_files_from_observed_directory/'//
     .                'NO3X_with_coretrend/'//tseg//
     .                '.ONO3X'
          dfnams(3) = tree//'pp/observed/'//
     .                'old_files_from_observed_directory/NO3X/'//tseg//
     .                '.ONO3X'

          anyexist = .false.
          do i = 1,3

            nobs(i) = 0

            open (unit=dfile,file=dfnams(i),status='old',iostat=err)
            if (err.eq.2) cycle
            if (err.ne.0) go to 990
            anyexist = .true.

************** file exists, read into memory
            
            do 
              read(dfile,'(a110)',err=994,end=222) dline
              read(dline,*,err=995,end=111)
     .              year,month,day,hour,mint,val,Q,sta
111           continue
              nobs(i) = nobs(i) + 1
              if (nobs(i) .ge. ndaymax) go to 991
              obyear(nobs(i),i) = year
              obmonth(nobs(i),i)= month
              obday(nobs(i),i)  = day
              obhour(nobs(i),i) = hour
              obmint(nobs(i),i) = mint
              obdata(nobs(i),i) = val
              Qflag(nobs(i),i)  = Q
              station(nobs(i),i)= sta
              val = -9
              Q = ''
              sta = ''
            end do 

222         close(dfile)

          end do


*********** got all three into memory
************ build off of file 3.  add any additional information
************ from the other files
          if (anyexist) then

            oldnobs3 = nobs(3)
            do no = 1,2
              do n1 = 1, nobs(no)
                match = .false.
                do n3 = 1,nobs(3)
                  if (obyear(n1,no).eq.obyear(n3,3)) then
                    if (obmonth(n1,no).eq.obmonth(n3,3)) then
                      if (obday(n1,no).eq.obday(n3,3)) then
                        if (obhour(n1,no).eq.obhour(n3,3)) then
                          if (obmint(n1,no).eq.obmint(n3,3)) then
                            if (station(n1,no).eq.station(n3,3)) then
                              match = .true.
                              exit
                            end if
                          end if
                        end if
                      end if
                    end if
                  end if
                end do
                if (.not.match) then
                  nobs(3) = nobs(3) + 1
                  if (nobs(3) .ge. ndaymax) go to 991
                  obyear(nobs(3),3) = obyear(n1,no)
                  obmonth(nobs(3),3) = obmonth(n1,no)
                  obday(nobs(3),3) = obday(n1,no)
                  obhour(nobs(3),3) = obhour(n1,no)
                  obmint(nobs(3),3) = obmint(n1,no)
                  obdata(nobs(3),3) = obdata(n1,no)
                  Qflag(nobs(3),3) = Qflag(n1,no)
                  station(nobs(3),3) = station(n1,no)
                end if
              end do
            end do
             
            cfnam =tree//'pp/observed/alldata/newNO3X/'
     .               //tseg//'.ONO3X'
            print*,nobs(1),nobs(2),oldnobs3,nobs(3),' ',cfnam(:80)
            open (unit=dfile+1,file=cfnam,status='unknown',iostat=err)
            if (err.ne.0) go to 992
            do n3 = 1,nobs(3)
              write(dfile+1,1001)obyear(n3,3),obmonth(n3,3),obday(n3,3),
     .                           obhour(n3,3),obmint(n3,3),obdata(n3,3),
     .                           Qflag(n3,3),station(n3,3)
            end do

            close(dfile+1)
          end if 

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
