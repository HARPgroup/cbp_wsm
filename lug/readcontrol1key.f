************************************************************************
** reads the control file for this scenario and finds the passed      **
**  keyword.  When found, gets:                                       **
**     - start time of each break                                     **
**     - number of Breaks (nB's)                                      **
**     - Files for each Breaks (Bfile's)                              **
************************************************************************

      subroutine readcontrol1key(lscen,lenlscen,keyword,
     O                           nB,Jday,Byear,Bmonth,Bday,Bfile)

      implicit none
      include 'lug.inc'
      include 'acts.inc'

      integer Byear(maxTimeBreaks)             ! year of break
      integer Bmonth(maxTimeBreaks)            ! month
      integer Bday(maxTimeBreaks)              ! day
      integer Jday(maxTimeBreaks)    ! julian day since start of sim

      character*100 templine(maxTimeBreaks)
      character*30 Bfile(maxTimeBreaks)

      character*(*) keyword
      integer l,n,i,nB

      integer julian
      external julian

      logical comment
      logical findkey, findtime

      integer T1year,T1month,T1day              ! start time
      integer T2year,T2month,T2day              ! end time


******* END DECLARATION *********************************************
      findkey = .false.
      findtime = .false.
     
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      call lencl(keyword,last)
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:last).eq.keyword(:last)) then
            findkey = .true.
            nB = 0
            read(dfile,'(a100)',err=993)line
            do while (line(:4).ne.'END '
     .           .or. line(5:last+4).ne.keyword(:last))
              if (.not.comment(line)) then
                nB = nB + 1
                if (nB.gt.maxTimeBreaks) go to 994
                templine(nB) = line
              end if
              read(dfile,'(a100)',err=993)line
            end do
            do n = 1,nB
             read(templine(n),1234)
     .            Byear(n),Bmonth(n),Bday(n),Bfile(n)
            end do

           else if (line(:4).eq.'TIME') then
            findtime = .true.
            read(dfile,1234) T1year, T1month, T1day
            read(dfile,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=993)line
            if (line(:8).ne.'END TIME') go to 992
           end if 
        end if            ! end if not comment line
      end do

      close (dfile)
 
      if (.not.findkey) call stopit(fnam,keyword)
      if (.not.findtime) call stopit(fnam,'TIME')

      if (nB.eq.1) then  ! need two points for interpolations
        nB = 2
        Byear(2) = T2year
        Bmonth(2) = T2month
        Bday(2) = T2day
        Bfile(2) = Bfile(1)
********** check for small denominator problem
        do n = 1, nB
          Jday(n) = julian(T1year,T1month,T1day,
     .                     Byear(n),Bmonth(n),Bday(n))
        end do
        if (abs(Jday(2)-Jday(1)).lt.100) then
          Byear(2) = Byear(2) + 1
          Jday(2) = julian(T1year,T1month,T1day,
     .                     Byear(2),Bmonth(2),Bday(2))
        end if
      end if
      
      do n = 1, nB
        Jday(n) = julian(T1year,T1month,T1day,
     .                   Byear(n),Bmonth(n),Bday(n))
      end do

      call sortdate(maxtimebreaks,Jday,Bfile,nB,Byear,Bmonth,Bday)

1234  format(i4,i3,i3,1x,a30)

      return

*********** ERROR SPACE ******************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) =  'control file '//fnam
      report(2) =  ' only allowed two rows in table TIME'
      report(3) = ' '
      go to 999

993   report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = 'maximum number of lines exceeded in table '//
     .            keyword(:last)
      report(2) = 'before end of table signifier END '//keyword(:last)
      report(3) = 'was found'
      go to 999

999   call stopreport(report)

      end


************************************************************************
**  sorts Break dates in case they were not entered sequentially      **
************************************************************************
      subroutine sortdate(maxTimeBreaks,jday,BBfile,n,year,month,day)

      implicit none

      integer maxTimeBreaks

      character*20 BBfile(maxTimeBreaks)        ! bmp factor files

      integer year(maxTimeBreaks)
      integer month(maxTimeBreaks)
      integer day(maxTimeBreaks)
      integer jday(maxTimeBreaks)
      character*20 tempfile
      integer i,j,n,itemp

      do i = 1,n
        do j = 1, n-i
          if (jday(j).gt.jday(j+1)) then
            itemp = jday(j)
            jday(j) = jday(j+1)
            jday(j+1) = itemp

            itemp = year(j)
            year(j) = year(j+1)
            year(j+1) = itemp

            itemp = month(j)
            month(j) = month(j+1)
            month(j+1) = itemp

            itemp = day(j)
            day(j) = day(j+1)
            day(j+1) = itemp

            tempfile = BBfile(j)
            BBfile(j) = BBfile(j+1)
            BBfile(j+1) = tempfile
          end if
        end do
      end do

      end
      
      
