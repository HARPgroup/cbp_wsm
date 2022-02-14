      integer year,month,day,hour,minute,lenf
      real value
      character*1 flag
      character*16 station
      character*50 fnam,fnam2
      character*50 id 

      character*10 date
      character*8 time

      read 12,fnam
      fnam2 = fnam
      do i = 1,50
        if (fnam2(i:i).eq.'.') then
          fnam2(i+1:i+1) = 'P'
          exit
        end if
      end do
	print*,fnam2

      call lencl(fnam,lenf)
      do i = 1,lenf
        if (fnam(i:i).eq.'/') then
          id(1:lenf-i) = fnam(i+1:lenf)
          print*,id(1:lenf-i)
          exit
        end if
      end do

	print*,fnam2
      open(11,file=fnam,status='old')
      open(12,file=fnam2,status='unknown')

      do 
        read(11,*,end=111,err=999)
     .       year,month,day,hour,minute,value,flag,station
             if (hour.eq.24) then
               hour = 0
               call tomorrow(year,month,day)
             end if
             write(date,1233) month,day,year
             if (month.lt.10) date(1:1) = '0'
             if (day.lt.10) date(4:4) = '0'
             write(time,1235) hour,minute
             if (hour.lt.10) time(1:1) = '0'
             if (minute.lt.10) time(4:4) = '0'
        write(12,1234) id(:lenf-i),date,time,value
      end do 

111   close(11)
      close(12)

      stop
12    format(a)
1234  format(a20,4x,a10,4x,a8,4x,f10.3)
1233  format(i2,'/',i2,'/',i4)
1235  format(i2,':',i2,':00')

999   print*,'problem reading file ',fnam
      print*,' near line ',year,month,day,hour,minute,value,flag,station


      end 

************************************************************************
** Gets the length of a character variable ignores trailing blanks    **
************************************************************************
      subroutine lencl(c,l)
      implicit none
      character*(*) c
      integer i,l
      l = 0
      do i = 1,len(c)
        if (c(i:i).ne.' ') l = i
      end do
      end


************************************************************************
************************************************************************
***** increments one day                                              **
************************************************************************

      subroutine tomorrow(iyear,imonth,iday)

      iday = iday + 1

      if (iday.gt.ndaysinmonth(iyear,imonth)) then
        iday = 1
        imonth = imonth + 1
        if (imonth.gt.12) then
          imonth = 1
          iyear = iyear + 1
        end if
      end if

      end

************************************************************************
***** finds days in month for a given year                            **
************************************************************************

      function ndaysinmonth(iyear,imonth)
 
      integer nominal(12)

      data nominal /31,28,31,30,31,30,31,31,30,31,30,31/

      ndaysinmonth = nominal(imonth)

      if (imonth.eq.2) then
        if (mod(iyear,4).eq.0)   ndaysinmonth = 29
        if (mod(iyear,100).eq.0) ndaysinmonth = 28
        if (mod(iyear,400).eq.0) ndaysinmonth = 29
      end if

      end

