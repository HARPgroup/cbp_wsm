************************************************************************
***** increments one hour                                             **
************************************************************************

      subroutine onehour(iyear,imonth,iday,ihour)

      ihour = ihour + 1

      if (ihour.gt.24) then
        ihour = 1
        call tomorrow(iyear,imonth,iday)
      end if

      end

************************************************************************
***** decrements one hour                                             **
************************************************************************

      subroutine onehourless(iyear,imonth,iday,ihour)

      ihour = ihour - 1

      if (ihour.lt. 1) then
        ihour = 24
        call yesterday(iyear,imonth,iday)
      end if

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
***** decrements one day                                              **
************************************************************************

      subroutine yesterday(iyear,imonth,iday)

      iday = iday - 1

      if (iday.lt.1) then
        imonth = imonth - 1
        if (imonth.lt.1) then
          imonth = 12
          iyear = iyear - 1
        end if
        iday = ndaysinmonth(iyear,imonth)
      end if

      end
      

************************************************************************
***** finds total days between starting point and end point           **
************************************************************************

      function julian(iy,im,id,ky,km,kd)

      jiday = ndate2day(iy,im,id) 
      jkday = ndate2day(ky,km,kd)       ! get julian day of the year

      julian = jkday - jiday + 1

      if (iy.lt.ky) then

        do i = iy,ky-1                      ! add years between
          julian = julian + ndaysinyear(i)
        end do
        
      else if (iy.gt.ky) then

        do i = iy-1,ky,-1
          julian = julian - ndaysinyear(i)
        end do

      end if
      
      end

************************************************************************
***** finds days in a given year                                      **
************************************************************************

      function ndaysinyear(iyear)
 
      ndaysinyear = 365

      if (mod(iyear,4).eq.0)   ndaysinyear = 366
      if (mod(iyear,100).eq.0) ndaysinyear = 365
      if (mod(iyear,400).eq.0) ndaysinyear = 366

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

************************************************************************
** changes year, month, day data to julian day within that year       **
************************************************************************

      function ndate2day(iyear,imonth,iday)

      integer count

      count=iday
      do i=1,imonth-1
        count=count+ndaysinmonth(iyear,i)
      end do

      ndate2day=count

      end

************************************************************************
** changes julian day to month and day data within one year           **
************************************************************************

      subroutine day2date(jday,iyear,imonth,iday)

      iday = jday
      imonth=1
      do while (iday.gt.ndaysinmonth(iyear,imonth))
        iday = iday - ndaysinmonth(iyear,imonth)
        imonth = imonth+1
      end do
      end

************************************************************************
** returns the date jday days from a date  (not efficient)            **
************************************************************************

      subroutine day2datelong(iy1,im1,id1,jday,
     O                        iy2,im2,id2)

      iy2 = iy1
      im2 = im1
      id2 = id1
      do n = 2,jday
        call tomorrow(iy2,im2,id2)
      end do
      end

