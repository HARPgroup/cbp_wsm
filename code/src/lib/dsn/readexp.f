      integer year1,year2
      parameter (year1=1984,year2=1997)
      real wind(year1:year2,12,31,24)
      character*1,dummy
      real value(400)

      do nh = 1,24
        do nd = 1,31
          do nm = 1,12
            do ny = year1,year2
              wind(ny,nm,nd,nh) = 0.0
            end do
          end do
        end do
      end do

      open(11,file='just54.exp',status='old')

      do i = 1,19
        read(11,'(a1)') dummy
      end do

      do

	read(11,*,err=999)ny,nm,nd,nh,i1,i2,i3,i4,i5,nv,iflag,value(1)

        if (iflag.eq.1) then   ! repeats
          do i = 1,nv
            value(i) = value(1)
          end do
        else         ! need to read them all
          backspace 11
         read(11,*)ny,nm,nd,nh,i1,i2,i3,i4,i5,nv,iflag,(value(i),i=1,nv)
        end if

        if (nv.gt.400) then
          print*,'value must have larger dimension'
          stop
        end if

        do i = 1,nv
          call increment(ny,nm,nd,nh)
          wind(ny,nm,nd,nh) = value(i)
        end do

      end do

999   do nh = 2,24
        do nd = 1,31
          do nm = 1,12
            do ny = year1,year2
              wind(ny,nm,nd,1) = wind(ny,nm,nd,1) + wind(ny,nm,nd,nh)
            end do
          end do
        end do
      end do


      do ny = year1,year2
        do nm = 1,12
          do nd = 1,ndaysinmonth(ny,nm)
            print*,ny,nm,nd,wind(ny,nm,nd,1)/24.0
          end do
        end do
      end do

      end

************************************************************************
***** increments one hour                                             **
************************************************************************

      subroutine increment(ny,nm,nd,nh)
      nh = nh + 1
      if (nh.eq.25) then
        nh = 1
        call tomorrow(ny,nm,nd)
      end if
      end

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

