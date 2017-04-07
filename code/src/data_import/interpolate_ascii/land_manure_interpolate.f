      implicit none
      character*100 header,fnam,ftype
      integer lentype,lenhead
      character*6 lseg1,lseg2
      character*3 lu1,lu2
      character*4 con1,con2
      integer year1,year2,outyear
      real ry1,ry2,ro
      character*4 cy1,cy2,oy

      real value1(12),value2(12),outval(12)

      integer nm

      real interpolate
      external interpolate

      read*,ftype,year1,year2,outyear
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      write(oy,'(i4)') outyear
      ry1 = real(year1)
      ry2 = real(year2)
      ro = real(outyear)

      call lencl(ftype,lentype)
      fnam = ftype(:lentype)//cy1//'.csv'
      open(11,file=fnam,status='old')
      fnam = ftype(:lentype)//cy2//'.csv'
      open(12,file=fnam,status='old')

      fnam = ftype(:lentype)//oy//'.csv'
      open(14,file=fnam,status='unknown')

      read(11,'(a100)') header
      read(12,'(a100)') header
      call lencl(header,lenhead)
      write(14,'(a)') header(:lenhead)

      do 
        read(11,*,end=111,err=111) lseg1,lu1,con1,(value1(nm),nm=1,12)
        read(12,*) lseg2,lu2,con2,(value2(nm),nm=1,12)

        if (lseg1.ne.lseg2 .or.
     .      lu1.ne.lu2     .or.
     .      con1.ne.con2   ) then
           print*,'not sorted the same'
          stop
        end if

        do nm = 1,12
          if (value1(nm).lt.0.00001) then
            outval(nm) = value2(nm)
          else
            if (value2(nm).lt.0.00001) then
              outval(nm) = value1(nm)
            else
              outval(nm) = interpolate(ro,ry1,value1(nm),ry2,value2(nm))
              outval(nm) = max(outval(nm),0.0)
            end if
          end if
        end do

        write(14,1234) lseg1,lu1,con1,(outval(nm),nm=1,12)

      end do
       
111   close(11)
      close(12)
      close(14)
1234  format (A6,',',a3,',',a4,12(',',f6.2))
      end

************************************************************************
**  function to get a value of y given two points that define a line  **
**   and a value of x                                                 **
************************************************************************
      function interpolate(x,xlow,ylow,xhigh,yhigh)
      implicit none
      real x,xlow,ylow,xhigh,yhigh
      real m,b
      real interpolate

      if (abs(xlow-xhigh).lt.0.001) then
        interpolate = ylow
        return
      end if

      m = (yhigh-ylow)/(xhigh-xlow)
      b = ylow - m*xlow

      interpolate = m*x + b
      return
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

