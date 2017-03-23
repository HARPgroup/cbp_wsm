      implicit none
      character*200 header1,header2,fnam,ftype
      integer lentype,lenhead
      character*13 rseg1,rseg2
      character*6 lseg1,lseg2
      character*3 lu1,lu2
      character*4 con1,con2
      integer year1,year2,outyear
      double precision ry1,ry2,ro
      character*4 cy1,cy2,oy

      integer nlu
      parameter (nlu=25)

      double precision value1(nlu),value2(nlu),outval(nlu)

      integer nm

      double precision posacres,totalacres  ! total of positive acres
      logical nonneg(nlu)  ! is this non-negative?

      double precision interpolate
      external interpolate

      print*,'enter file name up to year, year1, year2, and outyear'
      read*,ftype,year1,year2,outyear
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2
      write(oy,'(i4)') outyear
      ry1 = dble(year1)
      ry2 = dble(year2)
      ro = dble(outyear)

      call lencl(ftype,lentype)
      fnam = ftype(:lentype)//cy1//'.csv'
      open(11,file=fnam,status='old')
      fnam = ftype(:lentype)//cy2//'.csv'
      open(12,file=fnam,status='old')

      fnam = ftype(:lentype)//oy//'.csv'
      open(14,file=fnam,status='unknown')

      read(11,'(a200)') header1
      read(12,'(a200)') header2
      if (header1.ne.header2) then
        print*,'different headers'
        stop
      end if
      call lencl(header1,lenhead)
      write(14,'(a)') header1(:lenhead)

      do 
        read(11,*,end=111,err=111) rseg1,lseg1,(value1(nm),nm=1,nlu)
        read(12,*) rseg2,lseg2,(value2(nm),nm=1,nlu)

        if (lseg1.ne.lseg2 .or.
     .      rseg1.ne.rseg2   ) then
           print*,'not sorted the same'
          stop
        end if

        posacres = 0.0
        totalacres = 0.0

        do nm = 1,nlu-1  ! exclude water
          nonneg(nm) = .true.
          outval(nm) = interpolate(ro,ry1,value1(nm),ry2,value2(nm))
          totalacres = totalacres + outval(nm)
          if (outval(nm).lt.0.0) then
            nonneg(nm) = .false.
            outval(nm) = 0.0
          else
            posacres = posacres + outval(nm)
          end if
        end do
        do nm = 1,nlu-1
          outval(nm) = outval(nm) * totalacres / posacres
        end do

        outval(nlu) = interpolate(ro,ry1,value1(nlu),ry2,value2(nlu))
          
        write(14,1234) rseg1,lseg1,(outval(nm),nm=1,nlu)

      end do
       
111   close(11)
      close(12)
      close(14)
1234  format (a13,',',A6,26(',',e14.7))
      end

************************************************************************
**  function to get a value of y given two points that define a line  **
**   and a value of x                                                 **
************************************************************************
      function interpolate(x,xlow,ylow,xhigh,yhigh)
      implicit none
      double precision x,xlow,ylow,xhigh,yhigh
      double precision m,b
      double precision interpolate

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

