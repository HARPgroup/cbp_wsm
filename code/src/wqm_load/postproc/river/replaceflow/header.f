************************************************************************
**  subroutine for putting headers onto any pltgen file.  Takes the   **
**    file extention and puts it into the first four columns then     **
**    adds the lable(s)                                               **
************************************************************************

      subroutine header(sdate,ext,label)

      implicit none
      include 'Rdaily.inc'

      integer tempdate(ndate)

      integer nlines
      parameter (nlines = 26)

      character*4 ext

      character*100 pltline(nlines)

      integer i

      
      pltline( 1) = 'XXXX HSPF FILE FOR DRIVING SEPARATE PLOT PROGRAM'
      pltline( 2) = 'XXXX Time interval:   60 mins          Last month '
     .           //'in printout year: 12'
      pltline( 3) = 'XXXX No. of curves plotted:  Point-valued:  0   Me'
     .           //'an-valued:  5   Total  5'
      pltline( 4) = 'XXXX Label flag:  0          Pivl:    1          I'
     .           //'delt:   60'
      pltline( 5) = 'XXXX Plot title:   LOAD FILE 1          SEG230'
      pltline( 6) = 'XXXX Y-axis label:'
      pltline( 7) = 'XXXX Scale info:  Ymin:       0.               Thr'
     .           //'eshold: -1.0000'
      pltline( 8) = 'XXXX              Ymax:  0.10000E+06'
      pltline( 9) = 'XXXX              Time:   20.000     invl/inc'
      pltline(10) = 'XXXX Data for each curve (Point-valued first, then'
     .           //' mean-valued):'
      pltline(11) = 'XXXX Label                  '
      pltline(12) = 'XXXX '//label(1)
      pltline(13) = 'XXXX '//label(2)
      pltline(14) = 'XXXX '//label(3)
      pltline(15) = 'XXXX '//label(4)
      pltline(16) = 'XXXX '//label(5)
      pltline(17) = 'XXXX '//label(6)
      pltline(18) = 'XXXX '//label(7)
      pltline(19) = 'XXXX '//label(8)
      pltline(20) = 'XXXX '//label(9)
      pltline(21) = 'XXXX '//label(10)
      pltline(22) = 'XXXX Time series (pt-valued, then mean-valued):'
      pltline(23) = 'XXXX'
      pltline(24) = 'XXXX Date/time                      Values'
      pltline(25) = 'XXXX'
      pltline(26) = 'XXXX  yyyy mm dd 24  0  -0.10000E+31'
      do i = 1,ndate
        tempdate(i) = sdate(i)
      end do
      call yesterday(tempdate(1),tempdate(2),tempdate(3))
      write(pltline(26)(7:16),'(i4,i3,i3)') 
     .               tempdate(1),tempdate(2),tempdate(3)


      do i = 1,nlines
        pltline(i)(:4) = ext
        call ryt(pltline(i),pltfil)
      end do

      return
      end


