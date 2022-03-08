************************************************************************
*** logical function to determine if a line contains '***'            **
************************************************************************
      function comment(line)
      implicit none
      character*(*),line
      logical comment
      integer i

      comment = .false.
      do i=1,len(line)-2
        if (line(i:i+2).eq.'***') comment = .true.
      end do
      end
