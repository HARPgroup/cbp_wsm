************************************************************************
** subroutine fread reads a real number, even if in integer format    **
************************************************************************
      subroutine fread(line,f1)
      character*(*) line
      character*1,c
      real f1 
      logical afterdecimal,posexp,pos
      pos = .true.
      call findcomma(line,iend)
      last = iend-1
      afterdecimal = .false.
      divide = 1.0
      iexp = 0
      f1 = 0.0
      do i = 1,last             ! loop over number of places in number
        read(line(i:i),100) c
        if (c.eq.'-') then
          pos = .false.
          cycle
        end if
        if (c.ne.' ') then
          if (c.eq.'.') then
            afterdecimal = .true.
          else if (c.eq.'E'.or.c.eq.'e') then    ! start of exponential
            j = 1            ! j is the number of places past i
            read(line(i+j:i+j),100) c
            j = j + 1
            if (c.eq.'-') then
              posexp = .false.
            else if (c.eq.'+') then
              posexp = .true.
            else
              posexp = .true.                          ! default
              j = j - 1
            end if
            do k = i+j,last
              read(line(k:k),100) c
              if (c.ne.' ') then
                read(c,101) intsingle
                iexp = iexp * 10 + intsingle
              end if
            end do
            if (.not.posexp) iexp = -iexp
            goto 999
          else               ! not blank and not decimal, must be number
            read(c,101) intsingle
            f1 = f1 * 10.0 + real(intsingle)
            if (afterdecimal) divide = divide * 10.
          end if
        end if
      end do
999   f1 = f1 / divide
      f1 = f1 * 10.0**iexp
      if (.not.pos) f1 = -f1
100   format(a1)
101   format(i1)
      end

************************************************************************
** subroutine fread reads a real number, even if in integer format    **
************************************************************************
      subroutine freadr(line,f1,err)
      character*(*) line
      character*1,c
      real f1 
      logical afterdecimal,posexp,pos
      integer err
      pos = .true.
      err = 0
      call findcomma(line,iend)
      last = iend-1
      afterdecimal = .false.
      divide = 1.0
      iexp = 0
      f1 = 0.0
      do i = 1,last             ! loop over number of places in number
        read(line(i:i),100) c
        if (c.eq.'-') then
          pos = .false.
          cycle
        end if
        if (c.ne.' ') then
          if (c.eq.'.') then
            afterdecimal = .true.
          else if (c.eq.'E'.or.c.eq.'e') then    ! start of exponential
            j = 1                    ! j is the number of places past i
            read(line(i+j:i+j),100) c
            j = j + 1
            if (c.eq.'-') then
              posexp = .false.
            else if (c.eq.'+') then
              posexp = .true.
            else
              posexp = .true.                          ! default
              j = j - 1
            end if
            do k = i+j,last
              read(line(k:k),100) c
              if (c.ne.' ') then
                read(c,101,err=1001) intsingle
                iexp = iexp * 10 + intsingle
              end if
            end do
            if (.not.posexp) iexp = -iexp
            goto 999
          else          ! not blank and not decimal, must be number
            read(c,101,err=1001) intsingle
            f1 = f1 * 10.0 + real(intsingle)
            if (afterdecimal) divide = divide * 10.
          end if
        end if
      end do
999   f1 = f1 / divide
      f1 = f1 * 10.0**iexp
      if (.not.pos) f1 = -f1
100   format(a1)
101   format(i1)
      return
1001  err = 1
      return
      end

************************************************************************
** subroutine readi reads an integer                                  **
************************************************************************
      subroutine readi(line,ii)
      character*(*) line
      character*1,c
      call findcomma(line,iend)
      last = iend-1
      ii = 0
      do i = 1,last                                       ! loop over number of places in number
        read(line(i:i),100) c
        if (c.ne.' ') then
          read(c,101) intsingle
          ii = ii * 10 + intsingle
        end if
      end do
100   format(a1)
101   format(i1)
      end

************************************************************************
** subroutine readi reads an integer                                  **
************************************************************************
      subroutine readir(line,ii,err)
      character*(*) line
      character*1,c
      integer err
      err = 0
      call findcomma(line,iend)
      last = iend-1
      ii = 0
      do i = 1,last                                       ! loop over number of places in number
        read(line(i:i),100) c
        if (c.ne.' ') then
          read(c,101,err=1001) intsingle
          ii = ii * 10 + intsingle
        end if
      end do
100   format(a1)
101   format(i1)
      return
1001  err = 1
      return
      end

************************************************************************
** subroutine findcomma returns the place number of the next comma    **
**   or one more than the last column occupied by a character if no   **
**   comma found.  if no comma and no data, 'last' is returned as '0' **
************************************************************************
      subroutine findcomma(line,last)
      implicit none
      character*(*) line
      integer i,last
      integer eol
      parameter (eol=13)

      do i = 1,len(line)
        if (line(i:i).eq.',') then
          last = i
          return
        end if
      end do

      last = -1
      do i = 1,len(line)  ! only reaches if comma not found
        if (line(i:i).eq.char(eol)) line(i:i) = ' '
        if (line(i:i).ne.' ') last = i
      end do
      last = last + 1

      end

************************************************************************
** subroutine shift deletes the first character in a character string **
**  and shifts the remaining characters left one place, placing a     **
**  blank in the last place, until a comma is reached                 **
**  useful for reading comma delimited files                          **
************************************************************************
      subroutine shift(line)
      implicit none
      character*(*) line
      integer lenline,comma

      call findcomma(line,comma)
      lenline = len(line)
      line(1:lenline-comma)=line(comma+1:lenline)
      line(lenline-comma+1:lenline) = ' '
      end

************************************************************************
** subroutine spaceshift deletes initial spaces in a character string **
**  and then deletes contiguous characters until it reaches a new     **
**  space.  It returns the modified line and the number of contiguous **
**  nonblank characters.   useful for reading space delimited files   **
************************************************************************
      subroutine spaceshift(line,last)
      implicit none
      character*(*) line
      integer lenline,icount,j,last

      lenline = len(line)
      icount = 1
      do while (line(icount:icount).eq.' ')   ! clear starting blanks 
        icount = icount + 1
        if (icount.eq.lenline) then
          last = 0
          return
        end if
      end do
      if (icount.ne.1) then
        line(1:lenline-icount+1) = line(icount:lenline)
        line(lenline-icount+2:lenline) = ' '
      end if
    

      icount = 1
      do while (line(icount:icount).ne.' ')  ! shift one character string
        icount = icount + 1
        if (icount.eq.lenline) then
          last = lenline
          return
        end if
      end do
      do while (line(icount:icount).eq.' ')   ! clear starting blanks 
        icount = icount + 1
        if (icount.eq.lenline) then
          last = 0
          line = ' '
          return
        end if
      end do
      line(1:lenline-icount+1) = line(icount:lenline)
      line(lenline-icount+2:lenline) = ' '

      last = 1
      do while (line(last:last).ne.' ')
        last = last + 1
      end do

      last = last - 1

      end 

************************************************************************
** subroutine shiftchar deletes the first characters in a string until**
**  the specified character is reached                                **
************************************************************************
      subroutine shiftchar(line,c)
      implicit none
      character*(*) line,c
      integer lenline,comma,i,last
      lenline = len(line)
      last = -1
      do i = 1,lenline
        if (line(i:i).eq.c) then
          last = i
          exit
        end if
      end do
      if (last.eq.-1) then
        line = ' '
        return
      end if
      line(1:lenline-last)=line(last+1:lenline)
      line(lenline-last+1:lenline) = ' '
      end

