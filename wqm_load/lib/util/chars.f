************************************************************************
**  removed all spaces from a character variable and finds last       **
**     non blank character                                            **
************************************************************************
      subroutine noblanks(c,last)
      implicit none
      character*(*) c
      integer i,j,last,lenc          ! indices and last character
      logical allblank
      integer eol
      parameter (eol=13)

      lenc = len(c)

      do i = 1,lenc

        allblank = .true.
        do j = i,lenc
          if (c(j:j).ne.' ') then
            allblank = .false.
            exit
          end if
        end do

        if (.not.allblank) then
          do while (c(i:i).eq.' ')   ! remove initial blanks
            do j = i,lenc-1
              c(j:j) = c(j+1:j+1)
            end do
            c(lenc:lenc) = ' '
          end do
        else
          exit
        end if

      end do

      last = 1
      do i = 1,lenc                            ! find end character
        if (c(i:i).ne.' '.and.c(i:i).ne.char(eol)) last = i
      end do

      end

************************************************************************
**  removed initial spaces from a character variable and finds last   **
**     non blank character                                            **
************************************************************************
      subroutine trims(c,last)
      implicit none
      character*(*) c
      integer i,last,lenc          ! indices and last character
      logical allblank
      integer eol
      parameter (eol=13)

      lenc = len(c)

      allblank = .true.
      do i = 1,lenc
        if (c(i:i).ne.' ') allblank = .false.
      end do

      if (.not.allblank) then
        do while (c(1:1).eq.' ')   ! remove initial blanks
          do i = 1,lenc-1
            c(i:i) = c(i+1:i+1)
          end do
          c(lenc:lenc) = ' '
        end do
        do i = 1,lenc                            ! find end character
          if (c(i:i).ne.' '.and.c(i:i).ne.char(eol)) last = i
        end do
      else
        last = 1
      end if
      end

************************************************************************
**  subroutine d2x is a line version of dos2unix.  It works for       **
**   release 6 of RedHat Linux.  It may have to be modified for other **
**   releases.  The parameter 'eol' may have to be changed            **
**   It also finds the last non-blank character in the line           **
************************************************************************
      subroutine d2x(line,last)
      implicit none

      character*(*) line
      integer eol,i,last
      parameter (eol=13)

      last = 0
      do i = 1,len(line)
        if (ichar(line(i:i)).eq.eol) then
          line(i:i) = ' '
          last=i-1
          return
        end if
        if (line(i:i).ne.' ') last=i
      end do

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
**  routine to compare 2 strings of equal or unequal size and         **
**   determine if they are equal except for blanks                    **
************************************************************************
      function scompare(s1,s2)
      implicit none
      character*(*) s1,s2     ! characters to compare
      integer lens1,lens2
      logical scompare,scompare2

      lens1 = len(s1)
      lens2 = len(s2)
      scompare = scompare2(s1,s2,lens1,lens2)
      end

      function scompare2(s1,s2,lens1,lens2)

      integer lens1,lens2
      character*(*) s1,s2     ! characters to compare
      character*1 s1a(lens1)   ! surrogate so that intial variables are not trimmed
      character*1 s2a(lens2)   ! surrogate so that intial variables are not trimmed
      integer last1,last2     ! last nonblank character
      integer i               ! index
      logical scompare2

      do i = 1,lens1             ! save in other variables
        s1a(i) = s1(i:i)
      end do
      do i = 1,lens2
        s2a(i) = s2(i:i)
      end do

      call trims(s1,last1)
      call trims(s2,last2)

      scompare2 = .false.
      if (last1.ge.1.and.last1.le.lens1) then 
        if (last2.ge.1.and.last2.le.lens2) then 
          if (s1(:last1).eq.s2(:last2)) scompare2 = .true.
        end if
      end if

      do i = 1,lens1              ! restore to previous state
        s1(i:i) = s1a(i)
      end do
      do i = 1,lens2
        s2(i:i) = s2a(i)
      end do

      end

************************************************************************
**  routine to compare 2 strings of equal or unequal size and         **
**   determine if they are equal except for blanks                    **
**   corrects for different case                                      **
************************************************************************
      function scompcase(s1,s2)
      implicit none
      character*(*) s1,s2     ! characters to compare
      integer lens1,lens2
      logical scompcase,scompcase2

      lens1 = len(s1)
      lens2 = len(s2)
      scompcase = scompcase2(s1,s2,lens1,lens2)
      end

      function scompcase2(s1,s2,lens1,lens2)

      integer lens1,lens2
      character*(*) s1,s2     ! characters to compare
      character*1 s1a(lens1)   ! surrogate so that intial variables are not trimmed
      character*1 s2a(lens2)   ! surrogate so that intial variables are not trimmed
      integer last1,last2     ! last nonblank character
      integer i               ! index
      logical scompcase2

      do i = 1,lens1             ! save in other variables
        s1a(i) = s1(i:i)
      end do
      do i = 1,lens2
        s2a(i) = s2(i:i)
      end do

      call lowercase(s1)
      call lowercase(s2)
      call trims(s1,last1)
      call trims(s2,last2)

      scompcase2 = .false.
      if (s1(:last1).eq.s2(:last2)) scompcase2 = .true.

      do i = 1,lens1              ! restore to previous state
        s1(i:i) = s1a(i)
      end do
      do i = 1,lens2
        s2(i:i) = s2a(i)
      end do

      end

************************************************************************
**  routine to change all uppercase to lowercase                      **
************************************************************************
      subroutine lowercase(s)
      implicit none
      character*(*) s
      integer i
      integer iupA,iloa,iupZ,iloz,is
      iupA = ichar('A')
      iloa = ichar('a')
      iupZ = ichar('Z')

      do i = 1,len(s)
        is = ichar(s(i:i))
        if (is.ge.iupA.and.is.le.iupZ) then
          is = is + iloa - iupA
          s(i:i) = char(is)
        end if
      end do
      end

************************************************************************
**  routine to change all lowercase to uppercase                      **
************************************************************************
      subroutine uppercase(s)
      implicit none
      character*(*) s
      integer i
      integer iupA,iloa,iupZ,iloz,is
      iloa = ichar('a')
      iloz = ichar('z')
      iupA = ichar('A')

      do i = 1,len(s)
        is = ichar(s(i:i))
        if (is.ge.iloa.and.is.le.iloz) then
          is = is + iupA - iloa
          s(i:i) = char(is)
        end if
      end do
      end
