************************************************************************
** subroutine to write a floating point number to a character variable**
************************************************************************
      subroutine fchar(y,c)
      implicit none
      character*(*) c
      character*64 report(3)
      integer i,j1,j,k,l,lenvar,point
      real x,y

      x = y        ! don't change y
      c = ' '      ! initialize

      if (abs(y+999).lt..01) return      ! if default, write a blank

      lenvar = len(c)

      if (lenvar.lt.3) go to 991

      if (abs(y).lt.1.0e-8) then        ! write a zero for zero
        c(lenvar-2:lenvar) = '0.0'
        return
      end if

      if (int(log10(abs(x)))+3.gt.lenvar  .or.
     .    int(log10(abs(x)))-1.le.-lenvar) go to 992
      if (x.lt.0.0) then
        c(1:1) = '-'
        x = -x
      else
        c(1:1) = ' '
      end if
      j = int(log10(x))
      if (j.le.-2) then
        c(2:2) = '.'
        j1 = -j+1
        do i = 3,j1
          c(i:i) = '0'
        end do
        do i = j1+1,lenvar
          write(c(i:i),'(i1)')int(x/10.0**j)
          x = x-int(x/10.0**j)*10.0**j
          j = j-1
        end do
        if ((x/10.0**j).ge.5.0) then
          l = lenvar 
          read(c(l:l),'(i1)') k
          do while (k.eq.9)
            c(l:l) = '0'
            l = l - 1
            read(c(l:l),'(i1)') k
          end do
          write(c(l:l),'(i1)') k + 1
        end if
      else
        point = lenvar
        do i = 2,lenvar-1
          write(c(i:i),'(i1)')int(x/10.0**j)
          if (j.eq.-1) point = i
          x = x-int(x/10.0**j)*10.0**j
          j = j-1
        end do
        if ((x/10.0**j).ge.5.0) then
          l = lenvar - 1
          read(c(l:l),'(i1)') k
          do while (k.eq.9)
            c(l:l) = '0'
            l = l - 1
            read(c(l:l),'(i1)') k
          end do
          write(c(l:l),'(i1)') k + 1
        end if
        do i = lenvar,point+1,-1
          c(i:i) = c(i-1:i-1)
        end do
        c(point:point) = '.'
      end if

      return

**************** ERROR SPACE *******************************************
991   report(1) = 'trying to write a real variable to a string'
      report(2) = 'variable of less than three chararacters'
      report(3) = 'in subroutine pp/lib/utils/util/fchar.f'
      go to 999

992   report(1) = 'Need more code pp/lib/utils/util/fchar.f'
      report(2) = 'add code to incorporate exponentials'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end 
