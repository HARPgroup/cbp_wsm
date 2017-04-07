************************************************************************
**  subroutine ryt writes a line to the specified file with no blanks **
************************************************************************
      subroutine ryt(line,ifn)
      implicit none
      character*(*),line
      integer ifn,last,lenline,i

      lenline = len(line)
      last = -1
      do i=1,lenline
        if (line(i:i).ne.' ') last = i
      end do

      if (last.gt.0) then
        write(ifn,'(a)') line(:last)
      else
        write(ifn,*)
      end if

      end

************************************************************************
**  subroutine ryt writes a line to the specified file with no blanks **
************************************************************************
      subroutine rytdos(line,ifn)
      implicit none
      character*(*),line
      integer ifn,last,lenline,i

      lenline = len(line)
      last = -1
      do i=1,lenline
        if (line(i:i).eq.char(13)) line(i:i) = ' '
        if (line(i:i).ne.' ') last = i
      end do

      if (last.gt.0) then
        write(ifn,'(a,a1)') line(:last),char(13)
      else
        write(ifn,*)char(13)
      end if

      end

************************************************************************
**  takes values as the input and writes the line to the              **
**    specified output file with segment number 1                     **
************************************************************************
      subroutine rite12f5(uci,nval,value)
      implicit none
      real value(12)       ! values to write
      integer uci             ! file number to write to
      integer nval           ! number of values to write
      integer j              ! index
      character*80 vline
      character*7 evalue
      vline = '    1     '
      do j = 1,nval
        if (abs(value(j)+999.).gt.0.001) then
          if (value(j).gt.9999.) then
            write(vline(5*j+6:5*(j+2)),'(e5.0)')value(j)
          else if (value(j).gt.999.9) then
            write(vline(5*j+6:5*(j+2)),'(f5.0)')value(j)
          else if (value(j).gt.99.99) then
            write(vline(5*j+6:5*(j+2)),'(f5.1)')value(j)
          else if (value(j).gt.9.999) then
            write(vline(5*j+6:5*(j+2)),'(f5.2)')value(j)
          else if (value(j).gt..9999) then
            write(vline(5*j+6:5*(j+2)),'(f5.3)')value(j)
          else if (value(j).ge..0001) then
            write(vline(5*j+6:5*(j+2)),'(f5.4)')value(j)
          else if (abs(value(j)).lt.1.0e-30) then
            write(vline(5*j+6:5*(j+2)),'(a5)')'   0.'
          else
            write(evalue,'(e7.1)')value(j)
            write(vline(5*j+6:5*(j+2)),'(a4,a1)')
     .                    evalue(2:5),evalue(7:7)
          end if
        end if
      end do
      call clean5(vline)
      write(uci,'(a80)') vline
      end

      subroutine rite7f10(uci,nval,value)
      implicit none
      real value(12)       ! values to write
      integer uci             ! file number to write to
      integer nval           ! number of values to write
      integer j              ! index
      character*80 vline
      vline = '    1     '
      do j = 1,nval
        if (abs(value(j)+999.).gt.0.001) then
          if (value(j).gt.9999999.) then
            write(vline(10*j+1:10*(j+1)),'(e10.3)')value(j)
          else if (value(j).gt.999999.9) then
            write(vline(10*j+1:10*(j+1)),'(f10.0)')value(j)
          else if (value(j).gt.99999.99) then
            write(vline(10*j+1:10*(j+1)),'(f10.1)')value(j)
          else if (value(j).gt.9999.999) then
            write(vline(10*j+1:10*(j+1)),'(f10.2)')value(j)
          else if (value(j).gt.999.9999) then
            write(vline(10*j+1:10*(j+1)),'(f10.3)')value(j)
          else if (value(j).gt.99.99999) then
            write(vline(10*j+1:10*(j+1)),'(f10.4)')value(j)
          else if (value(j).gt.9.999999) then
            write(vline(10*j+1:10*(j+1)),'(f10.5)')value(j)
          else if (value(j).gt..9999999) then
            write(vline(10*j+1:10*(j+1)),'(f10.6)')value(j)
          else if (value(j).gt..0000100) then
            write(vline(10*j+1:10*(j+1)),'(f10.6)')value(j)
          else if (abs(value(j)).lt.1.0e-30) then
            write(vline(10*j+1:10*(j+1)),'(a10)')'        0.'
          else
            write(vline(10*j+1:10*(j+1)),'(e10.3)')value(j)
          end if
        end if
      end do
      call clean10(vline)
      write(uci,'(a80)') vline
      end

      subroutine rite8f8(uci,nval,value)
      implicit none
      real value(12)       ! values to write
      integer uci             ! file number to write to
      integer nval           ! number of values to write
      integer j              ! index
      character*80 vline
      vline = '    1     '
      do j = 1,nval
        if (abs(value(j)+999.).gt.0.001) then
          if (value(j).gt.99999.) then
            write(vline(8*j+3:8*(j+1)+2),'(e8.2)')value(j)
          else if (value(j).gt.9999.9) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.1)')value(j)
          else if (value(j).gt.999.99) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.2)')value(j)
          else if (value(j).gt.99.999) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.3)')value(j)
          else if (value(j).gt.9.9999) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.4)')value(j)
          else if (value(j).gt..99999) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.5)')value(j)
          else if (value(j).gt..0000100) then
            write(vline(8*j+3:8*(j+1)+2),'(f8.6)')value(j)
          else if (abs(value(j)).lt.1.0e-30) then
            write(vline(8*j+3:8*(j+1)+2),'(a8)')'      0.'
          else
            write(vline(8*j+3:8*(j+1)+2),'(e8.2)')value(j)
          end if
        end if
      end do
      call clean8(vline)
      write(uci,'(a80)') vline
      end

************************************************************************
** subroutine cleanN takes out trailing zeros for a cleaner line      **
************************************************************************
      subroutine clean5(vline)
      implicit none
      character*80 vline
      integer i,j,k       ! indices
      logical e           ! indicator of exponential, do not remove zeros
      do i = 15,70,5      ! last character of each value
        e = .false.
        do j = i-4,i
          if (vline(j:j).eq.'e'.or.vline(j:j).eq.'E') e = .true.
        end do
        if (.not. e) then
          do j = 0,2        ! can only have 3 trailing possible
            if (vline(i:i).eq.'0') then
              do k = 0,3
                vline(i-k:i-k)=vline(i-k-1:i-k-1)
              end do
              vline(i-4:i-4)=' '
            end if
          end do
        end if
      end do
      end

      subroutine clean8(vline)
      implicit none
      character*80 vline
      integer i,j,k       ! indices
      logical e           ! indicator of exponential, do not remove zeros
      do i = 18,74,8     ! last character of each value
        e = .false.
        do j = i-7,i
          if (vline(j:j).eq.'e'.or.vline(j:j).eq.'E') e = .true.
        end do
        if (.not. e) then
          do j = 0,5        ! can only have 8 trailing possible
            if (vline(i:i).eq.'0') then
              do k = 0,6
                vline(i-k:i-k)=vline(i-k-1:i-k-1)
              end do
              vline(i-7:i-7)=' '
            end if
          end do
        end if
      end do
      end

      subroutine clean10(vline)
      implicit none
      character*80 vline
      integer i,j,k       ! indices
      logical e           ! indicator of exponential, do not remove zeros
      do i = 20,70,10     ! last character of each value
        e = .false.
        do j = i-9,i
          if (vline(j:j).eq.'e'.or.vline(j:j).eq.'E') e = .true.
        end do
        if (.not. e) then
          do j = 0,7        ! can only have 8 trailing possible
            if (vline(i:i).eq.'0') then
              do k = 0,8
                vline(i-k:i-k)=vline(i-k-1:i-k-1)
              end do
              vline(i-9:i-9)=' '
            end if
          end do
        end if
      end do
      end

************************************************************************
** writes an integer into a line in the specified space               **
************************************************************************
      subroutine UwriteI(Ustart,Uend,ipar,line)
      integer Ustart,Uend,ipar,lenvar
      character*(*) line
      character*64 report(3)

      if (abs(ipar+999).gt.0.001) then           ! if not default
        lenvar = Uend-Ustart+1
        if (lenvar.eq.2) then
          write(line(Ustart:Uend),'(i2)') iPar
        else if (lenvar.eq.3) then
          write(line(Ustart:Uend),'(i3)') iPar
        else if (lenvar.eq.4) then
          write(line(Ustart:Uend),'(i4)') iPar
        else if (lenvar.eq.5) then
          write(line(Ustart:Uend),'(i5)') iPar
        else if (lenvar.eq.6) then
          write(line(Ustart:Uend),'(i6)') iPar
        else if (lenvar.eq.7) then
          write(line(Ustart:Uend),'(i7)') iPar
        else if (lenvar.eq.8) then
          write(line(Ustart:Uend),'(i8)') iPar
        else if (lenvar.eq.9) then
          write(line(Ustart:Uend),'(i9)') iPar
        else if (lenvar.eq.10) then
          write(line(Ustart:Uend),'(i10)')iPar
        else
          report(1) = 'need more coding for unique table formats'
          report(2) = ' file ./pp/lib/utils/util/writeline.f'
          report(3) = ' need more types of integers'
          call stopreport(report)
        end if
      end if

      end
                
************************************************************************
**  write one f10 variable to a line                                  **
************************************************************************
      subroutine ritef10(vline,value)
      implicit none
      real value       ! value to write
      character*10 vline  ! output variable
      if (abs(value+999.).gt.0.001) then
        if (value.gt.9999999.) then
          write(vline,'(e10.3)')value
        else if (value.gt.999999.9) then
          write(vline,'(f10.0)')value
        else if (value.gt.99999.99) then
          write(vline,'(f10.1)')value
        else if (value.gt.9999.999) then
          write(vline,'(f10.2)')value
        else if (value.gt.999.9999) then
          write(vline,'(f10.3)')value
        else if (value.gt.99.99999) then
          write(vline,'(f10.4)')value
        else if (value.gt.9.999999) then
          write(vline,'(f10.5)')value
        else if (value.gt..9999999) then
          write(vline,'(f10.6)')value
        else if (value.gt..0000100) then
          write(vline,'(f10.6)')value
        else if (abs(value).lt.1.0e-30) then
          write(vline,'(a10)')'        0.'
        else
          write(vline,'(e10.3)')value
        end if
      end if
      end
