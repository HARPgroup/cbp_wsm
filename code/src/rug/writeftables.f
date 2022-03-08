************************************************************************
**  read and write the standard ftable                                **
************************************************************************
      subroutine Sftable
      implicit none
      include 'rug.inc'

      read(dfile,'(a)')line
      line = '  FTABLE      1'

      do while (line(:12).ne.'  END FTABLE')
        call ryt(line,uci)
        read(dfile,'(a)')line
        call d2x(line,last)
      end do

      close(dfile)

      line = '  END FTABLE  1'
      call ryt(line,uci)

      line = 'END FTABLES'
      call ryt(line,uci)
      end

************************************************************************
**  read and write the start of the variable ftable                   **
**     based on the 'start with' line                                 **
************************************************************************
      subroutine Vftable(fnam)
      implicit none
      include 'rug.inc'
      integer startcolumn
      logical comment
      external comment
      integer nrows,ncols,nr,nc  ! number of rows and columns
      integer maxcols
      parameter (maxcols = 12)
      character*10 cvalue(maxcols)   ! variable to read and write table

      read(dfile,'(a)')line
      if (line(:5).ne.'specs') go to 991

      read(dfile,'(a)') line
      if (line(:10).ne.'start with') go to 992
      read (line(11:12),'(i2)',err=992)startcolumn
      startcolumn = startcolumn + 3  ! first discharge is fourth column

      do while (line(:8).ne.'  FTABLE')
        read(dfile,'(a)') line
      end do

      line = '  FTABLE      1'
      call ryt(line,uci)

      nrows = 0
      do nr = 1,4  ! put in comments and dimensions
        read(dfile,'(a)')line
        if (.not.comment(line)) then  ! must be dimension line
          read(line,*)nrows,ncols
          if (startcolumn.gt.ncols) go to 993
          if (ncols.gt.maxcols) go to 996
          line(9:10) = ' 4'
        end if
        call ryt(line,uci)
      end do
      if (nrows.eq.0) go to 994

      do nr = 1,nrows
        read(dfile,1234)(cvalue(nc),nc=1,ncols)
        write(uci,1234,err=951)(cvalue(nc),nc=1,3),cvalue(startcolumn)
      end do

      read(dfile,'(a)')line
      if (line(:12).ne.'  END FTABLE') go to 995

      close(dfile)

      line = '  END FTABLE  1'
      call ryt(line,uci)

      line = 'END FTABLES'
      call ryt(line,uci)

      return
1234  format(10A10)

************ ERROR SPACE ***********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'first line of variable ftable file:'
      report(2) = fnam
      report(3) = ' must be literal> specs'
      go to 999

992   report(1) = 'second line of variable ftable file:'
      report(2) = fnam
      report(3) = ' must specify start month like:start with X'
      go to 999

993   report(1) = 'in below named file, the column specified as the '
      write(report(2),*) ' Jan 1 1984 column: ',startcolumn,
     .                   ' does not exist'
      report(3) = fnam
      go to 999

994   report(1) = 'something wrong with file:'
      report(2) = fnam
      report(3) = '  could not get number of columns and rows.'
      go to 999

995   report(1) = 'problem with file:'
      report(2) = fnam
      report(3) = ' number of rows not specified correctly or last'
     .             //' line not > END FTABLE'
      go to 999

996   report(1) = ' number of columns in file:  greater than allowed'
      report(2) = fnam
      report(3) = 'change program ./pp/src/rug/writeftables.f'
      go to 999

999   call stopreport(report)
      end
