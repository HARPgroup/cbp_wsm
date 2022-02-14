************************************************************************
** reads the discharge from the ftable file                           **
************************************************************************
      subroutine getdischarge(
     I                        rseg,lenrseg,rscen,lenrscen,
     I                        maxrows,maxtabsize,
     O                        discharge,nrows,ncols)

      implicit none

      include 'rug.inc'
      integer maxrows,maxtabsize
      integer nrows,ncols

      real discharge(maxrows)  ! vectors of discharge

      integer nr,nc  ! row and colums indices

      logical comment,scompcase
      external comment,scompcase

****************** END DECLARATIONS 
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/ftables/'//rseg(:lenrseg)//'.ftable'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

************* READ HEADERS
      read(dfile,'(a)',err=993,end=994)line ! should be FTABLE
      read(dfile,'(a)',err=993,end=994)line ! should be ROWS COLS
      read(dfile,'(a)',err=993,end=994)line ! should be nrows,ncols
      call d2x(line,last)
      read(line,*,err=9932,end=9932) nrows,ncols

      if (ncols.ne.4) go to 995
      if (nrows.gt.maxrows) go to 996
      if (ncols*nrows.gt.maxtabsize) go to 998

************ read 2 comment lines
      read(dfile,'(a)',err=993,end=994)line ! should be headers 
      if (.not.comment(line)) go to 992
      read(dfile,'(a)',err=993,end=994)line ! should be units
      if (.not.comment(line)) go to 992

********** read in data
      do nr = 1,nrows
        read(dfile,'(a)',err=993,end=994)line 
        call d2x(line,last)
        read(line,'(30x,f10.0)',err=9933,end=9933) discharge(nr)
      end do
        
      read(dfile,'(a)',err=993,end=994)line 
      if (line(:12).ne.'  END FTABLE') go to 997

      close(dfile)

      return

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'trouble reading file: '
      report(2) = fnam
      report(3) = ' fifth and sixth lines should be commented headers'
      go to 999

993   report(1) = 'trouble reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

9932  report(1) = 'trouble getting rows and columns:  from file:'
      report(2) = line
      report(3) = fnam
      go to 999

9933  report(1) = 'trouble getting ftable data:  from file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'unexpected end of file found near line'
      report(2) = fnam
      report(3) = line
      go to 999

995   report(1) = 'maximum allowable columns in file:'
      report(2) = fnam
      report(3) = ' is four'
      go to 999

996   report(1) = 'maximum allowable rows in file:'
      report(2) = fnam
      write(report(3),*) ' is ',maxrows
      go to 999

997   report(1) = 'ftable file did not end correctly'
      report(2) = fnam
      report(3) = 'row mismatch?, END FTABLE missing?'
      go to 999

998   write(report(1),*)'rows X cols must be less than ',maxtabsize
      report(2) = fnam
      write(report(3),*)'rows, cols ',nrows,' ',ncols
      go to 999

999   call stopreport(report)

      end


