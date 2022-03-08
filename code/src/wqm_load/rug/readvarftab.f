************************************************************************
** reads the variable ftables from a file                             **
************************************************************************
      subroutine readvarftab(rseg,lenrseg,rscen,lenrscen,
     I                       maxtabsize,maxchanges,maxdisch,maxrows,
     O                       tabsize,nchanges,ndisch,nrows,ncols,
     O                       month1,day1,month2,day2,
     O                       changefrom,changeto,
     O                       rchtab,distab)

      implicit none

      include 'rug.inc'
      integer maxtabsize,maxchanges,maxdisch,maxrows
      integer tabsize,nchanges,ndisch,nrows,ncols

      integer month1(maxchanges),day1(maxchanges)
      integer month2(maxchanges),day2(maxchanges)
      integer changefrom(maxchanges),changeto(maxchanges) ! disch row

      real rchtab(maxtabsize)         ! ftable values
      real distab(maxrows,maxdisch)  ! vectors of discharge

      integer nr,nc  ! row and colums indices

      logical comment,scompcase
      external comment,scompcase

      character*5 specs
      character*10 startwith
      character*6 change
      character*7 weekend, weekday
      data specs,change,startwith /'specs','change','start with'/
      data weekend,weekday /'weekend','weekday'/

****************** END DECLARATIONS 

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/variable_ftables/'//rseg(:lenrseg)//'.varftable'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

************* READ HEADERS
      read(dfile,'(a)',err=993,end=994)line 
      if (.not.scompcase(line(:5),specs)) go to 992

      read(dfile,'(a)',err=993,end=994)line 
      if (.not.scompcase(line(:10),startwith)) go to 9921

      read(dfile,'(a)',err=993,end=994)line 

********** could be weekend/weekday or change info
      if (scompcase(line(:7),weekday)) then

        nchanges = -1  ! flag for weekend/day
        read(line(11:12),'(i2)')changeto(2)
        changefrom(1) = changeto(2)

        read(dfile,'(a)',err=993,end=994)line
        if (.not.scompcase(line(:7),weekend)) go to 9923
        read(line(11:12),'(i2)')changeto(1)
        changefrom(2) = changeto(1)

        read(dfile,'(a)',err=993,end=994)line
        if (.not.scompcase(line(5:9),specs)) go to 9923

      else if (scompcase(line(:7),weekend)) then

        nchanges = -1  ! flag for weekend/day
        read(line(11:12),'(i2)')changeto(1)
        changefrom(2) = changeto(1)

        read(dfile,'(a)',err=993,end=994)line
        if (.not.scompcase(line(:7),weekday)) go to 9923
        read(line(11:12),'(i2)')changeto(2)
        changefrom(1) = changeto(2)

        read(dfile,'(a)',err=993,end=994)line
        if (.not.scompcase(line(5:9),specs)) go to 9923


      else

************* get changes information
        nchanges = 0
        do while (.not.scompcase(line(5:9),specs)) 

          if (.not.scompcase(line(:6),change)) go to 9922
          nchanges = nchanges + 1
          read(line,1234,err=9931)changefrom(nchanges),
     .                            changeto(nchanges),
     .                            month1(nchanges),day1(nchanges),
     .                            month2(nchanges),day2(nchanges)
          read(dfile,'(a)',err=993,end=994)line 

        end do

      end if

********** GET RID OF TWO LINES
      read(dfile,'(a)',err=993,end=994)line 
      read(dfile,'(a)',err=993,end=994)line 


********* GET ROWS AND COLUMNS
      read(dfile,'(a)',err=993,end=994)line 
      read(line,*,err=9932,end=9932) nrows,ncols
      ndisch = ncols - 3
      ncols = 4 ! columns in base ftable always 4
      tabsize = nrows*ncols
      if (tabsize.gt.maxtabsize) go to 998

********** GET RID OF TWO MORE LINES
      read(dfile,'(a)',err=993,end=994)line 
      read(dfile,'(a)',err=993,end=994)line 

********** read in data
      do nr = 1,nrows
        read(dfile,'(a)',err=993,end=994)line 
        read(line,'(20f10.0)',err=9933,end=9933)
     .                        (rchtab((nr-1)*ncols+nc),nc=1,ncols),
     .                        (distab(nr,nc),nc=2,ndisch)
        distab(nr,1) = rchtab(nr*ncols)
      end do
        
      read(dfile,'(a)',err=993,end=994)line 
      if (line(:12).ne.'  END FTABLE') go to 997

      close(dfile)

      return
1234  format(6x,i2,2x,i2,1x,i2,1x,i2,3x,i2,1x,i2)

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'file does not appear to be a variable ftable file'
      report(2) = fnam
      report(3) = 'first line should be: specs'
      go to 999

9921  report(1) = 'file does not appear to be a variable ftable file'
      report(2) = fnam
      report(3) = 'second line should be: start with X'
      go to 999

9922  report(1) = 'file does not appear to be a variable ftable file'
      report(2) = fnam
      report(3) = 'check header lines'
      go to 999

9923  report(1) = 'file not formatted correctly for weekend/weekday'
      report(2) = fnam
      report(3) = 'check header lines'
      go to 999

993   report(1) = 'trouble reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

9931  report(1) = 'trouble parsing line:  from file:'
      report(2) = line
      report(3) = fnam
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

997   report(1) = 'variable ftable file did not end correctly'
      report(2) = fnam
      report(3) = 'row mismatch?, END FTABLE missing?'
      go to 999

998   write(report(1),*)'rows X cols must be less than ',maxtabsize
      report(2) = fnam
      write(report(3),*)'rows, cols ',nrows,' ',ncols
      go to 999

999   call stopreport(report)

      end


