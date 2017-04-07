************************************************************************
** subroutine to get parameter values from the param files            **
************************************************************************
      subroutine getParValues(
     I                        parModule,parTable,parName,parUsed,npar,
     I                        parLKflag,lakeflags,
     I                        paramscen,rsegs,nrsegs,
     O                        parval)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      integer nr

      integer varcolumn

      character*2000 parline,varline,tableline

      logical foundrseg(maxrsegs)

************* END DECLARATIONS
      call lencl(paramscen,lenparamscen)

********** loop over all parameters, open the file and get all segs
      do np = 1,npar
        if (.not.parUsed(np)) cycle  ! only get values for changed pars

********** open parameter file
        call lencl(parModule(np),lenmod)
        fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//parModule(np)(:lenmod)//'.csv'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

********** read header lines and find column
        read(dfile,'(a2000)',err=994) tableline
        call d2x(tableline,last)
        read(dfile,'(a2000)',err=994) varline
        call d2x(varline,last)
        call findcolumn(
     I                  parTable(np),parName(np),fnam,
     M                  tableline,varline,
     O                  varcolumn,err)

**********  read all lines and check to see if in segment list
        do nr = 1,nrsegs
          foundrseg(nr) = .false.
        end do
        read(dfile,'(a2000)',err=996,end=997)parline
        do while (parline(:3).ne.'end')
          read(parline,*,err=993,end=993) tseg
          do nr = 1,nrsegs
            if (tseg.eq.rsegs(nr)) then
              foundrseg(nr) = .true.
              if (parLKflag(np).eq.2 .or.
     .            parLKflag(np).eq.lakeflags(nr)) then
                call getvar(
     I                      parline,varcolumn,
     O                      parval(np,nr))
              else 
                parval(np,nr) = -9.0
              end if
            end if
          end do
          read(dfile,'(a2000)',err=996,end=997)parline
        end do
        close(dfile)
        do nr = 1,nrsegs
          if (.not.foundrseg(nr)) go to 992
        end do

      end do
          
      return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find '//rsegs(nr)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'Problem finding river segment in file: line:'
      report(2) = fnam
      report(3) = parline(:100)
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(:100)
      go to 999

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

999   call stopreport(report)

      end


************************************************************************
** subroutine to put parameter values into the param files            **
**  read whole file into memory, modify, then re-open and write       **
************************************************************************
      subroutine putParValues(
     I                        parModule,parTable,parName,parUsed,npar,
     I                        parLKflag,lakeflags,
     I                        paramscen,rsegs,nrsegs,
     I                        parval)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'WQcal.inc'

      integer nr,nl

      integer varcolumn

      integer nlinemax,nlines  ! number of parameter lines in code
      parameter (nlinemax=1000)
      character*2000 parline(nlinemax),varline,tableline,vkeep,tkeep

      logical foundrseg(maxrsegs)

************* END DECLARATIONS
      call lencl(paramscen,lenparamscen)

********** loop over all parameters, open the file and get all segs
      do np = 1,npar
        if (.not.parUsed(np)) cycle  ! only get values for changed pars

********** open parameter file
        call lencl(parModule(np),lenmod)
        fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//parModule(np)(:lenmod)//'.csv'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

********** read header lines and find column
        read(dfile,'(a2000)',err=994) tableline
        call d2x(tableline,last)
        tkeep = tableline
        read(dfile,'(a2000)',err=994) varline
        call d2x(varline,last)
        vkeep = varline
        call findcolumn( 
     I                  parTable(np),parName(np),fnam,
     M                  tableline,varline,
     O                  varcolumn,err)

**********  read all lines into memory
        nlines = 1
        read(dfile,'(a2000)',err=996,end=997)parline(nlines)
        do while (parline(nlines)(:3).ne.'end')
          nlines = nlines + 1
          if (nlines.gt.nlinemax) go to 990
          read(dfile,'(a2000)',err=996,end=997)parline(nlines)
        end do
        close(dfile)

********** make modifications 
        do nr = 1,nrsegs
          foundrseg(nr) = .false.
        end do
        do nl = 1,nlines
          read(parline(nl),*,err=993,end=993) tseg
          do nr = 1,nrsegs
            if (tseg.eq.rsegs(nr)) then
              foundrseg(nr) = .true.
              if (parLKflag(np).eq.2 .or.
     .            parLKflag(np).eq.lakeflags(nr)) then
                call putvar(
     M                      parline(nl),
     I                      varcolumn,parval(np,nr))
              end if
            end if
          end do
        end do
        do nr = 1,nrsegs
          if (.not.foundrseg(nr)) go to 992
        end do

**********  rewrite modifications to original file
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
        call rytdos(tkeep,dfile)
        call rytdos(vkeep,dfile)
        do nl = 1,nlines
          call rytdos(parline(nl),dfile)
        end do
        close(dfile)

      end do
          
      return

********************* ERROR SPACE **************************************
990   report(1) = 'more lines in file than allowed in program'
      report(2) = fnam
      report(3) = ' increase variable nlinemax'
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find '//rsegs(nr)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

993   report(1) = 'Problem finding river segment in file: line:'
      report(2) = fnam
      report(3) = parline(nl)(:100)
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines)(:100)
      go to 999

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

999   call stopreport(report)

      end


************************************************************************
** takes a row of comma delimited variables and reads a value         **
************************************************************************
      subroutine getvar(
     I                  parline,varcolumn,
     O                  value)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      character*(*) parline
      real value   ! output from that column
      integer varcolumn  ! column that the variable of interest occupies
 
      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) go to 991
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      read(parline(firstcomma+1:lastcomma-1),*,err = 992) value

      return
991   report(1) = 'problem reading line in parameter file'
      report(2) = parline(:64)
      report(3) = ' not enough commas '
      go to 999

992   report(1) = 'problem reading line in parameter file'
      report(2) = parline(:64)
      write(report(3),*) ' trying to read column number ',varcolumn,' ',
     .            parline(firstcomma+1:lastcomma-1)
      go to 999

999   call stopreport(report)
      
      end
     
************************************************************************
** takes a row of comma delimited variables and writes a value        **
************************************************************************
      subroutine putvar(
     M                  parline,
     I                  varcolumn,value)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      character*(*) parline
      real value   ! output from that column
      integer varcolumn  ! column that the variable of interest occupies

      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) go to 991
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      parline(firstcomma+11:) = parline(lastcomma:)
      call ritef10(parline(firstcomma+1:firstcomma+10),value)
      call noblanks(parline,i)

      return
991   report(1) = 'problem writing line in parameter file'
      report(2) = parline(:64)
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end
 
