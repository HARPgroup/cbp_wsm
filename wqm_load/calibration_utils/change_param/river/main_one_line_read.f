************************************************************************
** program to change parameters for river calibration.  Automatically **
** reads .csv files and performs functions on the variables                **
************************************************************************
      implicit none
    
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/rsegs.inc'

      character(250):: geography             ! spatial exent of change, 
                                         !either rseg or river seglist

      integer:: ns         ! number of land segments to change

      character(14):: module,table,variable  ! variable specifiers
      integer:: lenmod

      character(1):: action         ! [m,a,e] = [multiply,add,equals]
      real:: rvalue,rvalues(maxrsegs)        ! real value for action
      integer:: ivalue,ivalues(maxrsegs)     ! integer value for action

      character(1):: rsegflag ! [a,p,i,s] = [all,perlnds,implnds,single]
      integer:: numrseg,nr    ! number of land uses to act on
      
      character(1):: typeflag                ! [i,r] = [integer,real]

      integer:: nlinemax,nlines  ! number of parameter lines in code
      parameter (nlinemax=750)
      character(2000):: parline(nlinemax),varline,tableline,vkeep,tkeep
                            ! the 3 lines of the file with relevant info

      integer:: varcolumn ! column of the variable of interest

      logical:: scompare
      external scompare

      integer:: i,j                          ! indices
      character(4):: version                 ! version number
      logical:: fileexist,anyexist           ! if file already exists

      real x

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read(*,*) geography,rscen,module,table,variable,typeflag,action,x
      print*,geography(:25),',',table,',',variable
C      read(*,*) geography
C      read(*,*) rscen
C      read(*,*) module
C      read(*,*) table
C      read(*,*) variable
C      read(*,*) typeflag
C      read(*,*) action

      call lencl(rscen,lenrscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call readRiverSeglist(
     I                      geography,
     O                      rsegs,nrsegs)

      call uppercase(module)
      call uppercase(table)
      call uppercase(variable)
     
      if (typeflag.ne.'i'.and.typeflag.ne.'I'.and.
     .    typeflag.ne.'r'.and.typeflag.ne.'R') go to 981

      if (variable(2:5).eq.'TAUC') then
        if       (action.ne.'a'.and.action.ne.'A'.and.
     .            action.ne.'e'.and.action.ne.'E'.and.
     .            action.ne.'m'.and.action.ne.'M'.and.
     .            action.ne.'p'.and.action.ne.'P') go to 982
      else
        if       (action.ne.'a'.and.action.ne.'A'.and.
     .            action.ne.'e'.and.action.ne.'E'.and.
     .            action.ne.'m'.and.action.ne.'M') go to 983
      end if

      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        ivalue = int(x + 0.1)
        if (ivalue.eq.-9) then     ! unique value
          call readvalues(typeflag,paramscen,variable,maxrsegs,
     I                    nrsegs,rsegs,
     O                    rvalues,ivalues)
        else
          do ns = 1,nrsegs         ! single value for all
            ivalues(ns) = ivalue
          end do
        end if
      else
        rvalue = x
        if (abs(rvalue+9).lt.0.001) then   ! unique value
          call readvalues(typeflag,paramscen,variable,maxrsegs,
     I                    nrsegs,rsegs,
     O                    rvalues,ivalues)
        else
          do ns = 1,nrsegs         ! single value for all
            rvalues(ns) = rvalue
          end do
        end if
      end if


********** GET SOME OVERALL ACCOUNTING DONE
      call lencl(paramscen,lenparamscen)
      call lencl(module,lenmod)

**********           FIND FIRST FREE FILE NAME 
      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
        anyexist = .false.
        fnam =pardir//'river/'//paramscen(:lenparamscen)
     .         //'/'//module(:lenmod)//'_'//version//'.csv'
        inquire (file=fnam,exist=fileexist)
        if (fileexist) anyexist = .true.

        if (.not.anyexist) exit
      end do

      write(*,*) 'river segment     old param     new param'

************            OPEN FILE
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/'//module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********            FIND CORRECT COLUMN
      read(dfile,'(a2000)',err=994) tableline
      call d2x(tableline,last)
      tkeep = tableline
      read(dfile,'(a2000)',err=994) varline
      call d2x(varline,last)
      vkeep = varline
      call findcolumn(tableline,varline,table,variable,varcolumn,err)
      if (err.ne.0) go to 992

**********            READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=996,end=997) parline(nlines)
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=996,end=997) parline(nlines)
      end do

      close(dfile)
          
**********           WRITE FILE TO NEW FILE NAME
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/'//module(:lenmod)//'_'//version//'.csv'
      open(dfile,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991
      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do

      close(dfile)
        
**********          REWRITE MODS TO ORIGINAL FILE NAME        
      fnam= pardir//'river/'//paramscen(:lenparamscen)//
     .      '/'//module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines-1
        call d2x(parline(i),last)
        call findcomma(parline(i),last)
        Tseg = parline(i)(:last-1)
        call trims(Tseg,last)
        do ns = 1,nrsegs
          if (scompare(Tseg,rsegs(ns))) then  ! match
            call doaction(parline(i),action,rvalues(ns),ivalues(ns),
     .                    variable,varcolumn,typeflag,Tseg,
     .                    paramscen,err)
          end if
          if (err.ne.0) go to 995
        end do
        call rytdos(parline(i),dfile)
      end do
      call rytdos(parline(nlines),dfile)  ! write 'end'
    
      close(dfile)

      return

100   format (a,a)

********************* ERROR SPACE **************************************
981   report(1) = 'error in input'
      report(2) = 'only allowable variable types are'
      report(3) = char(39)//'r'//char(39)//' or '
     .          //char(39)//'i'//char(39)
      go to 999

982   report(1) = 'error in input'
      report(2) = 'action must be '//char(39)//'a'//char(39)//' or '
      report(3) = char(39)//'m'//char(39)//' or '
     .          //char(39)//'e'//char(39)
      go to 999

983   report(1) = 'error in input'
      report(2) = 'action must be '//char(39)//'a'//char(39)//' or '
      report(3) = char(39)//'m'//char(39)//' or '//char(39)//'e'
     .          //char(39)//' or '//char(39)//'p'//char(39)
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //table//','//variable
      if (err.eq.3) report(3) = 
     .                      'file has line longer than 2000 characters'
      go to 999

993   report(1) = 'file has line longer than 3000 characters'
      report(2) = fnam
      report(3) = 'fix pp/src/calibration_utils/change_param/rproc.f'
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

995   report(1)='problem reading file:  for segment:  table:  variable:'
      report(2) = fnam
      report(3) = rsegs(ns)//' '//table//' '//variable
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines-1)(:100)
      go to 999

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

999   write(*,*) report(1)
      write(*,*) report(2)
      write(*,*) report(3)

      end
