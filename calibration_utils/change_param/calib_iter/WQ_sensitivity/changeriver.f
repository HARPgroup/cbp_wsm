************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program is a subroutine form of the change_river program      **
************************************************************************
      subroutine change_river(
     I                        paramscen,uniqindex,lakeflags,
     I                        parLKflag,parModule,
     I                        parTable,parName,
     I                        parval)
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/rsegs.inc'

******* passed parameters
      integer lakeflags(maxrsegs),parLKflag
      character*(*) parModule
      character*(*) parTable
      character*(*) parName
      real parval

************* other vars
      integer:: ns         ! number of land segments to change

      integer:: lenmod

      character(1):: action                  ! [m,a,e] = [multiply,add,equals]
      real:: rvalue,rvalues(maxrsegs)        ! real value for action
      integer:: ivalue,ivalues(maxrsegs)     ! integer value for action

      integer:: numrseg,nr                   ! number of land uses to act on
      
      character(1):: typeflag                ! [i,r] = [integer,real]

      integer:: nlinemax,nlines              ! number of parameter lines in code
      parameter (nlinemax=750)
      character(2000):: parline(nlinemax),varline,tableline,vkeep,tkeep
                            ! the 3 lines of the file with relevant info

      integer:: varcolumn                    ! column that the variable of interest occupies

      logical:: scompare
      external scompare

      integer:: i,j                          ! indices
      character(4):: version                 ! version number
      logical:: fileexist,anyexist           ! if file already exists

      integer uniqueID

*************** END DECLARATIONS ***************************************
      typeflag = 'r'
      action = 'e'
      if (parName(2:4).eq.'TAU') action = 'p'

      call lencl(paramscen,lenrscen)
      call lencl(parModule,lenmod)

************            OPEN FILE
      fnam = pardir//'river/'//paramscen(:lenrscen)//
     .       '/'//parModule(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********            FIND CORRECT COLUMN
      read(dfile,'(a2000)',err=994) tableline
      call d2x(tableline,last)
      tkeep = tableline
      read(dfile,'(a2000)',err=994) varline
      call d2x(varline,last)
      vkeep = varline
      call findcolumn(tableline,varline,parTable,parName,varcolumn,err)
      if (err.ne.0) go to 992

**********            READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=996,end=997) parline(nlines)
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=996,end=997) parline(nlines)
      end do

      close(dfile)
          
**********          REWRITE MODS TO ORIGINAL FILE NAME        
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines-1
        call d2x(parline(i),last)
        call findcomma(parline(i),last)
        Tseg = parline(i)(:last-1)
        call trims(Tseg,last)
        read(Tseg(5:8),'(i4)') uniqueID
        if (lakeflags(uniqindex(uniqueID)).eq.parLKflag
     .         .or.parLKflag.eq.2) then
          call doaction(parline(i),action,parval,ivalues(ns),
     .                  parName,varcolumn,typeflag,Tseg,
     .                  paramscen,err)
          if (err.ne.0) go to 995
        end if
        call rytdos(parline(i),dfile)
      end do
      call rytdos(parline(nlines),dfile)  ! write 'end'
    
      close(dfile)

      return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //parTable//','//parName
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
      report(3) = rsegs(ns)//' '//parTable//' '//parName
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

************************************************************************
** finds the column that is occupied by the specified variable        **
************************************************************************
      subroutine findcolumn(tableline,varline,table,variable, 
     O           varcolumn,err)
      implicit none

      character*(*)table  ! table name
      character*(*) variable  ! variable name
      character*14 Ttab,Tvar

      integer varcolumn    ! column that contains the variable

      integer tablast,varlast,err,last

      character*(*) tableline,varline
  
      logical scompcase
      external scompcase
      
      err = 0

      if (tableline(2000-5:2000).ne.'      ') go to 993
      if (varline(2000-5:2000).ne.'      ') go to 993

      varcolumn = 1
      call findcomma(tableline,tablast)
      Ttab = tableline(:tablast-1)
      call trims(Ttab,last)
      call findcomma(varline,varlast)
      Tvar = varline(:varlast-1)
      call trims(Tvar,last)
      do while (.not.(scompcase(Ttab,table).and.
     .                scompcase(Tvar,variable)))
        if (tablast.eq.0.or.varlast.eq.0) go to 992
        varcolumn = varcolumn + 1
        call shift(tableline)
        call shift(varline)
        call findcomma(tableline,tablast)
        Ttab = tableline(:tablast-1)
        call trims(Ttab,last)
        call findcomma(varline,varlast)
        Tvar = varline(:varlast-1)
        call trims(Tvar,last)
      end do
 
      return
992   err = 2
      return
993   err = 3
      return
      end


************************************************************************
** takes a row of comma delimited variables and performs the          **
**   specified action on the specified column                         **
************************************************************************
      subroutine doaction(parline,action,value,ivalue,variable,
     .                    varcolumn,typeflag,Tseg,paramscen,err)
      implicit none
      character*(*) parline
      character*(*) Tseg
      character*(*) variable
      character*(*) paramscen

      character*1 action  ! [m,a,e,p] = [multiply,add,equals,percentile]
      real value, oldval, newval   ! value for action and old value
      integer ivalue,oldival,newival      ! value for action and old value


      character*1 typeflag  ! [i,r] = [integer,real]

      integer varcolumn  ! column that the variable of interest occupies
 
      integer err

      integer ncomma  ! number of commas found
      integer firstcomma,lastcomma ! space for variable
      integer i  ! index

      real findtaupercent
      external findtaupercent

      err = 0

      ncomma = 0
      i = 0
      do while (ncomma.lt.varcolumn-1)
        i = i + 1
        if (i.ge.len(parline)) then
          err = 1
          return
        end if
        if (parline(i:i).eq.',') ncomma = ncomma + 1
      end do

      firstcomma = i

      i = i + 1
      do while (parline(i:i).ne.','.and.i.le.firstcomma+20)
        i = i + 1
      end do
      lastcomma = i
      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        read(parline(firstcomma+1:lastcomma-1),*,err = 992) oldival  
        if (action.eq.'m'.or.action.eq.'M') then
          newival = oldival * ivalue
        else if (action.eq.'a'.or.action.eq.'A') then
          newival = oldival + ivalue
        else if (action.eq.'e'.or.action.eq.'E') then
          newival = ivalue
        end if
        parline(firstcomma+11:) = parline(lastcomma:)
        write(parline(firstcomma+1:firstcomma+10),'(i10)') newival
        call noblanks(parline,i)
      else
        read(parline(firstcomma+1:lastcomma-1),*,err = 993) oldval
        if (action.eq.'m'.or.action.eq.'M') then
          newval = oldval * value
          if (variable(:4).eq.'AGWR') then
            newval = 1.0 - (1.0 - oldval)/value
          end if
        else if (action.eq.'a'.or.action.eq.'A') then
          newval = oldval + value
        else if (action.eq.'e'.or.action.eq.'E') then
          newval = value
        else if (action.eq.'p'.or.action.eq.'P') then
          newval = findtaupercent(Tseg,paramscen,value)
        end if
        parline(firstcomma+11:) = parline(lastcomma:)
        call ritef10(parline(firstcomma+1:firstcomma+10),newval)
        call noblanks(parline,i)
      end if

      return

992   err = 2
      return

993   err = 3
      return

      end
     
 
************************************************************************
** takes a row of comma delimited variables and performs the          **
**   specified action on the specified column                         **
************************************************************************
      function findtaupercent(Tseg,paramscen,value)
      implicit none

      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'

      real findtaupercent   ! value of tau at 'value' percentile

      real value     ! percentile to find

      integer upindex,downindex  ! indices

      integer maxpcnt,npcnt,np    ! number of percentiles in file
      parameter (maxpcnt=100)
      real percentile(maxpcnt)    ! percentiles
      real pval(maxpcnt)          ! values of tau at that percentile

      real interpolate
      external interpolate

      integer ifl

*************** END DECLARATIONS **************************************

******** OPEN FILE AND READ HEADERS
      call findopen(ifl)
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/tau_percentiles.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(ifl,*,err=992,end=992) npcnt
      read(ifl,*,err=992,end=992) rseg,(percentile(np),np=1,npcnt)
      do np = 1,npcnt-1
        if (percentile(np).le.percentile(np+1)) go to 995
      end do

********* LOOP OVER SEGEMENTS UNTIL YOU FIND THE ONE YOU WANT
      do 
        read(ifl,*,err=993,end=994)rseg
        if (Tseg.eq.rseg) then
          backspace ifl
          read(ifl,*,err=993,end=994)rseg,(pval(np),np=1,npcnt)
          exit
        end if
      end do

      close(ifl)

******** INTERPOLATE TO FIND THE CORRECT PERCENTILE
      upindex = -1
      downindex = -1
      do np = 1,npcnt   ! get upper and lower point
        if (value.le.percentile(np)) upindex = np
      end do
      do np = npcnt,1,-1
        if (value.ge.percentile(np)) downindex = np
      end do

      if (upindex.eq.downindex) then    ! value is in the table no interp
        if (upindex.eq.-1) go to 996
        findtaupercent = pval(upindex)
      else 
        if (upindex.eq.-1) then        ! asking for above max, OK
          upindex = 1
          downindex = 2
        else if (downindex.eq.-1) then  ! asking for below min, OK
          upindex = npcnt-1
          downindex = npcnt
        else                           ! regular interpolation
           if (downindex-upindex.ne.1) then  !  should be 1
             if (downindex-upindex.eq.2) then !  2 because of inexact math
               if (abs(percentile(upindex)-value).lt.
     .             abs(percentile(downindex)-value)) then
                 downindex = downindex - 1
               else
                 upindex = upindex + 1
               end if
             else
               go to 996
             end if
           end if  ! end error checking
         end if
         findtaupercent=interpolate(value,
     .                            percentile(downindex),pval(downindex),
     .                            percentile(upindex),pval(upindex))
       end if

       if (findtaupercent.lt.1e-9) findtaupercent=1e-9

       return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading first two lines in file:'
      report(2) = fnam
      report(3) = 'line 1: number of percentiles, line2: percentiles'
      go to 999

993   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error near segment '//Tseg
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'found EOF before finding segment '//Tseg
      go to 999

995   report(1) = 'problem with file'
      report(2) = fnam
      report(3) = 'percentiles must be sorted in decreasing order '//
     .            'left to right'
      go to 999

996   report(1) = fnam
      report(2) = 'problem calculating percentiles check code'
      report(3) = './pp/src/calibration_utils/change_param/'//
     .            'river/findtau.f'
      go to 999

999   call stopreport(report)
      end
