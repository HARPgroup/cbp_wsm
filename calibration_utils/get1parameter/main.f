************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/land_use.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/lsegs.inc'

      character*250 geography   ! spatial exent of change, either lseg or seglist

      integer ns

      character*12 module,table,variable  ! variable specifiers
      integer lenmod,lenvar

      character*1 action  ! [m,a,e] = [multiply,add,equals]
      real value          ! value for action
      integer ivalue      ! value for action

      character*1 luflag  ! [a,p,i,s] = [all,perlnds,implnds,single]
      integer numlu,nl  ! number of land uses to act on
      integer lus(nlu) ! index of land uses to act on

      character*1 typeflag  ! [i,r] = [integer,real]

      integer nlinemax,nlines  ! number of parameter lines in code
      parameter (nlinemax=400)
      character*2000 parline(nlinemax),varline,tableline,vkeep,tkeep
              ! the 3 lines of the file with relevant info

      integer varcolumn  ! column that the variable of interest occupies

      logical scompare
      external scompare

      integer i,j  ! indices
      character*4 version ! version number
      logical fileexist,anyexist  ! if file already exists

      character*10 tree2
      integer lentree2

      real rvalues(nlu,maxlsegs)     ! stored values
      integer ivalues(nlu,maxlsegs)     ! stored values

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION

      print*,'enter land segment or seglist .land file name'
      read*,geography
      call procgeo(geography, maxlsegs,nlsegs,lsegs)

      print*,'enter the name of the directory tree'
      print*,'  like ',char(39),'p505',char(39),' for ',char(39),
     .       '/model/p505/',char(39)
      read*,tree2

      print*,'enter land scenario'
      read*,lscen

      print*,'enter module'
      read*,module

      print*,'enter table'
      read*,table

      print*,'enter variable'
      read*,variable

      err = 1
      do while (err.ne.0)
        print*,'enter variable type [i,r] = [integer,real]'
        read*,typeflag
        if (typeflag.eq.'i'.or.typeflag.eq.'I'.or.
     .      typeflag.eq.'r'.or.typeflag.eq.'R') err = 0
      end do

C      print*,'enter action [m,a,e] = [Multiply,Add,Equals]'
C      read*,action
C
C      print*,'enter value for action'
C      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
C        read*,ivalue
C      else
C        read*,value
C      end if

      call lencl(tree2,lentree2)
      err = 1
      do while (err.ne.0)
        print 1234,'enter land use flag [a,p,i,s,m] = ',
     .             '[All,Perlnds,Implnds,Single,Multiple]'
        read*,luflag
        call procluflag(lscen,luflag,tree2,lentree2, numlu,lus,err)
      end do

********** GET SOME OVERALL ACCOUNTING DONE
      call readcontrol_tree2paramscen(tree2,lentree2,lscen,paramscen)
      call lencl(paramscen,lenparamscen)
      call lencl(module,lenmod)
      call lencl(variable,lenvar)

************* LOOP OVER LAND USES, FIND THE FILE AND MAKE THE CHANGES
      do nl = 1,numlu

**********            OPEN FILE
        fnam = '/model/'//tree2(:lentree2)//'/pp/param/'//
     .         luname(lus(nl))//'/'//paramscen(:lenparamscen)//'/'//
     .         luname(lus(nl))//'_'//module(:lenmod)//'.csv'
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
          
**********          FIND AND STORE PARAMETERS
        do i = 1,nlines-1
          call d2x(parline(i),last)
          call findcomma(parline(i),last)
          Tseg = parline(i)(:last-1)
          call trims(Tseg,last)
          do ns = 1,nlsegs
            if (scompare(Tseg,lsegs(ns))) then  ! match
              call doaction(parline(i),varcolumn,typeflag,
     .                      rvalues(nl,ns),ivalues(nl,ns),err)
            end if
            if (err.ne.0) go to 995
          end do
        end do

      end do

**********           OPEN AND WRITE TO FILE
      call lencl(lscen,lenlscen)
      fnam = pardir//'/'//tree2(:lentree2)//'_'//lscen(:lenlscen)
     .           //'_'//variable(:lenvar)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,*,err=951) 'lseg', (',',luname(lus(nl)),nl=1,numlu) 

      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        do ns = 1,nlsegs
          write(dfile,*,err=951) lsegs(ns),
     .                           (',',ivalues(nl,ns),nl=1,numlu)
        end do
      else
        do ns = 1,nlsegs
          write(dfile,*,err=951) lsegs(ns),
     .                           (',',rvalues(nl,ns),nl=1,numlu)
        end do
      end if


      return
1234  format (a,a)
********************* ERROR SPACE **************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      if (err.eq.2) report(3) = 'did not find table, variable '
     .                           //table//variable
      if (err.eq.3) report(3) = 
     .                      'file has line longer than 2000 characters'
      go to 999

993   report(1) = 'file has line longer than 3000 characters'
      report(2) = fnam
      report(3) = 'fix pp/src/calibration_utils/change_param/proc.f'
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

995   report(1)='problem reading file:  for segment:  table:  variable:'
      report(2) = fnam
      report(3) = lsegs(ns)//' '//table//' '//variable
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

999   print*,report(1)
      print*,report(2)
      print*,report(3)

      end
