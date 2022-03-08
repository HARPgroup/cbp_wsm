************************************************************************
** program to change parameters for calibration.  Automatically reads **
**   csv files and performs functions on the variables                **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/lsegs.inc'

      character*250 geography   ! spatial exent of change, either lseg or seglist

      integer ns

      character*15 module,table,variable  ! variable specifiers
      integer lenmod

      character*1 action  ! [m,a,e] = [multiply,add,equals]
      real rvalue,rvalues(maxlsegs)          ! real value for action
      integer ivalue,ivalues(maxlsegs)      ! integer value for action

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

      character*1 monflag  ! [a,s,m] = [all,single,multiple]
      integer monthidx(12),nmonth, nm, nm2, moncolumn(12)

      character*3 monname(12),nonmonth
      data monname /'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     .             'SEP','OCT','NOV','DEC'/

*************** END DECLARATIONS ***************************************


************ SPECIFICATION ENTRY SECTION
      read*,geography, lscen, module, table, variable, typeflag, action,
     .      rvalue, luflag

      call procgeo(geography, maxlsegs,nlsegs,lsegs)
      call lencl(lscen,lenlscen)
      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      call uppercase(module)
      call uppercase(table)
      call uppercase(variable)

      if (typeflag.eq.'i'.or.typeflag.eq.'I') then
        ivalue = int(rvalue+.05)
        do ns = 1,nlsegs      ! single value for all
          ivalues(ns) = ivalue
        end do
      else
        do ns = 1,nlsegs      ! single value for all
          rvalues(ns) = rvalue
        end do
      end if

      call procluflag(lscen,luflag, numlu,lus,err)

********** GET SOME OVERALL ACCOUNTING DONE
      call lencl(paramscen,lenparamscen)
      call lencl(module,lenmod)

**********           FIND FIRST FREE FILE NAME FOR ALL LAND USES
      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
        anyexist = .false.
          do nl = 1,numlu
            fnam =pardir//luname(lus(nl))//'/'//paramscen(:lenparamscen)
     .            //'/'//module(:lenmod)//'_'//version//'.csv'
            inquire (file=fnam,exist=fileexist)
            if (fileexist) anyexist = .true.
          end do
        if (.not.anyexist) exit
      end do

************* LOOP OVER LAND USES, FIND THE FILE AND MAKE THE CHANGES
      print*,'lu   landseg   month    old param     new param'

      do nl = 1,numlu

**********            OPEN FILE
        fnam = pardir//luname(lus(nl))//'/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'.csv'
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

        if (table(1:3).eq.'MON') then
         do nm = 1, 12
          do nm2 = 1, nmonth
           if (monthidx(nm2) .eq. nm) then
            moncolumn(nm) = varcolumn + (nm -1)
           end if
          end do
         end do 
        end if

**********            READ WHOLE FILE INTO MEMORY
        nlines = 1
        read(dfile,'(a2000)',err=996,end=997) parline(nlines)
        do while (parline(nlines)(:3).ne.'end')
          nlines = nlines + 1
          read(dfile,'(a2000)',err=996,end=997) parline(nlines)
        end do
        close(dfile)
          
**********           WRITE FILE TO NEW FILE NAME
        fnam = pardir//luname(lus(nl))//'/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'_'//version//'.csv'
        open(dfile,file=fnam,status='new',iostat=err)
        if (err.ne.0) go to 991
        call rytdos(tkeep,dfile)
        call rytdos(vkeep,dfile)
        do i = 1,nlines
          call rytdos(parline(i),dfile)
        end do
        close(dfile)
        
**********          REWRITE MODS TO ORIGINAL FILE NAME        
        fnam= pardir//luname(lus(nl))//'/'//paramscen(:lenparamscen)//
     .        '/'//module(:lenmod)//'.csv'
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        call rytdos(tkeep,dfile)
        call rytdos(vkeep,dfile)
        do i = 1,nlines-1
          call d2x(parline(i),last)
          call findcomma(parline(i),last)
          Tseg = parline(i)(:last-1)
          call trims(Tseg,last)
          do ns = 1,nlsegs
           if (scompare(Tseg,lsegs(ns))) then  ! match
             if (table(1:3).eq.'MON') then
             do nm = 1, nmonth
              variable = monname(monthidx(nm)) 
              varcolumn = moncolumn(monthidx(nm))
              call doaction(parline(i),action,rvalues(ns),ivalues(ns),
     .                    variable,varcolumn,typeflag,Tseg,
     .                    luname(lus(nl)),monname(monthidx(nm)),err)
             end do
            else
             nonmonth = ' '            ! dummy variable for non-monthly data 
             call doaction(parline(i),action,rvalues(ns),ivalues(ns),
     .                    variable,varcolumn,typeflag,Tseg,
     .                    luname(lus(nl)),nonmonth,err)
            end if
           end if
           if (err.ne.0) go to 995
          end do
          call rytdos(parline(i),dfile)
        end do
        call rytdos(parline(nlines),dfile)  ! write 'end'
        close(dfile)

      end do

      return

1234  format (a,a)

********************* ERROR SPACE **************************************
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
