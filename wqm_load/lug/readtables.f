************************************************************************
** populates the tables for the PERLND or IMPLND simulation           **
** using the scenario file and module catalogs                        **
**  This file contains the major subroutines called from the major    **
**    subroutine 'tables'                                             **
************************************************************************
      subroutine readtables(
     I                      lseg,clu,lscen,lenlscen,perlnd,implnd,
     I                      modules,nmod,
     O                      table,nTabs,header,nHead,Var,tabformat,
     O                      iPar,fPar,evap)
      implicit none

      include 'lugtables.inc'
      include 'acts.inc'

      logical perlnd,implnd

      integer nm,nt,nh,nv              ! indices

      real twelveval(12)

      character*1 IorF        ! integer or real format

      real evap   ! evaporation modifier

      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)

      call gettabs(
     I             modules,nmod,
     O             table,nTabs,header,nHead,Var,tabformat)

      call getPar(
     I            lseg,clu,modules,nmod,
     I            table,nTabs,Var,tabformat,
     I            paramscen,
     O            iPar,fPar)

      call getevap(
     I             lseg,paramscen,
     O             evap)

      return

************* ERROR SPACE    *******************************************

      end


************************************************************************
** gets the parameter values from the databases                       **
**  loop over modules opening each file and getting the variables     **
************************************************************************
      subroutine getPar(lseg,clu,modules,nmod,table,nTabs,Var,tabformat,
     I                  paramscen,
     O                  iPar,fPar)
      implicit none
      include 'lugtables.inc'

      character*3000 parline,tableline,varline  ! the 3 lines of the file with info

      character*15 Ttab,Tvar           ! temp reading variables
      character*13 Tlseg               ! temp reading varible

      integer coltab(nTabmax*nVarMax)        ! table number for each column
                                 !   like = >  1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 4
      integer colvar(nTabmax*nVarMax)        ! variable number for each column
                                 !   like = >  1, 2, 3, 1, 2, 3, 4, 1, 2, 1, 2, 3

      logical foundT,foundV,foundS  ! have I found this info 
      logical scompare
      integer n,nm,nt,nh,nv,ic                ! indices

      integer clmlength      ! number of characters in column
      integer ncols          ! number of columns in this file

      integer lenmod,lentab,lenvar,tablast,parlast,varlast              ! string lengths

      logical done

      integer nf       ! file number
      character*1 cnf  ! file number 

      logical found(nTabMax,nVarMax)  ! have I found this table and variable
      logical foundfile

**************** END DECLARATION ***************************************

      call lencl(paramscen,lenparamscen)

      do nm = 1,nmod                !  loop over all active modules

        do nt = 1,nTabs(nm)
          do nv = 1,tabformat(nm,nt,1)
            found(nt,nv) = .false.
          end do
        end do
          
        call lencl(modules(nm),lenmod)
        done = .false.
        nf = 1

        do while (.not.done)
          foundfile = .false.
          fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .           '/'//modules(nm)(:lenmod)//'.csv'
          open(dfile,file=fnam,status='old',iostat=err)

          inquire (file=fnam,exist=foundfile) 
          if (foundfile) then
            done = .true.
          else if (.not.foundfile) then
            write(cnf,'(i1)') nf
            foundfile = .false.
            fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .             '/'//modules(nm)(:lenmod)//cnf//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            inquire (file=fnam,exist=foundfile)

            if (foundfile) then
              nf = nf + 1
            else if (.not. foundfile) then
              done = .true.
              cycle
            else
              go to 991
            end if
          else 
            go to 991
          end if

          foundT = .false.                   ! initializations
          foundV = .false.
          foundS = .false.
          do nt = 1,nTabmax*nVarMax
            coltab(nt) = -9
            colvar(nt) = -9
          end do
  
          read(dfile,'(a100)')line
          call d2x(line,last)
          do while (line(:3).ne.'end')               ! search for the 3 lines
            call findcomma(line,clmlength)
            Tlseg = line(:clmlength-1)
            if (line(:7).eq.'TABLE->') then
              backspace dfile
              read(dfile,'(a)') tableline
              call d2x(tableline,tablast)
              foundT = .true.
            else if (line(:6).eq.'VARS->') then
              backspace dfile
              read(dfile,'(a)') varline 
              call d2x(varline,varlast)
              foundV = .true.
            else if (scompare(Tlseg,lseg)) then
              backspace dfile
              read(dfile,'(a)') parline
              call d2x(parline,parlast)
              foundS = .true.
            end if
            read(dfile,'(a100)')line
            call d2x(line,last)
          end do                       ! loop over lines in file
          close(dfile)
          if (.not.foundT) go to 992
          if (.not.foundV) go to 993
          if (.not.foundS) go to 994


          call popcoltab(tableline(:tablast),coltab,nTabs,table,ncols,
     .                   nm)

          call popcolvar(varline(:varlast),coltab,colvar,tabformat,
     .                   Var,ncols,nm,nTabs(nm),fnam,found)

          call poppar(parline,iPar,fPar,coltab,colvar,tabformat,nm,
     .                ncols,err)
          if (err.ne.0) go to 995

        end do     ! loop over possible input files

        do nt = 1,nTabs(nm)   !  check for all tables found
          do nv = 1,tabformat(nm,nt,1)
            if (.not.found(nt,nv)) then
               fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .                '/'//modules(nm)(:lenmod)//'.csv'
               go to 996
            end if
          end do
        end do


      end do     ! loop over modules

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'IN '//fnam
      report(2)= ' could not find Table Name header, first 8 characters'
      report(3) = ' should be:TABLE->,'
      go to 999

993   report(1) = 'IN '//fnam
      report(2) = ' could not find Variable header, first 7 characters'
      report(3) = ' should be:VARS->,'
      go to 999

994   report(1) = 'IN '//fnam
      report(2) = ' could not find segment '//lseg
      report(3) = ' Segment name should have no leading or '
     .            //'trailing blanks'
      go to 999

995   report(1) = 'IN '//fnam
      report(2) = ' problem reading variables for segment '//lseg
      report(3) = 'check the file for bad characters'
      go to 999

996   report(1) = 'IN '//fnam
      report(2) =' or similar files: could not find table and variable:'
      report(3) = table(nm,nt)//'  '//Var(nm,nt,nv)
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  subroutine to populate coltab which is an index giving the table  **
**    number for each column                                          **
**  In a separate subroutine because the speed of the entire LUG is   **
**   dependent on the length of the variable colline.                 **
************************************************************************
      subroutine popcoltab(colline,coltab,nTabs,table,ncols,nm)
      implicit none
      include 'lugtables.inc'
      character*(*) colline
      character*15 Ttab
      integer coltab(nTabmax*nVarMax) 
      integer ncols,ic,clmlength,nm,nt    ! indices and utilities
      logical scompare

      ic = 0          ! column counter
      clmlength = -9          ! spaces to next label
      do while (clmlength.ne.0)
        ic = ic + 1
        call shift(colline)           ! get rid of first column
        call findcomma(colline,clmlength)
        if (clmlength.eq.0) exit
        if (clmlength.le.16) then
          Ttab = colline(:clmlength-1)
        else
          Ttab = colline(:15)
        end if
        call trims(Ttab,last)
        do nt = 1,nTabs(nm)
          if (scompare(table(nm,nt),Ttab)) coltab(ic) = nt
        end do
      end do

      ncols = ic        ! store number of columns

      end

************************************************************************
**  subroutine to populate colvar which is an index giving the        **
**    variable number for each column.                                **
**  In a separate subroutine because the speed of the entire LUG is   **
**   dependent on the length of the variable colline.                 **
************************************************************************
      subroutine popcolvar(colline,coltab,colvar,tabformat,Var,ncols,nm,
     .                     numtables,fnam,found)
      implicit none
      include 'lugtables.inc'
      character*(*) colline
      character*15 Tvar
      integer coltab(nTabmax*nVarMax),colvar(nTabmax*nVarMax) 
      integer ncols,ic,clmlength,nm,nt,nv,tablast    ! indices and utilities
      integer numtables    ! number of tables for this module
      logical scompare
      logical found(nTabMax,nVarMax)  ! have I found this info

      ic = 0          ! column counter
      clmlength = -9          ! spaces to next label
      do while (clmlength.ne.0)
        ic = ic + 1
        call shift(colline)
        call findcomma(colline,clmlength)
        if (clmlength.eq.0) exit
        if (clmlength.le.16) then
          Tvar = colline(:clmlength-1)
        else
          Tvar = colline(:15)
        end if
        call trims(Tvar,tablast)
        if (coltab(ic).gt.0) then
          do nv = 1,tabformat(nm,coltab(ic),1)
            if (scompare(Var(nm,coltab(ic),nv),Tvar)) then
              colvar(ic) = nv
              found(coltab(ic),nv) = .true.
            end if
          end do
        end if
      end do

      if (ncols.ne.ic) go to 995

      return

******************* ERROR SPACE ********************************

995   report(1) = 'IN '//fnam
      report(2) = 'number of columns in table line: '
      write(report(2)(34:36),'(i3)') ncols
      report(3) = 'does not match variable line: '
      write(report(3)(31:33),'(i3)') ic
      go to 999

999   call stopreport(report)

      end

************************************************************************
** populates the parameter variables according to the order of the    **
**   table line and parameter line.                                   **
************************************************************************
      subroutine poppar(parline,iPar,fPar,coltab,colvar,tabformat,nm,
     .                  ncols,err)
      implicit none
      include 'lugtables.inc'
      integer ic,ncols,clmlength,nm
      integer coltab(nTabmax*nVarMax)        ! table number for each column
      integer colvar(nTabmax*nVarMax)        ! variable number for each column
      logical scompare
      character*(*) parline
      character*3 na(6)
      data na /'na','NA','Na','n/a','N/a','N/A'/
      
      do ic = 1,ncols
        call shift(parline)
        call findcomma(parline,clmlength)
        if (colvar(ic).gt.0.and.coltab(ic).gt.0) then
          if (scompare(parline(:clmlength-1),na(1)).or.
     .        scompare(parline(:clmlength-1),na(2)).or.
     .        scompare(parline(:clmlength-1),na(3)).or.
     .        scompare(parline(:clmlength-1),na(4)).or.
     .        scompare(parline(:clmlength-1),na(5)).or.
     .        scompare(parline(:clmlength-1),na(6))) then
            if (tabformat(nm,coltab(ic),2).eq.0) then
              iPar(nm,coltab(ic),colvar(ic)) = -999
            else
              fPar(nm,coltab(ic),colvar(ic)) = -999.
            end if
          else
            if (tabformat(nm,coltab(ic),2).eq.0) then  ! integer table
              call readir(parline,iPar(nm,coltab(ic),colvar(ic)),err)
            else if (tabformat(nm,coltab(ic),2).eq.1) then ! real table
              call freadr(parline,fPar(nm,coltab(ic),colvar(ic)),err)
            else         ! unique format, read as real, store as both
              call freadr(parline,fPar(nm,coltab(ic),colvar(ic)),err)
              ipar(nm,coltab(ic),colvar(ic)) = 
     .                 int(fPar(nm,coltab(ic),colvar(ic)))
            end if
          end if
        end if
      end do

      end


************************************************************************
**  reads the tables file and finds the table names, headers, and     **
**   variable names for the active modules                            **
************************************************************************
      subroutine gettabs(modules,nmod,
     O                   table,nTabs,header,nHead,Var,tabformat)

      implicit none
      include 'lugtables.inc'
      logical found,comment,perlnd,implnd
      integer n,nm,nt,nh,nv                   ! indices
      integer lenmod,lentab,lenvar            ! string lengths
      character*6 tempmodname

      character*1 IorF        ! integer or real format

      fnam = catdir//'modules/tables'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:4).ne.'    ') then  ! start of a module
          found = .false.
          lenmod = last
          tempmodname = line(:6)
          do n = 1,nmod
            if (modules(n)(:6).eq.tempmodname) then
              found = .true.
              nm = n
            end if
          end do
          if (found) then                                ! need this module
            read(dfile,'(a100)')line
            call d2x(line,last)
            nt = 0
            do while (.not.comment(line).and.
     .                line(:last).ne.'END '//tempmodname)
C     .                line(:last).ne.'END '//modules(nm)(:lenmod))
              nt = nt + 1
              table(nm,nt) = line(3:17)          ! get table name

              if (line(21:21).eq.'i'.or.line(21:21).eq.'I') then
                tabformat(nm,nt,2) = 0
                read(line(19:20),'(i2)') tabformat(nm,nt,1) ! # of vars
                read(line(22:23),'(i2)') tabformat(nm,nt,3) ! size
              else if (line(21:21).eq.'f'.or.line(21:21).eq.'F') then
                tabformat(nm,nt,2) = 1
                read(line(19:20),'(i2)') tabformat(nm,nt,1) ! # of vars
                read(line(22:23),'(i2)') tabformat(nm,nt,3) ! size
              else if (line(21:21).eq.'u'.or.line(21:21).eq.'U') then
                tabformat(nm,nt,2) = 2
                tabformat(nm,nt,1) = 0
                tabformat(nm,nt,3) = 0
              else
                go to 992
              end if

              read(dfile,'(a100)')line
              call d2x(line,last)
              nh = 0
              do while (comment(line))     ! get header lines
                nh = nh + 1
                header(nm,nt,nh) = line
                if (line(3:3).eq.'V') then        ! signal for variable line
                  header(nm,nt,nh)(3:3) = ' '
                  do nv = 1,tabformat(nm,nt,1)
                    lenvar = tabformat(nm,nt,3)
                    Var(nm,nt,nv) = line(11+(nv-1)*lenvar:10+nv*lenvar)
                    call checkvar(Var(nm,nt,nv),err)
                    if (err.ne.0) go to 993
                  end do
                  if (tabformat(nm,nt,2).eq.2) then   ! unique format
                    call Uformat(modules(nm),table(nm,nt),
     O                           Unvar,Ustart,Uend,IorF)
                    tabformat(nm,nt,1) = Unvar  ! number of variables
                    do nv = 1,Unvar
                      Var(nm,nt,nv) = line(Ustart(nv):Uend(nv))
                      call checkvar(Var(nm,nt,nv),err)
                      if (err.ne.0) go to 993
                    end do
                  end if

                end if
                read(dfile,'(a100)')line
                call d2x(line,last)

              end do       ! end loop over headers

              nHead(nm,nt) = nh

            end do        ! end loop over tables in module

            ntabs(nm) = nt

          else               ! just read to next mod
            do while (comment(line).or.
     .                line(:last).ne.'END '//tempmodname)
              read(dfile,'(a100)')line
              call d2x(line,last)
            end do
          end if
        end if
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      close(dfile)

      return
            
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = fnam
      report(2) = ' Format for following table must be i or f, not '
     .              //line(21:21)
      report(3) = modules(nm)//'  '//table(nm,nt)
      go to 999

993   report(1) = fnam
      report(2) = 'Illegal variable name'
      report(3) = modules(nm)//'  '//table(nm,nt)//'  '//Var(nm,nt,nv)
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  checkvar makes sure that the variable doesn't have internal       **
***  blanks is not a comment and contains characters                  **
************************************************************************
      subroutine checkvar(c10,err)
      implicit none

      character*10 c10,Tc
      integer err,i,last

      Tc = c10
      err = 0

      do i = 1,8
        if (c10(i:i+2).eq.'***') err = 1
      end do

      if (c10 .eq. '          ') err = 1

      call trims(Tc,last)      ! take out leading blanks

      do i = 1,10         ! look for more than one word
        if (Tc(i:i).eq.' '.and.i.lt.last) err = 1
      end do

      end

************************************************************************
**  reads the file catdir//modules/Uformat to get the number of       **
**  variables and the start and stop column of each for uniquely      **
**  formatted tables                                                  **
************************************************************************
      subroutine Uformat(module,tabname,
     O                   Unvar,Ustart,Uend,IorF)
      implicit none
      include 'lugtables.inc'
      character*(*) module
      character*(*) tabname

      character*25 endmod,temptab

      integer iofile,nv
      logical found
      logical scompare

      character*1 IorF

      temptab = tabname
      call lencl(temptab,last)
      if (ichar(temptab(last-1:last-1)).eq.35) then  ! if '#'
        temptab = temptab(:last-2)  ! get rid of suffix
      end if

      endmod = module
      call trims(endmod,last)
      endmod = 'END '//endmod

      call findopen(iofile)
      fnam = catdir//'modules/Uformat'
      open(iofile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      found = .false.
      read(iofile,'(a100)',end=996,err=995)line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (scompare(line(:last),module)) then
          read(iofile,'(a100)')line
          call d2x(line,last)
          do while (.not.scompare(line(:last),endmod))
            if (scompare(line(:last),temptab)) then
              found = .true.
              read(iofile,'(i4,a1)',err=993) Unvar,IorF
              read(iofile,'(a100)',end=996,err=995)line
              do nv = 1,Unvar
                read(line(3*nv:3*nv+1),'(i2)',err=994) Ustart(nv)
              end do
              read(iofile,'(a100)',end=996,err=995)line
              do nv = 1,Unvar
                read(line(3*nv:3*nv+1),'(i2)',err=994) Uend(nv)
              end do
            else
              read(iofile,'(a100)',end=996,err=995)line
              read(iofile,'(a100)',end=996,err=995)line
              read(iofile,'(a100)',end=996,err=995)line
            end if
            read(iofile,'(a100)')line
            call d2x(line,last)
          end do
        end if
        read(iofile,'(a100)',end=996,err=995)line
        call d2x(line,last)
      end do

      close(iofile)

      if (.not.found) go to 992

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Could not find table '//temptab//' for module '//
     .             module//' in file:'
      report(2) = fnam
      report(3) = 'edit file for unique table format'
      go to 999

993   report(1) = 'Could not find parameters for table '//temptab//
     .            ' for module '//module
      report(2) = fnam
      report(3) = 'line after table name is number of variables (i4)'
     .            //' followed by an i or f (1a)'
      go to 999

994   report(1) = 'Can not read column numbers for table '//temptab
      report(2) = fnam
      report(3) = 'edit file for unique table format'
      go to 999

995   report(1) = 'Problem reading file: near line: '
      report(2) = fnam
      report(3) = line
      go to 999

996   report(1) = 'Problem reading file:'
      report(2) = fnam
      report(3) = 'End of File reached'
      go to 999

999   call stopreport(report)

      end


