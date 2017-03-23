************************************************************************
** populates the tables for the RCHRES simulation                     **
** using the scenario file and module catalogs                        **
**  This file contains the major subroutines called from the major    **
**    subroutine 'rtables'                                            **
************************************************************************
      subroutine scenario_rtables(rseg,rscen,lenrscen,Nexits,lakeflag)
      implicit none

      include 'rugtables.inc'

      integer nm,nt,nh,nv,lenvar              ! indices

      real twelveval(12)

      character*1 IorF        ! integer or real format

      call readcontrol_modules(rscen,lenrscen,modules,nmod)

      call readcontrol_pscen(rscen,lenrscen,paramscen)

      call gettabs(
     I             modules,nmod,
     O             table,nTabs,header,nHead,Var,tabformat)

      call getPar(
     I            rseg,modules,nmod,
     I            table,nTabs,Var,tabformat,
     I            paramscen,
     O            iPar,fPar)

      call scenario_parmod(
     I                     rseg,rscen,paramscen,
     I                     modules,nmod,
     I                     table,nTabs,Var,tabformat,
     M                     iPar,fPar)

      line = 'RCHRES'
      call ryt(line,uci)

      call activity(modules,nmod)

      call geninfo(rseg,Nexits,lakeflag)

      call hydrparm1(rseg,Nexits,lakeflag)

      do nm = 1,nmod

        do nt = 1,nTabs(nm)

           call lencl(table(nm,nt),last)                   ! deal with same table name problem
           if (ichar(table(nm,nt)(last-1:last-1)).eq.35) then  ! if the one next to last is #
             table(nm,nt) = table(nm,nt)(:last-2)            ! get rid of last two characters, for example:#1
           end if

          line = '  '//table(nm,nt)
          call ryt(line,uci)
          do nh = 1,nHead(nm,nt)
            call ryt(header(nm,nt,nh),uci)
          end do
          if (tabformat(nm,nt,2).eq.0) then          ! integer table
            line = '    1'
            if (tabformat(nm,nt,3).eq.5) then  ! five places per variable
              do nv = 1,tabformat(nm,nt,1)
                if (iPar(nm,nt,nv) .eq. -999) then
                  line(5*nv+6:5*(nv+2)) = '     '
                else
                  write(line(5*nv+6:5*(nv+2)),'(i5)') iPar(nm,nt,nv)
                end if
              end do
            else if (tabformat(nm,nt,3).eq.10) then    ! ten places per variable
              do nv = 1,tabformat(nm,nt,1)
                if (iPar(nm,nt,nv) .eq. -999) then
                  line(10*nv+1:10*(nv+1)) = '          '
                else
                  write(line(10*nv+1:10*(nv+1)),'(i10)') iPar(nm,nt,nv)
                end if
              end do
            else
              go to 991
            end if
            call ryt(line,uci)
          else if (tabformat(nm,nt,2).eq.1) then     ! real table
            do nv = 1,tabformat(nm,nt,1)
              twelveval(nv) = fPar(nm,nt,nv)
            end do
            if (tabformat(nm,nt,3).eq.5) then          ! five places per variable
              call rite12f5(uci,tabformat(nm,nt,1),twelveval)
            else if (tabformat(nm,nt,3).eq.10) then    ! ten places per variable
              call rite7f10(uci,tabformat(nm,nt,1),twelveval)
            else
              go to 991
            end if
          else if (tabformat(nm,nt,2).eq.2) then     ! unique format table
            line = '    1'
            call Uformat(modules(nm),table(nm,nt),
     O                   Unvar,Ustart,Uend,IorF)
            if (IorF.eq.'i'.or.IorF.eq.'I') then
              do nv = 1,Unvar
                call Uwritei(Ustart(nv),Uend(nv),ipar(nm,nt,nv),line)
              end do
              call ryt(line,uci)
            else if (IorF.eq.'f'.or.IorF.eq.'F') then
              do nv = 1,Unvar
                call fchar(fpar(nm,nt,nv),line(Ustart(nv):Uend(nv)))
              end do
              call ryt(line,uci)
            else
              go to 991
            end if
          else
            go to 991
          end if
          line = '  END '//table(nm,nt)
          call ryt(line,uci)
          line = ' '
          call ryt(line,uci)
        end do
      end do

      line = 'END RCHRES'
      call ryt(line,uci)

      line = '          '
      call ryt(line,uci)

      return

************* ERROR SPACE    *******************************************

991   report(1) = 'need more coding for different table formats'
      report(2) = ' file ./pp/rug/src/rtables.f'
      report(3) = ' format: '
      write(report(3)(10:11),'(i2)') tabformat(nm,nt,1)
      write(report(3)(13:14),'(i2)') tabformat(nm,nt,2)
      write(report(3)(16:17),'(i2)') tabformat(nm,nt,3)
      go to 999

993   report(1) = 'need more coding for unique table formats'
      report(2) = ' file ./pp/rug/src/rtables.f'
      report(3) = ' need more types of reals'
      go to 999

999   call stopreport(report)

      end
************************************************************************
** writes the GEN-INFO table                                          **
************************************************************************
      subroutine hydrparm1(rseg,Nexits,lakeflag)
      implicit none
      include 'rug.inc'

      line = '  HYDR-PARM1'
      call ryt(line,uci)

      line = '    RCHRES  Flags for HYDR section                    ***'
      call ryt(line,uci)
      line = '    # -  #  VC A1 A2 A3  ODFVFG for each     ODGTFG for '
     .     //'each *** FUNCT  for each'
      call ryt(line,uci)
      line = '            FG FG FG FG  possible   exit     possible   '
     .     //'exit *** possible   exit'
      call ryt(line,uci)
      line = '                           1  2  3  4  5       1  2  3  '
     .     //'4  5 ***   1  2  3  4  5'
      call ryt(line,uci)
      line = '            VC A1 A2 A3   V1 V2 V3 V4 V5      G1 G2 G3 G'
     .     //'4 G5 ***  F1 F2 F3 F4 F5'
      call ryt(line,uci)
      line = '    1        0  1  1  1    0  0  4  0  0       1  2  0  '
     .     //'0  0       0  0  0  0  0'
      if (Nexits.eq.4) line(57:57) = '3'
      call ryt(line,uci)

      line = '  END HYDR-PARM1'
      call ryt(line,uci)

      line = ' '
      call ryt(line,uci)

      end

************************************************************************
** writes the GEN-INFO table                                          **
************************************************************************
      subroutine geninfo(rseg,Nexits,lakeflag)
      implicit none
      include 'rug.inc'

      line = '  GEN-INFO'
      call ryt(line,uci)

      line = '    RCHRES<-------Name------->Nexit   Unit Systems   '
     .       //'Printer      ***'
      call ryt(line,uci)
      line = '    # -  #                          User t-series  Engl '
     .       //'Metr LKFG ***'
      call ryt(line,uci)

      line = '    1     FOREST                  X    1    1    1   26 '
     .       //'   0    L'
      line(11:30) = ' '//rseg
      write(line(35:35),'(i1)') Nexits
      write(line(65:65),'(i1)') lakeflag
      call ryt(line,uci)

      line = '  END GEN-INFO'
      call ryt(line,uci)

      line = ' '
      call ryt(line,uci)

      end

************************************************************************
** writes the ACTIVITY and PRINT-INFO tables according to modules     **
************************************************************************
      subroutine activity(modules,nmod)
      implicit none

      include 'rugtables.inc'
      integer nm

      line = '  ACTIVITY'
      call ryt(line,uci)

      line = '    # -  # HYFG ADFG CNFG HTFG SDFG GQFG OXFG NUFG PKFG '
     .       //'PHFG ***'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0    0    0    0 '
     .       //'   0'
      do nm = 1,nmod
        if (modules(nm).eq.'HYDR')   line(15:15) = '1'
        if (modules(nm).eq.'ADCALC') line(20:20) = '1'
        if (modules(nm).eq.'CONS')   line(25:25) = '1'
        if (modules(nm).eq.'HTRCH')  line(30:30) = '1'
        if (modules(nm).eq.'SEDTRN') line(35:35) = '1'
        if (modules(nm).eq.'GQUAL')  line(40:40) = '1'
        if (modules(nm).eq.'OXRX')   line(45:45) = '1'
        if (modules(nm).eq.'NUTRX')  line(50:50) = '1'
        if (modules(nm).eq.'PLANK')  line(55:55) = '1'
        if (modules(nm).eq.'PHCARB') line(60:60) = '1'
      end do
      call ryt(line,uci)

      line = '  END ACTIVITY'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      line = '  PRINT-INFO'
      call ryt(line,uci)

      line = '    # -  # HYFG ADFG CNFG HTFG SDFG GQFG OXFG NUFG PKFG '
     .       //'PHFG PIVL***PY'
      call ryt(line,uci)

      line = '    1         0    0    0    0    0    0    0    0    0 '
     .       //'   0    0   12'
      do nm = 1,nmod
        if (modules(nm).eq.'HYDR')   line(15:15) = '5'
        if (modules(nm).eq.'ADCALC') line(20:20) = '5'
        if (modules(nm).eq.'CONS')   line(25:25) = '5'
        if (modules(nm).eq.'HTRCH')  line(30:30) = '5'
        if (modules(nm).eq.'SEDTRN') line(35:35) = '5'
        if (modules(nm).eq.'GQUAL')  line(40:40) = '5'
        if (modules(nm).eq.'OXRX')   line(45:45) = '5'
        if (modules(nm).eq.'NUTRX')  line(50:50) = '5'
        if (modules(nm).eq.'PLANK')  line(55:55) = '5'
        if (modules(nm).eq.'PHCARB') line(60:60) = '5'
      end do
      call ryt(line,uci)

      line = '  END PRINT-INFO'
      call ryt(line,uci)

      line = '  '
      call ryt(line,uci)

      end

************************************************************************
** gets the parameter values from the databases                       **
**  loop over modules opening each file and getting the variables     **
************************************************************************
      subroutine getPar(
     I                  rseg,modules,nmod,
     I                  table,nTabs,Var,tabformat,
     I                  paramscen,
     O                  iPar,fPar)
      implicit none
      include 'rugtables.inc'
      character*2000 parline,tableline,varline  

      character*15 Ttab,Tvar           ! temp reading variables
      character*13 Trseg               ! temp reading varible

      integer coltab(nTabmax*nVarMax)  ! table number for each column
                  !   like = >  1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 4
      integer colvar(nTabmax*nVarMax)  ! variable number for each column
                  !   like = >  1, 2, 3, 1, 2, 3, 4, 1, 2, 1, 2, 3

      logical foundT,foundV,foundS  ! have I found this info 
      logical scompcase
      integer n,nm,nt,nh,nv,ic                ! indices

      integer clmlength      ! number of characters in column
      integer ncols          ! number of columns in this file

      integer lenmod,lentab,lenvar,tablast,parlast,varlast              ! string lengths

**************** END DECLARATIONS **************************************

      call lencl(paramscen,lenparamscen)

      do nm = 1,nmod                !  loop over all active modules

        foundT = .false.                   ! initializations
        foundV = .false.
        foundS = .false.
        do nt = 1,nTabmax*nVarMax
          coltab(nt) = -9
          colvar(nt) = -9
        end do

        call lencl(modules(nm),lenmod)
        
C        call ttyput(modules(nm))
        fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .         '/'//modules(nm)(:lenmod)//'.csv'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a100)')line
        call d2x(line,last)
        do while (line(:3).ne.'end')               ! search for the 3 lines
          call findcomma(line,clmlength)
          Trseg = line(:clmlength-1)
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
          else if (scompcase(Trseg,rseg)) then
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

        call popcoltab(fnam,tableline(:tablast),
     .                 coltab,nTabs,table,ncols,nm)

        call popcolvar(varline(:varlast),coltab,colvar,tabformat,
     .                 Var,ncols,nm,nTabs(nm),fnam)

        call poppar(parline,iPar,fPar,coltab,colvar,tabformat,nm,ncols)

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
      report(2) = ' could not find segment '//rseg
      report(3) = ' Segment name should have no leading or '
     .            //'trailing blanks'
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  subroutine to populate coltab which is an index giving the table  **
**    number for each column                                          **
**  In a separate subroutine because the speed of the entire LUG is   **
**   dependent on the length of the variable colline.                 **
************************************************************************
      subroutine popcoltab(fnam,colline,
     .                     coltab,nTabs,table,ncols,nm)
      implicit none
      include 'rugtables.inc'
      character*(*) colline
      character*15 Ttab
      integer coltab(nTabmax*nVarMax) 
      integer ncols,ic,clmlength,nm,nt    ! indices and utilities
      logical scompare,found

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
        found = .false.
        do nt = 1,nTabs(nm)
          if (scompare(table(nm,nt),Ttab)) then
            coltab(ic) = nt
            found = .true.
            exit
          end if
        end do
        if (.not.found) go to 992
      end do

      ncols = ic        ! store number of columns

      return
992   report(1) = 'unexpected field found in file: '
      report(2) = fnam
      write(report(3),*) 'field number ',ic,' field name ',Ttab
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  subroutine to populate colvar which is an index giving the        **
**    variable number for each column.                                **
**  In a separate subroutine because the speed of the entire LUG is   **
**   dependent on the length of the variable colline.                 **
************************************************************************
      subroutine popcolvar(colline,coltab,colvar,tabformat,Var,ncols,nm,
     .                     numtables,fnam)
      implicit none
      include 'rugtables.inc'
      character*(*) colline
      character*15 Tvar
      integer coltab(nTabmax*nVarMax),colvar(nTabmax*nVarMax) 
      integer ncols,ic,clmlength,nm,nt,nv,tablast    ! indices and utilities
      integer numtables    ! number of tables for this module
      logical scompare
      logical found(nTabMax,nVarMax)  ! have I found this info

      do nt = 1,numtables
        do nv = 1,tabformat(nm,nt,1)
          found(nt,nv) = .false.
        end do
      end do
          
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

      do nt = 1, numtables
        do nv = 1,tabformat(nm,nt,1)
          if (.not.found(nt,nv)) go to 996
        end do
      end do

      return

******************* ERROR SPACE ********************************

995   report(1) = 'IN '//fnam
      report(2) = 'number of columns in table line: '
      write(report(2)(34:36),'(i3)') ncols
      report(3) = 'does not match variable line: '
      write(report(3)(31:33),'(i3)') ic
      go to 999

996   report(1) = 'IN '//fnam
      report(2) = ' could not find table and variable: '
      report(3) = table(nm,nt)//'  '//Var(nm,nt,nv)
      go to 999

999   call stopreport(report)

      end

************************************************************************
** populates the parameter variables according to the order of the    **
**   table line and parameter line.                                   **
************************************************************************
      subroutine poppar(parline,iPar,fPar,coltab,colvar,tabformat,nm,
     .                  ncols)
      implicit none
      include 'rugtables.inc'
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
            if (tabformat(nm,coltab(ic),2).eq.0) then   ! integer
              call readi(parline,iPar(nm,coltab(ic),colvar(ic)))
            else if (tabformat(nm,coltab(ic),2).eq.1) then  ! real
              call fread(parline,fPar(nm,coltab(ic),colvar(ic)))
            else  !Unique format   - use both variables
              call fread(parline,fpar(nm,coltab(ic),colvar(ic)))
              ipar(nm,coltab(ic),colvar(ic)) = 
     .             int(fpar(nm,coltab(ic),colvar(ic)))
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
      include 'rugtables.inc'
      logical found,comment,perlnd,implnd
      integer n,nm,nt,nh,nv                   ! indices
      integer lenmod,lentab,lenvar            ! string lengths
      character*6 tempmodname
      character*1 IorF

      fnam = catdir//'modules/rivtables'
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
              table(nm,nt) = line(3:17)               ! get table name

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
                  do nv = 1,tabformat(nm,nt,1)      ! skips unique format
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

            nTabs(nm) = nt

          else                          ! just read to next mod
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
      if (line(21:21).eq.' ') then
        report(2)= ' Format for following table must be i,f, or u, not '
     .              //'blank'
      else
        report(2)= ' Format for following table must be i,f, or u, not '
     .              //line(21:21)
      end if
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
      include 'rugtables.inc'
      character*(*) module
      character*(*) tabname

      character*25 endmod,temptab

      integer iofile,nv
      logical found
      logical scompare

      character*1 IorF

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
            if (scompare(line(:last),tabname)) then
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

992   report(1) = 'Could not find table '//tabname//' for module '//
     .             module//' in file:'
      report(2) = fnam
      report(3) = 'edit file for unique table format'
      go to 999

993   report(1) = 'Could not find parameters for table '//tabname//
     .            ' for module '//module
      report(2) = fnam
      report(3) = 'line after table name is number of variables (i4)'
     .            //' followed by an i or f (1a)'
      go to 999

994   report(1) = 'Can not read column numbers for table '//tabname
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



************************************************************************
**  reads the control file and determines the active modules for this **
**   land use and scenario                                            **
************************************************************************
      subroutine readcontrol_pscen(rscen,lenrscen,
     O                             paramscen)

      implicit none
      include 'rugtables.inc'
      logical found,comment,foundparam
      character*7 tabname

      integer i           ! index

      fnam = controldir//'/river/'//rscen(:lenrscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundparam = .false.

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')

        if (.not.comment(line).and.line(:10).eq.'PARAMETERS') then
          foundparam = .true.
          read(dfile,'(a100)',err=992)line
          call d2x(line,last)
          paramscen = line(:last)
          read(dfile,'(a100)',err=992)line
          call d2x(line,last)
          if (line(:14).ne.'END PARAMETERS') go to 993
        end if

        read(dfile,'(a100)',err=992)line
      end do

      close (dfile)

      if (.not.foundparam) go to 994

      return
            
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'table: PARAMETERS   only allowed one line in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = 'table: PARAMETERS   not found in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

************************************************************************
** subroutine to modify river parameters according to ratios          **
**  specified in a file in the parameters section                     **
** The ratios are usually the ratio of input loads in a scenario to   **
**   input loads in the calibration.  See the code in                 **
**   ./pp/src/postproc/scenario_compare                               **
** The relationships are defined in the file                          **
**   ./pp/catalog/iovars/scenario_parameter_modifications             **
************************************************************************
      subroutine scenario_parmod(
     I                           rseg,rscen,paramscen,
     I                           modules,nmod,
     I                           table,nTabs,Var,tabformat,
     M                           iPar,fPar)
      implicit none
      include 'rugtables.inc'

************* load variables used for parameter modifications
      integer nloadmax,nloads,nload,nl
      parameter (nloadmax = 10)
      character*4 loadname(nloadmax)

************ number of parameters associated with each load type
      integer maxpars,np,npars(nloadmax)
      parameter (maxpars = 10)

************ indices of module, table, and variable for each
*********** variable associated with a load
      integer parmod(nloadmax,maxpars)
      integer partab(nloadmax,maxpars)
      integer parvar(nloadmax,maxpars)

      character*6 Tmod     ! temporary reading variable
      character*15 Ttable
      character*10 Tvar
      character*4 Tload

********** useful variables
      integer nm,nt,nv,ifl

      logical moduleActive, foundload, foundparam ! error checking
      logical foundld(nloadmax)

      logical comment,scompcase
      external comment,scompcase

*********** reading variables
      character*1 dummy
      character*25 Tscen
      real st,sstr,sp,ss,sa
      real ct,cstr,cp,cs,ca

*********** parameter change factor = scenario load / calib load
      real factor(nloadmax)
 
************* determine which parameters are modified by searching in 
********* scenario_parameter_modifications

******** initialize
      do nl = 1,nloadmax
        npars(nl) = 0
      end do

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

********* open file
      call findopen(ifl)
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/scenario_parameter_modifications'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

******** loop over lines, populating modload
      nloads = 0
      do
        read(ifl,'(a100)',end=990,err=989) line
        if (comment(line)) cycle
        if (line(:3).eq.'end') exit
        if (line(:3).eq.'END') exit

        read(line(:6),*) Tmod   ! check for active module
        moduleActive = .false.
        do nm = 1,nmod
          if (modules(nm).eq.Tmod) then
            moduleActive = .true.
            exit
          end if
        end do
        if (.not.moduleActive) cycle

*********** module is active, get load name and number
        read(line(39:42),'(a4)',iostat=err) Tload
        if (err.ne.0) cycle
        if (Tload.eq.'    ') cycle
        foundload = .false.
        do nl = 1,nloads
          if (Tload.eq.loadname(nl)) then
            foundload = .true.
            nload = nl
            exit
          end if
        end do
        if (.not.foundload) then
          nloads = nloads + 1
          if (nloads.gt.nloadmax) go to 993
          loadname(nloads) = Tload
          nload = nloads
        end if

*********** read table and variable
        read(line(9:37),'(a14,1x,a10)') Ttable,Tvar

************** check to make sure that variables are active
************ and store the indices
        foundparam = .false.
        do nm = 1,nmod
          if (scompcase(modules(nm),Tmod)) then
            do nt = 1,nTabs(nm)
              if (scompcase(table(nm,nt),Ttable)) then
                do nv = 1,tabformat(nm,nt,1)
                  if (scompcase(Var(nm,nt,nv),Tvar)) then
                    foundparam = .true.
                    npars(nload) = npars(nload) + 1
                    if (npars(nload).gt.maxpars) go to 994
                    parmod(nload,npars(nload)) = nm
                    partab(nload,npars(nload)) = nt
                    parvar(nload,npars(nload)) = nv
                  end if
                end do
              end if
            end do
          end if
        end do
        if (.not.foundparam) go to 995

      end do
      close (ifl)

************* Get the factor for each load type
      fnam = outdir//'river/scenario_compare/'//
     .       rscen(:lenrscen)//'/'//rseg(:lenrseg)//'.csv'
      open(ifl,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nl = 1,nloads
        foundld(nl)= .false.
      end do

      read(ifl,*,err=996,end=996) line    ! get rid of header line
      do 
        read(ifl,*,err=996,end=111) Tload,Tscen,st,sstr,sp,ss,sa,
     .                                    Tscen,ct,cstr,cp,cs,ca
        if (ct.lt.0.0001) go to 997  ! check for div0 error
        do nl = 1,nloads
          if (Tload .eq. loadname(nl)) then
            foundld(nl) = .true.
            factor(nl) = st/ct
            exit
          end if
        end do
      end do

      do nl = 1,nloads
        if (.not.foundld(nl)) go to 998
      end do

111   close(ifl)

************ modify the parameters
      do nl = 1,nloads
        do np = 1,npars(nl)
          nm = parmod(nl,np)
          nt = partab(nl,np)
          nv = parvar(nl,np)
          if (tabformat(nm,nt,2).eq.0) then
            iPar(nm,nt,nv) = iPar(nm,nt,nv) * factor(nl)
          else
            fPar(nm,nt,nv) = fPar(nm,nt,nv) * factor(nl)
          end if
        end do
      end do

      return

***************** ERROR SPACE ******************************************
989   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

990   report(1) = 'Problem reading file'
      report(2) = fnam
      report(3) = 'file ended before literal '//
     .            char(39)//'end'//char(39)//' was found'
      go to 999

991   report(1) = 'Problem opening '
      report(2) = fnam
      write(report(3),*)' error code = ',err
      go to 999

992   report(1) = 'Problem in file:'
      report(2) = fnam
      report(3) = 'on line for '//loadname(nloads)
      go to 999

993   report(1) = 'too many different load types in file'
      report(2) = fnam
      report(3) = 'reduce numbers of loads or modify scenario_rug'
      go to 999

994   report(1) = 'too many different parameter types in file'
      report(2) = fnam
      report(3) = 'reduce parameters or modify scenario_rug'
      go to 999

995   report(1) = 'problem in file'
      report(2) = fnam
      report(3) = 'parameter not found '//Tmod//' '//Ttable//' '//Tvar
      go to 999

996   report(1) = 'error reading file: near line:'
      report(2) = fnam
      report(3) = Tseg
      go to 999

997   report(1) = 'calibration input loads very low in file'
      report(2) = fnam
      report(3) = 'check for program error'
      go to 999

998   report(1) = 'load not found in scenario compare file'
      report(2) = fnam
      report(3) = 'check for simulation error'
      go to 999

999   call stopreport(report)

      end

