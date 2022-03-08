************************************************************************
** program to change parameters for sedmnet calibration. Automatically**
** read csv files and performs functions on the variables             **
************************************************************************
      implicit none
      include 'imp_sed.inc'

      integer ns
      character*12 module,table,variable  ! variable specifiers
      integer lenmod

      character*50 basin

      integer nlinemax, nlines  ! number of parameter lines in code
      parameter (nlinemax=400)
      character*2000 parline(nlinemax),
     .                       varline,tableline,
     .                       vkeep,tkeep

      integer varcolumn
      integer  vcKEIM  ! column that the variable of interest occupies

      integer i,j,nl       ! indices
      integer iline
      character*4 version ! version number
      character*25 tarscen

      logical found
      logical fileexist  ! if file already exists

      character*3 clu

      logical done(maxlsegs)
      character*6 keepsegs(maxlsegs)
      integer nkeep
*************** END DECLARATIONS ***************************************

************ SPECIFICATION ENTRY SECTION
      read*,calscen,lscen,basin,clu

      call lencl(lscen,lenlscen)
      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      module = 'SOLIDS'
      call uppercase(module)
      call lencl(module,lenmod)

************ GET LAND SEGMENTS
      call readLandSeglist(basin,
     O                     lsegs,nlsegs)
      if (nlsegs.eq.0) return  ! all done previously
   
************ READ EOF TARGETS FOR ALL LAND USE AND ALL SEGLEMTS
      call gettarscen(calscen,
     O                tarscen)
      call gettargets(
     I                lsegs,nlsegs,tarscen,clu,
     O                targets)
      
************ FIND FIRST FREE FILE NAME FOR THIS LAND USE
      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do

        fnam =pardir//clu//'/'//paramscen(:lenparamscen)
     .            //'/'//module(:lenmod)//'_'//version//'.csv'
        inquire (file=fnam,exist=fileexist)
        if (.not.fileexist) exit
      end do

************ READ IN SIMULATED EOF AND ADJUST KRER
      call getKEIMfac(lscen,version,lsegs,nlsegs,targets,clu,
     O                facKEIM)

      print*,'all accounting'

************* LOOP OVER LAND USES, FIND THE FILE AND MAKE THE CHANGES
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .          '/'//module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********          READ HEADER LINES
      read(dfile,'(a2000)',err=994) tableline
      call d2x(tableline,last)
      tkeep = tableline
      read(dfile,'(a2000)',err=994) varline
      call d2x(varline,last)
      vkeep = varline

**********          READ WHOLE FILE INTO MEMORY
      nlines = 1
      do
        read(dfile,'(a2000)',err=996,end=111) parline(nlines)
        nlines = nlines + 1
        if (parline(nlines-1)(:3).eq.'end') exit
      end do
111   close(dfile)
      nlines = nlines - 1

**********          WRITE FILE TO NEW FILE NAME
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .         '/'//module(:lenmod)//'_'//version//'.csv'
      open(dfile,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991
      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do
      close(dfile)
  
**************GET THE COLUMN NUMBER OF PARAMETER KEIM
      table = 'SLD-PARM2'
      variable = 'KEIM'
      varline = vkeep
      tableline = tkeep
      call findcolumn(tableline,varline,table,variable,
     O                vcKEIM,err)
      if (err.ne.0) go to 992
      
***************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
      do ns = 1,nlsegs  

        done(ns) = .false.
        if (abs(facKEIM(ns)-1.0).lt.0.005) then
          done(ns) = .true.
          cycle
        end if

        if (mod(ns,20) .eq. 0) 
     .          print*,'modifying ',ns,' out of ',nlsegs,' segments'
    
********** GET THE LINE NUMBER
        found = .false.
        do i = 1,nlines
          read(parline(i),*,err=996,end=996) tseg
          if (tseg .eq. lsegs(ns)) then
            found = .true.
            iline = i
            exit
          end if
        end do
        if (.not.found) go to 992

**************** get the KEIM value from previous run
        call getvar(parline(iline),vcKEIM,KEIM)  

***************** apply factors if not yet calibrated
        KEIM  = KEIM * facKEIM(ns) 

*************** put variables in files in memory
        call putvar(parline(iline),vcKEIM,KEIM)
      end do       ! end loop for land segments

**********          REWRITE MODS TO ORIGINAL FILE NAME
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .        '/'//module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)

      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do

      close(dfile)

************ REWRITE BASIN FILE WITH NON-CALIBRATED SEGS
      nkeep = 0
      do ns = 1,nlsegs
        if (.not.done(ns)) then
          nkeep = nkeep + 1
          keepsegs(nkeep) = lsegs(ns)
        end if
      end do

      call writeLandSeglist(basin,
     O                      keepsegs,nkeep)
   
      stop

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'did not find '//lsegs(ns)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

996   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines-1)(:100)
      go to 999

999   call stopreport(report)

      end
