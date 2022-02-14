************************************************************************
** program to change parameters for iqual calibration.                **
**   reads compares outputs to targets and modifies the parameters    **
*************************************************************************
      implicit none
      include 'iqual.inc'

      integer ns
      character*12 module
      integer lenmod

      character*25 targscen

      integer nlinemax, nlines  ! # of lines in parameter files
      parameter (nlinemax=400)
      character*2000 parline(nlinemax),
     .                       varline,tableline,
     .                       vkeep,tkeep
      integer i,j,nl,nc       ! indices
      integer iline
      character*4 version ! version number

      logical found
      logical fileexist  ! if file already exists

      logical calibrated ! true if segment is calibrated
      external calibrated
      character*6 uncalsegs(maxlsegs)  ! segments still not calibrated
      integer nuncalsegs

      character*100 basin  ! name of seglist file
      character*1 cnq  ! character number of quality consituent
      character*3 clu  ! character land use
   
*************** END DECLARATIONS ***************************************

************ read in specifications
      read*,calscen,lscen,basin,clu
      call lencl(lscen,lenlscen)

      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
     
      module = 'IQUAL'
      call uppercase(module)
      call lencl(module,lenmod)

************ GET LAND SEGMENTS 
      call readLandSeglist(
     I                     basin,
     O                     lsegs,nlsegs)
    
******* READ IN TARGETS
      call gettargscen(
     I                 calscen,
     O                 targscen)
      call gettarget(
     I               targscen,Tsnh4Name,lsegs,nlsegs,clu,
     O               Tsnh4)
      call gettarget(
     I               targscen,Tsno3Name,lsegs,nlsegs,clu,
     O               Tsno3)
      call gettarget(
     I               targscen,TslonName,lsegs,nlsegs,clu,
     O               Tslon)
      call gettarget(
     I               targscen,TsronName,lsegs,nlsegs,clu,
     O               Tsron)
      call gettarget(
     I               targscen,Tspo4Name,lsegs,nlsegs,clu,
     O               Tspo4)

************ FIND FIRST FREE FILE NAME FOR ALL LAND USES
      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
        fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .         module(:lenmod)//'1_'//version//'.csv'
        inquire (file=fnam,exist=fileexist)
        if (.not.fileexist) exit
      end do

************ READ IN SIMULATED EOF
      call getsimEOF(
     I               Xdnh3Name,clu,lscen,version,lsegs,nlsegs,
     O               Xdnh3)
      call getsimEOF(
     I               Xsnh3Name,clu,lscen,version,lsegs,nlsegs,
     O               Xsnh3)
      call getsimEOF(
     I               Xsno3Name,clu,lscen,version,lsegs,nlsegs,
     O               Xsno3)
      call getsimEOF(
     I               XdlonName,clu,lscen,version,lsegs,nlsegs,
     O               Xdlon)
      call getsimEOF(
     I               XslonName,clu,lscen,version,lsegs,nlsegs,
     O               Xslon)
      call getsimEOF(
     I               XdronName,clu,lscen,version,lsegs,nlsegs,
     O               Xdron)
      call getsimEOF(
     I               XsronName,clu,lscen,version,lsegs,nlsegs,
     O               Xsron)
      call getsimEOF(
     I               Xdpo4Name,clu,lscen,version,lsegs,nlsegs,
     O               Xdpo4)
      call getsimEOF(
     I               Xspo4Name,clu,lscen,version,lsegs,nlsegs,
     O               Xspo4)

      print*,'all accounting'

************** REWRITE THE BASIN FILE SO THAT YOU ONLY RUN
************ UNCALIBRATED SEGMENTS
      nuncalsegs = 0
      do ns = 1,nlsegs
        if (.not.calibrated(
     I                      Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                      Tspo4(ns),
     I                      Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                      Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                      Xspo4(ns))) then
          nuncalsegs = nuncalsegs + 1
          uncalsegs(nuncalsegs) = lsegs(ns)
        end if
      end do

C BHATT      if (nuncalsegs.eq.0) go to 989   !!! FINISHED !!!!!
      call writeLandSeglist(
     I                      basin,uncalsegs,nuncalsegs)
      if (nuncalsegs.eq.0) go to 989   !!! FINISHED !!!!!

************* LOOP OVER QUALITY CONSTITUENTS AND PARAM FILES
*************** COPY FILE AND MAKE CHANGES
      do nq = 1,nquals
        write(cnq,'(i1)') nq

        fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .       module(:lenmod)//cnq//'.csv'
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
        read(dfile,'(a2000)',err=996,end=997) parline(nlines)
        if (parline(nlines)(2000-5:2000).ne.'      ') go to 990
        do while (parline(nlines)(:3).ne.'end')
          nlines = nlines + 1
          read(dfile,'(a2000)',err=996,end=997) parline(nlines)
          if (parline(nlines)(2000-5:2000).ne.'      ') go to 990
        end do
        close(dfile)
        
**********          WRITE FILE TO NEW FILE NAME
        fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .         module(:lenmod)//cnq//'_'//version//'.csv'
        open(dfile,file=fnam,status='new',iostat=err)
        if (err.ne.0) go to 991

        call rytdos(tkeep,dfile)
        call rytdos(vkeep,dfile)
        do i = 1,nlines
          call rytdos(parline(i),dfile)
        end do
        close(dfile)
                    
**************GET THE COLUMN NUMBER OF PARAMETERS
        call getcolumns(
     I                  tkeep,vkeep,cnq,clu,
     O                  vcPOTFW,vcSQO,vcACQOP,
     O                  vcSQOLIM,vcWSQOP)
      
****************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
        do ns = 1,nlsegs  

          if (mod(ns,50) .eq. 0) 
     .            print*,'modifying ',ns,' out of ',nlsegs,' segments'
    
***************** GET LINE NUMBERS
          do i = 1,nlines
            found = .false.
            read(parline(i),*,err=992,end=992) tseg
            if (tseg .eq. lsegs(ns)) then
              iline = i
              found = .true.
              exit
            end if
          end do
          if (.not.found) go to 992

**************** GET OLD PARAMETERS
          call getvar(parline(iline),vcPOTFW,POTFW)
          call getvar(parline(iline),vcSQO,SQO)
          call getvar(parline(iline),vcACQOP,ACQOP)
          call getvar(parline(iline),vcSQOLIM,SQOLIM)
          call getvar(parline(iline),vcWSQOP,WSQOP)


**************** update parameters
          POTFW  = newPOTFW (
     I                       POTFW ,nq,
     I                       Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                       Tspo4(ns),
     I                       Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                       Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                       Xspo4(ns))
          SQO    = newSQO   (
     I                       SQO   ,nq,
     I                       Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                       Tspo4(ns),
     I                       Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                       Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                       Xspo4(ns))
          ACQOP  = newACQOP (
     I                       ACQOP ,nq,
     I                       Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                       Tspo4(ns),
     I                       Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                       Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                       Xspo4(ns))
          SQOLIM = newSQOLIM(
     I                       SQOLIM,nq,
     I                       Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                       Tspo4(ns),
     I                       Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                       Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                       Xspo4(ns))
          WSQOP  = newWSQOP (
     I                       WSQOP ,nq,
     I                       Tsnh4(ns),Tsno3(ns),Tslon(ns),Tsron(ns),
     I                       Tspo4(ns),
     I                       Xdnh3(ns),Xsnh3(ns),Xsno3(ns),Xdlon(ns),
     I                       Xslon(ns),Xdron(ns),Xsron(ns),Xdpo4(ns),
     I                       Xspo4(ns))

**************** PUT NEW PARAMETERS
          call putvar(parline(iline),vcPOTFW,POTFW)
          call putvar(parline(iline),vcSQO,SQO)
          call putvar(parline(iline),vcACQOP,ACQOP)
          call putvar(parline(iline),vcSQOLIM,SQOLIM)
          call putvar(parline(iline),vcWSQOP,WSQOP)

        end do      ! end loop over land segments

**********          REWRITE MODS TO ORIGINAL FILE NAME
        fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .         module(:lenmod)//cnq//'.csv'
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        call rytdos(tkeep,dfile)
        call rytdos(vkeep,dfile)
        do i = 1,nlines
          call rytdos(parline(i),dfile)
        end do
        close(dfile)

      end do       ! end loop over quality constituents


      stop

********************* ERROR SPACE **************************************
989   report(1) = 'all segments calibrated'
      report(2) = 'basin '//basin
      report(3) = 'land use '//clu
      call NoProblemReport(report)
      go to 999 ! BHATT

990   report(1) = 'problem reading parameter file'
      report(2) = ' for land use '//clu//' quality constituent '//cnq
      report(3) = 'file has line longer than 2000 characters'
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading parameter file'
      report(2) = ' for land use '//clu//' quality constituent '//cnq
      write(report(3),*) 'did not find segment ',lsegs(ns)
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
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

999   call stopreport(report)

      end
