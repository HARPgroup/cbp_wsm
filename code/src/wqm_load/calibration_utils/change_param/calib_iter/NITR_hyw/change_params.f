*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
       subroutine change_params(
     I                          nlsegs,lsegs,lscen,paramscen,module,clu,
     I                          version,tarscen,targets,simEOF,
     I                          limitKIMNI,limitKAM,limitKDNI,limitKNI,
     I                          limitKIMAM,limitKLON,limitKRON)

      implicit none
      include 'calib_nitr.inc'

      integer ns,nl
      integer vcKIMNI(4),vcKAM(4),vcKNI(4),vcKDNI(4)      ! each variable has FOUR layers
      integer vcKIMAM(4),vcKLON(4),vcKRON(4)
      integer vcSKVOL,vcUKVOL

      real SNH4tar(maxlsegs),BNH4tar(maxlsegs)
      real SNO3tar(maxlsegs),BNO3tar(maxlsegs)
      real SLONtar(maxlsegs),BLONtar(maxlsegs)
      real SRONtar(maxlsegs),BRONtar(maxlsegs)

      real SNH4(maxlsegs),BNH4(maxlsegs)
      real SNO3(maxlsegs),BNO3(maxlsegs)  
      real SLON(maxlsegs),BLON(maxlsegs)
      real SRON(maxlsegs),BRON(maxlsegs)

      real taruptk(maxlsegs),simuptk

      integer i,nly       ! indices
      integer iline
      character*4 version ! version number

*************** END DECLARATIONS ***************************************

      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(paramscen,lenparamscen)

************* LOOP OVER TARGET, FIND THE FILE AND MAKE THE CHANGES
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .       module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********          READ HEADER LINES
      read(dfile,'(a2000)',err=992) tableline
      call d2x(tableline,last)
      tkeep = tableline

      read(dfile,'(a2000)',err=992) varline
      call d2x(varline,last)
      vkeep = varline

**********  READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=993,end=994) parline(nlines)
       
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=993,end=994) parline(nlines)
      end do
      close(dfile)
                  
**********          WRITE FILE TO NEW FILE NAME
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .       module(:lenmod)//'_'//version//'.csv'
      open(dfile,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)
      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do
      close(dfile)

**********         FIND THE COLUME OF EACH PARAMETER
      call getcolumns1(
     I                 tkeep,vkeep,module,clu,
     O                 vcKIMNI,vcKAM,vcKDNI,vcKNI,vcKIMAM,vcKLON,vcKRON,
     O                 vcSKVOL,vcUKVOL)

******* READ IN UPTAKE TARGETS
      call gettaruptk(
     I                tarscen,clu,lsegs,nlsegs,
     O                taruptk)

***************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
      do ns = 1,nlsegs  
       if (mod(ns,5) .eq. 0) 
     .     print*,'modifying ',ns,' out of ',nlsegs,' segments'
    
       do i = 1,nlines
         read(parline(i),*,err=995,end=995) tseg
         if (tseg .eq. lsegs(ns)) then
           iline = i
           exit
         end if
       end do
       print*, tseg       
************ read .out files to get SIMULATED UPTAKE
       call read_uptake(
     I                  clu,lsegs(ns),lscen,
     O                  simuptk)

***************** apply factors to all land uses
       do nly = 1, 4
         call getvar(parline(iline),vcKIMNI(nly),KIMNI(nly))
         call getvar(parline(iline),vcKAM(nly),KAM(nly))   ! get the param value 
         call getvar(parline(iline),vcKDNI(nly),KDNI(nly)) ! for each layer
         call getvar(parline(iline),vcKNI(nly),KNI(nly))
         call getvar(parline(iline),vcKIMAM(nly),KIMAM(nly))
         call getvar(parline(iline),vcKLON(nly),KLON(nly))
         call getvar(parline(iline),vcKRON(nly),KRON(nly))
       end do
     
************ re-arrange target and simulated loads to make codes more readable
       SNH4tar(ns) = targets(1,ns)
       BNH4tar(ns) = targets(2,ns)
       SNO3tar(ns) = targets(3,ns)
       BNO3tar(ns) = targets(4,ns)
       SLONtar(ns) = targets(5,ns)
       BLONtar(ns) = targets(6,ns)
       SRONtar(ns) = targets(7,ns)
       BRONtar(ns) = targets(8,ns)

       SNH4(ns) = simEOF(1,ns)+simEOF(2,ns)+simEOF(3,ns)
       BNH4(ns) = simEOF(4,ns)
       SNO3(ns) = simEOF(5,ns)+simEOF(6,ns)
       BNO3(ns) = simEOF(7,ns)
       SLON(ns) = simEOF(8,ns)+simEOF(9,ns)+simEOF(10,ns)
       BLON(ns) = simEOF(11,ns)
       SRON(ns) = simEOF(12,ns)+simEOF(13,ns)+simEOF(14,ns)
       BRON(ns) = simEOF(15,ns)

********** adjust Params for surface
       do nly = 1, 2
  
         if ((SNH4(ns)+SNO3(ns)) .lt. 0.000001) then
            KAM(nly) = KAM(nly)*2.0
         else
           KAM(nly) = KAM(nly)*(SNH4tar(ns) + SNO3tar(ns))
     .                        /(SNH4(ns) + SNO3(ns))            !Update based on inorganic N              
         end if

         if (SNO3(ns) .lt. 0.000001) then
            KNI(nly) = KNI(nly)*2.
         else
            KNI(nly) = KNI(nly)*(SNH4(ns)/SNO3(ns))
     .                         /(SNH4tar(ns)/SNO3tar(ns))
         end if
       end do

       if (SLON(ns). lt. 0.000001) then
         KLON(1)=KLON(1)*2.0
       else
         KLON(1) = KLON(1)*SLONtar(ns)/SLON(ns)   ! Surface layer
       end if
       KLON(2) = KLON(2)*SLON(ns)/SLONtar(ns)   ! Up-surface layer

       if (SRON(ns). le. 0.000001) then
         KRON(1)=KRON(1)*2.
       else
         KRON(1) = KRON(1)*SRONtar(ns)/SRON(ns)
       end if
       KRON(2) = KRON(2)*SRON(ns)/SRONtar(ns)

********* adjust Params for GW targets
       do nly = 3, 4
         if ((BNH4(ns)+BNO3(ns)) .lt. 0.000001) then
           KAM(nly) = KAM(nly)*2.0
         else
           KAM(nly) = KAM(nly)*(BNH4tar(ns)+BNO3tar(ns))
     .                             /(BNH4(ns)+BNO3(ns))            !Update KAM based on inorganic N
         end if
         KDNI(nly) = KDNI(nly)*(BNO3(ns)/BNO3tar(ns))
         KNI(nly ) = KNI(nly) *(BNH4(ns)/BNH4tar(ns))
         KLON(nly) = KLON(nly)*BLON(ns)/BLONtar(ns)
         KRON(nly) = KRON(nly)*BRON(ns)/BRONtar(ns)
       end do 

********* adjust KIMNI and KIMAM based on uptake
       if (taruptk(ns) .gt. 0.01) then    ! adjust only segments woth Nonzero uptake
         do nly = 1, 3
           KIMAM(nly)=KIMAM(nly)*(simuptk/taruptk(ns))
           KIMNI(nly)=KIMAM(nly)
         end do
       end if

********* adjust NH3 volatilization
       if (abs((SNH4(ns)-SNH4tar(ns))/SNH4tar(ns)) .gt. 0.5) then
         if (abs((SNO3(ns)-SNO3tar(ns))/SNO3tar(ns)) .gt. 0.5) then
           call AMVolat(parline(iline),vcSKVOL,clu,SNH4(ns),SNH4tar(ns))  ! Update params for NH4 Volat, only on pasture
           call AMVolat(parline(iline),vcUKVOL,clu,SNH4(ns),SNH4tar(ns))
         end if
       end if

******** Enfore Max and Min values for parameters
       call minmax (
     M              KIMNI,KAM,KDNI,KNI,KIMAM,KLON,KRON,
     I              limitKIMNI,limitKAM,limitKDNI,limitKNI,limitKIMAM,
     I              limitKLON,limitKRON)
    
*************** put variables in files in memory
       do nly = 1, 4
         call putvar(parline(iline),vcKIMNI(nly),KIMNI(nly))
         call putvar(parline(iline),vcKAM(nly),KAM(nly))   ! store the param value
         call putvar(parline(iline),vcKDNI(nly),KDNI(nly)) ! for each layer
         call putvar(parline(iline),vcKNI(nly),KNI(nly))
         call putvar(parline(iline),vcKIMAM(nly),KIMAM(nly))
         call putvar(parline(iline),vcKLON(nly),KLON(nly))
         call putvar(parline(iline),vcKRON(nly),KRON(nly))
       end do

      end do       ! end loop for land segments

**********          REWRITE MODS TO ORIGINAL FILE NAME
      fnam= pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .      module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      call rytdos(vkeep,dfile)

      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do

      close(dfile)
      
      return 
********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'error occured in first two lines'
      go to 999

993   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = parline(nlines-1)(:100)
      go to 999

994   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

995   report(1) = 'did not find '//lsegs(ns)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end
