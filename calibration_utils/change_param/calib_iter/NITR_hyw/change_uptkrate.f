*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      subroutine change_uptkrate(module,nlsegs,lsegs,version,
     .                           lscen,paramscen,clu,tarscen)

      implicit none
      include 'calib_nitr.inc'

      integer vcNITUPT1(12),vcNITUPT2(12),vcNITUPT3(12)
      real NITUPT1(12),NITUPT2(12),NITUPT3(12)

      integer i,nm,ns       ! indices
      integer iline
      
      character*4 version ! version number

      real taruptk(maxlsegs),Simuptk

*************** END DECLARATIONS ***************************************
      call uppercase(module)
      call lencl(module,lenmod)

************* LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
      
      call lencl(paramscen,lenparamscen)
  
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'
     .         //module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********          READ HEADER LINES
      read(dfile,'(a2000)',err=994) tableline
      call d2x(tableline,last)
      tkeep = tableline

      read(dfile,'(a2000)',err=994) varline
      call d2x(varline,last)
      vkeep = varline

**********  READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=996,end=997) parline(nlines)
       
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=996,end=997) parline(nlines)
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
      
******* READ IN UPTAKE TARGETS 
      call gettaruptk(
     I                tarscen,clu,lsegs,nlsegs,
     O                taruptk)
      
*********** FIND THE COLUME OF EACH STORAGE
      call getcolumns3(
     I                  tkeep,vkeep,module,clu,
     O                  vcNITUPT1,vcNITUPT2,vcNITUPT3)

************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
      do ns = 1,nlsegs
       if (mod(ns,5) .eq. 0)
     .     print*,'modifying ',ns,' out of ',nlsegs,' segments'

       do i = 1,nlines
         read(parline(i),*,err=992,end=992) tseg
         if (tseg .eq. lsegs(ns)) then
           iline = i
           exit
         end if
       end do
        
**************** find values for all parameters
      do nm = 1, 12
        call getvar(parline(iline),vcNITUPT1(nm),NITUPT1(nm))    
        call getvar(parline(iline),vcNITUPT2(nm),NITUPT2(nm))
        call getvar(parline(iline),vcNITUPT3(nm),NITUPT3(nm))
      end do
      
********** adjust first-order uptake rate
************ read .out files to get the EOF and target
       call read_uptake(
     I                  clu,lsegs(ns),lscen,
     O                  Simuptk)
    
*********** Update uptake rate
      do nm = 1, 12
        if (taruptk(ns) .ne. 0.0) then       !don't need to do zero acreage land
          NITUPT1(nm)=NITUPT1(nm)*(taruptk(ns)/Simuptk)
          NITUPT2(nm)=NITUPT2(nm)*(taruptk(ns)/Simuptk)
          NITUPT3(nm)=NITUPT3(nm)*(taruptk(ns)/Simuptk)
          if (NITUPT1(nm) .gt. 3.) NITUPT1(nm) = 3.
          if (NITUPT2(nm) .gt. 3.) NITUPT2(nm) = 3.
          if (NITUPT3(nm) .gt. 0.5) NITUPT3(nm) = 0.5 
        end if
      end do 

*************** put adjusted storages in files in memory
      do nm = 1, 12
        call putvar(parline(iline),vcNITUPT1(nm),NITUPT1(nm))
        call putvar(parline(iline),vcNITUPT2(nm),NITUPT2(nm))
        call putvar(parline(iline),vcNITUPT3(nm),NITUPT3(nm))
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

992   report(1) = 'did not find '//lsegs(ns)
      report(2) = 'in file'
      report(3) = fnam
      go to 999

9921  report(1) = 'extra segment '//tseg
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

997   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = 'file ended before literal '//char(39)//'end'//
     .             char(39)//' found'
      go to 999

998   report(1) = 'problem reading file '
      report(2) = fnam
      report(3) = ' '
      if (err.eq.993) report(3) = 'file ended before literal '//char(39)
     .                             //'end'//char(39)//' found'
      if (err.eq.995) report(3) = 'problem reading land seg'

      go to 999

999   call stopreport(report)

      end
