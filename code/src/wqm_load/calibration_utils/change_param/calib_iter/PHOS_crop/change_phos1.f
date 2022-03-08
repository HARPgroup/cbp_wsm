*************************************************************************
** program to change parameters for PHOS calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      subroutine change_phos1(nlsegs,lsegs,module,clu,lscen,paramscen,
     .                        version,tarscen,targets,simEOF,
     .                        limitKIMP,limitKMP) 

      implicit none
      include 'calib_phos.inc'

      integer ns,nl
      integer i,j,nly       ! indices
      integer iline
      character*12 table,variable 
      
      real taruptk(maxlsegs),simuptk
 
*************** END DECLARATIONS ***************************************

      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(paramscen,lenparamscen)

      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .       module(:lenmod)//'.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********   READ HEADER LINES
      read(dfile,'(a2000)',err=992) tableline
      call d2x(tableline,last)
      tkeep = tableline

      read(dfile,'(a2000)',err=992) varline
      call d2x(varline,last)
      vkeep = varline

**********   READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=993,end=994) parline(nlines)

      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=993,end=994) parline(nlines)
      end do
      close(dfile)
                  
**********   WRITE FILE TO NEW FILE NAME
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
    
**********   READ IN UPTAKE TARGETS
      call gettaruptk(
     I                tarscen,clu,lsegs,nlsegs,
     O                taruptk) 
      
**********   FIND THE COLUMN OF PARAM
      call getcolumn1(
     I                tkeep,vkeep,module,clu,
     O                vcKIMP,vcKMP)
      
**********   LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
      do ns = 1,nlsegs  
 
        if (mod(ns,5) .eq. 0) 
     .      print*,'modifying ',ns,' out of ',nlsegs,' segments'
    
        do i = 1,nlines
          read(parline(i),*,err=995,end=995) tseg
          if (tseg .eq. lsegs(ns)) then
            iline = i
            exit
          end if
        end do
   
*************  apply factors to all layers
        do nly = 1, 4 
          call getvar(parline(iline),vcKMP(nly),KMP(nly))   ! get the param value 
          call getvar(parline(iline),vcKIMP(nly),KIMP(nly))
        end do
  
************  re-arrange target and simulated loads to make codes more readable
        SPO4tar(ns) = targets(1,ns)
        BPO4tar(ns) = targets(2,ns)
   
        SPO4(ns) = simEOF(1,ns)+simEOF(2,ns)+simEOF(3,ns)
        BPO4(ns) = simEOF(4,ns)
          
************  read .out files to get SIMULATED UPTAKE
        call read_uptake(
     I                   clu,lsegs(ns),lscen,
     O                   simuptk)
       
************  adjust KMP
        do nly = 1, 3
          if (taruptk(ns) .gt. 0.01) then
            KMP(nly) = KMP(nly)*(taruptk(ns)/simuptk)
          end if
        end do

        KMP(4) = KMP(4)*(BPO4tar(ns)/BPO4(ns))

************  adjust KIMP
        do nly = 1, 2
          KIMP(nly) = KIMP(nly)*(SPO4(ns)/SPO4tar(ns))
        end do

************  enfore Max and Min values for parameters
        do nly = 1, 4
          if (KIMP(nly).gt.limitKIMP(2)) then
            KIMP(nly) = limitKIMP(2)
          else if (KIMP(nly).lt.limitKIMP(1)) then
            KIMP(nly) = limitKIMP(1)
          end if
        end do
       
        do nly = 1, 3
          if (KMP(nly).gt.limitKMP(2)) then
            KMP(nly) = limitKMP(2)
          else if (KMP(nly).lt.limitKMP(1)) then
            KMP(nly) = limitKMP(1)
          end if
        end do
        if (KMP(4).gt. 0.005) KMP(4) = 0.005       !KMP has to be really small in GW layer 

************  put variables in files in memory
        do nly = 1, 4
          call putvar(parline(iline),vcKMP(nly),KMP(nly))   ! store the param value
          call putvar(parline(iline),vcKIMP(nly),KIMP(nly))
        end do
 
      end do       ! end loop for land segments

**********   REWRITE MODS TO ORIGINAL FILE NAME
      fnam= pardir//clu//'/'//paramscen(:lenparamscen)//
     .      '/'//module(:lenmod)//'.csv'
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
