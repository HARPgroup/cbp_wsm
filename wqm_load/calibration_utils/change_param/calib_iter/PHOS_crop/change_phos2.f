*************************************************************************
** program to change parameters for PHOS calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      subroutine change_phos2(nlsegs,lsegs,module,clu,paramscen,
     .                        version,targets,simEOF,limitXFIX,limitK1)

      implicit none
      include 'calib_phos.inc'

      integer ns,nl
      integer i,j,nly       ! indices
      integer iline
*************** END DECLARATIONS ***************************************

      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(paramscen,lenparamscen)

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

**********          READ WHOLE FILE INTO MEMORY
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

**************** FIND COLUMN OF PARAMETERS
      call getcolumn2( 
     I                tkeep,vkeep,module,clu,
     O                vcXFIX,vcK1)

***************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
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
        
***************** apply factors to all land uses
        do nly = 1, 4 
          call getvar(parline(iline),vcXFIX(nly),XFIX(nly)) ! for each layer
          call getvar(parline(iline),vcK1(nly),K1(nly))
        end do

************ re-arrange target and simulated loads to make codes more readable
        SPO4tar(ns) = targets(1,ns)
        BPO4tar(ns) = targets(2,ns)

        SPO4(ns) = simEOF(1,ns)+simEOF(2,ns)+simEOF(3,ns)
        BPO4(ns) = simEOF(4,ns)

********* adjust K1, surface ONLY
        K1(1) = K1(1)*(SPO4tar(ns)/SPO4(ns))
        K1(2) = K1(2)*(SPO4(ns)/SPO4tar(ns))

******** Enfore Max and Min values for parameters
        do nly = 1, 4
          if (K1(nly).gt.limitK1(2)) then
            K1(nly)=limitK1(2)
          else if (K1(nly).lt.limitK1(1)) then
            K1(nly) = limitK1(1)
          end if
        end do

*************** put variables in files in memory
        do nly = 1, 4
          call putvar(parline(iline),vcXFIX(nly),XFIX(nly)) ! for each layer
          call putvar(parline(iline),vcK1(nly),K1(nly))
        end do

      end do       ! end loop for land segments

**********          REWRITE MODS TO ORIGINAL FILE NAME
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
