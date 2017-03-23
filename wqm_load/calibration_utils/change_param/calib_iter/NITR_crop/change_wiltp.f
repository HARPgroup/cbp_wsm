**************************************************************************
** program to change wilting points for NITR calibration. Automatically **
** read csv files and perform functions on the variables                **
**************************************************************************
      subroutine change_wiltp (module,clu,nlsegs,lsegs,version,
     .                         lscen,paramscen,tarscen)

      implicit none
      include 'calib_nitr.inc'

      integer vcSWILTP,vcUWILTP,vcLWILTP
      real SWILTP,UWILTP,LWILTP
      real taruptk(maxlsegs),simuptk

      integer i,nl,ns,nly       ! indices
      integer iline
      
      character*4 version ! version number
*************** END DECLARATIONS ***************************************

      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(paramscen,lenparamscen)

************ LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
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
     
******* READ IN UPTAKE TARGETS
      call gettaruptk(
     I                tarscen,clu,lsegs,nlsegs,
     O                taruptk) 
      
*********** FIND THE COLUME OF EACH STORAGE
      call getcolumns4(
     I                 tkeep,vkeep,module,clu,
     O                 vcSWILTP,vcUWILTP,vcLWILTP)
    
************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
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
  
************ read .out files to get SIMULATED UPTAKE
        call read_uptake(
     I                   clu,lsegs(ns),lscen,
     O                   simuptk)

**************** find values for all parameters
        call getvar(parline(iline),vcSWILTP,SWILTP)
        call getvar(parline(iline),vcUWILTP,UWILTP)
        call getvar(parline(iline),vcLWILTP,LWILTP)

********* adjust KIMNI and KIMAM based on uptake
        if (taruptk(ns) .gt. 0.01) then    ! adjust only segments woth Nonzero uptake
          SWILTP = SWILTP*(simuptk/taruptk(ns))
          UWILTP = UWILTP*(simuptk/taruptk(ns))
          LWILTP = LWILTP*(simuptk/taruptk(ns))
        end if

        if (SWILTP .gt. 0.2) SWILTP = 0.2        ! Max limit of wilting point
        if (UWILTP .gt. 0.2) UWILTP = 0.2
        if (LWILTP .gt. 0.2) LWILTP = 0.2

        if (SWILTP .lt. 0.01) SWILTP = 0.01        ! Min limit of wilting point
        if (UWILTP .lt. 0.01) UWILTP = 0.01
        if (LWILTP .lt. 0.01) LWILTP = 0.01

*************** put adjusted storages in files in memory
        call putvar(parline(iline),vcSWILTP,SWILTP)
        call putvar(parline(iline),vcUWILTP,UWILTP)
        call putvar(parline(iline),vcLWILTP,LWILTP)

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
