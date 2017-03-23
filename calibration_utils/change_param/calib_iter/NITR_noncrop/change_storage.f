*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      subroutine change_storage(module,clu,nlsegs,lsegs,version,
     .                          lscen,paramscen)

      implicit none
      include 'calib_nitr.inc'

      integer vcORGN(4),vcRORGN(4)      ! each storage has FOUR layers
      real ORGN(4),RORGN(4)

      integer i,j,nl,ns,nly       ! indices
      integer ny,nyears
      integer iline
      character*4 version ! version number

      real lonst(4,0:nyearmax),ronst(4,0:nyearmax)
      real median, x(0:nyearmax),y(0:nyearmax)
      external median

*************** END DECLARATIONS ***************************************

      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(paramscen,lenparamscen)

************* LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
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

*********** FIND THE COLUME OF EACH STORAGE
      call getcolumns8(
     I                 tkeep,vkeep,module,clu,
     O                 vcORGN,vcRORGN)

************** LOOP OVER ALL LAND SEGS AND MAKE PARM CHANGES
      do ns = 1,nlsegs
        if (mod(ns,5) .eq. 0)
     .    print*,'modifying ',ns,' out of ',nlsegs,' segments'

        do i = 1,nlines
          read(parline(i),*,err=995,end=995) tseg
          if (tseg .eq. lsegs(ns)) then
            iline = i
            exit
          end if
        end do
   
**************** find values for all parameters
        do nly = 1, 4
          call getvar(parline(iline),vcORGN(nly),ORGN(nly))         ! get the param value 
          call getvar(parline(iline),vcRORGN(nly),RORGN(nly))        ! for each layer
        end do

************ read .out files to get Organic N storage
        call read_storage(
     I                    clu,lsegs(ns),lscen,
     O                    lonst,ronst,nyears)

********* adjust initial stoarge at each layer
        do nly = 1, 4
          do ny = 1, nyears
            x(ny) = lonst(nly,ny)
            y(ny) = ronst(nly,ny)
          end do
          ORGN(nly)  = median(x,nyears,nyears,err)      ! set to median value
          RORGN(nly) = median(y,nyears,nyears,err)
        end do 

*************** put adjusted storages in files in memory
        do nly = 1, 4
          call putvar(parline(iline),vcORGN(nly),ORGN(nly))   ! store the param value
          call putvar(parline(iline),vcRORGN(nly),RORGN(nly)) ! for each layer
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
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

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
