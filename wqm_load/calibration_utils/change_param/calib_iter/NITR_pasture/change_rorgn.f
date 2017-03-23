*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      subroutine change_rorgn(
     I                        nlsegs,lsegs,lscen,paramscen,clu,version)

      implicit none
      include 'calib_nitr.inc'

      integer i,j,nl,ns,nly       ! indices
      integer ny,nyears
      integer iline
      character*4 version ! version number

      real lonst(4,0:nyearmax),ronst(4,0:nyearmax)  ! end of year storage at each layer
      real Xron(maxlsegs,4)
      real lsegron(4)
*************** END DECLARATIONS ***************************************

************* LOOP OVER LAND USES, FIND THE FILE AND MAKE THE CHANGES
      call lencl(paramscen,lenparamscen)
      fnam= pardir//clu//'/'//paramscen(:lenparamscen)//'/RORGN.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********          READ HEADER LINES
      read(dfile,'(a2000)',err=992) tableline
      call d2x(tableline,last)
      tkeep = tableline

**********  READ WHOLE FILE INTO MEMORY
      nlines = 1
      read(dfile,'(a2000)',err=993,end=994) parline(nlines)
       
      do while (parline(nlines)(:3).ne.'end')
        nlines = nlines + 1
        read(dfile,'(a2000)',err=993,end=994) parline(nlines)
      end do
      close(dfile)
                  
**********          WRITE FILE TO NEW FILE NAME
      fnam = pardir//clu//'/'//paramscen(:lenparamscen)//
     .       '/RORGN_'//version//'.csv'
      open(dfile,file=fnam,status='new',iostat=err)
      if (err.ne.0) go to 991

      call rytdos(tkeep,dfile)
      do i = 1,nlines
        call rytdos(parline(i),dfile)
      end do
      close(dfile)

************** LOOP OVER ALL LAND SEGS AND MAKE RORGN CHANGES
      do ns = 1,nlsegs
        if (mod(ns,5) .eq. 0)
     .    print*,'modifying ',ns,' out of ',nlsegs,' segments'
      
************ read current refractory storage adjustment 
        call read_rorgn(
     I                  paramscen,lsegs(ns),clu,
     O                  lsegron)

************ read .out files to get Organic N storage
        call read_storage(
     I                    lscen,lsegs(ns),clu,
     O                    lonst,ronst,nyears)

************ update refractory storage adjustment for next run
        do nly = 1, 4
          Xron(ns,nly) = (ronst(nly,nyears)-ronst(nly,1))/real(nyears)  ! amonut build up last run
          Xron(ns,nly) = Xron(ns,nly) + lsegron(nly)
          if (Xron(ns,nly) .lt. 0.) Xron(ns,nly) = 0.                 ! not allow negative value
        end do

      end do       ! end loop for land segments

**********          REWRITE MODS TO ORIGINAL FILE NAME
      fnam= pardir//clu//'/'//paramscen(:lenparamscen)//'/RORGN.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991
      
      call rytdos(tkeep,dfile)
      do ns = 1, nlsegs
        write(dfile,1233,err=995) lsegs(ns),(Xron(ns,nly),nly=1,4)
      end do
      write(dfile,1234,err=995) 'end'
      close(dfile)

      return

1233  format(a6,4(',',f10.3))
1234  format(a3)
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

995   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

999   call stopreport(report)

      end
