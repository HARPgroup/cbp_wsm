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

      integer vcPLTN(4),vcAGPLTN,vcLITTRN      ! each storage has FOUR layers
      real PLTN(4),AGPLTN,LITTRN

      integer i,j,nl,ns,nly       ! indices
      integer ny,years,calyear(nyearmax),startyear
      integer iline
      
      character*4 version ! version number

      real lorgn(4,0:nyearmax),ls3storage
      real plantn(4,0:nyearmax),agplant(0:nyearmax),litter(0:nyearmax)
      real ls3pltn,ls3agpltn,ls3littrn

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
     O                 vcORGN,vcRORGN,vcPLTN,vcAGPLTN,vcLITTRN)
      
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
          call getvar(parline(iline),vcPLTN(nly),PLTN(nly))
        end do

        call getvar(parline(iline),vcAGPLTN,AGPLTN)
        call getvar(parline(iline),vcLITTRN,LITTRN)

************ read .out files to get labile ORGN storage and plant N
        call read_outfile(
     I                    clu,lsegs(ns),lscen,
     O                    lorgn,plantn,agplant,litter,years)
        
*********** Adjust initial labile organic storage for all layers
        do nly = 1, 4
          ls3storage = 0.
          do ny = years-2, years
            if (lorgn(nly,ny) .ne. -1.) then
              ls3storage = ls3storage + lorgn(nly,ny)
            else
              ls3storage = -1.
            end if
          end do

          if(ls3storage.ne. -1.) then 
            ORGN(nly) = ls3storage/3.
          end if
        end do 

*********** Adjust plant N storage 
        do nly = 1, 3
          ls3pltn = 0.
          do ny = years-2, years
            if (plantn(nly,ny) .ne. -1.) then
              ls3pltn = ls3pltn + plantn(nly,ny)
            else
              ls3pltn = -1.
            end if
          end do

          if(ls3pltn.ne. -1.) then
            PLTN(nly) = ls3pltn/3.
          end if
        end do

*********  Adjust above-ground plantN and litter
        ls3agpltn = 0.
        do ny = years-2, years
          if (agplant(ny) .ne. -1.) then
            ls3agpltn = ls3agpltn + agplant(ny)
          else
            ls3agpltn = -1.
          end if
        end do
        if(ls3agpltn .ne. -1.) then
          AGPLTN = ls3agpltn/3.
        end if

        ls3littrn = 0.
        do ny = years-2, years
          if (litter(ny) .ne. -1.) then
            ls3littrn = ls3littrn + litter(ny)
          else
            ls3littrn = -1.
          end if
        end do
        if(ls3littrn .ne. -1.) then
          LITTRN = ls3littrn/3.
        end if
       
*************** put adjusted storages in files in memory
        do nly = 1, 4
          call putvar(parline(iline),vcORGN(nly),ORGN(nly))   ! store the param value
          call putvar(parline(iline),vcPLTN(nly),PLTN(nly))
        end do

        call putvar(parline(iline),vcAGPLTN,AGPLTN)
        call putvar(parline(iline),vcLITTRN,LITTRN)

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
