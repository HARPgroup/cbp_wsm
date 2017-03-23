*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      implicit none
      include 'calib_nitr.inc'

      character*4 version ! version number
      character*100 basin  ! variable specifiers

      integer ns,nr,nl
      integer i,j,l,nly       ! indices
      integer iline

      logical found
      logical filexist    ! if file already exists
*************** END DECLARATIONS ***************************************

************ SPECIFICATION ENTRY SECTION
      read*,calscen,lscen,basin,clu
      call lencl(lscen,lenlscen)

      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

************ GET LAND SEGMENTS 
      call readLandSeglist(
     I                     basin,
     O                     lsegs,nlsegs)
  
******* FIND NUMBER OF TARGETS (NH4, NO3, ORGN, TN)
      call getname(paramscen,
     O             tarscen,ntargets,tarnam,nplts,pltnam,
     O             limitKIMNI,limitKAM,limitKDNI,limitKNI,
     O             limitKIMAM,limitKLON,limitKRON)
     
******* READ IN TARGETS FOR EACH CONSTITUENT
      call gettargets(
     I                tarscen,clu,lsegs,nlsegs,ntargets,tarnam,
     O                targets)

************ FIND FIRST FREE FILE NAME FOR ALL LAND USES
      module = 'NITR1'
      call uppercase(module)
      call lencl(module,lenmod)

      do i = 0,9999
        write(version,'(i4)') i
        do j = 1,4
          if (version(j:j).eq.' ') version(j:j) = '0'
        end do
    
        fnam = pardir//clu//'/'//paramscen(:lenparamscen)//'/'//
     .         module(:lenmod)//'_'//version//'.csv'
        inquire (file=fnam,exist=filexist)
        if (.not.filexist) exit
      end do
      
************ READ IN SIMULATED EOF 
      call getsimEOF(
     I               lscen,clu,version,lsegs,nlsegs,nplts,pltnam,
     O               simEOF)
      
************* LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
      print*,'changing model parameters, all accounting '
      call change_params(nlsegs,lsegs,module,clu,lscen,paramscen,
     I                   version,tarscen,targets,simEOF,
     I                   limitKIMNI,limitKAM,limitKDNI,limitKNI,
     I                   limitKIMAM,limitKLON,limitKRON)
      
      module = 'NITR8'
      print*,'changing initial storage, all accounting  '
      call change_storage(module,clu,nlsegs,lsegs,version,
     .                    lscen,paramscen)
      
      stop
********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)

      end
