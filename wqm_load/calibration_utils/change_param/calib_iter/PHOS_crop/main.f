*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      implicit none
      include 'calib_phos.inc'

      character*100 basin  ! variable specifiers
      
      integer nl,i,j             ! indices
     
      logical found
      logical filexist  ! if file already exists
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
      
******* FIND NUMBER OF TARGETS AND PLTGEN
      call getname(paramscen,
     O             tarscen,ntargets,tarnam,nplts,pltnam,
     O             limitKIMP,limitKMP,limitXFIX,limitK1)   
      
******* READ IN TARGETS FOR EACH CONSTITUENT 
      call gettargets(
     I                tarscen,clu,lsegs,nlsegs,ntargets,tarnam,
     O                targets)
      
************ FIND FIRST FREE FILE NAME FOR ALL LAND USES
      module = 'PHOS1'
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
        
************ READ IN SIMULATED EOF AND DETS TO ADJUST NVSI, KSER,KRER
      call getsimEOF(
     I               lscen,clu,version,lsegs,nlsegs,nplts,pltnam,
     O               simEOF)
      
************* LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
      print*,'all accounting, module  ', module
      call change_phos1(nlsegs,lsegs,module,clu,lscen,paramscen,
     .                  version,tarscen,targets,simEOF,
     .                  limitKIMP,limitKMP)
    
      module = 'PHOS2'
      print*,'all accounting, module  ', module
      call change_phos2(nlsegs,lsegs,module,clu,paramscen,version,
     .                  targets,simEOF,limitXFIX,limitK1)
      
      stop

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)

      end
