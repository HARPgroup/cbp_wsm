*************************************************************************
** program to change parameters for NITR calibration.  Automatically   **
** read csv files and performs functions on the variables              **
*************************************************************************
      implicit none
      include 'calib_nitr.inc'

      character*100 basin  ! variable specifiers
      character*4 version ! version number
 
      integer i,j       ! indices
      integer iline
      
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
  
************ READ EOF TARGETS FOR ALL LAND USE, ALL SEGLEMTS AND ALL CONSTITUENTS

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
    
        fnam =pardir//clu//'/'//paramscen(:lenparamscen)//'/'
     .        //module(:lenmod)//'_'//version//'.csv'
        inquire (file=fnam,exist=filexist)
        if (.not.filexist) exit
      end do
       
************ READ IN SIMULATED EOF AND DETS TO ADJUST NVSI, KSER,KRER
      call getsimEOF(
     I               lscen,clu,version,lsegs,nlsegs,nplts,pltnam,
     O               simEOF)

************* LOOP OVER TARGET AND LAND USES, FIND THE FILE AND MAKE THE CHANGES
      print*,'changing model parameters, all accounting '
      call change_params(
     I                   nlsegs,lsegs,lscen,paramscen,module,clu,
     I                   version,tarscen,targets,simEOF,
     I                   limitKIMNI,limitKAM,limitKDNI,limitKNI,
     I                   limitKIMAM,limitKLON,limitKRON)
      print*, '  '

      module = 'NITR8'
      print*,'changing initial labile orgn storage, all accounting  '
      call change_lorgn(
     I                  module,clu,nlsegs,lsegs,version,
     I                  lscen,paramscen)
      print*, '  '

      print*,'calculating refractory orgn storage accumulation  '
      call change_rorgn(
     I                  nlsegs,lsegs,lscen,paramscen,clu,version)
 

      stop
********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

999   call stopreport(report)

      end
