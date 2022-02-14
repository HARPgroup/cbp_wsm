************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********* for upstream and downstream of a conflence                  **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine Rmasslink_confluence(
     I                     ioscen,lenioscen,
     I                     upNexits,Nexits,modules,nmod,
     O                     nRvarDown, RdsnDown, RnameDown,
     O                     nRvarUp,RdsnUp,RnameUp)   
      implicit none

      include 'stream_wdm.inc'
      
      include 'confluence.inc'

      integer upNexits,Nexits  ! number of exits of upstream reach

      integer nIn,nOut     ! indices

      character*4 name  ! temp reading variable

      character*6 Tmod  ! temp reading variable for module

      logical found,scompcase
      external scompcase

******** populate nRvarDown, RdsnDown, RnameDown from 'rchres_out'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_Xx'   ! dsn and name are same for 1x,Nx
      call lencl (fnam,last)
      write(fnam(last-1:last-1),'(i1)') Nexits
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvarDown = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')

        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod    ! test if module is in use
            if (scompcase(Tmod,modules(nm))) found = .true.
          end do
          if (found) then
            nRvarDown = nRvarDown + 1
            if (nRvarDown.gt.MaxRvar) go to 994
            RdsnDown (nRvarDown) = dsn
            RnameDown(nRvarDown) = name
          end if
        end if
        
        read (dfile,'(a100)') line
        
      end do
    
      close (dfile)

******** populate nRvarUp, RdsnUp, RnameUp from 'rchres_out'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_Xx'   ! dsn and name are same for 1x,Nx
      call lencl(fnam,last)
      write(fnam(last-1:last-1),'(i1)') upNexits
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      
      nRvarUp = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')

        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod    ! test if module is in use
            if (scompcase(Tmod,modules(nm))) found = .true.
          end do
          if (found) then
            nRvarUp = nRvarUp + 1
            if (nRvarUp.gt.MaxRvar) go to 994
            RdsnUp(nRvarUp) = dsn
            RnameUp(nRvarUp) = name
          end if
        end if

        read(dfile,'(a100)') line

      end do

      close (dfile)

********** check for matches both ways
      do nIn = 1,nRvarDown
        found = .false.
        do nOut = 1,nRvarUp
          if (RnameDown(nIn).eq.RnameUp(nOut)) found = .true.
        end do
        if (.not.found) go to 993
      end do

      do nOut = 1,nRvarUp
        found = .false.
        do nIn = 1,nRvarDown
          if (RnameUp(nOut).eq.RnameDown(nIn)) found = .true.
        end do
        if (.not.found) go to 992
      end do

      return

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = fnam
      report(2)=' has out variable '//RnameOut(nOut)//' not found in '
      report(3)=' rchres_in in the same directory'
      go to 999

993   report(1) = fnam
      report(2)=' has no variable for '//RnameIn(nIn)//' in the file'
      report(3)=' rchres_in in the same directory'//'Rmas..confluence.f'
      go to 999

994   report(1) = 'Too many dsns in file:'
      report(2) = fnam
      write(report(3),*)
     .     ' raise maxRvar in stream_wdm.inc above ',maxRvar 
      go to 999

999   call stopreport(report)

      end 
