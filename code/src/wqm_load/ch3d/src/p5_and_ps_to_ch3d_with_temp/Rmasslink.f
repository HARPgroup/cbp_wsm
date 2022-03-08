************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********* for input and output separately                             **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine Rmasslink(upNexits,modules,nmod,
     O                     nRvarOut,RdsnOut,RnameOut)   
      implicit none

      include 'transfer_wdm.inc'

      integer upNexits     ! number of exits of upstream reach

      character*4 name  ! temp reading variable for name

      character*6 Tmod  ! temp reading variable for module

      logical found,scompcase
      external scompcase

      integer nm

      print*,'Rmasslink.f start'
******** populate nRvarOut, RdsnOut, RnameOut from 'rchres_out'
      fnam = catdir//'iovars/rchres_out_Xx'   ! dsn and name are same for 1x,Nx
      call lencl(fnam,last)
      write(fnam(last-1:last-1),'(i1)') upNexits
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvarOut = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod
            if (scompcase(Tmod,modules(nm))) found=.true.
          end do
          if (found) then
            nRvarOut = nRvarOut + 1
            if (nRvarOut.gt.MaxRvar) go to 994
            RdsnOut(nRvarOut) = dsn
            RnameOut(nRvarOut) = name
          end if
        end if
        read(dfile,'(a100)') line
      end do

      close (dfile)

      print*,'Rmasslink.f end'
      return

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

994   report(1) = 'Too many dsns in file:'
      report(2) = fnam
      write(report(3),*)
     .     ' raise maxRvar in stream_wdm.inc above ',maxRvar 
      go to 999

999   call stopreport(report)

      end 
