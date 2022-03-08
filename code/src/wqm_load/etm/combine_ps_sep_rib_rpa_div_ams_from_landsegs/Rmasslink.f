************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********* for input and output separately                             **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine Rmasslink(
     I                     ioscen,lenioscen,upNexits,modules,nmod,
     O                     nRvarIn, RdsnIn, RnameIn,
     O                     nRvarOut,RdsnOut,RnameOut)   
      implicit none

      include 'res.inc'

      integer nIn,nOut,nm     ! indices

      character*4 name  ! temp reading variable

      character*6 Tmod  ! temp reading variable

      logical found,scompcase
      external scompcase

******** populate nRvarIn, RdsnIn, RnameIn from 'rchres_in'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_in'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvarIn = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod
            if (scompcase(Tmod,modules(nm))) found=.true.
          end do
          if (found) then
            nRvarIn = nRvarIn + 1
            if (nRvarIn.gt.MaxRvar) go to 994
            RdsnIn(nRvarIn) = dsn
            RnameIn(nRvarIn) = name
          end if
        end if
        read(dfile,'(a100)') line
      end do

      close (dfile)

******** populate nRvarOut, RdsnOut, RnameOut from 'rchres_out'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_out_Xx'  
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

********** check for matches both ways
      do nIn = 1,nRvarIn
        found = .false.
        do nOut = 1,nRvarOut
          if (RnameIn(nIn).eq.RnameOut(nOut)) found = .true.
        end do
        if (.not.found) go to 993
      end do

      do nOut = 1,nRvarOut
        found = .false.
        if(RnameOut(nOut).eq.'SSCR') found=.true.
        do nIn = 1,nRvarIn
          if (RnameOut(nOut).eq.RnameIn(nIn)) found = .true.
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
      report(2)=' has a variable '//RnameOut(nOut)//' not found in '
      report(3)=' rchres_in in the same directory'
      go to 999

993   report(1) = fnam
      report(2)=' has no variable for '//RnameIn(nIn)//' in the file'
      report(3)=' rchres_in in the same directory'
      go to 999

994   report(1) = 'Too many dsns in file:'
      report(2) = fnam
      write(report(3),*)
     .     ' raise maxRvar in stream_wdm.inc above ',maxRvar 
      go to 999

999   call stopreport(report)

      end 
