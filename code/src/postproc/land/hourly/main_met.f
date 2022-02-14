      implicit none

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'

      integer     wdmfil
      parameter   (wdmfil=21)

      integer     I_year1,I_year2
      integer     sdate(ndate),edate(ndate)
      data        sdate /0,1,1,0,0,0/
      data        edate /0,12,31,24,0,0/
      integer     iy, im, id, ih
      integer     ival

      character*4 C_VAR

      integer     ndaysinmonth
      external    ndaysinmonth

      character*25 lcalib
      integer      lenlcalib
      real         calibFactor

      read(*,*) lscen,lcalib,lseg,C_VAR,I_year1,I_year2

      call lencl(lscen,lenlscen)
      call lencl(lcalib,lenlcalib)
      call lencl(lseg, lenlseg)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      wdmfnam = ScenDatDir//'climate/met/'//
     .         lscen(:lenlscen)//'/met_'//lseg(:lenlseg)//'.wdm'
      print*,'>',wdmfnam,'<'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 903

      sdate(1) = I_year1
      edate(1) = I_year2

      calibFactor = 1.0
      if ( C_VAR .eq. 'EVAP' ) then
         dsn = 1000
         call EVAP_FACTOR(lseg,lenlseg,lcalib,lenlcalib,calibFactor)
         print*,'Evap Calib Factor = ',calibFactor
      else if ( C_VAR .eq. 'ATMP' ) then
         dsn = 1004
      else
         print*, 'dsn not found for '//C_VAR//' update code'
         return
      end if

      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)


      fnam = outdir//'land/hourly/'//lscen(:lenlscen)//'/'//
     .       lseg(:lenlseg)//'_'//C_VAR//'.out'
      print*,fnam
      open (dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 901

      ival = 1
      do iy = sdate(1),edate(1)
         do im = 1,12
            do id = 1,ndaysinmonth(iy,im)
C               write(dfile+1,'(A,A,A$)'),lseg(:lenlseg),',',C_lu
               do ih = 1,24
                 write(dfile+1,'(A,A,I4,A,I2,A,I2,A,I2,A,E10.5)')
     .               lseg(:lenlseg),',',
     .               iy,',',im,',',id,',',ih,',',hval(ival)*calibFactor
                 ival = ival + 1
               end do
            end do
         end do
      end do
      close(dfile+1)


      return


901   report(1) = 'Problem opening file '//fnam
      goto 999
         
902   report(1) = 'Problem reading line '// fnam
      goto 999
            
903   report(1) = 'Problem opening file'//wdmfnam
      goto 999 

999   call stopreport(report)
            
      end




      SUBROUTINE EVAP_FACTOR(lseg,lenlseg,lcalib,lenlcalib,factor)
      !{
         implicit none
         include '../../../lib/inc/standard.inc'
         include '../../../lib/inc/locations.inc'

         character*25  lcalib
         integer       lenlcalib
         logical       found
         character*200 filename
         real          factor

         print*, 'EVAP_FACTOR calculation ... '
         filename = pardir//'common/'//lcalib(:lenlcalib)//
     .      '/land_evap.csv'

         found = .false.

         open (dfile,file=filename,status='old',iostat=err)

         read(dfile,*) line
c         print*, line

         do while (line .ne. 'end')
            read(dfile,*) line,factor
            if ( line .eq. lseg(:lenlseg) ) then
               found = .true.
               return
            end if
c            print*, line
         end do

         if ( found .eqv. .false. ) go to 993

         return

991      report(1) = 'Error 991b Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

993      report(1) = 'Evap factor not found - '
         report(2) = filename
         report(3) = lseg(:lenlseg)
         go to 999

999   call stopreport(report)

      !}
      END
