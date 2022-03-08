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
      character*25 pradscen
      integer     lenpradscen

      real        aveann

      integer     ndaysinmonth
      external    ndaysinmonth

      read(*,*) lscen,lseg,I_year1,I_year2,C_VAR

      call lencl(lscen,lenlscen)
      call lencl(lseg, lenlseg)

      call readlcontrol_prad(lscen,lenlscen,pradscen)
      call lencl(pradscen,lenpradscen)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      wdmfnam = ScenDatDir//'climate/prad/'//
     .         pradscen(:lenpradscen)//'/prad_'//lseg(:lenlseg)//'.wdm'
      print*,'>',wdmfnam,'<'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 903

      sdate(1) = I_year1
      edate(1) = I_year2

      if ( C_VAR .eq. 'HPRC' ) then
         dsn = 2000
      else if ( C_VAR .eq. 'NO23' ) then
         dsn = 2001
      else
         print*, 'dsn not found for '//C_VAR//' update code'
         return
      end if

      call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)


      fnam = outdir//'land/aveann/'//lscen(:lenlscen)//'/'//
     .       lseg(:lenlseg)//'_'//C_VAR//'.out'
      print*,fnam
      open (dfile+1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 901

      ival = 1
      aveann = 0.0
      do iy = sdate(1),edate(1)
         do im = 1,12
            do id = 1,ndaysinmonth(iy,im)
C               write(dfile+1,'(A,A,A$)'),lseg(:lenlseg),',',C_lu
               do ih = 1,24
                 aveann = aveann + hval(ival)
                 ival = ival + 1
               end do
            end do
         end do
      end do
      aveann = aveann / ( edate(1) - sdate(1) + 1 )
      write(dfile+1,'(A,A,I4,A,I4,A,E10.5)')
     .               lseg(:lenlseg),',',
     .               sdate(1),',',edate(1),',',aveann
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
