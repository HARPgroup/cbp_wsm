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
      real        F_AVG_Load

      character*4 C_VAR
      character*3 C_lu

      integer     ndaysinmonth
      external    ndaysinmonth

      character*20  table
      integer       lentable

      read(*,*) lscen,lseg,C_lu,I_year1,I_year2,table

      call lencl(lscen,lenlscen)
      call lencl(lseg, lenlseg)
      call lencl(table,lentable)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      wdmfnam = outwdmdir//'land/'//C_lu//'/'//
     .         lscen(:lenlscen)//'/'//C_lu//lseg(:lenlseg)//'.wdm'
      print*,'>',wdmfnam,'<'
      call wdbopnlong(wdmfil,wdmfnam,0,err)
      if (err.ne.0) go to 903

      sdate(1) = I_year1
      edate(1) = I_year2
      
      call readcontrol_Lioscen(lscen,lenlscen,ioscen)
      call lencl(ioscen,lenioscen)
      !fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/land_loads'
      fnam= catdir//'iovars/'//ioscen(:lenioscen)//'/requests/'
     .   //table(:lentable)
      print*,fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 901

      read(dfile,'(a4)',err=902) C_VAR
      do while ( C_VAR(:1) .ne. ' ' )
         print*,'calling lndDSN ',C_VAR
         call lndDSN(C_VAR,ioscen,lenioscen,
     O               dsn)
         print*,C_VAR,'->',dsn
         print*,sdate(1),sdate(2),sdate(3),sdate(4),sdate(5),sdate(6)
         print*,edate(1),edate(2),edate(3),edate(4),edate(5),edate(6)

         call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)

         fnam = outdir//'land/hourly/'//lscen(:lenlscen)//'/'//
     .       lseg(:lenlseg)//'_'//C_lu//'_'//C_VAR//'.out'
         print*,fnam
         open (dfile+1,file=fnam,status='unknown',iostat=err)
         if (err.ne.0) go to 901

         ival = 1
         do iy = sdate(1),edate(1)
            do im = 1,12
               do id = 1,ndaysinmonth(iy,im)
C                  write(dfile+1,'(A,A,A$)'),lseg(:lenlseg),',',C_lu
                  do ih = 1,24
                    write(dfile+1,'(A,A,A,A,I4,A,I2,A,I2,A,I2,A,E10.5)')
     .                  lseg(:lenlseg),',',C_lu,',',
     .                  iy,',',im,',',id,',',ih,',',hval(ival)
                    ival = ival + 1
                  end do
               end do
            end do
         end do
         close(dfile+1)


         read(dfile,'(a4)',err=902) C_VAR
      end do
      write(dfile+1,*),''

      close(dfile)


      return

901   report(1) = 'Problem opening file '//fnam
      goto 999

902   report(1) = 'Problem reading line '// fnam
      goto 999

903   report(1) = 'Problem opening file'//wdmfnam
      goto 999

999   call stopreport(report)

      end

