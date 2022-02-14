      implicit none

      include 'scrorgx.inc'


      integer     wdmfil
      parameter   (wdmfil=13)

      character*2000 parline,varline,tableline

      logical foundrseg
      logical  comment
      external comment

      integer ndaysinmonth
      external ndaysinmonth

      integer     outflag

      read(*,*) rscen,rseg,outflag

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)


      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)


      call readcontrol_time(
     I            rscen,lenrscen,
     O            sdate,edate)

      call readcontrol_Rcalscen(
     I                          rscen,lenrscen,
     O                          calscen)
      call lencl(calscen,lencalscen)
      print*,'calscen = ',calscen(:lencalscen)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      wdmfnam = rseg(:lenrseg)//'.wdm'
c      print*,wdmfnam
      call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
      if (err .ne. 0) go to 881


      do iy = sdate(1),edate(1)
         do irow=1,maxrow
            annscr(iy,irow) = 0.0
         end do
      end do
***** READ IOVAR TABLE
      nrow = 0
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//
     .      '/rchres_scrorg'
      open(dfile+1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

***** FOR EACH ROW OF THE TABLE
      read(dfile+1,'(a100)',err=992) ioline
c      call d2x(ioline,last)
      do while (ioline(:3).ne.'end')
      !{
c         read(11,'(a100)',err=992) ioline
c         call d2x(line,last)

         if (.not.comment(ioline)) then
         !{

c            print*, '... processing ',ioline

            parModule(1) = 'SCRORG'
            parUsed(1)   = .true.
            read(ioline( 1:9 ),*,err=993) parTable(1)
            read(ioline(11:19),*,err=993) parName(1)

            parModule(2) = 'SCRORG'
            parUsed(2)   = .true.
            read(ioline(21:29),*,err=993) parTable(2)
            read(ioline(31:39),*,err=993) parName(2)

            read(ioline(41:49),*,err=993) idsn
            read(ioline(51:59),*,err=993) odsn

            read(ioline(61:69),*,err=993) C4_scenmod

            nrow = nrow + 1
            parscr(nrow) = parName(2)
            npar      = 2

******** GET PARAM VALS
            rsegs(1)  = rseg
            call getParValues(
     I                      parModule,parTable,parName,parUsed,npar,
     I                      parLKflag,lakeflags,
     I                      paramscen,rsegs,1,
     O                      parval)
******** DO PROCESSING IF NECESSARY
CA            print*,parval(1,1),parval(2,1)
            if ( parval(1,1).eq.1 ) then
            !{
               print*,C4_scenmod,rscen(:lenrscen),calscen(:lencalscen)
               if ( rscen(:lenrscen) .eq. calscen(:lencalscen) ) then
                  R_scenmod = 1.0
               else
                  call getscenmod(
     I              rscen,lenrscen,rseg,lenrseg,C4_scenmod,
     O              R_scenmod)

c                  call getscenmod(
c     I              rscen,lenrscen,rseg,lenrseg,'TSSX',
c     O              R_tssxmod)

c                  R_scenmod = R_scenmod/R_tssxmod

               end if
c              wdmfnam = rseg(:lenrseg)//'.wdm'
c              call wdbopnlong(wdmfil,wdmfnam,0,err)                           ! open main wdm read/write
c              if (err .ne. 0) go to 991

CA              print*,odsn
c              call wtdate(wdmfil,1,odsn,2,sdate,edate,err)
              call gethourdsn(wdmfil,sdate,edate,idsn,nvals,hscr)
              call gethourdsn(wdmfil,sdate,edate,odsn,nvals,horg)
c              do ival = 1,nvals
              ival = 0
              do iy = sdate(1),edate(1)
               anntssx(iy) = 0.0
               do im = 1,12
                do id = 1,ndaysinmonth(iy,im)
                 do ih = 1,24
                    ival = ival + 1
                    if ( hscr(ival) .lt. 0 ) then
c                       print*,'...< ',horg(ival),hscr(ival),ival
CA                      tval = horg(ival)
                       tscr = hscr(ival)*parval(2,1)*R_scenmod
                       horg(ival) = horg(ival) - tscr
                       annscr(iy,nrow) = annscr(iy,nrow) - tscr
                       anntssx(iy)   = anntssx(iy) - hscr(ival)
CA                       print*,tval,',',horg(ival),',',hscr(ival),',',
CA     .                   parval(2,1),',',ival/24
CA                   else
CA                      print*,horg(ival),',',horg(ival),',',0.0,',',
CA     .                   parval(2,1),',',ival/24
                    end if
                 end do
                end do
               end do
              end do
              call puthourdsn(wdmfil,sdate,edate,odsn,nvals,horg)
            !}
            end if


         !}
         end if
         read(dfile+1,'(a100)',err=992,end=101) ioline
c         print*,'... read ',ioline
         call d2x(ioline,last)
      !}
      end do

101   close(dfile+1)

      if ( outflag .eq. 1 ) then
         fnam=rseg(:lenrseg)//'_scr.out'
         open(dfile+1,file=fnam,status='unknown',iostat=err)
         if (err.ne.0) go to 991
         write(dfile+1,1001,err=994)
     .          'Year',(',',parscr(irow),irow=1,nrow),',','SCRTSSX'
         do iy = sdate(1),edate(1)
            write(dfile+1,1002,err=994)
     .         iy,(',',annscr(iy,irow),irow=1,nrow),',',anntssx(iy)
         end do
         close(dfile+1)
      end if

      call wdflcl(wdmfil,err)

      stop

1001  format(a4,3(a,a10))
1002  format(I4,3(a,e16.8))

881   report(1) = 'could not open file'
      report(2) = wdmfnam
      write(report(3),*) 'error = ',err
      go to 999

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'error reading file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

993   report(1) = 'error parsing line'
      report(2) = fnam
      report(3) = ioline
      go to 999

994   report(1) = 'error writing to the file'
      report(2) = fnam
      report(3) = 'check path or permission'
      go to 999

999   call stopreport(report)

      end
