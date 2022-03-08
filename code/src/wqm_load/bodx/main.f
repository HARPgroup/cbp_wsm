
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/wdm.inc'

      include '../lib/inc/land_use.inc'

      integer I_BOD(4), I_LON(4), I_RON(4), I_LOP(4), I_ROP(4)

      character*4 C4_BOD(4), C4_LON(4), C4_RON(4), C4_LOP(4), C4_ROP(4)
      data C4_BOD /'DBOD','SBOD','IBOD','ABOD'/
      data C4_LON /'DLON','SLON','ILON','ALON'/
      data C4_RON /'DRON','SRON','IRON','ARON'/
      data C4_LOP /'DLOP','SLOP','ILOP','ALOP'/
      data C4_ROP /'DROP','SROP','IROP','AROP'/

      integer I_Year1, I_Year2

      integer wdmfil
      parameter (wdmfil=dfile+10)

      integer i,j,k,l
      integer I_WDM
      integer I_DEBUG0

      integer sdate(ndate),edate(ndate)

      real hbod(ndaymax*24)
      real hlon(ndaymax*24)
      real hron(ndaymax*24)
      real hlop(ndaymax*24)
      real hrop(ndaymax*24)

      character*4 SPECIES1, SPECIES2
      real CVP2N, CVN2BOD

      logical debug

      debug = .true.

      read*,lseg,lscen,I_Year1,I_Year2,I_WDM,I_DEBUG0

C      I_WDM = 1
C      I_DEBUG0 = 1

      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

      call readcontrol_Lioscen(
     I                          lscen,lenlscen,
     O                          ioscen)
      call lencl(ioscen,lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/species_cvfactor'
      SPECIES1 = 'PHOS'
      SPECIES2 = 'NITR'
      call SPECIES_CVFACTOR(fnam,SPECIES1,SPECIES2,CVP2N)

      SPECIES1 = 'NITR'
      SPECIES2 = 'BODX'
      call SPECIES_CVFACTOR(fnam,SPECIES1,SPECIES2,CVN2BOD)

      if (debug) print*,'CVP2N   = ', CVP2N
      if (debug) print*,'CVN2BOD = ', CVN2BOD



      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/perlnd'

c     * Now replace by function calls below that reads perlnd file
c      data I_BOD /162,163,262,462/
c      data I_LON /141,146,246,446/
c      data I_RON /143,147,247,447/
c      data I_LOP /154,155,255,455/
c      data I_ROP /156,157,257,457/

      do i = 1,4
         call MY_DSN(fnam,C4_BOD(i),I_BOD(i))
         call MY_DSN(fnam,C4_LON(i),I_LON(i))
         call MY_DSN(fnam,C4_RON(i),I_RON(i))
         call MY_DSN(fnam,C4_LOP(i),I_LOP(i))
         call MY_DSN(fnam,C4_ROP(i),I_ROP(i))
      end do


      sdate(1) = I_Year1
      sdate(2) = 1
      sdate(3) = 1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = I_Year2
      edate(2) = 12
      edate(3) = 31
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0


      !>> OPEN Dummy WDM
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+1,wdmfnam,0,err)
      if (err.ne.0) go to 801

      do l = 1,nlu

         if ( luname(l).eq.'wat' ) cycle

         if ( I_WDM .eq. 1) then
            wdmfnam = luname(l)//lseg(:lenlseg)//'.wdm'
         else
            wdmfnam = outwdmdir//'/land/'//luname(l)//'/'//
     .         lscen(:lenlscen)//'/'//luname(l)//lseg(:lenlseg)//'.wdm'
         end if
         if ( I_DEBUG0 .eq. 1 )
     .    print*,'Land WDM File: ',wdmfnam
         call wdbopnlong(wdmfil,wdmfnam,0,err)
         if (err.ne.0) go to 801


         do i = 1,4
            call gethourdsn(wdmfil,sdate,edate,I_LON(i),nvals,hlon)
            if ( I_DEBUG0 .eq. 1 )  print*,'I_LON ',i
            call gethourdsn(wdmfil,sdate,edate,I_RON(i),nvals,hron)
            if ( I_DEBUG0 .eq. 1 )  print*,'I_RON ',i
            call gethourdsn(wdmfil,sdate,edate,I_LOP(i),nvals,hlop)
            if ( I_DEBUG0 .eq. 1 )  print*,'I_LOP ',i
            call gethourdsn(wdmfil,sdate,edate,I_ROP(i),nvals,hrop)
            if ( I_DEBUG0 .eq. 1 )  print*,'I_ROP ',i

            do j = 1,nvals
               if ( hlon(j) < CVP2N * hlop(j) ) then
                  hbod(j) = CVN2BOD * hlon(j)
                  hrop(j) = hrop(j) + ( hlop(j) - hlon(j) / CVP2N )
                     if ( hrop(j) .lt. 0 ) hrop(j) = 0.0
                  hlon(j) = 0
                  ! hron(j) = hron(j)
                  hlop(j) = 0
               else
                  hbod(j) = CVN2BOD * hlop(j) * CVP2N
                  ! hrop(j) = hrop(j)
                  hron(j) = hron(j) + ( hlon(j) - hlop(j) * CVP2N )
                     if ( hron(j) .lt. 0 ) hron(j) = 0.0
                  hlon(j) = 0
                  hlop(j) = 0
               end if
            end do

            call puthourdsn(wdmfil,sdate,edate,I_LON(i),nvals,hlon)
            call puthourdsn(wdmfil,sdate,edate,I_RON(i),nvals,hron)
            call puthourdsn(wdmfil,sdate,edate,I_LOP(i),nvals,hlop)
            call puthourdsn(wdmfil,sdate,edate,I_ROP(i),nvals,hrop)
            call puthourdsn(wdmfil,sdate,edate,I_BOD(i),nvals,hbod)

         end do

         !>> CLOSE WDM
         call wdflcl(wdmfil,err)
         if (err.ne.0) go to 803

      end do

      print*,"iRunning BODX ",
     .     lscen(:lenlscen),lseg(:lenlseg)," Finished"

      return


801   print*, 'Error 801 WDM file must previously exist'
      report(1) = 'Error 801 WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

803   report(1) = 'Error 803 Problem closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end


      SUBROUTINE MY_DSN(filename,dsnstr,dsn)
      !{
         implicit none
         include '../lib/inc/standard.inc'

         character*200 filename
c         integer       dfile
c         parameter     (dfile=10)
c         integer       err
c         character*200 line

         character*4   dsnstr
         integer       dsn

         logical       comment
         external      comment

         logical       found

         found = .false.

         open(dfile,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile,'(a100)',err=992,end=310) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(26:29).eq.dsnstr) then
            !{
               read(line(:3),*) dsn
               found = .true.
               goto 310
            !}
            end if
         !}
         end do
         if ( found .eqv. .false. ) goto 993

310      close (dfile)

         return

991      report(1) = 'Error 991b Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992b Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

993      report(1) = 'Error 993b Problem DSN not found ' // dsnstr
         report(2) = filename
         report(3) = dsnstr
         go to 999

999   call stopreport(report)

      !}
      END


      SUBROUTINE SPECIES_CVFACTOR(filename,tspecies1,tspecies2,factor)
      !{
         implicit none
         include '../lib/inc/standard.inc'
         
         character*200 filename
         character*4   tspecies1,tspecies2
         real          factor

         logical       comment
         external      comment

         logical       found

         found = .false.

         open(dfile,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile,'(a100)',err=992,end=410) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(1:4).eq.tspecies1.and.line(10:13).eq.tspecies2)then
            !{
               read(line(19:),*) factor
               found = .true.
               goto 410
            !}
            end if
         !}
         end do
         if ( found .eqv. .false. ) goto 993

410      close (dfile)

         return

991      report(1) = 'Error 991c Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

993      report(1) = 'Error 993c Problem factor not found '
         report(2) = tspecies1
         report(3) = tspecies2
         go to 999

999   call stopreport(report)

      !}
      END
