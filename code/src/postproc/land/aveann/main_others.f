      implicit none

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'

      integer     wdmfil
      integer     wdmland,wdmprad,wdmmetx
      parameter   (wdmland=21)
      parameter   (wdmprad=22)
      parameter   (wdmmetx=23)

      integer     I_Year1,I_Year2
      integer     sdate(ndate),edate(ndate)
      data        sdate /0,1,1,0,0,0/
      data        edate /0,12,31,24,0,0/

      integer     ival
      real        F_AVG_Load

      character*4 C_VAR, t_C_VAR
      integer     nvar,I_VAR
      real        t_F_FAC
      character*3 C_lu

      character*25 pradscen
      integer     lenpradscen

      character*200 ofnam
      character*20  table
      integer       lentable

      character*3   ftype
      character*6   fmode

c      character*30 C30_ioscen
c      integer      lenioscen

      read(*,*) lscen,lseg,C_lu,I_Year1,I_Year2,table

      call lencl(lscen,lenlscen)
      call lencl(lseg, lenlseg)

      call readlcontrol_prad(lscen,lenlscen,pradscen)
      call lencl(pradscen,lenpradscen)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmland+5,wdmfnam,0,err)

      if ( C_lu .ne. 'xxx' ) then
         wdmfnam = outwdmdir//'land/'//C_lu//'/'//
     .         lscen(:lenlscen)//'/'//C_lu//lseg(:lenlseg)//'.wdm'
         print*,'>',wdmfnam,'<'
         call wdbopnlong(wdmland,wdmfnam,0,err)
         if (err.ne.0) go to 903
      endif

      wdmfnam = ScenDatDir//'climate/prad/'//
     .         pradscen(:lenpradscen)//'/prad_'//lseg(:lenlseg)//'.wdm'
      print*,'>',wdmfnam,'<'
      call wdbopnlong(wdmprad,wdmfnam,0,err)
      if (err.ne.0) go to 903

      sdate(1) = I_Year1
      edate(1) = I_Year2

      call readcontrol_Lioscen(lscen,lenlscen,ioscen)
      call lencl(ioscen,lenioscen)

      call lencl(table,lentable)
c      fnam = catdir//'iovars/'//'p600'//'/land_loads'
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/'//table(:lentable)
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 900

      ofnam = outdir//'land/aveann/'//lscen(:lenlscen)//'/'//C_lu//'_'//
     .       lseg(:lenlseg)//'_'//table(:lentable)//'.out'
      print*,ofnam
      open (dfile+1,file=ofnam,status='unknown',iostat=err)
      if (err.ne.0) go to 901

      if ( C_lu .eq. 'xxx' ) then
         write(dfile+1,'(A$)'),lseg(:lenlseg)
      else
         write(dfile+1,'(A,A,A$)'),lseg(:lenlseg),',',C_lu
      endif
      read(dfile,'(a100)',err=902) line
      !print*,line
      read(line,*)C_VAR,I_VAR
      do while ( C_VAR(:1) .ne. ' ' )
         print*,''
         print*,'->',C_VAR,I_VAR
         F_AVG_Load = 0
         do nvar=0,I_VAR-1
            read(line( 8+nvar*13: 8+nvar*13+3),*,err=902) t_C_VAR
            read(line(13+nvar*13:13+nvar*13+7),*,err=902) t_F_FAC
c            print*,'-->',t_C_VAR,' x',t_F_FAC
            fmode = 'DAILY '
            call FlowDSN(ioscen,lenioscen,t_C_VAR,dsn)
            wdmfil = wdmland
            if ( dsn .eq. -9999 ) then
               call DataDSN(ioscen,lenioscen,t_C_VAR,dsn,ftype,fmode)
               wdmfil = wdmprad
            end if
            print*,'-->',t_C_VAR,'(',dsn,')',' x',t_F_FAC
            !print*,C_VAR,'->',dsn
            !print*,sdate(1),sdate(2),sdate(3),sdate(4),sdate(5),sdate(6)
            !print*,edate(1),edate(2),edate(3),edate(4),edate(5),edate(6)

            if ( fmode .eq. 'HOURLY' ) then
               call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hval)
            else if ( fmode .eq. 'DAILY ' ) then
               call getdailydsn(wdmfil,sdate,edate,dsn,nvals,hval)
            else
               go to 904
            end if
            !print*,nvals

            do ival = 1,nvals
               F_AVG_Load = F_AVG_Load + hval(ival)*t_F_FAC
            end do
         end do
         F_AVG_Load = F_AVG_Load / ( I_Year2 - I_Year1 + 1 )
         write(dfile+1,'(A,E14.9$)') ',',F_AVG_Load


         read(dfile,'(a100)',err=902) line
         if (line(:3) .eq. 'end') goto 111
         read(line,*)C_VAR,I_VAR
      end do
111   write(dfile+1,*),''

      close(dfile)
      close(dfile+1)


      return

900   report(1) = 'Problem opening file '//fnam
      goto 999

901   report(1) = 'Problem opening file '//ofnam
      goto 999

902   report(1) = 'Problem reading line '// fnam
      goto 999

903   report(1) = 'Problem opening file'//wdmfnam
      goto 999

904   report(1) = 'Problem unknown time step '// fmode
      goto 999

999   call stopreport(report)

      end


      SUBROUTINE FlowDSN(ioscen,lenioscen,dsnstr,dsn)
      !{
         implicit none
         include '../../../lib/inc/standard.inc'
         include '../../../lib/inc/locations.inc'

         character*200 filename
c         integer       dfile
c         parameter     (dfile=10)
c         integer       err
c         character*200 line

         character*4   dsnstr
         integer       dsn

         logical       comment
         external      comment

         dsn = -9999

c         filename = catdir//'iovars/'//'p600'//'/perlnd'
         filename = catdir//'iovars/'//ioscen(:lenioscen)//'/perlnd'
         open(dfile+2,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile+2,'(a100)',err=992,end=310) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(26:29).eq.dsnstr) then
            !{
               read(line(:3),*) dsn
               go to 310
            !}
            end if
         !}
         end do
310      close (dfile+2)

         return

991      report(1) = 'Error 991b Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992b Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

999   call stopreport(report)

      !}
      END





      SUBROUTINE DataDSN(ioscen,lenioscen,dsnstr,dsn,ftype,fmode)
      !{
         implicit none
         include '../../../lib/inc/standard.inc'
         include '../../../lib/inc/locations.inc'

         character*200 filename
c         integer       dfile
c         parameter     (dfile=10)
c         integer       err
c         character*200 line

         character*4   dsnstr
         integer       dsn

         character*3   ftype
         character*6   fmode

         logical       comment
         external      comment

         dsn = -9999

c         filename = catdir//'iovars/'//'p600'//'/perlnd'
      filename = catdir//'iovars/'//ioscen(:lenioscen)//'/data_dsns_fix'
       print*,filename
         open(dfile+3,file=filename,status='old',iostat=err)
         if (err.ne.0) go to 991

         do
         !{
            read(dfile+3,'(a100)',err=992,end=310) line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:20).eq.'                    ') cycle
c            print*,line(26:29),'<>',C4_tStr
            if (line(9:12).eq.dsnstr) then
            !{
               read(line(:4),*) dsn
               read(line(27:29),*) ftype
               read(line(17:22),*) fmode
               go to 310
            !}
            end if
         !}
         end do
310      close (dfile+3)

         if (ftype .eq. 'PRE') ftype = 'ATD'

         return

991      report(1) = 'Error 991c Problem opening file:'
         report(2) = fnam
         write(report(3),*)'iostat error = ',err
         go to 999

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

999   call stopreport(report)

      !}
      END
