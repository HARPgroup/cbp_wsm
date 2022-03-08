      implicit none

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/wdm.inc'

      integer     wdmfil
      parameter   (wdmfil=21)

      integer     I_year1,I_year2,I_YR
      integer     sdate(ndate),edate(ndate)
      data        sdate /0,1,1,0,0,0/
      data        edate /0,12,31,24,0,0/

      integer     tsdate(ndate),tedate(ndate)
      data        tsdate /0,1,1,0,0,0/
      data        tedate /0,12,31,24,0,0/

      integer     ival
      real        F_AVG_Load,F_ANN_LOAD(1984:2025,10)

      character*4 C_VAR, t_C_VAR
      integer     nvar,I_VAR,I_NCOL,I_COL
      real        t_F_FAC
      character*3 C_lu

      character*200 ofnam
      character*20  table
      integer       lentable

c      character*30 C30_ioscen
c      integer      lenioscen

      read(*,*) lscen,lseg,C_lu,I_year1,I_year2,table

      call lencl(lscen,lenlscen)
      call lencl(lseg, lenlseg)

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

      call lencl(table,lentable)
c      fnam = catdir//'iovars/'//'p600'//'/land_loads'
      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/'//table(:lentable)
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 900

      ofnam = outdir//'land/annual/'//lscen(:lenlscen)//'/'//C_lu//'_'//
     .       lseg(:lenlseg)//'.out'
      print*,ofnam
      open (dfile+1,file=ofnam,status='unknown',iostat=err)
      if (err.ne.0) go to 901

c      write(dfile+1,'(A,A,A$)'),lseg(:lenlseg),',',C_lu
      read(dfile,'(a100)',err=902) line
      !print*,line
      I_NCOL = 0
      read(line,*)C_VAR,I_VAR
      do while ( C_VAR(:1) .ne. ' ' )
         print*,''
         print*,'->',C_VAR,I_VAR
         I_NCOL = I_NCOL + 1
       do I_YR = sdate(1),edate(1)
         F_AVG_Load = 0
         do nvar=0,I_VAR-1
            read(line( 8+nvar*13: 8+nvar*13+3),*,err=902) t_C_VAR
            read(line(13+nvar*13:13+nvar*13+7),*,err=902) t_F_FAC
c            print*,'-->',t_C_VAR,' x',t_F_FAC
            call FlowDSN(ioscen,lenioscen,t_C_VAR,dsn)
            !print*,C_VAR,'->',dsn
            !print*,sdate(1),sdate(2),sdate(3),sdate(4),sdate(5),sdate(6)
            !print*,edate(1),edate(2),edate(3),edate(4),edate(5),edate(6)

            tsdate(1) = I_YR
            tedate(1) = I_YR
            call gethourdsn(wdmfil,tsdate,tedate,dsn,nvals,hval)
            !print*,nvals

            do ival = 1,nvals
               F_AVG_Load = F_AVG_Load + hval(ival)*t_F_FAC
            end do
         end do
         F_ANN_Load(I_YR,I_NCOL) = F_AVG_Load
c         F_AVG_Load = F_AVG_Load / ( I_Year2 - I_Year1 )
c         write(dfile+1,'(A,E10.5$)') ',',F_AVG_Load
       end do

         read(dfile,'(a100)',err=902) line
         if (line(:3) .eq. 'end') goto 111
         read(line,*)C_VAR,I_VAR
      end do

111   do I_YR = sdate(1),edate(1)
         write(dfile+1,'(A,A,A,A,I4$)'),lseg(:lenlseg),',',C_lu,',',I_YR
         do I_COL = 1,I_NCOL
            write(dfile+1,'(A,E10.5$)'),',',F_ANN_Load(I_YR,I_COL)
         end do
         write(dfile+1,*),''
      end do
c     write(dfile+1,*),''

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

992      report(1) = 'Error 992c Problem reading file: near line:'
         report(2) = fnam
         report(3) = line
         go to 999

999   call stopreport(report)

      !}
      END
