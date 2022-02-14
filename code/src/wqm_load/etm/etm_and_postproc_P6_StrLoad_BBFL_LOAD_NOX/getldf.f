      subroutine getLvar(
     I                     ioscen,lenioscen,
     O                     I_Lvar,C_Lvar,F_Lvar)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      integer maxLvar
      parameter (maxLvar = 50)
      integer     I_Lvar
      character*4 C_Lvar(maxLvar)
      real        F_Lvar(maxLvar)
      integer iLvar

      logical comment
      external comment

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lvars'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990

      iLvar = 1
      do
         read(dfile,'(a100)',err=991,end=101) line
         call d2x(line,last)
         if (comment(line)) cycle
         if (line(:20).eq.'                    ') cycle

         read(line(1:4 ),*) C_Lvar(iLvar)
         read(line(6:11),*) F_Lvar(iLvar)

         iLvar = iLvar + 1
      end do

101   close(dfile)

      I_Lvar = iLvar - 1

      return

990   report(1) = 'Error 990 Problem opening file:'
      report(2) = fnam
      write(report(3),*)'iostat error = ',err
      go to 999

991   report(1) = 'Error 991 Problem reading file:'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)

      end





      subroutine getldf(
     I                     rscen,lenrscen,
     I                     lseg,lenlseg,
     I                     I_Lvar,C_Lvar,F_Lvar,
     O                     F_ldf)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      integer ilu,ilu1,ilu2
      character*3 clu(nlu)

      integer maxLvar,iLvar
      parameter (maxLvar = 50)
      integer     I_Lvar
      character*4 C_Lvar(maxLvar)
      real        F_Lvar(maxLvar)

      integer  I_ldf
      real     F_ldf(maxLvar,nlu)
      real     F_tldf(nlu)

      logical comment
      external comment

      character*6 clseg

      logical found

      character*30 scenfac
      integer lenscenfac

      call readcontrol_scenfac(rscen,lenrscen,
     .                        'SCENARIO FACTOR',15,
     .                        'END SCENARIO FACTOR',19,
     .                        scenfac)
      call lencl(scenfac,lenscenfac)
      print*,scenfac(:lenscenfac)
      
      do iLvar = 1,I_Lvar
         do ilu = 1,nlu
            F_ldf(iLvar,ilu) = 1.0
         end do
c....... if F_Lvar > 0 then use it a multiplication factor
         if (F_Lvar(iLvar) .gt. 0) then
            print*,'... > ',C_Lvar(iLvar),F_Lvar(iLvar)
            do ilu = 1,nlu
               F_ldf(iLvar,ilu) = F_Lvar(iLvar)
            end do
c....... else read the factor
         else
            print*,'... < ',C_Lvar(iLvar),F_Lvar(iLvar)
            ! call read datafile, e.g., input/scenario/river/ldf/ldf_SEDM_scenario.csv
c            do ilu = 1,nlu
c               F_ldf(iLvar,ilu) = F_Lvar(1)
c            end do
            fnam = ScenDatDir//'river/scenfac/scenfac_'//C_Lvar(iLvar)
     .             //'_'//scenfac(:lenscenfac)//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 990

c            read(dfile,'(a100)',err=991) line
c            call d2x(line,last)
c            if (comment(line)) cycle
c            if (line(:20).eq.'                    ') cycle

            read(dfile,*,err=991) clseg,(clu(ilu),ilu=1,nlu-1)
c            print*,clseg,(',',clu(ilu),ilu=1,nlu-1)

            found = .false.
            do
               read(dfile,*,err=991)clseg,(F_tldf(ilu),ilu=1,nlu-1)
c               print*,'<',clseg,'> <',lseg(:lenlseg),'>'
               if ( clseg .eq. lseg(:lenlseg) ) then
                  do ilu1 = 1,nlu
                     do ilu2 = 1,nlu
                        if (luname(ilu1).eq.clu(ilu2)) then
                           F_ldf(iLvar,ilu1) = F_tldf(ilu2)
                        end if
                     end do
                     print*,'... < ',C_Lvar(iLvar),' ',luname(ilu1),
     .                      F_ldf(iLvar,ilu1)
                  end do
                  found = .true.
                  go to 111
               end if
            end do
111         close(dfile)
            if ( .not.found ) go to 998
         end if
      end do

      return

990   report(1) = 'Error 990 Problem opening file:'
      report(2) = fnam
      write(report(3),*)'iostat error = ',err
      go to 999

991   report(1) = 'Error 991 Problem reading file:'
      report(2) = fnam
      report(3) = line
      go to 999

998   report(1) = 'Error 998 Land segment not found in file'
      report(2) = lseg(:lenlseg)//'-'//clseg(:lenlseg)
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end





      subroutine readcontrol_scenfac(
     I                            rscen,lenrscen,
     I                            C_STABLE, I_STABLE,
     I                            C_ETABLE, I_ETABLE,
     O                            LBfile)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      character*40 LBfile

      character*100 templine

      logical comment

      integer nLB,l,n,i

      logical findload

      character*40 C_STABLE, C_ETABLE
      integer      I_STABLE, I_ETABLE

*************** END OF DECLARATION *************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      findload = .false.
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
        if (.not.comment(line)) then

C          print*,'->',line(:I_STABLE),':',C_STABLE(:I_STABLE),'<-'
          if (line(:I_STABLE).eq.C_STABLE(:I_STABLE)) then
            findload = .true.
            nLB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:I_ETABLE).ne.C_ETABLE(:I_ETABLE))
C              print*,'->',line(:I_ETABLE),':',C_ETABLE(:I_ETABLE),'<-'
              if (.not.comment(line)) then
                 read(line,1234)
     .              LBfile
              end if
              read(dfile,'(a100)',err=1001)line
            end do
          end if

        end if
      end do

      close (dfile)

      if (.not.findload) go to 992

c1234  format(i4,i3,i3,1x,a40)
1234  format(a40)

      return
*********** ERROR SPACE ************************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'did not find '//C_STABLE(:I_STABLE)//
     .             ' line in control file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end
