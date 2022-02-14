
      subroutine gettransport_res_eff(rscen,rseg,facQ,facN,facP,facS)

      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'


      logical  comment
      external comment

      character*25 tranfile
      integer      lastran
      logical      findtran
      data         findtran / .false. /

      character*200 dline

      integer      ncons
      parameter    (ncons = 4)
      character*3  cons(ncons)
      data         cons /'flo','tnx','tpx','sed'/
      integer      icons(4)
      data         icons /1,2,3,4/

      double precision         factors(ncons)
      double precision         facQ,facN,facP,facS

      logical      findrseg
      data         findrseg / .false. /
      character*13 trseg
      double precision         tfac(ncons)

      integer      i,j,x
      character*20 ctemp

      call lencl(rscen,lenrscen)

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
c      print*,fnam
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
         read(dfile,'(a100)',err=1001)line
         call d2x(line,last)
         if (.not.comment(line)) then

            if (line(:9).eq.'TRANSPORT') then
               findtran = .true.
               read(dfile,'(a25)') tranfile
               read(dfile,'(a100)',err=1001)line
               if (line(:13).ne.'END TRANSPORT') then
                  print*,' control file ',fnam,
     .               ' only allowed one row in table TRANSPORT'
                  call stopit(fnam,'TRANSPORT')
               end if

            end if
         end if
      end do
      close (dfile)

      if (.not.findtran) call stopit(fnam,'TRANSPORT')



      call trims(tranfile,lastran)
      fnam = pardir//'transport/'//tranfile(:lastran)//'_res.csv'
      open (dfile+1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      print*,'begin reading '//fnam
      read(dfile+1,'(a200)',err=996) dline  ! read header line
c      print*,dline
      call d2x(dline,i)

      call findcomma(dline,x)
      ctemp = dline(:x-1)
      call shift(dline)

      !print*,(cons(i),i=1,ncons)
      print*,(cons(i),' ',i=1,ncons),'->',(icons(i),i=1,ncons)

      do i = 1,ncons
         call findcomma(dline,x)
         ctemp = dline(:x-1)
         !print*,'~',ctemp(:3)
         do j = 1,ncons
            if (ctemp(:3) .eq. cons(j)) then
               !print*,'#',cons(j)
               !print*,' < ',icons(i)
               icons(i) = j
               !print*,' > ',i,j,icons(i)
               exit
            end if
         end do
         call shift(dline)
      end do

c      print*,(icons(i),i=1,ncons)
      print*,(cons(i),' ',i=1,ncons),'->',(icons(i),i=1,ncons)

      do i = 1,ncons
         factors(icons(i)) = 1.0D0
      end do

      read(dfile+1,'(a200)',err=996)dline
c      print*,dline
      do while (dline(:3).ne.'end')
         read(dline,*)trseg,(tfac(i),i=1,ncons)
         if (trseg .eq. rseg) then
c            findrseg = .true.
            do i = 1,ncons
               factors(icons(i)) = tfac(i)
            end do
            !print*,trseg,(tfac(i),i=1,ncons)
            exit
         end if
         read(dfile+1,'(a200)',err=996)dline
c         print*,dline(:3).ne.'end'.and. findrseg .eqv. .false.
c         read(dfile,*,err=996)trseg,(tfac(i),i=1,ncons)
      end do
c      print*,(factors(i),i=1,ncons)

      close(dfile+1)

      print*,'done reading '//fnam 

      facQ = factors(icons(1))
      facN = factors(icons(2))
      facP = factors(icons(3))
      facS = factors(icons(4))

      return



991   report(1) = 'problem opening Res Passthru Eff file'
      report(2) = fnam
      report(3) = ' '
      go to 999

996   report(1) = 'problem reading Res Passthru Eff file'
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Error reading Riv Control file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999


999   call stopreport(report)
      
      end
