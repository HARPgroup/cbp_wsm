************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine getRvars(
     I                    ioscen,modules,nmod,
     O                    nRvar,Rname)
      implicit none

      include 'data.inc'


      integer nm     ! index

      character*4 name           !     catalog files
      character*6 Tmod                  ! temp variable for modules

      logical found,scompcase
      external scompcase

******** populate nRvar, Rdsn, Rname from 'rchres_in'
      call lencl(ioscen,lenioscen)
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_in'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nRvar = 0
      read(dfile,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod
            if (scompcase(Tmod,modules(nm))) found=.true.
          end do
          if (found) then
            nRvar = nRvar + 1
            Rname(nRvar) = name
          end if
        end if
        read(dfile,'(a100)') line
      end do

      close (dfile)

      return

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

999   call stopreport(report)

      end 
