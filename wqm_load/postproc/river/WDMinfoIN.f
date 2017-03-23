************************************************************************
******* Routine to populate nRvar, Rdsn, Rname                        **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
      subroutine WDMinfoIN(rscen,Nexits,
     O                   nRvar,Rdsn,Rname)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/masslinks.inc'
      include '../../lib/inc/modules.inc'

      integer dsn                            ! temp variables for reading
      character*4 name,nameR                  !     catalog files
 
      character*6 Tmod  ! temp reading variable

      logical found,scompcase
      external scompcase

      integer Nexits  ! number of exits

      integer nm ! index

*************** END DECLARATIONS
      call lencl(rscen,lenrscen)
      call readcontrol_modules(rscen,lenrscen,modules,nmod)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

******** populate nRvar, RdsnOut, Rname from 'rchres_out_1x'
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_in'  
c      call lencl(fnam,last)
c      write(fnam(last-1:last-1),'(i1)') Nexits
      open(11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 992

      nRvar = 0
      read(11,'(a100)') line
      do while (line(:3).ne.'end')
        read(line,'(a6,1x,i3,24x,a4)',iostat=err) Tmod,dsn,name
        if (err.eq.0.and.dsn.ne.0) then
          found = .false.
          do nm = 1,nmod
            if (scompcase(Tmod,modules(nm))) found=.true.
          end do
          if (found) then
            nRvar = nRvar + 1
            if (nRvar.gt.MaxRvar) go to 991
            Rdsn(nRvar) = dsn
            Rname(nRvar) = name
          end if
        end if
        read(11,'(a100)') line
      end do

      close (11)

      return

**************  ERROR SPACE  *******************************************
991   report(1) = 'Too many dsns in file:'
      report(2) = fnam
      write(report(3),*)
     .     ' raise maxRvar in Rvars.inc above ',maxRvar
      go to 999

992   report(1) = 'file not found'
      report(2) = fnam
      write(report(3),*)'error code = ',err
      go to 999

999   call stopreport(report)

      end 
