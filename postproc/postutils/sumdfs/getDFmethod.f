************************************************************************** gets the special action species and the PQUAL and IQUAL variables  **
**   that correspond to them for parameter modification               **
************************************************************************
      subroutine  getDFmethod(rscen,lenrscen,ndel,delname,
     O                        DFmethod)

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer ndelmax      ! maximum number of delivery variables
      parameter (ndelmax=10)
      integer ndel,nl
      character*4 delname(ndelmax)

      logical comment
      external comment

************ several different schemes for calculating Delivery
******** user specifies which one to use
      character*3 DFmethod(ndelmax)  ! possible values 'seg','bas','res'
      character*4 tload
      
*************************************************************************
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//
     .       '/load_delivery_method'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)',err=992,end=111) line
      call d2x(line,last)
      
      do while (line(:3).ne.'end')

        if (.not.comment(line)) then
          read(line,*) tload
          do nl = 1, ndel
            if (delname(nl) .eq. tload) then
              read(line,*,err=993) tload,DFmethod(nl)
              exit 
            end if
          end do
        end if

        read(dfile,'(a100)',err=992,end=111) line
        call d2x(line,last)
      end do

111   close(dfile)

      return

************* ERROR SPACE ******************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'error reading DF method in file'
      report(2) = fnam
      report(3) = line
      go to 999
 
999   call stopreport(report)
      end


