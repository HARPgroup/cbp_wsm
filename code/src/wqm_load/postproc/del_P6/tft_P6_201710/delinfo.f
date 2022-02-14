************************************************************************
**  subroutine to get delivery information                            **
**    1.  Which loads get the calculatation                           **
**    2.  Index of species to total mass so that all loads can be     **
**        delivered                                                   **
************************************************************************
      subroutine delinfo(
     I                   ioscen,lenioscen,nloads,loadname,
     O                   ndel,delname,load2del)
      implicit none

      include 'tfs.inc'

      character*4 Tdelload

      integer nl,j,i,ns

      logical comment,found

      fnam=catdir//'iovars/'//ioscen(:lenioscen)//'/delivery_ratio_calc'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do nl = 1,nloads
        load2del(nl) = -1
      end do


      ndel = 0
      read(dfile,'(a100)',err=992,end=992)line
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          read(line(6:6),'(i1)',err=888) ns
          if (ns.ne.0) then        ! found a line
            found = .false.
            do nl = 1,nloads
              if (line(:4).eq.loadname(nl)) then
                found = .true.
              end if
            end do

            if (found) then  ! load for this del is active
              ndel = ndel + 1
              delname(ndel) = line(:4)
              do j = 1,ns
                read(line(3+j*5:6+j*5),'(a4)') Tdelload
                do nl = 1,nloads
                  if (loadname(nl).eq.Tdelload) load2del(nl) = ndel
                end do
              end do
            end if
          end if
        end if
888     read(dfile,'(a100)')line
      end do

      do nl = 1,nloads                     ! check for completeness
        if (load2del(nl).le.0) go to 993
      end do

      return

********************************** ERROR SPACE *************************
991   report(1) = '  Problem opening file '
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = ' Problem reading file near line'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = ' load XXXX not found '
      write(report(1)(7:10),'(a4)') loadname(nl)
      report(2) = 'all loads in pp/catalog/iovars/rchres_out_to_load'
      report(3) = ' must be in pp/catalog/iovars/delivery_ratio_calc' 
      go to 999

999   call stopreport(report)

      end
  


