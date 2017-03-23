************************************************************************
** given the land scenario, finds the variable types in pltgens       **
************************************************************************
      subroutine readcontrol_vartypes(clu,lscen,maxtypes,
     O                                vartype,ntypes)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      character*3 clu             ! land use

      logical comment
      external comment

      logical pltgen

      integer maxtypes,ntypes
      character*4 vartype(maxtypes)

      integer i ! index

**************** END DECLARATIONS *************************
      

      call lencl(lscen,lenlscen)
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      ntypes = 0
      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        read(dfile,'(a100)')line
        call d2x(line,last)
        if (.not.comment(line).and.line(:6).eq.'PLTGEN') then
          read(dfile,'(a100)')line
          call d2x(line,last)
          do while (line(:10).ne.'END PLTGEN')
            i = 1
            pltgen = .false.
            do while (line(i:i+2).ne.'   '.and.i.lt.100)
              if (line(i:i+2).eq.clu) then
                ntypes = ntypes + 1
                pltgen = .true.
              end if
              i = i + 4
            end do
            read(dfile,'(a100)')line
            if (pltgen) then
              call d2x(line,last)
              read(line,*) vartype(ntypes)
            end if
            read(dfile,'(a100)')line
            call d2x(line,last)
          end do
        end if
      end do

      close(dfile)

      return
************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)
      end

