************************************************************************
** populates the tables for the PERLND or IMPLND simulation           **
** using the scenario file and module catalogs                        **
**  This file contains the major subroutines called from the major    **
**    subroutine 'tables'                                             **
************************************************************************
      subroutine readcontrol_Lmodules(clu,lscen,lenlscen,perlnd,implnd,
     O                                modules,nmod)

      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/modules.inc'
      logical found,perlnd,implnd,comment
      character*11 tabname
      character*(*) clu

      integer i           ! index


      tabname = 'PER MODULES'
      if (implnd) tabname = 'IMP MODULES'

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:11).eq.tabname) then
          found = .false.
          read(dfile,'(a100)')line
          call d2x(line,last)
          i = 1
          do while (line(i:i+2).ne.'   '.and.i.lt.100)
            if (line(i:i+2).eq.clu) found = .true.
            i = i + 4 
          end do
          if (found) then     ! this table applies
            read(dfile,'(a100)')line
            call d2x(line,last)
            nmod = 0
            do while (comment(line).or.line(:15).ne.'END '//tabname)
              if (.not.comment(line)) then
                nmod = nmod + 1
                modules(nmod) = line(:last)
C              print*, 'main ',nmod, modules(nmod)    
              end if
              read(dfile,'(a100)')line
              call d2x(line,last)
            end do
          end if
        end if

        read(dfile,'(a100)')line
      end do

      close (dfile)

      return
            
************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'table: PARAMETERS   only allowed one line in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = 'table: PARAMETERS   not found in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


