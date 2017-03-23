************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine readcontrol_tree2paramscen(tree2,lentree2,lscen,
     O                                      paramscen)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      logical found,comment,foundparam
      character*11 tabname
      character*(*) tree2
      integer lentree2

      integer i           ! index

      call lencl(lscen,lenlscen)
      fnam = '/model/'//tree2(:lentree2)//'/run/control/land/'
     .        //lscen(:lenlscen)//'.con'
	print*,fnam
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundparam = .false.

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:10).eq.'PARAMETERS') then
          foundparam = .true.
          read(dfile,'(a100)',err=992)line
          call d2x(line,last)
          paramscen = line(:last)
          read(dfile,'(a100)',err=992)line
          call d2x(line,last)
          if (line(:14).ne.'END PARAMETERS') go to 993
        end if
        
        read(dfile,'(a100)')line
      end do

      close (dfile)

      if (.not.foundparam) go to 994

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

999   print*,report(1)
      print*,report(2)
      print*,report(3)

      end


