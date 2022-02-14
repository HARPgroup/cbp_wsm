************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine getrivfacs(module,lenmod,calscen,lencalscen,
     M                      limitsKATRAD)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      logical found,comment
      logical foundlimits
      character*11 tabname
      character*3 templu
      character*6 tempar

      integer i           ! index
      integer nc

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/PSTEMP/'//calscen(:lencalscen)//
     .       '/'//calscen(:lencalscen)//'_rfactors.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundlimits = .false.
      
      read(dfile,'(a100)',err=992,end=9931)line
      call d2x(line,last)
      do while (line(:3).ne.'end')

        if (.not.comment(line).and.line(:6).eq.'LIMITS') then
          foundlimits = .true.
          read(dfile,'(a100)',err=992,end=9931)line
          call d2x(line,last)
          do while (comment(line).or.line(:10).ne.'END LIMITS')
            read(line,*,err=992,end=993) tempar
            call shift(line)
            if (.not.comment(line)) then
              if (tempar.eq.'KATRAD') then
                read(line,*,err=992,end=993) (limitsKATRAD(i),i=1,4)
              else
                go to 995
              end if
            end if
            read(dfile,'(a100)',err=992,end=9931)line
            call d2x(line,last)
          end do
        end if
        
        read(dfile,'(a100)')line
        call d2x(line,last)
      end do

      close (dfile)

      if (.not.foundlimits) then
        tabname = 'LIMITS'
        go to 994
      end if

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

993   report(1) = 'problem reading file, EOL found unexpectedly'
      report(2) = fnam
      report(3) = line
      go to 999

9931  report(1) = 'file ended without finding literal =>end'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'table: '//tabname//' not found in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'problem reading: variable name not understood' 
      report(2) = fnam
      report(3) = line
      go to 999

996   report(1) = 'problem: land use '//templu//' not understood'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      stop

      end


