************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine getlufacs(module,lenmod,calscen,lencalscen,
     M            limitsLGTP1,limitsASLT,limitsULTP1,BSLT,ULTP2)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'tempcal.inc'
      logical found,comment
      logical foundslopes,foundlimits
      character*11 tabname
      character*3 templu
      character*6 tempar

      integer i           ! index
      integer nc

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/PSTEMP/'//calscen(:lencalscen)//
     .       '/'//calscen(:lencalscen)//'_factors.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundlimits = .false.
      foundslopes = .false.
      
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
              if (tempar.eq.'ASLT  ') then
                read(line,*,err=992,end=993) (limitsASLT(i),i=1,4)
              else if (tempar.eq.'ULTP1 ') then
                read(line,*,err=992,end=993) (limitsULTP1(i),i=1,4)
              else if (tempar.eq.'LGTP1 ') then
                read(line,*,err=992,end=993) (limitsLGTP1(i),i=1,4)
              else
                go to 995
              end if
            end if
            read(dfile,'(a100)',err=992,end=9931)line
            call d2x(line,last)
          end do
        end if
        
        if (.not.comment(line).and.line(:6).eq.'SLOPES') then
          foundslopes = .true.
          read(dfile,'(a100)',err=992,end=9931)line
          do while (comment(line))
            read(dfile,'(a100)',err=992,end=9931)line
          end do
          call d2x(line,last)
          nc = 0
          call shift(line)
          do           ! find column order
            read(line,*,err=992,end=1002) templu
            if (templu.eq.'   ') exit
            nc = nc + 1
            found = .false.
            do i = 1,nlu
              if (templu.eq.luname(i)) then
                found = .true.
                columnorder(nc) = i
              end if
            end do
            if (.not.found) go to 996
            call shift(line)
          end do
1002      read(dfile,'(a100)',err=992,end=9931)line
          call d2x(line,last)
          do while (comment(line).or.line(:10).ne.'END SLOPES')
            read(line,*,err=992,end=993) tempar
            if (.not.comment(line)) then
              call shift(line)
              if (tempar.eq.'BSLT  ') then
                read(line,*,err=992,end=993) 
     .                 (BSLT(columnorder(i)),i=1,nc)
              else if (tempar.eq.'ULTP2 ') then
                read(line,*,err=992,end=993) 
     .                 (ULTP2(columnorder(i)),i=1,nc)
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
      if (.not.foundslopes) then
        tabname = 'SLOPES'
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


