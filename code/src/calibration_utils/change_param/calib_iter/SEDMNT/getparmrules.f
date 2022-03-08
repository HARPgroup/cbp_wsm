************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine getparmrules(calscen,clu,
     O                        tarscen,lufacAFFIX,lufacNVSI,lufacKSKR,
     O                        limitsKRER,limitsKSER)

      implicit none
      include 'calib_sed.inc'

      character*11 tabname
      character*3 templu
      character*6 tempar
      character*(*) tarscen
      character*(*) clu       ! land use

      real value

      integer i           ! index

      logical comment
      external comment

      logical foundlimits,foundparams,foundtarget
      logical foundAFFIX,foundNVSI,foundKSKR
************** END DECLARATION ****************************************

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/SEDMNT/'//calscen(:lencalscen)
     .          //'/sed_parms.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundparams = .false.
      foundlimits = .false.
      foundtarget = .false.
      
      read(dfile,'(a100)',err=992,end=9931)line
      call d2x(line,last)

      do 
        read(dfile,'(a100)',err=992,end=111)line
        call d2x(line,last)
        if (comment(line)) cycle

        if (line(:10).eq.'PARAMETERS') then
          foundparams = .true.
          foundAFFIX = .false.
          foundNVSI = .false.
          foundKSKR = .false.
          do 
            read(dfile,'(a100)',err=992,end=9931)line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:14).eq.'END PARAMETERS') exit

            read(line,*,err=992,end=993) templu,tempar,value
            call lowercase(templu)

            if (templu.eq.clu.or.templu.eq.'all') then
              if (tempar.eq.'AFFIX') then
                lufacAFFIX = value 
                foundAFFIX = .true.
              else if (tempar.eq.'NVSI ') then
                lufacNVSI = value
                foundNVSI = .true.
              else if (tempar.eq.'KS-KR') then
                lufacKSKR = value
                foundKSKR = .true.
              else
                go to 995
              end if
            end if
          end do
          if (.not.foundAFFIX) go to 987
          if (.not.foundNVSI) go to 988
          if (.not.foundKSKR) go to 989
        end if  ! end if parameters found
        
        if (line(:6).eq.'TARGET') then
          foundtarget = .true.
          do 
            read(dfile,'(a100)',err=992,end=9931)line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:10).eq.'END TARGET') exit
            read(line,*,err=992,end=993) tarscen
          end do
        end if 
          
        if (line(:6).eq.'LIMITS') then
          foundlimits = .true.
          do 
            read(dfile,'(a100)',err=992,end=9931)line
            call d2x(line,last)
            if (comment(line)) cycle
            if (line(:10).eq.'END LIMITS') exit
            read(line,*,err=992,end=993) tempar
            if (tempar.eq.'KRER  ') then
              read(line,*,err=992,end=993) tempar,(limitsKRER(i),i=1,4)
            else if (tempar.eq.'KSER  ') then
              read(line,*,err=992,end=993) tempar,(limitsKSER(i),i=1,4)
            else
              go to 995
            end if
          end do
        end if
     
      end do               ! finish reading the whole file

111   close (dfile)

      if (.not.foundparams) then
        tabname = 'PARAMETERS'
        go to 994
      end if

      if (.not.foundtarget) then
        tabname = 'TARGET'
        go to 994
      end if

      if (.not.foundlimits) then
        tabname = 'LIMITS'
        go to 994
      end if

      return
            
************* ERROR SPACE ****************
987   report(1) = 'variable AFFIX not found in PARAMETER table in file'
      report(2) = fnam
      report(3) = ''
      go to 999

988   report(1) = 'variable NVSI not found in PARAMETER table in file'
      report(2) = fnam
      report(3) = ''
      go to 999

989   report(1) = 'variable KS-KR not found in PARAMETER table in file'
      report(2) = fnam
      report(3) = ''
      go to 999

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

999   call stopreport(report)
     
      end


