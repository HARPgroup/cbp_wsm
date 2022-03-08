************************************************************************
**  reads the control file and gets the parameters for this scenario  **
************************************************************************
      subroutine getlufacs(calscen,
     O      lufacLZSN,lufacINFILT,lufacIRC,lufacAGWR,
     O      lufacINTFW,lufacAGWETP,lufacKVARY,columnorder,
     O      limitsLandEvap,limitsLZSN,limitsINFILT,limitsIRC,limitsAGWR,
     O      limitsINTFW,limitsAGWETP,limitsKVARY,UZSNfac,monthlyUZSN,
     O      monthlyUZSNfac,SUROtargets)

      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include 'calib.inc'
      logical found,comment
      logical foundsuro,foundLE, foundmonuzsn, founduzsn, foundlimits,
     .          foundparams
      character*11 tabname
      character*3 templu
      character*6 tempar

      character*200 tline

      integer i           ! index
      integer nc

      call lencl(calscen,lencalscen)
      fnam = controldir//'calib/PWATER/'//calscen(:lencalscen)//
     .       '/'//calscen(:lencalscen)//'_factors.dat'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      foundparams = .false.
      foundsuro = .false.
      foundLE = .false.
      foundmonuzsn = .false.
      founduzsn = .false.
      foundlimits = .false.
      

      read(dfile,'(a200)',err=992,end=9931)tline
      call d2x(tline,last)
      do while (tline(:3).ne.'end')

        if (.not.comment(tline).and.tline(:10).eq.'PARAMETERS') then
          foundparams = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          do while (comment(tline))
            read(dfile,'(a200)',err=992,end=9931)tline
          end do
          call d2x(tline,last)
          call shift(tline)  ! get rid of 'param'
          nc = 0
          do           ! find column order
            read(tline,*,err=992,end=1001) templu
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
            call shift(tline)
          end do
1001      read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while(comment(tline).or.tline(:14).ne.'END PARAMETERS')
            read(tline,*,err=992,end=993) tempar
            call shift(tline)
            if (.not.comment(tline)) then
              if (tempar.eq.'LZSN  ') then
                read(tline,*,err=992,end=993) 
     .               (lufacLZSN(columnorder(i)),i=1,nc)
              else if (tempar.eq.'AGWR  ') then
                read(tline,*,err=992,end=993) 
     .               (lufacAGWR(columnorder(i)),i=1,nc)
              else if (tempar.eq.'IRC   ') then
                read(tline,*,err=992,end=993) 
     .               (lufacIRC(columnorder(i)),i=1,nc)
              else if (tempar.eq.'INTFW ') then
                read(tline,*,err=992,end=993) 
     .               (lufacINTFW(columnorder(i)),i=1,nc)
              else if (tempar.eq.'INFILT') then
                read(tline,*,err=992,end=993) 
     .               (lufacINFILT(columnorder(i)),i=1,nc)
              else if (tempar.eq.'AGWETP') then
                read(tline,*,err=992,end=993) 
     .               (lufacAGWETP(columnorder(i)),i=1,nc)
              else if (tempar.eq.'KVARY ') then
                read(tline,*,err=992,end=993) 
     .               (lufacKVARY(columnorder(i)),i=1,nc)
              else
                go to 995
              end if
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        if (.not.comment(tline).and.tline(:6).eq.'LIMITS') then
          foundlimits = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while (comment(tline).or.tline(:10).ne.'END LIMITS')
            read(tline,*,err=992,end=993) tempar
            call shift(tline)
            if (.not.comment(tline)) then
              if (tempar.eq.'LZSN  ') then
                read(tline,*,err=992,end=993) (limitsLZSN(i),i=1,4)
              else if (tempar.eq.'AGWR  ') then
                read(tline,*,err=992,end=993) (limitsAGWR(i),i=1,4)
              else if (tempar.eq.'IRC   ') then
                read(tline,*,err=992,end=993) (limitsIRC(i),i=1,4)
              else if (tempar.eq.'INTFW ') then
                read(tline,*,err=992,end=993) (limitsINTFW(i),i=1,4)
              else if (tempar.eq.'INFILT') then
                read(tline,*,err=992,end=993) (limitsINFILT(i),i=1,4)
              else if (tempar.eq.'AGWETP') then
                read(tline,*,err=992,end=993) (limitsAGWETP(i),i=1,4)
              else if (tempar.eq.'KVARY ') then
                read(tline,*,err=992,end=993) (limitsKVARY(i),i=1,4)
              else
                go to 995
              end if
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        if (.not.comment(tline).and.tline(:11).eq.'UZSN FACTOR') then
          founduzsn = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          do while (comment(tline))
            read(dfile,'(a200)',err=992,end=9931)tline
          end do
          call d2x(tline,last)
          nc = 0
          do           ! find column order
            read(tline,*,err=992,end=1002) templu
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
            call shift(tline)
          end do
1002      read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while (comment(tline).or.tline(:15).ne.'END UZSN FACTOR')
            if (.not.comment(tline)) then
              read(tline,*,err=992,end=993) 
     .                 (UZSNfac(columnorder(i)),i=1,nc)
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        if (.not.comment(tline).and.tline(:12).eq.'MONTHLY UZSN') then
          foundmonuzsn = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while (comment(tline).or.tline(:16).ne.'END MONTHLY UZSN')
            if (.not.comment(tline)) then
              read(tline,*,err=992,end=993) templu
              found = .false.
              do i = 1,nlu
                if (templu.eq.luname(i)) then
                  found = .true.
                  monthlyUZSN(i) = .true.
                  call shift(tline)
                  read(tline,*,err=992,end=993)
     .                    (monthlyUZSNfac(i,nc),nc=1,12)
                  exit
                end if
              end do
              if (.not.found) go to 996
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        if (.not.comment(tline).and.tline(:9).eq.'LAND EVAP') then
          foundLE = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while (comment(tline).or.tline(:13).ne.'END LAND EVAP')
            if (.not.comment(tline)) then
              read(tline,*,err=992,end=993) (limitsLandEvap(i),i=1,4)
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        if (.not.comment(tline).and.tline(:4).eq.'SURO') then
          foundsuro = .true.
          read(dfile,'(a200)',err=992,end=9931)tline
          call d2x(tline,last)
          do while(comment(tline).or.tline(:8).ne.'END SURO')
            if (.not.comment(tline)) then
              read(tline,*,err=992,end=993) (SUROtargets(i),i=1,2)
            end if
            read(dfile,'(a200)',err=992,end=9931)tline
            call d2x(tline,last)
          end do
        end if
        
        read(dfile,'(a200)')tline
        call d2x(tline,last)
      end do

      close (dfile)

      if (.not.foundparams) then
        tabname = 'PARAMETERS'
        go to 994
      end if
      if (.not.foundsuro) then
        tabname = 'SURO'
        go to 994
      end if
      if (.not.foundLE) then
        tabname = 'LAND EVAP'
        go to 994
      end if
      if (.not.foundmonuzsn) then
        tabname = 'MONTHLY UZSN'
        go to 994
      end if
      if (.not.founduzsn) then
        tabname = 'UZSN FACTOR'
        go to 994
      end if
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

992   report(1) = 'problem reading file: near tline:'
      report(2) = fnam
      report(3) = tline
      go to 999

993   report(1) = 'problem reading file, EOL found unexpectedly'
      report(2) = fnam
      report(3) = tline
      go to 999

9931  report(1) = 'file ended without finding literal =>end'
      report(2) = fnam
      report(3) = tline
      go to 999

994   report(1) = 'table: '//tabname//' not found in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

995   report(1) = 'problem reading: variable name not understood' 
      report(2) = fnam
      report(3) = tline
      go to 999

996   report(1) = 'problem: land use '//templu//' not understood'
      report(2) = fnam
      report(3) = tline
      go to 999

999   call stopreport(report)
      stop

      end


