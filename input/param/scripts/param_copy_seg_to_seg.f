      implicit none
      integer nplu,nilu,nptab,nitab
      parameter (nplu=3,nilu=1,nptab=3,nitab=3)
      character*3 plu(nplu),ilu(nilu)
      character*10 ptab(nptab),itab(nitab)

      data plu /'crp','for','pur'/
      data ilu /'imp'/
      data ptab /'ATEMP.csv','PWATER.csv','SNOW.csv'/
      data itab /'ATEMP.csv','IWATER.csv','SNOW.csv'/

      integer nl,nt,i,last

      character*1000 line,targetline
      character*100 fnam

      character*6 oldseg,newseg

      logical found

      print*,'old segment?'
      read*,oldseg
      print*,'new segment?'
      read*,newseg

      do nl = 1,nplu
        do nt = 1,nptab
          fnam = plu(nl)//'/initial/'//ptab(nt)
          open(11,file=fnam,status='unknown')
          found = .false.
          line = 'go hoos'
          do while (line(:3).ne.'end')
            read(11,'(a1000)')line
            if (line(:6).eq.oldseg) then
              found = .true.
              targetline = line
            end if
          end do
          targetline(:6) = newseg
          backspace 11
          call lencl(targetline,last)
          write(11,'(a)')targetline(:last)
          call lencl(line,last)
          write(11,'(a)')line(:last)
          close(11)
          if (.not.found) go to 999
        end do
      end do

      do nl = 1,nilu
        do nt = 1,nitab
          fnam = ilu(nl)//'/initial/'//itab(nt)
          open(11,file=fnam,status='unknown')
          found = .false.
          line = 'go hoos'
          do while (line(:3).ne.'end')
            read(11,'(a1000)')line
            if (line(:6).eq.oldseg) then
              found = .true.
              targetline = line
            end if
          end do
          targetline(:6) = newseg
          backspace 11
          call lencl(targetline,last)
          write(11,'(a)')targetline(:last)
          call lencl(line,last)
          write(11,'(a)')line(:last)
          close(11)
          if (.not.found) go to 999
        end do
      end do

C      fnam = '../data/land_met_wdm.csv'
C      open(11,file=fnam,status='unknown')
C      found = .false.
C      line = 'go hoos'
C      do while (line(:3).ne.'end')
C        read(11,'(a1000)')line
C        if (line(:6).eq.oldseg) then
C          found = .true.
C          targetline = line
C        end if
C      end do
C      targetline(:6) = newseg
C      backspace 11
C      call lencl(targetline,last)
C      write(11,'(a)')targetline(:last)
C      call lencl(line,last)
C      write(11,'(a)')line(:last)
C      close(11)
C      if (.not.found) go to 999
C
C      fnam = '../data/land_prad_wdm.csv'
C      open(11,file=fnam,status='unknown')
C      found = .false.
C      line = 'go hoos'
C      do while (line(:3).ne.'end')
C        read(11,'(a1000)')line
C        if (line(:6).eq.oldseg) then
C          found = .true.
C          targetline = line
C        end if
C      end do
C      targetline(:6) = newseg
C      backspace 11
C      call lencl(targetline,last)
C      write(11,'(a)')targetline(:last)
C      call lencl(line,last)
C      write(11,'(a)')line(:last)
C      close(11)
C      if (.not.found) go to 999
C
      fnam = 'common/initial/'//'land_evap.csv'
      open(11,file=fnam,status='unknown')
      found = .false.
      line = 'go hoos'
      do while (line(:3).ne.'end')
        read(11,'(a1000)')line
        if (line(:6).eq.oldseg) then
          found = .true.
          targetline = line
        end if
      end do
      targetline(:6) = newseg
      backspace 11
      call lencl(targetline,last)
      write(11,'(a)')targetline(:last)
      call lencl(line,last)
      write(11,'(a)')line(:last)
      close(11)
      if (.not.found) go to 999

      stop

999   print*,'could not find ',oldseg
      print*,' in file ',fnam

      end

      subroutine lencl(line,last)
      implicit none
      character*(*)line
      integer i,last
      do i = 1,len(line)
        if (line(i:i).ne.' ')last=i
      end do
      end

         

