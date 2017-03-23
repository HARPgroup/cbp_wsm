************************************************************************
** subroutines to read refractory storage adjustment                  **
************************************************************************
      subroutine readrorgn(
     I                     lscen,lenlscen,lseg,lenlseg,clu,
     O                     lsegron)

      implicit none
      include 'lug.inc'
      include 'acts.inc'

      logical found
      integer i,ns,nly

      character*6 Tlseg
      real lsegron(4)

**************END DECLARATION ******************************************
      call readcontrol_Lparamscen(
     I                            lscen,lenlscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

************ open RORGN file
      fnam= pardir//clu//'/'//paramscen(:lenparamscen)//'/RORGN.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      read(dfile,'(a100)',err=992,end=992) line     ! read header line
      call d2x(line,i)

*************** loop over all segments in file, look for active segs
      found = .false.
      read(dfile,'(a100)',err=992) line            
      call d2x(line,i)

      do while (line(:3).ne.'end')
        read(line,*) Tseg
        if (Tseg .eq. lseg(:lenlseg)) then
          found = .true.
          do nly = 1, 4
            call shift(line)
            read(line,*,err=992,end=993) lsegron(nly)
          end do
          exit
        end if
        read(dfile,'(a100)',err=992,end=994) line    ! read next line
        call d2x(line,i)
      end do

      if (.not.found) go to 997
      close (dfile)

      return

**************** ERROR SPACE *******************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   report(1) = 'error reading file'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'could not find RORGN in file'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'did not literal '//char(39)//'end'//char(39)//' in'
      report(2) = fnam
      report(3) = ' '
      go to 999

997   report(1) = 'did not find segment '//lseg//' in file'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      end 


