************************************************************************
** subroutine to modify HSPF pltgen files for readiblilty             **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer file1          ! file number

      character*16 date(ndaymax),value(ndaymax)   ! reading variables
      character*3 clu  ! character land use

      integer i,nd   ! indices

********** END DECLARATIONS  *******************************************
      read(*,'(a)')fnam
	print*,fnam

      call findopen(file1)           ! open pltgen

      open(file1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(file1,'(A26)') line
      if (line(6:26).ne.'HSPF FILE FOR DRIVING') then  ! not a pltgen file
        close(file1)
        stop
      end if

      do i = 1,25                       ! skip comments and junk line
        read(file1,'(a)',err=992)line
      end do

      nd = 1
      do
        read(file1,'(6x,2a16)',err=994,end=111)date(nd),value(nd)
        date(nd)(5:5) = ','
        date(nd)(8:8) = ','
        date(nd)(11:11) = ','
        date(nd)(14:14) = ','
        nd = nd + 1
      end do

111   nd = nd-1
      close(file1)

      open(file1,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991    ! open file of same name, overwriting
      do i = 1,nd
        write(file1,*,err=951)date(i),',',value(i)
      end do
      close(file1)

      stop

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      report(3) = date(nd-1)//'  '//value(nd-1)
      go to 999

999   call stopreport(report)
      end

