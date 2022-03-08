************************************************************************
** subroutine to modify HSPF pltgen files for readiblilty             **
************************************************************************
      subroutine plt2cal(file1,pfnam)

      implicit none

      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'

      integer:: file1          ! file number
      character*100 pfnam

      character(16):: date(ndaymax),pltvalue(ndaymax)   ! reading variables
      character(3):: clu                                ! character land use

      integer:: i,nd   ! indices

********** END DECLARATIONS  *******************************************
      
        do i = 1,25                       ! skip comments and junk line (i=25, skip 1984, i=24 not)
          read(file1,'(a)',err=992)line
        end do

        nd = 1
        do
          read(file1,'(6x,2a16)',err=994,end=111) date(nd), pltvalue(nd)
          date(nd)(5:5) = ','
          date(nd)(8:8) = ','
          date(nd)(11:11) = ','
          date(nd)(14:14) = ','
          nd = nd + 1
        end do

111     nd = nd-1
        close(file1)

        open(file1,file=pfnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991    ! open file of same name, overwriting
        
        do i = 1,nd
          write(file1,*,err=951) date(i),',', pltvalue(i)
        end do
        
        rewind file1               ! go back to the first record of modified file

        return

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = pfnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = pfnam
      report(3) = ' '
      go to 999

992   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      report(3) = line
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      report(3) = date(nd-1)//'  '//pltvalue(nd-1)
      go to 999

999   call stopreport(report)
      end

