************************************************************************
** subroutine finds an available file number between 50 and 99        **
************************************************************************
      subroutine findopen(i)
      include '../inc/standard.inc'
      integer i
      logical isopen

      do i = 50,99
        inquire(unit=i, opened=isopen)
        if (.not.isopen) return
      end do

      report(1) = 'PROBLEM IN CODING'
      report(2) = 'ALL NUMBERS BETWEEN 50 AND 99 ARE OPEN'
      report(3) = ' '
      call stopreport(report)
      end

