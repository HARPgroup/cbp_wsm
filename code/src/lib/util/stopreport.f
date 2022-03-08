************************************************************************
** writes a file with three lines and closes it.  stops the program   **
************************************************************************

      subroutine stopreport(report)
      implicit none

      character*64 report(3)
      integer i,iofile

      call findopen(iofile)         ! find open file number

      open(iofile,file='problem',status='unknown')
      write(iofile,'(a)')' '
      write(iofile,'(a)')' PROBLEM FILE WRITTEN'
      write(iofile,'(a)')' '
      write(iofile,'(a64)')report(1)
      write(iofile,'(a64)')report(2)
      write(iofile,'(a64)')report(3)
      write(iofile,'(a)')' '
      close (iofile)

      stop

      end
************************************************************************
** writes a file with three lines and closes it.  stops the program   **
************************************************************************

      subroutine NoProblemReport(report)
      implicit none

      character*64 report(3)
      integer i,iofile

      call findopen(iofile)         ! find open file number

      open(iofile,file='problem',status='unknown')
      write(iofile,'(a)')' '
      write(iofile,'(a64)')report(1)
      write(iofile,'(a64)')report(2)
      write(iofile,'(a64)')report(3)
      write(iofile,'(a)')' '
      close (iofile)

      stop

      end

