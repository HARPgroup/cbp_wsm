************************************************************************
**  gets dsns based on type, either MET, PRE, ATD, PTS, DIV, or SEP   **
**   for meteorology, precipitation, atmospheric deposition, point    **
**   source, diversion, or septic                                     **
************************************************************************
      subroutine getdsns(
     I                   ioscen,lenioscen,
     I                   type,maxvars,
     O                   nvars,varname,dsns)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      integer maxvars
 
      integer nvars      ! number of dsns
      integer dsns(maxvars)
      character*4 varname(maxvars)
      character*3 type

      logical comment

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/data_dsns'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nvars = 0
      read(dfile,'(a100)') line
      call d2x(line,last)
      do while (line(:last).ne.'end')
        if (.not.comment(line).and.line(27:29).eq.type) then
          nvars = nvars + 1
          varname(nvars) = line(9:12)
          read(line(:4),'(i4)') dsns(nvars)
        end if
        read(dfile,'(a100)') line
        call d2x(line,last)
      end do

      close (dfile)

      return

************** ERROR SPACE *********************************************
991   report(1) = 'Could not open file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:10),'(i2)') err
      go to 999

999   call stopreport(report)

      end

