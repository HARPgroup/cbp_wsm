      subroutine resoutdsn(
     I                     ioscen,lenioscen,
     O                     dsn)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      integer dsn

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/rchres_extsources'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do
        read(dfile,'(a100)',end=992,err=993) line
        if (line(:11).eq.'Nexits=4WDM') then
          read(line(15:18),'(i4)') dsn
          return
        end if
      end do

991   report(1) = 'Problem with opening file:'
      report(2) = fnam
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

992   report(1) = 'could not find 4 exit specifier in file'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = line
      go to 999

999   call stopreport(report)
      end
