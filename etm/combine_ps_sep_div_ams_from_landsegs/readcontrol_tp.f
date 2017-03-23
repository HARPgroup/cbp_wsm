************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           scenario for each keyword, {Ps, Sep, Div }               **
************************************************************************
      subroutine readcontrol_tp(rscen,lenrscen,keyword,
     O                          T1year,T1month,T1day,
     O                          T2year,T2month,T2day,
     O                          KeyScen)
      implicit none

      include 'combine_ps_sep_div_from_landsegs.inc'

      character*3 clu                            ! character land use
      character*(*) keyword
      character*(*) KeyScen

      logical comment,foundkey,foundtime

      integer l,n,lenkeyword

      integer julian
      external julian,comment

************************************************************************
      call lencl(keyword,lenkeyword)

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      foundkey = .false.
      foundtime = .false.
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:lenkeyword).eq.keyword) then
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=1001)line
              call d2x(line,last)
            end do
            KeyScen = line(:last)
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            if (line(5:lenkeyword+4).ne.keyword) go to 991
            foundkey = .true.

          else if (line(:4).eq.'TIME') then
            read(dfile,1234) T1year, T1month, T1day
            read(dfile,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=1001)line
            if (line(:8).ne.'END TIME') go to 992
            foundtime = .true.

          end if
        end if
      end do

      close (dfile)

      if  (.not.foundkey) go to 993
      if  (.not.foundtime) go to 994

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE
991   report(1) = 'problem in file:'
      report(2) = fnam
      report(3) = 'only one line allowed in table: '//keyword
      go to 999

992   report(1) = 'problem in file:'
      report(2) = fnam
      report(3) = 'only two lines allowed in table'//
     .            'TIME'
      go to 999

993   report(1) = 'problem in file:'
      report(2) = fnam
      report(3) = 'could not fine table: '//keyword
      go to 999

994   report(1) = 'problem in file:'
      report(2) = fnam
      report(3) = 'could not fine table: TIME'
      go to 999

1001  report(1) = 'Error reading line in file:  line:'
      report(2) = fnam
      report(3) = line(:64)
       go to 999

999   call stopreport(report)

      end

