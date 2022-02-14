************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           scenario for each land use (LandScen)                    **
************************************************************************
c      subroutine readcontrol_tmsce(rscen,lenrscen,luname,
c     .                            T1year,T1month,T1day,
c     .                            T2year,T2month,T2day,
c     .                            LandScen)
      subroutine readcontrol_tmsce(lscen,lenlscen,luname,
     .                            T1year,T1month,T1day,
     .                            T2year,T2month,T2day)
      implicit none
      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
c      include '../lib/inc/land_use.inc'

      character*25 LandScen                ! scenario for land wdms
      character*3  luname
      integer lenls

      integer T1year,T1month,T1day              ! start time
      integer T2year,T2month,T2day              ! end time

      character*3 clu                            ! character land use
      logical comment
      integer l,n

      integer julian
      external julian,comment

      logical foundLU

************** END DECLARATION *****************************************

c      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      print*,fnam
      open (dfile,file=fnam,status='old')
      print*,fnam

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
        if (.not.comment(line)) then

          if (line(:13).eq.'LAND SCENARIO') then
            foundLU = .false.
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            do while (line(:17).ne.'END LAND SCENARIO')

              if (.not.comment(line)) then
                clu = line(:3)
                if (clu .eq. 'all') then        ! allow for 'all' in land use specifier
                    if (foundLU) go to 992  ! 'all' can only specify at the first line
                    LandScen = line(5:29)
                    foundLU = .true.
                end if

                if (clu.eq.luname) then
                    foundLU = .true.
                    LandScen = line(5:29)
                end if
              end if

              read(dfile,'(a100)',err=1001)line
              call d2x(line,last)
            end do

            if (.not.foundLU) go to 993

          else if (line(:4).eq.'TIME') then
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            read(line,1234) T1year, T1month, T1day
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            read(line,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            if (line(:8).ne.'END TIME') then
              print*,' control file ',fnam,
     .               ' only allowed two rows in table TIME'
              stop
            end if

          end if
        end if
      end do

      close (dfile)

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE ************************************************
1001  report(1) = 'Error reading line in file:  line:'
      report(2) = fnam
      report(3) = line(:64)
      go to 999

992   report(1) = ' "all" not specified in the right position'
      report(2) = ' "all" can only specified at first land use line '
      report(3) = '   '
      go to 999

993   report(1) = 'did not find land use '//luname
      report(2) = ' in control file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end
