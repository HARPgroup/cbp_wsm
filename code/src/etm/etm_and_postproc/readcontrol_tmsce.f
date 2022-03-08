************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_tmsce(rscen,lenrscen,
     .                            T1year,T1month,T1day,
     .                            T2year,T2month,T2day,
     .                            LandScen)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      character*25 LandScen(nlu)                ! scenario for land wdms
      integer lenls

      integer T1year,T1month,T1day              ! start time
      integer T2year,T2month,T2day              ! end time

      character*3 clu                            ! character land use
      logical comment
      integer l,n

      integer julian
      external julian,comment

      logical foundLU(nlu)

************** END DECLARATION *****************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
        if (.not.comment(line)) then

          if (line(:13).eq.'LAND SCENARIO') then
            do l = 1,nlu
              foundLU(l) = .false.
            end do
            read(dfile,'(a100)',err=1001)line
            call d2x(line,last)
            do while (line(:17).ne.'END LAND SCENARIO')

              if (.not.comment(line)) then
                clu = line(:3)
                if (clu .eq. 'all') then        ! allow for 'all' in land use specifier
                  do l = 1,nlu
                    if (foundLU(l)) go to 992  ! 'all' can only specify at the first line
                    LandScen(l) = line(5:29)
                    foundLU(l) = .true.
                  end do
                end if

                do l = 1,nlu
                  if (clu.eq.luname(l)) then
                    foundLU(l) = .true.
                    LandScen(l) = line(5:29)
                  end if
                end do
              end if

              read(dfile,'(a100)',err=1001)line
              call d2x(line,last)
            end do

            do l = 1,nlu  ! check for all found
              if (.not.foundLU(l)) go to 993
            end do

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

993   report(1) = 'did not find land use '//luname(l)
      report(2) = ' in control file'
      report(3) = fnam
      go to 999

999   call stopreport(report)

      end
