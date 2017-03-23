************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol(rscen,lenrscen,
     .                       LBJday,LBfile,
     .                       T1year,T1month,T1day,
     .                       T2year,T2month,T2day,
     .                       LandScen,nLB)
      implicit none

      include 'psu.inc'

      integer LByear(maxTimeBreaks)             ! land use year
      integer LBmonth(maxTimeBreaks)            ! land use month
      integer LBday(maxTimeBreaks)              ! bmp factor application day

      character*100 templine(maxTimeBreaks)
      character*3 clu                            ! character land use

      logical comment

      integer nLB,l,n,i

      integer julian
      external julian

      logical findlu,findbmp,findlscen,findtime,findps,findad,findmet

      data findlu,findbmp,findlscen,findtime,findps,findad,findmet
     .     /.false.,.false.,.false.,.false.,.false.,.false.,.false./

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        if (.not.comment(line)) then

          if (line(:8).eq.'LAND USE') then
            findlu = .true.
            nLB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:12).ne.'END LAND USE')
              if (.not.comment(line)) then
                nLB = nLB + 1
                templine(nLB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nLB
              read(templine(n),1234)
     .              LByear(n),LBmonth(n),LBday(n),LBfile(n)
            end do

          else if (line(:13).eq.'LAND SCENARIO') then
            findlscen = .true.
            read(dfile,'(a100)',err=1001)line
            do while (line(:17).ne.'END LAND SCENARIO')
              if (.not.comment(line)) then
                clu = line(:3)
                do l = 1,nlu
                  if (clu.eq.luname(l)) LandScen(l) = line(5:29)
                end do
              end if
              read(dfile,'(a100)',err=1001)line
            end do

          else if (line(:4).eq.'TIME') then
            findtime = .true.
            read(dfile,1234) T1year, T1month, T1day
            read(dfile,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=1001)line
            if (line(:8).ne.'END TIME') then
              print*,' control file ',fnam,
     .               ' only allowed two rows in table TIME'
              stop
            end if

          end if
        end if
      end do

      close (dfile)

      if (.not.findlu) call stopit(fnam,'LAND USE')
      if (.not.findlscen) call stopit(fnam,'LAND SCENARIO')
      if (.not.findtime) call stopit(fnam,'TIME')

      do n = 1, maxTimeBreaks           ! find the breaks in terms of julian days from the start
        LBJday(n) = -ndaymax-9            ! initialize low since negative values are allowed
      end do
      do n = 1, nLB
        LBJday(n) = julian(T1year,T1month,T1day,
     .                        LByear(n),LBmonth(n),LBday(n))
      end do
      call sortB(LBJday,LBfile,nLB)

      return

1234  format(i4,i3,i3,1x,a20)

*********** ERROR SPACE
1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      call stopreport(report)

      end

************************************************************************
**  subroutine to call subroutine stopreport for different reading    **
***     errors.                                                       **
************************************************************************

      subroutine stopit(fnam,tabnam)
      character*64 report(3),fnam,tabnam

      report(1) = 'Problem reading control file for table'
      report(2) = fnam
      report(3) = tabnam
      call stopreport(report)

      end

