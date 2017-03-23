************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times                                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol(
     I                       rscen,lenrscen,
     O                       LBJday,LBfile,nLB,LandScen,
     O                       BBJday,BBfile,nBB,BmpTypeScen,
     O                       PBJday,PBfile,nPB,
     O                       tranfile,
     O                       StartY,StartM,StartD,
     O                       EndY,EndM,EndD)
      implicit none

      include 'mbtc.f'

      integer LByear(maxTimeBreaks)             ! land use year
      integer LBmonth(maxTimeBreaks)            ! land use month
      integer LBday(maxTimeBreaks)              ! land use day

      integer BByear(maxTimeBreaks)             ! bmp factor year
      integer BBmonth(maxTimeBreaks)            ! bmp factor month
      integer BBday(maxTimeBreaks)              ! bmp factor day

      integer PByear(maxTimeBreaks)             ! bmp lb factor year
      integer PBmonth(maxTimeBreaks)            ! bmp lb factor month
      integer PBday(maxTimeBreaks)              ! bmp lb factor day

      character*100 templine(maxTimeBreaks)
      character*3 clu                            ! character land use

      logical comment

      integer nLB,nBB,nPB,l,n,i

      integer julian
      external julian

      logical findlu,findbmp,findpound,findlscen,findtime
      logical findtran,findtypebmp

      data findlu,findbmp,findpound,findlscen,findtime
     .     /.false.,.false.,.false.,.false.,.false./
      data findtran,findtypebmp /.false.,.false./

      logical foundLU(nlu)

*************** END OF DECLARATION *************************************

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=1001)line
        call d2x(line,last)
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

          else if (line(:8).eq.'BMPACRES') then
            findbmp = .true.
            nBB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:12).ne.'END BMPACRES')
              if (.not.comment(line)) then
                nBB = nBB + 1
                templine(nBB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nBB
              read(templine(n),1234)
     .              BByear(n),BBmonth(n),BBday(n),BBfile(n)
            end do

          else if (line(:9).eq.'BMPPOUNDS') then
            findpound = .true.
            nPB = 0
            read(dfile,'(a100)',err=1001)line
            do while (line(:13).ne.'END BMPPOUNDS')
              if (.not.comment(line)) then
                nPB = nPB + 1
                templine(nPB) = line
              end if
              read(dfile,'(a100)',err=1001)line
            end do
            do n = 1,nPB
              read(templine(n),1234)
     .              PByear(n),PBmonth(n),PBday(n),PBfile(n)
            end do

          else if (line(:7).eq.'TYPEBMP') then
            findtypebmp = .true.
            read(dfile,'(a25)') BmpTypeScen
            read(dfile,'(a100)',err=1001)line
            if (line(:11).ne.'END TYPEBMP') then
              print*,' control file ',fnam,
     .               ' only allowed one row in table TYPEBMP'
              call stopit(fnam,'TYPEBMP')
            end if

          else if (line(:9).eq.'TRANSPORT') then
            findtran = .true.
            read(dfile,'(a25)') tranfile
            read(dfile,'(a100)',err=1001)line
            if (line(:13).ne.'END TRANSPORT') then
              print*,' control file ',fnam,
     .               ' only allowed one row in table TRANSPORT'
              call stopit(fnam,'TRANSPORT')
            end if

          else if (line(:13).eq.'LAND SCENARIO') then
            findlscen = .true.
            do l = 1,nlu
              foundLU(l) = .false.
            end do
            read(dfile,'(a100)',err=1001)line
            do while (line(:17).ne.'END LAND SCENARIO')

              if (.not.comment(line)) then
                clu = line(:3)
                if (clu .eq. 'all') then   ! allow for 'all' 
                  do l = 1,nlu
                    if (foundLU(l)) go to 992  ! 'all' in first line only
                    LandScen(l) = line(5:29)
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
            end do

          else if (line(:4).eq.'TIME') then
            findtime = .true.
            read(dfile,1234) StartY,StartM,StartD
            read(dfile,1234) EndY,EndM,EndD
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
      if (.not.findbmp) call stopit(fnam,'BMPACRES')
      if (.not.findpound) call stopit(fnam,'BMPPOUNDS')
      if (.not.findtypebmp) call stopit(fnam,'TYPEBMP')
      if (.not.findtran) call stopit(fnam,'TRANSPORT')
      if (.not.findlscen) call stopit(fnam,'LAND SCENARIO')
      if (.not.findtime) call stopit(fnam,'TIME')

      do n = 1, maxTimeBreaks   ! breaks in julian days from the start
        LBJday(n) = -ndaymax-9  ! initialize low as neg values allowed
        BBJday(n) = -ndaymax-9
        PBJday(n) = -ndaymax-9
      end do
      do n = 1, nLB
        LBJday(n) = julian(StartY,StartM,StartD,
     .                        LByear(n),LBmonth(n),LBday(n))
      end do
      call sortB(LBJday,LBfile,nLB)

      do n = 1, nBB
        BBJday(n) = julian(StartY,StartM,StartD,
     .                        BByear(n),BBmonth(n),BBday(n))
      end do
      call sortB(BBJday,BBfile,nBB)

      do n = 1, nPB
        PBJday(n) = julian(StartY,StartM,StartD,
     .                        PByear(n),PBmonth(n),PBday(n))
      end do
      call sortB(PBJday,PBfile,nPB)

      return

1234  format(i4,i3,i3,1x,a40)

*********** ERROR SPACE
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = ' "all" not specified in the right position'
      report(2) = ' "all" can only specify at the first land use line '
      report(3) = '   '
      go to 999

1001  report(1) = 'Error reading file after line: '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end

************************************************************************
**  subroutine to call subroutine stopreport for different reading    **
***     errors.                                                       **
************************************************************************

      subroutine stopit(fnam,tabnam)
      include '../../lib/inc/standard.inc'
      character*(*) tabnam

      report(1) = 'Problem reading control file for table'
      report(2) = fnam
      report(3) = tabnam
      call stopreport(report)

      end

