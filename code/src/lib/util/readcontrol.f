************************************************************************
**  reads the control file and determines the active modules for this **
**   land use and scenario                                            **
************************************************************************
      subroutine readcontrol_modules(rscen,lenrscen,
     O                               modules,nmod)

      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/modules.inc'

      logical found,comment
      character*7 tabname

      integer i           ! index

      tabname = 'MODULES'

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a100)')line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line).and.line(:7).eq.tabname) then
          read(dfile,'(a100)')line
          call d2x(line,last)
          nmod = 0
          do while (comment(line).or.line(:11).ne.'END '//tabname)
            if (.not.comment(line)) then
              nmod = nmod + 1
              modules(nmod) = line(:last)
            end if
            read(dfile,'(a100)')line
            call d2x(line,last)
          end do
        end if
        read(dfile,'(a100)')line
      end do

      close (dfile)

      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_time(rscen,lenrscen,sdate,edate)
      implicit none

      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/wdm.inc'

      integer T1year, T1month, T1day, T2year, T2month, T2day

      integer sdate(ndate),edate(ndate)

      logical comment

      integer l,n

      integer julian
      external julian,comment

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:4).eq.'TIME') then
            read(dfile,1234) T1year, T1month, T1day
            read(dfile,1234) T2year, T2month, T2day
            read(dfile,'(a100)',err=993)line
            if (line(:8).ne.'END TIME') go to 992
          end if
        end if
      end do

      close (dfile)

********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = T1year
      sdate(2) = T1month
      sdate(3) = T1day
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(1) = T2year
      edate(2) = T2month
      edate(3) = T2day
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE ***********************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed two rows in table TIME'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end


************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  start and end times (T1's and T2's)                      **
**           number of Land Breaks and Bmp Breaks (nLB's and nBB's)   **
**           Days and Files of Breaks (LB's and BB's)                 **
**           scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_Ltime(
     I                             lscen,lenlscen,
     O                             startY,startM,startD,
     O                             endY,endM,endD)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment
      external comment

      integer l,n

      integer startY,startM,startD,endY,endM,endD

      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (11,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 993

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(11,'(a100)',err=991)line
        if (.not.comment(line)) then

          if (line(:4).eq.'TIME') then
            read(11,1234) startY,startM,startD
            read(11,1234) endY,endM,endD
            read(11,'(a100)',err=991)line
            if (line(:8).ne.'END TIME') go to 992
            return
          end if
        end if
      end do

      close (11)

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE
991   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed two rows in table TIME'
      go to 999

993   report(1) = 'could not open land control file'
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  scenario for each land use (LandScen)                    **
************************************************************************
      subroutine readcontrol_lscen(rscen,
     O                             LandScen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      include '../inc/land_use.inc'

      character*25 LandScen(nlu)                ! scenario for land wdms
      integer lenls

      character*3 clu                            ! character land use

      logical comment,findlscen,foundLU(nlu)
      external comment

      integer l,n

************** END DECLARATION *****************************************
      call lencl(rscen,lenrscen)

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      findlscen = .false.

      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=992,end=995)line
        call d2x(line,last)
        if (comment(line)) cycle

        if (line(:13).eq.'LAND SCENARIO') then
          findlscen = .true.
          do l = 1,nlu
            foundLU(l) = .false.
          end do

          read(dfile,'(a100)',err=992,end=995)line
          call d2x(line,last)
          do while (line(:17).ne.'END LAND SCENARIO')
            if (.not.comment(line)) then
              clu = line(:3)
              if (clu.eq.'all') then  ! allow for 'all'
                do l = 1,nlu
                  if (foundLU(l)) go to 993
                  foundLU(l) = .true.
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
            read(dfile,'(a100)',err=992)line
            call d2x(line,last)
          end do

        end if
      end do

      close (dfile)

      if (.not.findlscen) go to 996
      do l = 1,nlu
        if (.not.foundLU(l)) go to 994
      end do

      return

1234  format(i4,i3,i3,1x,a10)

*********** ERROR SPACE ******************************************
991   report(1) = 'could not open control file:'
      report(2) = fnam
      write(report(3),*)'error = ',err
      go to 999

992   report(1) = 'Error reading line in file:  line:'
      report(2) = fnam
      report(3) = line(:64)
      go to 999

993   report(1) = ' In the Land Scenario Section of control file:'
      report(2) = fnam
      report(3) = ' "all" can only be in the first land use line '
      go to 999

994   report(1) = ' In the Land Scenario Section of control file:'
      report(2) = fnam
      report(3) = ' Land use '//luname(l)//' not found'
      go to 999

995   report(1) = 'end of file found unexpectedly'
      report(2) = fnam
      report(3) = line(:64)
      go to 999

996   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have LAND SCENARIO table' 
      go to 999

999   call stopreport(report)
      return

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  I/O variable specification for river scenarios           **
************************************************************************
      subroutine readcontrol_Rioscen(
     I                               rscen,lenrscen,
     O                               ioscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993) line
        if (.not.comment(line)) then

          if (line(:6).eq.'IOVARS') then
            read(dfile,'(a)') ioscen
            read(dfile,'(a100)',err=993)line
            if (line(:10).ne.'END IOVARS') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table IOVARS'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have IOVARS table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  I/O variable specification for river scenarios           **
************************************************************************
      subroutine readcontrol_Lioscen(
     I                               lscen,lenlscen,
     O                               ioscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:6).eq.'IOVARS') then
            read(dfile,'(a)') ioscen
            read(dfile,'(a100)',err=993)line
            if (line(:10).ne.'END IOVARS') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table IOVARS'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have IOVARS table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  parameter scenario for river scenarios                   **
************************************************************************
      subroutine readcontrol_Rparamscen(
     I                                  rscen,lenrscen,
     O                                  paramscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:10).eq.'PARAMETERS') then
            read(dfile,'(a)') paramscen
            read(dfile,'(a100)',err=993)line
            if (line(:14).ne.'END PARAMETERS') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table PARAMETERS'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have PARAMETERS table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  parameter scenario for land scenarios                    **
************************************************************************
      subroutine readcontrol_Lparamscen(
     I                                  lscen,lenlscen,
     O                                  paramscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993,end=993)line
        if (.not.comment(line)) then

          if (line(:10).eq.'PARAMETERS') then
            read(dfile,'(a)') paramscen
            read(dfile,'(a100)',err=993)line
            if (line(:14).ne.'END PARAMETERS') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table PARAMETERS'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have PARAMETERS table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  geometry scenario for land scenarios                     **
************************************************************************
      subroutine readcontrol_Lgeoscen(
     I                                  lscen,lenlscen,
     O                                  geoscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:8).eq.'GEOMETRY') then
            read(dfile,'(a)') geoscen
            read(dfile,'(a100)',err=993)line
            if (line(:12).ne.'END GEOMETRY') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table GEOMETRY'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have GEOMETRY table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  geometry scenario for river scenarios                    **
************************************************************************
      subroutine readcontrol_Rgeoscen(
     I                                  rscen,lenrscen,
     O                                  geoscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

************** END DECLARATION *****************************************
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:8).eq.'GEOMETRY') then
            read(dfile,'(a)') geoscen
            read(dfile,'(a100)',err=993)line
            if (line(:12).ne.'END GEOMETRY') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table GEOMETRY'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have GEOMETRY table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  special action flag scenario for land scenarios          **
************************************************************************
      subroutine readcontrol_Lspecscen(
     I                                  lscen,lenlscen,
     O                                  specscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

      character*(*) specscen

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:20).eq.'SPECIAL ACTION FLAGS') then
            read(dfile,'(a)') specscen
            read(dfile,'(a100)',err=993)line
            if (line(:24).ne.'END SPECIAL ACTION FLAGS') go to 992
            close(dfile)
            return
          end if
        end if
      end do

      close (dfile)

      go to 994

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table SPECIAL ACTION FLAGS'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

994   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have SPECIAL ACTION FLAGS table'
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  calibration scenario for a river scenario                **
************************************************************************
      subroutine readcontrol_Rcalscen(
     I                                rscen,lenrscen,
     O                                calscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      logical comment

      character*(*) calscen

************** END DECLARATION *****************************************
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:9).eq.'CALIBSCEN') then
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            calscen = line(:last)
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            if (line(:13).ne.'END CALIBSCEN') go to 992

          end if
        end if
      end do

      close (dfile)

      return

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table CALIBSCEN'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end

************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  calibration scenario for a land scenario                 **
************************************************************************
      subroutine readcontrol_Lcalscen(
     I                                lscen,lenlscen,
     O                                calscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      logical comment

      character*(*) calscen

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:9).eq.'CALIBSCEN') then
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            calscen = line(:last)
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            if (line(:13).ne.'END CALIBSCEN') go to 992

          end if
        end if
      end do

      close (dfile)

      return

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table CALIBSCEN'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end


************************************************************************
** reads the control file for this scenario into memory               **
**   finds:  point source scenario for river scenarios                **
************************************************************************
      subroutine readcontrol_Rpsscen(
     I                               rscen,lenrscen,
     O                               psscen)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'

      logical comment,found
      external comment

      integer l,n

      character*(*) psscen

************ END DECLARATION *******************************************
      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (11,file=fnam,status='old')

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(11,'(a100)',err=991)line
        if (.not.comment(line)) then

          if (line(:12).eq.'POINT SOURCE') then
            read(11,'(a)') psscen
            read(11,'(a100)',err=991)line
            if (line(:16).ne.'END POINT SOURCE') go to 992
            close(11)
            return
          end if
        end if
      end do

      close (11)

      go to 993

*********** ERROR SPACE ************************************************
991   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table GEOMETRY'
      go to 999

993   report(1) = ' Error in control file'
      report(2) = fnam
      report(3) = ' must have POINT SOURCE table'
      go to 999

999   call stopreport(report)

      end




************************************************************************
** gbhatt added on 04/25/2013                                         **
** reads the LAND control file for this scenario into memory          **
**   finds:  HYDROBASE for a land scenario                            **
************************************************************************
      subroutine readcontrol_LHYDROBASE(
     I                                lscen,lenlscen,
     O                                HYDROBASE)
      implicit none
      include '../inc/standard.inc'
      include '../inc/locations.inc'
      logical comment

      character*(*) HYDROBASE

************** END DECLARATION *****************************************
      fnam = controldir//'land/'//lscen(:lenlscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=993)line
        if (.not.comment(line)) then

          if (line(:9).eq.'HYDROBASE') then
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            HYDROBASE = line(:last)
            read(dfile,'(a100)',err=993)line
            call d2x(line,last)
            do while (comment(line))
              read(dfile,'(a100)',err=993)line
              call d2x(line,last)
            end do
            if (line(:13).ne.'END HYDROBASE') go to 992

          end if
        end if
      end do

      close (dfile)

      return

*********** ERROR SPACE ************************************************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3)=' only allowed one row in table HYDROBASE'
      go to 999

993   report(1) = 'ERROR reading line in file '
      report(2) = fnam
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end
