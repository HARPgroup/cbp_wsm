************************************************************************
** writes the variable portion of ftables to the special actions      **
************************************************************************
      subroutine cowanesque(rseg,lenrseg,rscen,lenrscen)

      implicit none

      include 'rug.inc'
      integer maxchanges,nchanges,nc  ! number of transition periods
      parameter (maxchanges = 7)

      integer month1(maxchanges),day1(maxchanges)
      integer month2(maxchanges),day2(maxchanges)
      integer changefrom(maxchanges),changeto(maxchanges) ! disch row

      integer maxtabsize,tabsize,nt      ! number of values in table
      parameter (maxtabsize = 100)    ! stored in vector HSPF-style
      real rchtab(maxtabsize)         ! ftable values
      
      integer ndisch, maxdisch,ndis   ! number of discharges in file
      parameter (maxdisch = 10)

      integer maxrows,nrows,nr  ! number of rows in ftable
      parameter (maxrows = 25)

      integer ncols  ! number of columns in base ftable (always 4)

      real distab(maxrows,maxdisch)  ! vectors of discharge

      character*4 y1,y2   ! for subroutine getstart
      character*2 m1,m2,d1,d2
      integer year1,year2,repeat
      character*2 rpt

      real flowold,flownew,flowchange,flow
      integer changedays,nd

      integer year,month,day

      integer julian
      external julian


*********** END DECLARATIONS

           ! populate the variable tables
      call readvarftab(rseg,lenrseg,rscen,lenrscen,
     I                 maxtabsize,maxchanges,maxdisch,maxrows,
     O                 tabsize,nchanges,ndisch,nrows,ncols,
     O                 month1,day1,month2,day2,
     O                 changefrom,changeto,
     O                 rchtab,distab)

            ! get the start year and the years to repeat
      call getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      read(y1,'(i4)')year1
      read(y2,'(i4)')year2
      repeat = year2-year1+1
      if (repeat.gt.99) go to 992
      write (rpt,'(i2)')repeat

      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

      line = '  RCHRES  1         Y1Y1  M  D  1      3 '//
     .       ' RCHTAB NT        =<<<value>>'
C      line(21:24) = y1    ! put in year
C      line(79:80) = rpt   ! put in repeat number

C      do nc = 1,nchanges   ! loop over change periods

        do nr = 1,nrows      ! loop over rows

          flowold = distab(nr,1)
          flownew = distab(nr,2)
          flowchange = flownew - flowold

          if (abs(flowchange).gt..01 ) then  ! need to perform change

            write(line(50:51),'(i2)') nr*ncols    ! write rchtab index

            changedays = julian(1989,11,1,1990,6,1) ! Cowanesque special

            year = 1989
            month = 11
            day = 1

            do nd = 1,changedays

              call tomorrow(year,month,day)
              flow = flowold + flowchange*real(nd)/real(changedays)

              write(line(21:24),'(i4)') year    ! put in year
              write(line(26:27),'(i2)') month   ! write month
              write(line(29:30),'(i2)') day
              call ritef10(line(61:70),flow)

              call ryt(line,uci)

            end do

          end if
        end do
C      end do

      return

************* ERROR SPACE **********************************************
992   report(1) = 'the years of simulation are too far apart for an'
      report(2) = ' annual repeat value'
      write(report(3),*)'year1 = ',year1,' year2 = ',year2
      go to 999

999   call stopreport(report)

      end


************************************************************************
** writes the variable portion of ftables to the special actions      **
************************************************************************
      subroutine curwensville(rseg,lenrseg,rscen,lenrscen)

      implicit none

      include 'rug.inc'
      integer maxchanges,nchanges,nc  ! number of transition periods
      parameter (maxchanges = 7)

      integer month1(maxchanges),day1(maxchanges)
      integer month2(maxchanges),day2(maxchanges)
      integer changefrom(maxchanges),changeto(maxchanges) ! disch row

      integer maxtabsize,tabsize,nt      ! number of values in table
      parameter (maxtabsize = 100)    ! stored in vector HSPF-style
      real rchtab(maxtabsize)         ! ftable values
      
      integer ndisch, maxdisch,ndis   ! number of discharges in file
      parameter (maxdisch = 10)

      integer maxrows,nrows,nr  ! number of rows in ftable
      parameter (maxrows = 25)

      integer ncols  ! number of columns in base ftable (always 4)

      real distab(maxrows,maxdisch)  ! vectors of discharge

      character*4 y1,y2   ! for subroutine getstart
      character*2 m1,m2,d1,d2
      integer year1,year2,repeat1,repeat2
      character*2 rpt(maxchanges)

      real flowold,flownew,flowchange,flow
      integer changedays,nd

      integer year,month,day

      integer julian
      external julian


*********** END DECLARATIONS

           ! populate the variable tables
      call readvarftab(rseg,lenrseg,rscen,lenrscen,
     I                 maxtabsize,maxchanges,maxdisch,maxrows,
     O                 tabsize,nchanges,ndisch,nrows,ncols,
     O                 month1,day1,month2,day2,
     O                 changefrom,changeto,
     O                 rchtab,distab)

            ! get the start year and the years to repeat
      call getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      read(y1,'(i4)')year1
      read(y2,'(i4)')year2
      repeat1 = 1997-year1+1
      repeat2 = 1996-year1+1
      if (repeat1.gt.99) go to 992
      write (rpt(1),'(i2)')repeat1
      write (rpt(2),'(i2)')repeat2
      if (year1.gt.1996) then
        rpt(1) = '1'
        rpt(2) = '0'
      end if

      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

      line = '  RCHRES  1         Y1Y1  M  D  1      3 '//
     .       ' RCHTAB NT        =<<<value>> YR   1 RP'
      line(21:24) = y1    ! put in year

      do nc = 1,nchanges   ! loop over change periods

        line(79:80) = rpt(nc)   ! put in repeat number
        do nr = 1,nrows      ! loop over rows

          flowold = distab(nr,changefrom(nc))
          flownew = distab(nr,changeto(nc))
          flowchange = flownew - flowold

          if (abs(flowchange).gt..01 ) then  ! need to perform change

            write(line(50:51),'(i2)') nr*ncols    ! write rchtab index

            changedays = julian(year1,month1(nc),day1(nc),
     .                          year1,month2(nc),day2(nc))

            year = year1
            month = month1(nc)
            day = day1(nc)

            do nd = 1,changedays

              call tomorrow(year,month,day)
              flow = flowold + flowchange*real(nd)/real(changedays)

              write(line(26:27),'(i2)') month   ! write month
              write(line(29:30),'(i2)') day
              call ritef10(line(61:70),flow)

              call ryt(line,uci)

            end do

          end if
        end do
      end do

      return

************* ERROR SPACE **********************************************
992   report(1) = 'the years of simulation are too far apart for an'
      report(2) = ' annual repeat value'
      write(report(3),*)'year1 = ',year1,' year2 = ',year2
      go to 999

999   call stopreport(report)

      end


************************************************************************
** writes the variable portion of ftables to the special actions      **
************************************************************************
      subroutine sayersblanchard(rseg,lenrseg,rscen,lenrscen)
      implicit none
      include 'rug.inc'
      call sayblanc1(rseg,lenrseg,rscen,lenrscen)
      call sayblanc2(rseg,lenrseg,rscen,lenrscen)
      return
      end

      subroutine sayblanc1(rseg,lenrseg,rscen,lenrscen)

      implicit none

      include 'rug.inc'
      integer maxchanges,nchanges,nc  ! number of transition periods
      parameter (maxchanges = 7)

      integer month1(maxchanges),day1(maxchanges)
      integer month2(maxchanges),day2(maxchanges)
      integer changefrom(maxchanges),changeto(maxchanges) ! disch row

      integer maxtabsize,tabsize,nt      ! number of values in table
      parameter (maxtabsize = 100)    ! stored in vector HSPF-style
      real rchtab(maxtabsize)         ! ftable values
      
      integer ndisch, maxdisch,ndis   ! number of discharges in file
      parameter (maxdisch = 10)

      integer maxrows,nrows,nr  ! number of rows in ftable
      parameter (maxrows = 25)

      integer ncols  ! number of columns in base ftable (always 4)

      real distab(maxrows,maxdisch)  ! vectors of discharge

      character*4 y1,y2   ! for subroutine getstart
      character*2 m1,m2,d1,d2
      integer year1,year2,repeat
      character*2 rpt

      real flowold,flownew,flowchange,flow
      integer changedays,nd

      integer year,month,day

      integer julian
      external julian


*********** END DECLARATIONS

           ! populate the variable tables
      call readvarftab(rseg,lenrseg,rscen,lenrscen,
     I                 maxtabsize,maxchanges,maxdisch,maxrows,
     O                 tabsize,nchanges,ndisch,nrows,ncols,
     O                 month1,day1,month2,day2,
     O                 changefrom,changeto,
     O                 rchtab,distab)

            ! get the start year and the years to repeat
      call getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      y2 = '1993'  ! sayblanc1 ends in 1993
      read(y1,'(i4)')year1
      read(y2,'(i4)')year2
      if (year2.lt.year1) return   ! skip this if not in simulation time
      repeat = year2-year1+1
      if (repeat.gt.99) go to 992
      write (rpt,'(i2)')repeat

      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

      line = '  RCHRES  1         Y1Y1  M  D  1      3 '//
     .       ' RCHTAB NT        =<<<value>> YR   1 RP'
      line(21:24) = y1    ! put in year
      line(79:80) = rpt   ! put in repeat number

      do nc = 1,nchanges   ! loop over change periods

        do nr = 1,nrows      ! loop over rows

          flowold = distab(nr,changefrom(nc))
          flownew = distab(nr,changeto(nc))
          flowchange = flownew - flowold

          if (abs(flowchange).gt..01 ) then  ! need to perform change

            write(line(50:51),'(i2)') nr*ncols    ! write rchtab index

            changedays = julian(year1,month1(nc),day1(nc),
     .                          year1,month2(nc),day2(nc))

            year = year1
            month = month1(nc)
            day = day1(nc)

            do nd = 1,changedays

              call tomorrow(year,month,day)
              flow = flowold + flowchange*real(nd)/real(changedays)

              write(line(26:27),'(i2)') month   ! write month
              write(line(29:30),'(i2)') day
              call ritef10(line(61:70),flow)

              call ryt(line,uci)

            end do

          end if
        end do
      end do

      return

************* ERROR SPACE **********************************************
992   report(1) = 'the years of simulation are too far apart for an'
      report(2) = ' annual repeat value'
      write(report(3),*)'year1 = ',year1,' year2 = ',year2
      go to 999

999   call stopreport(report)

      end

      subroutine sayblanc2(rseg,lenrseg,rscen,lenrscen)

      implicit none

      include 'rug.inc'
      integer maxchanges,nchanges,nc  ! number of transition periods
      parameter (maxchanges = 7)

      integer month1(maxchanges),day1(maxchanges)
      integer month2(maxchanges),day2(maxchanges)
      integer changefrom(maxchanges),changeto(maxchanges) ! disch row

      integer maxtabsize,tabsize,nt      ! number of values in table
      parameter (maxtabsize = 100)    ! stored in vector HSPF-style
      real rchtab(maxtabsize)         ! ftable values
      
      integer ndisch, maxdisch,ndis   ! number of discharges in file
      parameter (maxdisch = 10)

      integer maxrows,nrows,nr  ! number of rows in ftable
      parameter (maxrows = 25)

      integer ncols  ! number of columns in base ftable (always 4)

      real distab(maxrows,maxdisch)  ! vectors of discharge

      character*4 y1,y2   ! for subroutine getstart
      character*2 m1,m2,d1,d2
      integer year1,year2,repeat
      character*2 rpt

      real flowold,flownew,flowchange,flow
      integer changedays,nd

      integer year,month,day

      integer julian
      external julian

      character*13 tempseg


*********** END DECLARATIONS

           ! populate the variable tables
      tempseg = rseg(:9)//'2222'
      call readvarftab(tempseg,lenrseg,rscen,lenrscen,
     I                 maxtabsize,maxchanges,maxdisch,maxrows,
     O                 tabsize,nchanges,ndisch,nrows,ncols,
     O                 month1,day1,month2,day2,
     O                 changefrom,changeto,
     O                 rchtab,distab)

            ! get the start year and the years to repeat
      call getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      y1 = '1994'   ! sayblanc2 starts in 1994
      read(y1,'(i4)')year1
      read(y2,'(i4)')year2
      if (year2.lt.year1) return   ! skip this if not in simulation time
      repeat = year2-year1+1
      if (repeat.gt.99) go to 992
      write (rpt,'(i2)')repeat

      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

      line = '  RCHRES  1         Y1Y1  M  D  1      3 '//
     .       ' RCHTAB NT        =<<<value>> YR   1 RP'
      line(21:24) = y1    ! put in year
      line(79:80) = rpt   ! put in repeat number

      do nc = 1,nchanges   ! loop over change periods

        do nr = 1,nrows      ! loop over rows

          flowold = distab(nr,changefrom(nc))
          flownew = distab(nr,changeto(nc))
          flowchange = flownew - flowold

          if (abs(flowchange).gt..01 ) then  ! need to perform change

            write(line(50:51),'(i2)') nr*ncols    ! write rchtab index

            changedays = julian(year1,month1(nc),day1(nc),
     .                          year1,month2(nc),day2(nc))

            year = year1
            month = month1(nc)
            day = day1(nc)

            do nd = 1,changedays

              call tomorrow(year,month,day)
              flow = flowold + flowchange*real(nd)/real(changedays)

              write(line(26:27),'(i2)') month   ! write month
              write(line(29:30),'(i2)') day
              call ritef10(line(61:70),flow)

              call ryt(line,uci)

            end do

          end if
        end do
      end do

      return

************* ERROR SPACE **********************************************
992   report(1) = 'the years of simulation are too far apart for an'
      report(2) = ' annual repeat value'
      write(report(3),*)'year1 = ',year1,' year2 = ',year2
      go to 999

999   call stopreport(report)

      end


************************************************************************
** causes the backup of water with the January 1996 ice jam           **
**   strategy:                                                        **
**      read the discharge from the ftable                            **
**      divide by a parameter                                         **
**      put in modified ftable starting a specific date               **
**      re-create the original ftable on release date                 **
************************************************************************
      subroutine januarystorm(rseg,lenrseg,rscen,lenrscen)

      implicit none
      include 'rug.inc'

      real restriction
      parameter (restriction = 0.15)

      integer startY,startM,startD  ! start of restriction
      integer endY,endM,endD        ! end of restriction
      data startY,startM,startD /1996, 1, 7/
      data endY,endM,endD       /1996, 1,20/

      integer maxrows,nrows,nr  ! number of rows in ftable
      parameter (maxrows = 25)
      integer ncols  ! number of columns in base ftable (always 4)

      real discharge(maxrows),flow

      integer maxtabsize              ! HSPF-allowed maximum ftable size
      parameter (maxtabsize = 100)

      character*4 y1,y2   ! for subroutine getstart
      character*2 m1,m2,d1,d2
      integer year1,year2

************** END DECLARATIONS ****************************************

********* make sure the event is in the simulation period
      call getstart(rscen,lenrscen,y1,m1,d1,y2,m2,d2)
      read(y1,'(i4)')year1
      read(y2,'(i4)')year2
      if (year1.gt.endY) return  ! starts after the event
      if (year2.lt.startY) return ! ends before the event

******** get the original discharge from the ftable
      call getdischarge(
     I                  rseg,lenrseg,rscen,lenrscen,
     I                  maxrows,maxtabsize,
     O                  discharge,nrows,ncols)

********** comment line
      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

********** blank line
      line = '  RCHRES  1         Y1Y1  M  D  9      3 '//
     .       ' RCHTAB NT        =<<<value>>'

********** make restriction change
      write(line(21:24),'(i4)') startY
      write(line(26:27),'(i2)') startM
      write(line(29:30),'(i2)') startD
      do nr = 1,nrows      ! loop over rows
        write(line(50:51),'(i2)') nr*ncols    ! write rchtab index
        flow = discharge(nr) * restriction
        call ritef10(line(61:70),flow)
        call ryt(line,uci)
      end do

********** put it back
      write(line(21:24),'(i4)') endY
      write(line(26:27),'(i2)') endM
      write(line(29:30),'(i2)') endD
      do nr = 1,nrows      ! loop over rows
        write(line(50:51),'(i2)') nr*ncols    ! write rchtab index
        flow = discharge(nr)
        call ritef10(line(61:70),flow)
        call ryt(line,uci)
      end do

      return
************* ERROR SPACE **********************************************

999   call stopreport(report)

      end

