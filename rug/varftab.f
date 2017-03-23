************************************************************************
** writes the variable portion of ftables to the special actions      **
************************************************************************
      subroutine varftab(rseg,lenrseg,rscen,lenrscen)

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
      integer year1,year2,repeat,mon1,mon2,dy1,dy2
      character*2 rpt

      real flowold,flownew,flowchange,flow
      integer changedays,nd,totaldays

      integer year,month,day,dayofweek  ! dayofweek=1=Monday

      integer julian
      external julian

      integer IWKDAY
      external IWKDAY

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

      line = '***oper><f><-l>dc<ds<yr><m><d><h><n>dstp  <vari>'//
     .       '<1><2><3><a><-value--> tc tstnum'
      call ryt(line,uci)   ! put comments into special actions

      line = '  RCHRES  1         Y1Y1  M  D  1      3 '//
     .       ' RCHTAB NT        =<<<value>> YR   1 RP'

      if (nchanges.lt.0) then  ! weekend / weekday changes

        line(77:80) = '    '
        line(32:33) = '23'

        read(m1,'(i2)')mon1
        read(m2,'(i2)')mon2
        read(d1,'(i2)')dy1
        read(d2,'(i2)')dy2

        dayofweek = IWKDAY(mon1,dy1,year1)
        totaldays = julian(year1,mon1,dy1,year2,mon2,dy2)

        year = year1
        month = mon1
        day = dy1

        do nd = 1, totaldays
          call tomorrow(year,month,day)
          dayofweek = dayofweek + 1
          if (dayofweek.eq.8) dayofweek = 1
          if (dayofweek.eq.1) then   ! Monday   - switch to weekday
            call yesterday(year,month,day)
            write(line(21:24),'(i4)') year    ! write year
            write(line(26:27),'(i2)') month   ! write month
            write(line(29:30),'(i2)') day     ! write day
            call tomorrow(year,month,day)
            do nr = 1,nrows      ! loop over rows
              flowchange = abs(distab(nr,changeto(2))-
     .                         distab(nr,changeto(1)))
              if (flowchange.gt..01 ) then  ! need to perform change
                write(line(50:51),'(i2)') nr*ncols    ! write rchtab index
                call ritef10(line(61:70),distab(nr,changeto(2)))
                call ryt(line,uci)

              end if
            end do
          end if
          if (dayofweek.eq.6) then   ! Saturday - switch to weekend
            call yesterday(year,month,day)
            write(line(21:24),'(i4)') year    ! write year
            write(line(26:27),'(i2)') month   ! write month
            write(line(29:30),'(i2)') day     ! write day
            call tomorrow(year,month,day)
            do nr = 1,nrows      ! loop over rows
              flowchange = abs(distab(nr,changeto(1))-
     .                         distab(nr,changeto(2)))
              if (flowchange.gt..01 ) then  ! need to perform change
                write(line(50:51),'(i2)') nr*ncols    ! write rchtab index
                call ritef10(line(61:70),distab(nr,changeto(1)))
                call ryt(line,uci)

              end if
            end do
          end if

        end do
      
      else        ! regular variable ftable file

        repeat = year2-year1+1
        if (repeat.gt.99) go to 992
        write (rpt,'(i2)')repeat

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
     .                            year1,month2(nc),day2(nc))

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

      end if

      return

************* ERROR SPACE **********************************************
992   report(1) = 'the years of simulation are too far apart for an'
      report(2) = ' annual repeat value'
      write(report(3),*)'year1 = ',year1,' year2 = ',year2
      go to 999

999   call stopreport(report)

      end

      INTEGER FUNCTION IWKDAY
     I                        (mtemp,dtemp,ytemp)
C
      implicit none
      INTEGER   M,IDY,IYR,mtemp,dtemp,ytemp
      REAL      X

C     FUNCTION IWKDAY DETERMINES THE DAY OF THE WEEK FOR ANY DATE FROM
C     JAN1,1901 TO DEC31,1999 AND RETURNS A CODE NUMBER FROM 1 TO 7.
C     MONDAY IS CODE NO. 1.  INPUT: MONTH,DAY, AND LAST TWO DIGITS OF YEAR.
C     INPUT:INTEGER. OUTPUT:INTEGER.
C
      IYR = ytemp
      M = mtemp
      IDY = dtemp
      if (ytemp.gt.1000) IYR = ytemp - 1900
      IF (M .LE. 2) IYR= IYR - 1
      IF (M .LE. 2) M= M + 12
      M= M - 2
      X= IDY + INT(2.6*M - 0.1) + INT((IYR/4) + IYR + .1) - 33.25
 10   CONTINUE
      IF (X .GE. 0.0) GO TO 20
      X= X + 7.0
      GO TO 10
 20   CONTINUE
      X= X/7.0
      X= AMOD(X,1.0)*7.0
      IWKDAY= INT(X)
      IF (IWKDAY .EQ. 0) IWKDAY= 7
C
      RETURN
      END

