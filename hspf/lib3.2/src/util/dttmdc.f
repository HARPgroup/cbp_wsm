C
C
C
      SUBROUTINE SYDATM
     O                 ( YR, MO, DY, HR, MN, SC )
C
C     + + + PURPOSE + + +
C     Returns the current date and time.  Calls the subroutines sydate and
C     sytime to get the current date and time from the machine storage
C     For use on a DEC5000/25
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DY, HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS
C     YR     - year
C     MO     - month 
C     DA     - day 
C     HR     - hour 
C     MN     - minute
C     SC     - second
C
C     + + + EXTERNALS + + +
      EXTERNAL   SYDATE, SYTIME
C
C     + + + END SPECIFICATIONS + + +
C     get date
      CALL SYDATE ( YR, MO, DY )
C
C     get time
      CALL SYTIME ( HR, MN, SC )
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATE
     +                   ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system date.
C     This version of SYDATE calls the special ForTran date() function
C     converts the character output to yr, mo, da integer format.
C     For use on a DEC5000/25
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA, NM, I 
      PARAMETER( NM=12 ) 
      CHARACTER*9 ADATE
      CHARACTER*3 MONTH,M( NM )
C 
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C     ADATE  - argument of FORTRAN date() function.  Returns e.g. 20-Oct-95. 
C     MONTH  - character string of month
C     M      - array of months where m(1)=Jan, m(12)=Dec
C
C     + + + INTRINSIC FUNCTIONS + + +
C     INTRINSIC DATE
      EXTERNAL  DATE
C
C     + + + ENTER DATA TO THE M-ARRAY:
      DATA M/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep',
     +       'Oct','Nov','Dec'/
C
C     + + + END SPECIFICATIONS + + +
C     Get the date in character format from the machine:
      CALL DATE ( ADATE )
C
C     Convert the date, adate, to integer format using read() and m():
      READ( ADATE(1:2), 10 ) DA
      READ( ADATE(4:6), 20 ) MONTH
      READ( ADATE(8:9), 10 ) YR
 10   FORMAT( I2.0 )
 20   FORMAT( A3 )
      DO 100 I = 1, 12
         IF ( MONTH .EQ. M(I) ) THEN
            MO = I
         END IF
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                   ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     This version of SYTIME calls the special ForTran function time()
C     to get the system time from the machine.
C     For use on a DEC5000/25
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*9 ATIME
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ATIME  - Character sting of system time, e.g. 13:45:54
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + INTRINSIC FUNCTIONS + + +
C     INTRINSIC TIME
      EXTERNAL  TIME
C
C     + + + END SPECIFICATIONS + + +
C     Get the current time from the computer via the spectial 
C     function time().
C
      CALL TIME(ATIME)
C     
C     Collect the hr, mn, and sc from the returned values in atime:
      READ(ATIME(1:2),10) HR
      READ(ATIME(4:5),10) MN
      READ(ATIME(7:8),10) SC
 10   FORMAT(I2.0)
C
      RETURN
      END
