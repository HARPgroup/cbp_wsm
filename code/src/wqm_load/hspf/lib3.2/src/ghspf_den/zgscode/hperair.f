C
C
C
      SUBROUTINE   PATEMP
C
C     + + + PURPOSE + + +
C     Process input to the atemp section of module perlnd or implnd
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ATEMP1 + + +
      INCLUDE    'cplat.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TBSB,TBNO,NVAL
      REAL      RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION ATEMP')
 2010 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION ATEMP')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     set values of variables not controlled by user
C     hourly values of assumed dry air lapse rate (degF/ft)
      LAPSE(1) = 0.0035
      LAPSE(2) = 0.0035
      LAPSE(3) = 0.0035
      LAPSE(4) = 0.0035
      LAPSE(5) = 0.0035
      LAPSE(6) = 0.0035
      LAPSE(7) = 0.0037
      LAPSE(8) = 0.0040
      LAPSE(9) = 0.0041
      LAPSE(10)= 0.0043
      LAPSE(11)= 0.0046
      LAPSE(12)= 0.0047
      LAPSE(13)= 0.0048
      LAPSE(14)= 0.0049
      LAPSE(15)= 0.0050
      LAPSE(16)= 0.0050
      LAPSE(17)= 0.0048
      LAPSE(18)= 0.0046
      LAPSE(19)= 0.0044
      LAPSE(20)= 0.0042
      LAPSE(21)= 0.0040
      LAPSE(22)= 0.0038
      LAPSE(23)= 0.0037
      LAPSE(24)= 0.0036
C
C     process value in table - type elev-diff
      TBNO= 4
      TBSB= 1
      NVAL= 2
      CALL RTABLE(TBNO,TBSB,NVAL,UUNITS,
     M            RVAL)
C
      ELDAT = RVAL(1)
      AIRTMP= RVAL(2)
C
      IF (OUTLEV.GT.1) THEN
C       end processing section message
        WRITE (MESSU,2010)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ATEMP
C
C     + + + PURPOSE + + +
C     Correct air temperature for elevation difference.
C     This is done here because the temperatureis required by more
C     than one section of the PERLND or IMPLND modules.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ATEMP2 + + +
      INCLUDE 'cplat.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL    LAPS, PRRAT
C
C     + + + END SPECIFICATIONS + + +
C
C     save air temperature at the start of the current interval -
C     units are deg F
      AIRTS= AIRTMP
C
      IF (HRFG .EQ. 1) THEN
C       it is time to update air temperature
C       find the precipitation rate during the interval - in./min
C       read precipitation from inpad
        PRECA= PAD(PRECFP + IVL1)
        PRRAT= PRECA/DELT
C
        IF (PRRAT .GT. 0.000833) THEN
C         use rain period lapse rate.
          LAPS= 3.5E-03
        ELSE
C         use dry period lapse rate
          LAPS= LAPSE(HR)
        END IF
C
C       read air temperature at the gage from inpad
        GATMP= PAD(GATFP + IVL1)
C
C       compute corrected air temperature for end of current interval -
C       units are deg f
        AIRTMP= GATMP - LAPS * ELDAT
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   AIRPRT
     I                   (UNITFG,PRINTU)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and produce
C     printout.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UNITFG,PRINTU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG - output units   1-english, 2-metric
C     PRINTU - fortran unit number on which to print output
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ATEMP2 + + +
      INCLUDE   'cplat.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL      PTEMP1,PTEMP2
C
C     + + + FUNCTIONS + + +
      REAL      CELSUS
C
C     + + + EXTERNALS + + +
      EXTERNAL  CELSUS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** ATEMP ***')
 2010 FORMAT (/,'   STATE VARIABLES')
 2020 FORMAT (/,'     AIR TEMPERATURES (DEG F)        GAGE   SEGMENT')
 2030 FORMAT (/,'     AIR TEMPERATURES (DEG C)        GAGE   SEGMENT')
 2040 FORMAT (  ' ',30X,2F10.1)
C
C     + + + END SPECIFICATIONS + + +
C
C     convert to external units
      IF (UNITFG .EQ. 1) THEN
C       english system
        PTEMP1= GATMP
        PTEMP2= AIRTMP
      ELSE
C       metric system - celsus is a function subprogram
        PTEMP1= CELSUS(GATMP)
        PTEMP2= CELSUS(AIRTMP)
      END IF
C
C     write, to unit PRINTU
      WRITE (PRINTU,2000)
      WRITE (PRINTU,2010)
C
      IF (UNITFG .EQ. 1) THEN
        WRITE (PRINTU,2020)
      ELSE
        WRITE (PRINTU,2030)
      END IF
C
      WRITE (PRINTU,2040)  PTEMP1, PTEMP2
C
      RETURN
      END
C
C
C
      SUBROUTINE   AIRTPT
C
C     + + + PURPOSE + + +
C     Handle section ATEMP.
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION ATEMP2 + + +
      INCLUDE 'cplat.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (AIRTFP .GE. 1)  THEN
         PAD(AIRTFP + IVL1)= AIRTMP
      END IF
C
      RETURN
      END
