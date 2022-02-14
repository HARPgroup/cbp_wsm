C
C
C
      SUBROUTINE   ACCVEC
     I                   (DIM,FRMVEC,
     M                    TOVEC)
C
C     + + + PURPOSE + + +
C     Add the elements of FRMVEC to the corresponding elements of
C     TOVEC.  Both vectors of dimension dim are one dimensional.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       FRMVEC(DIM),TOVEC(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     FRMVEC - ???
C     TOVEC  - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,DIM
        TOVEC(I)= TOVEC(I) + FRMVEC(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   BALCHK
     I                   (OPTYP,OPNO,DATIM,MESSU,PRINTU,MSGFL,
     I                    STORS,STOR,MATIN,MATDIF,UNITID,HDRFG,
     M                    WCOUNT)
C
C     + + + PURPOSE + + +
C     Perform a material balance check and report the result.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      OPTYP,DATIM(5),MESSU,PRINTU,OPNO,MSGFL,HDRFG,WCOUNT
      REAL         MATDIF,MATIN,STOR,STORS
      CHARACTER*8  UNITID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPTYP  - operation type
C     OPNO   - operation number
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     PRINTU - fortran unit number on which to print output
C     MSGFL  - fortran unit number of HSPF message file
C     STORS  - storage of material at start of print interval
C     STOR   - storage of material at end of print interval
C     MATIN  - total inflow of material during print interval
C     MATDIF - net inflow(in-out) of material during print interval
C     UNITID - units of reference value
C     HDRFG  - flag indicating whether to print header - 1=yes else no
C     WCOUNT - count of times this message has been produced
C
C     + + + SAVE VARIABLES + + +
      SAVE         COPTYP
      CHARACTER*6  COPTYP(3)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU,SGRP,I6
      REAL         ERROR,REFVAL,RELERR,PCTERR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (COPTYP,COPTY1)
      CHARACTER*1  COPTY1(6,3)
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     OMSTR,OMSG,OMSTI,OMSTD,OMSTC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA COPTYP/'PERLND','IMPLND','RCHRES'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,'   BALANCE',22X,'% ERROR IN ',A8)
 2010 FORMAT (29X,F10.3,2X,1PE10.3)
C
C     + + + END SPECIFICATIONS + + +
C
C     the "reference" value is taken as the sum of initial storage
C     and inflow because, at one extreme, storage might completely
C     dominate throughput over a printout interval, and at the
C     other extreme, the reverse might be the case
      I6= 6
      REFVAL= STORS+ MATIN
      ERROR= (STOR- STORS)- MATDIF
      IF (REFVAL .GT. 0.0) THEN
C       compute relative error
        RELERR= ERROR/REFVAL
        PCTERR= 100.0*RELERR
      ELSE
C       cannot compute
        RELERR= 0.0
        PCTERR= 0.0
      END IF
C
C     write to output file
      IF (HDRFG .EQ. 1) THEN
C       write header
        WRITE (PRINTU,2000) UNITID
      END IF
      WRITE (PRINTU,2010)  PCTERR, REFVAL
C
      IF (ABS (RELERR) .GT. 0.001) THEN
C       issue a warning
        CALL OMSTD (DATIM)
        CALL OMSTC (I6,COPTY1(1,OPTYP))
        CALL OMSTI (OPNO)
        CALL OMSTR (RELERR)
        CALL OMSTR (STORS)
        CALL OMSTR (STOR)
        CALL OMSTR (MATIN)
        CALL OMSTR (MATDIF)
        SCLU= 238
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             WCOUNT)
      END IF
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   DAYVAL
     I                        (MVAL1,MVAL2,DAY,NDAYS)
C
C     + + + PURPOSE + + +
C     Linearly interpolate a value for this day (DAYVAL), given
C     values for the start of this month and next month (MVAL1 and
C     MVAL2).  ndays is the number of days in this month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,NDAYS
      REAL       MVAL1,MVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MVAL1  - ???
C     MVAL2  - ???
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C
C     + + + LOCAL VARIABLES + + +
      REAL       RDAY,RNDAYS
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
      RDAY  = FLOAT(DAY)
      RNDAYS= FLOAT(NDAYS)
      DAYVAL= MVAL1 + (MVAL2 - MVAL1)*(RDAY - 1)/RNDAYS
C
      RETURN
      END
C
C
C
      SUBROUTINE   TRNVEC
     I                   (DIM,FROM,FACTA,FACTB,
     O                   TO)
C
C     + + + PURPOSE + + +
C     Multiply the elements of a vector FROM, with dimension DIM, by
C     FACTA and add FACTB.  Store the results in a vector to of
C     similar size, i.e. perform a linear transform.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       FACTA,FACTB,FROM(DIM),TO(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     FROM   - ???
C     FACTA  - ???
C     FACTB  - ???
C     TO     - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
      IF ((ABS(FACTB)) .LE. 0.0) THEN
        IF ((ABS(FACTA - 1.0)) .GE. 1.0E-5) THEN
          DO 10 I= 1,DIM
            TO(I)= FROM(I)*FACTA
 10       CONTINUE
        ELSE
          DO 20 I= 1,DIM
            TO(I)= FROM(I)
 20       CONTINUE
        END IF
      ELSE
C       factb must be added
        IF ((ABS(FACTA - 1.0)) .GE. 1.0E-5) THEN
          DO 30 I= 1,DIM
            TO(I)= FROM(I)*FACTA + FACTB
 30       CONTINUE
        ELSE
          DO 40 I= 1,DIM
            TO(I)= FROM(I) + FACTB
 40       CONTINUE
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SETVEC
     I                   (DIM,VALUE,
     O                    VEC)
C
C     + + + PURPOSE + + +
C     Assign the value VALUE to all the elements of a vector VEC.
C     Dimension is DIM.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DIM
      REAL       VALUE,VEC(DIM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIM    - ???
C     VALUE  - ???
C     VEC    - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,DIM
        VEC(I)= VALUE
 10   CONTINUE
C
      RETURN
      END
