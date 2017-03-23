C
C
C
      SUBROUTINE CHKSEQ
     I                 (OLDDAT,OLDNUM,NEWNUM,OLDOFF,DATLEN,
     I                  MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                  KNT,EFLAG,NEWDAT)
C
C     + + + PURPOSE + + +
C     Check validity of the date,newdat, and card number,
C     newnum, of the new card and also make sure that the
C     new card is in proper seuence. olddat and oldnum
C     give the values for the old card. datlen gives
C     the length of the date and maxcrd gives the
C     maximum card number. the year field of newdat
C     is adjusted so that it contains the calendar
C     year and not just the last two digits of the
C     calendar year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OLDDAT(3),OLDNUM,NEWNUM,OLDOFF,DATLEN,MAXCRD,
     $          BEGYR,FILE,MSGFL,MESSU,YEAR,
     $          KNT,EFLAG,NEWDAT(3)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OLDDAT - ???
C     OLDNUM - ???
C     NEWNUM - ???
C     OLDOFF - ???
C     DATLEN - ???
C     MAXCRD - ???
C     BEGYR  - year at start of run
C     FILE   - ???
C     MSGFL  - fortran unit number of HSPF message file
C     MESSU  - ftn unit no. to be used for printout of messages
C     YEAR   - current year of simulation
C     KNT    - ???
C     EFLAG  - ???
C     NEWDAT - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,YR,MN,DY,ERR,MAXDY,SGRP,SCLU,
     $          NEWOFF,DATIM(5)
C
C     + + + FUNCTIONS + + +
      INTEGER   DYPMON
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  DYPMON,TDIF,OMSG,OMSTI,OMSTD
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 232
C     expand the new date with valid values.
      I=DATLEN +1
   10 IF (I .GT. 3) GO TO 20
        NEWDAT(I)=1
        I=I+1
        GO TO 10
   20 CONTINUE
C
C     check date for validity.
      YR= NEWDAT(1) + 100*(YEAR/100)
      IF ( (NEWDAT(1) .EQ. 0) .AND. (MOD(YEAR,100) .EQ. 99) ) THEN
C       assume next card has jumped the century boundary
        YR= YR+ 100
      ELSE IF ( (NEWDAT(1) .GE. 90) .AND. (MOD(YEAR,100) .LT. 10) ) THEN
C       assume cards start at end of previous century
        YR= YR- 100
      END IF
      NEWDAT(1)=YR
      MN       =NEWDAT(2)
      DY       =NEWDAT(3)
      ERR      =0
      EFLAG    =0
C
      IF (YR .LT. 0) ERR=1
      IF (DATLEN .LE. 1) GO TO 40
C       check the month.
        IF (MN.LT.1.OR.MN .GT. 12) ERR=1
        IF (ERR.NE.0.OR.DATLEN .LE. 2) GO TO 30
C         check the day.
          MAXDY=DYPMON(YR,MN)
          IF (DY.LT.1.OR.DY .GT. MAXDY) ERR=1
   30   CONTINUE
   40 CONTINUE
C
      IF (ERR .LE. 0) GO TO   50
        EFLAG   =1
        ERR     =0
        DATIM(1)=YR
        DATIM(2)=MN
        DATIM(3)=DY
        DATIM(4)=1
        DATIM(5)=1
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSTI (NEWDAT(1))
        CALL OMSTI (NEWDAT(2))
        CALL OMSTI (NEWDAT(3))
        SGRP = 15
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
   50 CONTINUE
C
C     check card number.
      IF (NEWNUM .GT. 0.AND.NEWNUM .LE. MAXCRD) GO TO 60
        EFLAG   =1
        DATIM(1)=YR
        DATIM(2)=MN
        DATIM(3)=DY
        DATIM(4)=1
        DATIM(5)=1
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSTI (NEWNUM)
        CALL OMSTI (NEWDAT(1))
        CALL OMSTI (NEWDAT(2))
        CALL OMSTI (NEWDAT(3))
        SGRP = 16
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
   60 CONTINUE
C
C     check sequence of the card if date is valid.
      IF (EFLAG .NE. 0 .OR. BEGYR .GT. YR) GO TO 100
        CALL TDIF(BEGYR,NEWDAT(1),NEWDAT(2),NEWDAT(3),
     O            NEWOFF)
C
        IF (OLDNUM .GE. 0) GO TO 65
C         clear the sequence checking flag
          OLDNUM = NEWNUM
          GO TO 95
 65     CONTINUE
          IF (NEWOFF .LT. OLDOFF .OR. (NEWOFF+NEWNUM) .LE.
     $       (OLDOFF+OLDNUM)) GO TO 80
C           cards are in proper sequence.
            DO 70 I=1,3
              OLDDAT(I)=NEWDAT(I)
 70         CONTINUE
            OLDNUM=NEWNUM
            OLDOFF=NEWOFF
            GO TO 90
 80       CONTINUE
            EFLAG   =1
C           cards out of sequence.
            DATIM(1)= OLDDAT(1)
            DATIM(2)= OLDDAT(2)
            DATIM(3)= OLDDAT(3)
            DATIM(4)= 1
            DATIM(5)= 1
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSTI (NEWNUM)
            CALL OMSTI (NEWDAT(1))
            CALL OMSTI (NEWDAT(2))
            CALL OMSTI (NEWDAT(3))
            CALL OMSTI (OLDNUM)
            CALL OMSTI (OLDDAT(1))
            CALL OMSTI (OLDDAT(2))
            CALL OMSTI (OLDDAT(3))
            SGRP = 17
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
 95     CONTINUE
 100  CONTINUE
C
      RETURN
      END
C
C     4.1.2.1.2
C
      SUBROUTINE DACRD
     I                 (ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with a hydrocomp daily format and calculate the
C     minute at start and end of card.  return the card values in
C     array crdval and the number of values maxcnt
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ENDMIN,MAXCNT,MIN
      REAL        TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DY,I,I4,J,LPYRF1,DATLEN,MAXCRD,
     $            EFLAG,SCLU,SGRP,DATIM(5)
      REAL        CRDVAL(11)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF,LPYEAR,OMSTD,OMSG,OMSTI,FDATIM
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING DACRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $         'MAXCNT=',I6,3X,'CARD VALUES=',3(E16.7,3X),
     $         (/,3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 232
      IF (TESTFG .GE.1) WRITE(MESSU,2000)
C
C     Fmtcls= 2
C
      DATLEN=2
      MAXCRD=3
C     Format in instruction file
C
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
C
        DO 20 IPVARC=1,20
 20        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
        READ(FILE,PVARC,ERR=30,END=500) (CRDDAT(J),J=1,DATLEN),
     $       CRDNUM,CRDVAL
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
        IF(EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C
C     Whiledo crdyr< year then
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ(FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $         CRDNUM,CRDVAL
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Enddo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year of
C     Start of run
C     Tdif returns minute of start of day.  to get end of this day
C     Interval, add 24 hours or ask for start of next day.
      IF (CRDNO .EQ. 1) DY= 2
      IF (CRDNO .EQ. 2) DY= 12
      IF (CRDNO .EQ. 3) DY= 22
      CALL TDIF (BEGYR,CRDSEQ(1),CRDSEQ(2),DY,      MIN)
C
C     Calculate endmin on card
      IF (CRDNO .EQ. 3) GO TO 110
C       crdno is 1 or 2
        ENDMIN= MIN+ 9*1440
C       maximum number of values allowed in crdval
        MAXCNT= 10
        GO TO 220
C
 110  CONTINUE
C       crdno is 3
C
C       case entry month
        I4=CRDSEQ(2)
        GO TO (130,140,130,120,130,120,130,130,120,130,120,130,170), I4
C
C       case apr., june, sept., nov.   4,6,9,11
 120    CONTINUE
          ENDMIN= MIN+ 9*1440
C         maximum number of values allowed in crdval
          MAXCNT= 10
          GO TO 200
C
C       case jan.,mar.,may,july,aug.,oct.,dec.    1,3,5,7,8,10,12
 130    CONTINUE
          ENDMIN= MIN+ 10*1440
          MAXCNT= 11
          GO TO 200
C
C       case feb.   2
 140    CONTINUE
C         get leap year flag for crdyr
          CALL LPYEAR (CRDYR,LPYRF1)
          IF (LPYRF1 .NE. 1) GO TO 150
            ENDMIN= MIN+ 8*1440
            MAXCNT= 9
            GO TO 160
C
 150      CONTINUE
            ENDMIN= MIN+ 7*1440
            MAXCNT= 8
 160      CONTINUE
          GO TO 200
C
C       case error
 170    CONTINUE
          CALL FDATIM(INPSTR,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (FILE)
          CALL OMSTI (CRDSEQ(2))
          SGRP = 14
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
C
C       endcase
 200    CONTINUE
C
 220  CONTINUE
C
      GO TO 510
C
C     End of file.  set min, endmin, and card year to values
C     Larger than ever used.
 500  CONTINUE
        MIN      = 60000000
        ENDMIN   = 60000000
        CRDSEQ(1)= 3000
        ENDF     = 1
C
 510  CONTINUE
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $        (CRDVAL(I),I=1,11)
C
      RETURN
      END
C
C     4.1.2.1.6
C
      SUBROUTINE FIFCRD
     I                  (ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with a hydrocomp 15-minute format and calculate the
C     minute at start and end of card.  return the card values in
C     array crdval and number of values, maxcnt.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ENDMIN,MAXCNT,MIN
      REAL      TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DATLEN,EFLAG,I,J,MAXCRD,SCLU,
     $            SGRP
      REAL        CRDVAL(12)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING FIFCRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $        'MAXCNT=',I6,3X,'CARDVALUES=',3(E16.7,3X),
     $        (/,3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE. 1) WRITE (MESSU,2000)
C
C     Fmtcls= 6
C
      SCLU = 232
      DATLEN=3
      MAXCRD=8
C
C     Format is in instruction file
C
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
C
        DO 20 IPVARC=1,20
 20        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
        READ(FILE,PVARC,ERR=30,END=500) (CRDDAT(J),J=1,DATLEN),
     $       CRDNUM,CRDVAL
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
        IF(EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C     Whiledo crdyr< year
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ(FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $         CRDNUM,CRDVAL
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Endo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year at start
C     Of run
      CALL TDIF (BEGYR,CRDSEQ(1),CRDSEQ(2),CRDSEQ(3),      MIN)
      MIN= MIN+ (CRDNO-1)*180+ 15
C
C     Calculate endmin on card
      ENDMIN= MIN+ 165
C
C     Maximum number of values allowed in crdval
      MAXCNT= 12
C
      GO TO 510
C
C     End of file.  set min, endmin, and card year to values
C     Larger than ever used
 500  CONTINUE
        MIN      = 60000000
        ENDMIN   = 60000000
        CRDSEQ(1)= 3000
        ENDF     = 1
C
 510  CONTINUE
C
      IF (TESTFG .GE.2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $        (CRDVAL(I),I=1,12)
C
      RETURN
      END
C
C     4.1.2.1.5
C
      SUBROUTINE FIVCRD
     I                  (      ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with a hydrocomp 15-minute format and calculate the
C     minute at start and end of card.  return the card values in
C     array crdval and number of values, maxcnt.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ENDMIN,MAXCNT,MIN
      REAL      TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DATLEN,EFLAG,I,J,MAXCRD,SCLU,
     $            SGRP
      REAL        CRDVAL(36)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING FIVCRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $        'MAXCNT=',I6,3X,'CARDVALUES=',3(E16.7,3X),
     $        (/,3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE. 1) WRITE (MESSU,2000)
C
C     Fmtcls= 5
C
      SCLU = 232
      DATLEN=3
      MAXCRD=8
C
C     Format is in instruction file
C
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
C
        DO 20 IPVARC=1,20
 20        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
        READ(FILE,PVARC,ERR=30,END=500) (CRDDAT(J),J=1,DATLEN),
     $       CRDNUM,CRDVAL
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
      IF (EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C     Whiledo crdyr< year
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ(FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $         CRDNUM,CRDVAL
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Endo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year at start
C     Of run
      CALL TDIF (BEGYR,CRDSEQ(1),CRDSEQ(2),CRDSEQ(3),      MIN)
      MIN= MIN+ (CRDNO-1)*180+ 5
C
C     Calculate endmin on card
      ENDMIN= MIN+ 175
C
C     Maximum number of values allowed in crdval
      MAXCNT= 36
C
      GO TO 510
C
C     End of file.  set min, endmin, and card year to values
C     Larger than ever used
 500  CONTINUE
        MIN      = 60000000
        ENDMIN   = 60000000
        CRDSEQ(1)= 3000
        ENDF     = 1
C
 510  CONTINUE
C
      IF (TESTFG .GE.2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $        (CRDVAL(I),I=1,36)
C
      RETURN
      END
C
C     4.1.2
C
      SUBROUTINE GETSEQ
     I                  (DELT,WIDTH)
C
C     + + + PURPOSE + + +
C     Get values from a sequential file and fill a row of the inpad
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT,WIDTH
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     WIDTH  - inpad width
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE   'ctsin.inc'
      INCLUDE   'ctsex.inc'
      INCLUDE   'ctser.inc'
      INCLUDE   'ctsbu.inc'
      INCLUDE   'ctsbx.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INPEND,INPTIM,WORDI,SCLU,SGRP,DATIM(5)
C
C     + + + EXTERNALS + + +
      EXTERNAL  FIVCRD,FIFCRD,HRCRD,DACRD,SMOCRD,MOCRD,SEQINP
      EXTERNAL  OMSTD,OMSG,OMSTI,FDATIM
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING GETSEQ')
 2010 FORMAT (3X,'INPAD DELT AND WIDTH=',I6,I12,3X,'INPSTR=',I12,
     $         3X,'FORMAT CLASS=',I6)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 232
      IF (TESTFG. LT. 1) GO TO 10
        WRITE (MESSU,2000)
 10   CONTINUE
      IF (TESTFG .LT. 2) GO TO 20
         WRITE (MESSU,2010) DELT,WIDTH,INPSTR,FMTCLS
 20   CONTINUE
C
C     Agg/disagg is not allowed
C     Check delt and delta are equal if not monthly or semi-monthly data
      IF (FMTCLS .EQ. 3 .OR. FMTCLS .EQ. 4) GO TO 50
        IF (DELT .EQ. DELTAT) GO TO 40
          CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (DELT)
          CALL OMSTI (DELTAT)
          CALL OMSTI (FILE)
          SGRP = 10
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
 50   CONTINUE
C
      INPTIM= INPSTR
C
C     Calculate end time of inpad
      WORDI =WIDTH
      INPEND= (WORDI- 1)*DELT+ INPSTR
C
C     Case entry fmtcls
      GO TO (70,90,110,150,180,185,190), FMTCLS
C
C     Case hydrocomp hourly mean data
 70   CONTINUE
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,HRCRD,   INPTIM)
        GO TO 210
C
C     Case hydrocomp daily mean data
 90   CONTINUE
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,DACRD,   INPTIM)
        GO TO 210
C
C     Case hydrocomp semi-monthly mean data
 110  CONTINUE
        IF (DELT .EQ. 1440) GO TO 130
C         time interval on inpad is not daily
          CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (DELT)
          CALL OMSTI (DELTAT)
          CALL OMSTI (FILE)
          CALL OMSTI (FMTCLS)
          SGRP = 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 130    CONTINUE
C
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,SMOCRD,INPTIM)
        GO TO 210
C
C     Case hydrocomp monthly mean data
 150  CONTINUE
        IF (DELT .EQ. 1440) GO TO 170
C         time interval on inpad is not daily
          CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
          CALL OMSTD (DATIM)
          CALL OMSTI (FILE)
          CALL OMSTI (DELT)
          CALL OMSTI (DELTAT)
          CALL OMSTI (FILE)
          CALL OMSTI (FMTCLS)
          SGRP = 11
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 170    CONTINUE
C
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,MOCRD,INPTIM)
        GO TO 210
C
C     Case hydrocomp 5-minute mean data
 180  CONTINUE
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,FIVCRD,INPTIM)
        GO TO 210
C
C     Case hydrocomp 15-minute mean data
 185  CONTINUE
C       increment inptim to point to first interval
        INPTIM= INPTIM+ DELT
        CALL SEQINP (DELT,INPEND,FIFCRD,   INPTIM)
        GO TO 210
C
C     Case error
 190  CONTINUE
        CALL FDATIM (INPSTR,YEAR,TYREND,DATIM)
        CALL OMSTD (DATIM)
        CALL OMSTI (FILE)
        CALL OMSTI (FMTCLS)
        CALL OMSTI (FILE)
        SGRP = 12
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             KNT)
C
C     Endcase
 210  CONTINUE
C
      RETURN
      END
C
C     4.1.2.1.1
C
      SUBROUTINE HRCRD
     I                 (ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with a hydrocomp hourly format and calculate the
C     minute at start and end of card.  return the card values in
C     array crdval and number of values, maxcnt.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ENDMIN,MAXCNT,MIN
      REAL        TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DATLEN,EFLAG,I,J,MAXCRD,SCLU,
     $            SGRP
      REAL        CRDVAL(12)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING HRCRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $        'MAXCNT=',I6,3X,'CARDVALUES=',3(E16.7,3X),
     $        (/3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE. 1) WRITE (MESSU,2000)
C
C     Fmtcls= 1
C
      SCLU = 232
      DATLEN=3
      MAXCRD=2
C
C     Format is in instruction file
C
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
        DO 20 IPVARC=1,20
           WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
 20     CONTINUE
C
        READ(FILE,PVARC,ERR=30,END=500) CRDDAT,CRDNUM,CRDVAL
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
      IF (EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C     Whiledo crdyr< year
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ (FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $          CRDNUM,CRDVAL
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Endo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year at start
C     Of run
      CALL TDIF (BEGYR,CRDSEQ(1),CRDSEQ(2),CRDSEQ(3),MIN)
      IF (CRDNO .NE. 1) GO TO 110
        MIN= MIN+ 60
        GO TO 120
 110  CONTINUE
        MIN= MIN+ 780
 120  CONTINUE
C
C     Calculate endmin on card
      ENDMIN= MIN+ 660
C
C     Maximum number of values allowed in crdval
      MAXCNT= 12
C
      GO TO 510
C
C     End of file.  set min, endmin, and card year to values
C     Larger than ever used
 500  CONTINUE
        MIN      = 60000000
        ENDMIN   = 60000000
        CRDSEQ(1)= 3000
        ENDF     = 1
C
 510  CONTINUE
C
      IF (TESTFG .GE.2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $                         (CRDVAL(I),I=1,12)
C
      RETURN
      END
C
C     4.1.2.1.4
C
      SUBROUTINE MOCRD
     I                 (ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with hydrocomp monthly format and calculate the
C     minute at start and end of card.  return the card values in
C     array crdval and number of values, maxcnt.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ENDMIN,MAXCNT,MIN
      REAL        TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DATLEN,DY,EFLAG,I,J,LPYRF1,
     $            MAXCRD,SCLU,SGRP
      REAL        CRDVAL(366)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF,LPYEAR,MOVAL
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING MOCRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $         'MAXCNT=',I6,3X,'MONTH CRD VALS',3(E16.7,3X),
     $         (/,3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE.1) WRITE(MESSU,2000)
C
C     Fmtcls= 4
C
      SCLU = 232
      DATLEN=1
      MAXCRD=1
C     Crdnum set to 1 because crdnum not read
      CRDNUM=1
C     Format in instruction file
C
C     Place the new card values in a temporary location, tloc
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
C
        DO 20 IPVARC=1,20
 20        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
        READ(FILE,PVARC,ERR=30,END=500) (CRDDAT(J),J=1,DATLEN),
     $                          TLOC
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
        IF(EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C
C     Whiledo crdyr< year then
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ(FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $         TLOC
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Enddo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year of
C     Start of run
C
        MO= 1
        DY= 2
        CALL TDIF (BEGYR,CRDSEQ(1),MO,DY,MIN)
C
C       calculate endmin on card
C       set leap year flag for year on card
        CALL LPYEAR(CRDYR,      LPYRF1)
C
        IF (LPYRF1.NE. 1) GO TO 180
C         525600 = 365*1440
          ENDMIN = MIN+ 525600
C
C         maximum number of values allowed in crdval
C         each month is expanded to daily values
          MAXCNT= 366
C
C         place daily values in crdval
          CALL MOVAL (LPYRF1,TLOC)
          GO TO 200
C
 180    CONTINUE
C         524160 = 364*1440
          ENDMIN = MIN+ 524160
C
          MAXCNT = 365
C
C         place daily values in crdval
          CALL MOVAL (LPYRF1,TLOC)
C
 200    CONTINUE
        GO TO 520
C
 500  CONTINUE
C     End of file
C     Set min,endmin, and crdyr to larger than ever used
      MIN   = 60000000
      ENDMIN= 60000000
      CRDYR = 3000
      ENDF  = 1
C
 520  CONTINUE
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $        (TLOC(I),I= 1,12)
C
      RETURN
      END
C
C     4.1.2.1.4.1
C
      SUBROUTINE MOVAL
     I                 (LPYRF1,TLOC)
C
C     + + + PURPOSE + + +
C     Place monthly data values in crdval(*) after disaggregating
C     into daily values
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LPYRF1
      REAL        TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPYRF1 - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,PT
      REAL        CRDVAL(366)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING MOVAL')
C
C     + + + END SPECIFICATIONS + + +
C
      IF(TESTFG .GE. 1) WRITE(MESSU,2000)
C
C     Pt points to location in array crdval for disaggregated value
      PT= 0
C
      DO 220 I= 1,12
C
C     Case entry month
      GO TO (20,60,20,160,20,160,20,20,160,20,160,20),I
C
C       case 1,3,5,7,8,10,12
 20     CONTINUE
C         jan.,mar.,may,july,aug.,oct.,dec. have 31 days
          DO 40 J= 1,31
            PT        = PT+ 1
            CRDVAL(PT)= TLOC(I)
 40       CONTINUE
          GO TO 210
C
C       case 2
 60     CONTINUE
C         feb. has 28 or 29 days
          IF (LPYRF1 .NE. 1) GO TO 100
            DO 80 J= 1,29
              PT        = PT+ 1
              CRDVAL(PT)= TLOC(I)
 80         CONTINUE
            GO TO 140
C
 100      CONTINUE
            DO 120 J= 1,28
              PT        = PT+ 1
              CRDVAL(PT)= TLOC(I)
 120        CONTINUE
 140      CONTINUE
C
          GO TO 210
C
C       case 4,6,9,11
 160    CONTINUE
C         apr.,june,sept.,nov. have 30 days
          DO 170 J= 1,30
            PT        = PT+ 1
            CRDVAL(PT)= TLOC(I)
 170      CONTINUE
C
          GO TO 210
C
C       case error
C       endcase
 210    CONTINUE
C
 220  CONTINUE
C
      RETURN
      END
C
C
C     4.1.2.1
C
      SUBROUTINE SEQINP
     I                  (DELT,INPEND,SUBNAM,   INPTIM)
C
C     + + + PURPOSE + + +
C     Read a sequential file using proper format,
C     and fill an inpad row
C
C     + + + KEYWORDS + + +
C     ???
C
C     Inptim points to the end of the interval containing the mean
C     value being transferred.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DELT,INPEND,INPTIM
      EXTERNAL    SUBNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - simulation time interval in minutes
C     INPEND - ???
C     SUBNAM - ???
C     INPTIM - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDCNT,ENDMIN,I1,MAXCNT,MIN,NPTS,SCLU,SGRP,DATIM(5)
      REAL        CRDVAL(545),TLOC(12)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
C
C     + + + EXTERNALS + + +
      EXTERNAL  OMSTD,OMSG,OMSTI,INMOD,LPYEAR,LTRAN,FDATIM
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING SEQINP')
 2010 FORMAT (3X,'FMTCLS=',I6,3X,'INPEND=',I12)
 2030 FORMAT (3X,'CRDCNT=',I6,3X,'NPTS=',I6,3X,
     $        'INPTIM=',I12, 3X, 'VALUE PLACED ON INPAD=',E16.7)
C
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 232
      I1   = 1
      IF (TESTFG .GE. 1) WRITE (MESSU,2000)
      IF (TESTFG .GE. 2) WRITE (MESSU,2010) FMTCLS,INPEND
C
C     Npts is the offset from vopadr at which values
C     are placed in the inpad.  npts=1 for mean
C     value data and npts=0 for point value data.
C     mean value data is currently supported.
C
      NPTS=1
C
C     Call subroutine that reads a card or record with the proper
C     Format and returns min, endmin, crdval(*), and maxcnt.
      CALL SUBNAM (ENDMIN,MAXCNT,MIN,TLOC)
C
C     Crdcnt points to a data value in crdval
      CRDCNT= 1
C
C     Do until inptim> inpend or inptim > tendr
 50   CONTINUE
        IF(MIN .LE. ENDMIN) GO TO 60
C         read next card and return min, endmin, crdval(*) and maxcnt
          CALL SUBNAM (ENDMIN,MAXCNT,MIN,TLOC)
C
          CRDCNT= 1
 60     CONTINUE
C
        IF(MIN .GE. INPTIM) GO TO 140
C         check if time needed is on this card
C         whiledo endmin< inptim
 80       CONTINUE
          IF (ENDMIN .GE. INPTIM) GO TO 100
C           read next card and return min, endmin, crdval(*) and maxcnt
            CALL SUBNAM (ENDMIN,MAXCNT,MIN,TLOC)
C
            CRDCNT= 1
            GO TO 80
C         enddo
 100      CONTINUE
C
C         check if present card has inptim
          IF ((MIN .GT. INPTIM) .OR. (INPTIM .GT. ENDMIN)) GO TO 120
C            get value and time on card that is at inptim
            CRDCNT= (INPTIM- MIN)/DELTAT+ 1
            MIN   = INPTIM
 120     CONTINUE
 140    CONTINUE
C
        IF(MIN.LE. INPTIM) GO TO 260
C         fill gaps with zero or undefined values
          IF (GAPVAL .LE. UNDEF) GO TO 200
C           set suitfg off except for first mean value
            IF (STKIND(1) .NE. 1) GO TO 160
              SUITFG= 0
              GO TO 180
 160        CONTINUE
C             mean data on inpad
              IF (INPSTR .NE. INPTIM) SUITFG=0
 180        CONTINUE
 200      CONTINUE
C
C         do until inptim> or = min or inptim> inpend or inptim> tendr
 220      CONTINUE
C           place gapval on inpad at inptim
            XVAR(1)= GAPVAL
            NPTS   = NPTS+ 1
            CALL INMOD (NPTS)
            INPTIM = INPTIM+ DELT
C
C           check for crossing year end boundary
            IF (INPTIM .LE. TYREND) GO TO 235
              YEAR= YEAR+ 1
C             set lpyrfg for this new year
              CALL LPYEAR (YEAR,      LPYRFG)
              IF (LPYRFG .EQ. 0) GO TO 225
                TYREND= 527040+ TYREND
                GO TO 230
 225          CONTINUE
                TYREND= 525600+ TYREND
 230          CONTINUE
 235        CONTINUE
            IF (INPTIM .LT. MIN .AND. INPTIM .LE. INPEND .AND.
     $        INPTIM .LE. TENDR) GO TO 220
C
C         enddo
          GO TO 300
C
 260    CONTINUE
C
C         min= inptim
C         check that crdcnt never exceeds allowable size for number
C         of values read in this format
          IF (CRDCNT .LE. MAXCNT) GO TO 280
            CALL FDATIM (INPTIM,YEAR,TYREND,DATIM)
            CALL OMSTD (DATIM)
            CALL OMSTI (FILE)
            CALL OMSTI (MAXCNT)
            CALL OMSTI (CRDCNT)
            CALL OMSTI (FILE)
            SGRP = 13
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 280      CONTINUE
C
          XVAR(1)= CRDVAL(CRDCNT)
C         perform linear transformation
          IF (LTRNFG .EQ. 1) CALL LTRAN (I1,A,B,   XVAR)
C
          NPTS= NPTS+ 1
C         insert on inpad
          CALL INMOD (NPTS)
C
          INPTIM= INPTIM+ DELT
C
C         check for crossing year end boundary
          IF (INPTIM .LE. TYREND) GO TO 295
            YEAR= YEAR+ 1
C           set lpyrfg for this new year
            CALL LPYEAR (YEAR,      LPYRFG)
            IF (LPYRFG .EQ. 0) GO TO 285
              TYREND= 527040+ TYREND
              GO TO 290
 285        CONTINUE
              TYREND= 525600+ TYREND
 290        CONTINUE
 295      CONTINUE
C
          CRDCNT= CRDCNT+ 1
          MIN   = MIN+ DELTAT
 300    CONTINUE
C
        IF (TESTFG.GE.2) WRITE(MESSU,2030) CRDCNT,NPTS,INPTIM,XVAR(1)
C
        IF (INPTIM.LE.INPEND .AND. INPTIM.LE.TENDR) GO TO 50
C
C     Enddo
C
C     If all values on present card not used, backspace to reread
C     This card
      IF (ENDMIN .LT. INPEND) GO TO 370
        BACKSPACE FILE
C       reset checking for sequential input
        CRDNO = -1
 370  CONTINUE
C
      RETURN
      END
C
C     4.1.2.1.3
C
      SUBROUTINE SMOCRD
     I                  (ENDMIN,MAXCNT,MIN,TLOC)
C
C     + + + PURPOSE + + +
C     Read a card with hydrocomp semi-monthly format and calculate the
C     mnute at start and end of card.  return the card values in
C     array crdval and number of values, maxcnt.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ENDMIN,MAXCNT,MIN
      REAL        TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ENDMIN - ???
C     MAXCNT - ???
C     MIN    - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CRDDAT(3),CRDNUM,CRDYR,DATLEN,DY,EFLAG,I,J,LPYRF1,
     $            MAXCRD,SCLU,SGRP
      REAL        CRDVAL(184)
      CHARACTER*4 PVARC(20)
      INTEGER     IPVARC
      CHARACTER*1 EBUFF(80)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
      EQUIVALENCE (PVARC1,PVARC)
      CHARACTER*1  PVARC1(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHKSEQ,TDIF,LPYEAR,SMOVAL
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING SMOCRD')
 2020 FORMAT (3X,'MIN=',I12,3X,'ENDMIN=',I12,3X,
     $        'MAXCNT=',I6,3X,'SEM-MON CRD VALS',3(E16.7,3X),
     $        (/,3X,6(E16.7,3X)))
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TESTFG .GE.1) WRITE(MESSU,2000)
C
C     Fmtcls= 3
C
      SCLU = 232
      DATLEN=1
      MAXCRD=2
C     Format in instruction file
C
C     Place the new card values in a temporary location, tloc
      EFLAG=0
 10   CONTINUE
        IF (ENDF.EQ.1) GO TO 500
C
        DO 20 IPVARC=1,20
 20        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
        READ(FILE,PVARC,ERR=30,END=500) (CRDDAT(J),J=1,DATLEN),
     $       CRDNUM,TLOC
          GO TO 40
 30     CONTINUE
C         error - cannot read card
          CALL OMSTI (FILE)
          BACKSPACE FILE
          READ (FILE,1000) EBUFF
          I= 80
          CALL OMSTC (I,EBUFF)
          CALL OMSTC (I,PVARC1)
          SGRP= 21
          CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M               KNT)
 40     CONTINUE
        CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I              MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M              KNT,EFLAG,CRDDAT)
        IF(EFLAG.EQ.1) GO TO 10
C
C     Year is year at beginning of inpad
C     Crdyr is year on card
      CRDYR= CRDDAT(1)
C
C     Whiledo crdyr< year then
 50   CONTINUE
      IF (CRDYR .GE. YEAR) GO TO 100
        EFLAG=0
 60     CONTINUE
          IF (ENDF.EQ.1) GO TO 500
C
        DO 70 IPVARC=1,20
 70        WRITE(PVARC(IPVARC),'(A4)')PVAR(IPVARC)
C
          READ(FILE,PVARC,ERR=80,END=500) (CRDDAT(J),J=1,DATLEN),
     $         CRDNUM,TLOC
            GO TO 90
 80       CONTINUE
C           error - cannot read card
            CALL OMSTI (FILE)
            BACKSPACE FILE
            READ (FILE,1000) EBUFF
            I= 80
            CALL OMSTC (I,EBUFF)
            CALL OMSTC (I,PVARC1)
            SGRP= 21
            CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M                 KNT)
 90       CONTINUE
          CALL CHKSEQ(CRDSEQ,CRDNO,CRDNUM,OLDOFF,DATLEN,
     I                MAXCRD,BEGYR,FILE,MSGFL,MESSU,YEAR,
     M                KNT,EFLAG,CRDDAT)
          IF(EFLAG.EQ.1) GO TO 60
        CRDYR=CRDDAT(1)
        GO TO 50
C     Enddo
 100  CONTINUE
C
C     Calculate first minute on card, min, relative to year of
C     Start of run
C
C       get leap year flag for crdyr
        CALL LPYEAR (CRDYR,LPYRF1)
C       min for semi-monthly is returned as minute at end
C       of first day of the month
        IF (CRDNO .NE. 1) GO TO 110
          DY= 2
          MO= 1
          GO TO 120
 110    CONTINUE
          MO= 7
          DY= 2
 120    CONTINUE
C
        CALL TDIF (BEGYR,CRDSEQ(1),MO,DY,MIN)
C
C       calculate endmin on card
        IF (CRDNO .NE. 1) GO TO 180
C         first half of year
          IF (LPYRF1 .NE. 1) GO TO 140
C           182 days in first half of year
C           260640 = 181*1440
            ENDMIN = MIN+ 260640
C
C           maximum number of values allowed in crdval
C           each semi-month is expanded to daily values
            MAXCNT= 182
C
C           place daily values in crdval
            CALL SMOVAL (LPYRF1,TLOC)
            GO TO 160
C
 140      CONTINUE
C           181 days in first half of year
C           259200 = 180*1440
            ENDMIN = MIN+ 259200
            MAXCNT = 181
C
C           place daily values in crdval
            CALL SMOVAL (LPYRF1,TLOC)
C
 160      CONTINUE
C
          GO TO 200
C
 180    CONTINUE
C         crdno  = 2, last half of year
C         184 days in last half of year
C         263520 = 183*1440
          ENDMIN = MIN+ 263520
C
          MAXCNT= 184
C
C         place daily values in crdval
          CALL SMOVAL (LPYRF1,TLOC)
C
 200    CONTINUE
        GO TO 520
C
 500  CONTINUE
C     End of file
C     Set min,endmin, and crdyr to larger than ever used
      MIN   = 60000000
      ENDMIN= 60000000
      CRDYR = 3000
      ENDF  = 1
C
 520  CONTINUE
C
      IF (TESTFG .GE. 2) WRITE (MESSU,2020) MIN,ENDMIN,MAXCNT,
     $                          (TLOC(I),I= 1,12)
C
      RETURN
      END
C
C     4.1.2.1.3.1
C
      SUBROUTINE SMOVAL
     I                  (LPYRF1,TLOC)
C
C     + + + PURPOSE + + +
C     Place semi-monthly data values in crdval(*) after
C     disaggregating into daily values
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LPYRF1
      REAL TLOC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPYRF1 - ???
C     TLOC   - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GETCOM + + +
      INCLUDE     'ctsin.inc'
      INCLUDE     'ctsex.inc'
      INCLUDE     'ctser.inc'
      INCLUDE     'ctsbu.inc'
      INCLUDE     'ctsbx.inc'
      INCLUDE     'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,PT
      REAL        CRDVAL(184)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CRDVAL(1),TBUFF(1))
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ENTERING SMOVAL')
C
C     + + + END SPECIFICATIONS + + +
C
      IF(TESTFG .GE. 1) WRITE(MESSU,2000)
C
C     Pt points to location in array crdval for disaggregated value
      PT= 0
C
C     There are 6 months to an input card
      DO 410 I= 1,6
C       disaggregate first half of each month
        DO 20 J= 1,15
          PT= PT+ 1
          CRDVAL(PT)= TLOC(I*2- 1)
 20     CONTINUE
C
        IF (CRDNO .NE. 1) GO TO 250
C         card read was for first half of year
C
C         case entry i (first 6 months)
          GO TO (40,70,40,170,40,170),I
C
C         case 1,3,5
 40       CONTINUE
C           jan., march, and may have 31 days
            DO 60 J= 1,16
              PT= PT+ 1
              CRDVAL(PT)= TLOC(I*2)
 60         CONTINUE
            GO TO 230
C
C         case 2
 70       CONTINUE
C           feb.
            IF (LPYRF1 .NE. 1) GO TO 110
              DO 90 J= 1,14
                PT        = PT+ 1
                CRDVAL(PT)= TLOC(4)
 90           CONTINUE
              GO TO 150
 110        CONTINUE
              DO 130 J= 1,13
                PT        = PT+ 1
                CRDVAL(PT)= TLOC(4)
 130          CONTINUE
 150        CONTINUE
C
            GO TO 230
C
C         case 4,6
 170      CONTINUE
C           april and june have 30 days
            DO 190 J= 1,15
              PT        = PT+ 1
              CRDVAL(PT)= TLOC(I*2)
 190        CONTINUE
            GO TO 230
C
C         case error
C         endcase
 230      CONTINUE
C
          GO TO 390
C
 250    CONTINUE
          CRDNO= 2
C         card read was for last half of year
C         case entry i (last 6 months)
          GO TO (270,270,310,270,310,270),I
C
C         case 1,2,4,6
 270      CONTINUE
C           july, aug.,oct.,dec. have 31 days
            DO 290 J= 1,16
              PT        = PT+ 1
              CRDVAL(PT)= TLOC(I*2)
 290        CONTINUE
            GO TO 370
C
C         case 3,5
 310      CONTINUE
C           sept. and nov. have 30 days
            DO 330 J= 1,15
              PT        = PT+ 1
              CRDVAL(PT)= TLOC(I*2)
 330        CONTINUE
            GO TO 370
C
C         case error
C         endcase
 370      CONTINUE
C
 390    CONTINUE
C
C     Enddo
 410  CONTINUE
C
      RETURN
      END
