C
C
C
      SUBROUTINE   ZTUTOR
     I                   (TERIFL,IBACK,
     M                    GROUP,CODE,SCRCNT,ERRFLG,DONLOG,ILEN,INBUFF)
C
C     + + + PURPOSE + + +
C     tutorial handling
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TERIFL,IBACK,GROUP,CODE,SCRCNT,ERRFLG,DONLOG,ILEN
      CHARACTER*1 INBUFF(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TERIFL - file tutorial being read from
C     IBACK  - integer equivalent for character indicating move back
C     GROUP  - current character group type
C     CODE   - current character code
C     SCRCNT - count of screens allowed to move back
C     ERRFLG - flag to stay in ZGTKEY
C     DONLOG - user wants out of log file flag
C     ILEN   - length of current input record
C     INBUFF - current input record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      BCKSCR,PGROUP,PCODE,I,J,I0,DONFG,
     1             XPOS,YPOS,XLEN,YLEN,FCOL,BCOL,TFCOL,TBCOL,
     2             OXPOS,OYPOS,OXLEN,OYLEN
      CHARACTER*21 WNDNAM
      CHARACTER*1  LNBUFF(80)
C
C     + + + SAVES + + +
      SAVE         XPOS,YPOS,XLEN,YLEN
C
C     + + + EQUIVALENCE + + +
      EQUIVALENCE (TBUFF,LNBUFF)
      CHARACTER*78 TBUFF
C
C     + + + FUNCTIONS + + +
      INTEGER   CHRINT, LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHRINT, LENSTR, COLGET, COLSET, ZDRWDW, ZWRSCR
      EXTERNAL  GETKEY, ZRFRSH
C
C     + + + DATA INITIALIZATIONS + + +
      DATA      XPOS,YPOS,XLEN,YLEN,I0/5*0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (SCRCNT .EQ. 0) THEN
C       no current box
        XPOS= 0
        YPOS= 0
        XLEN= 0
        YLEN= 0
      END IF
C
 70   CONTINUE
C       loop back to here on screen backup
        BCKSCR= 0
        I= 1
        PGROUP= CHRINT (I,INBUFF(3))
        I= 2
        PCODE = CHRINT (I,INBUFF(4))
C       is this a tutor box
        IF (GROUP .EQ. 6) THEN
C         yes, save current box
          OXPOS= XPOS
          OYPOS= YPOS
          OXLEN= XLEN
          OYLEN= YLEN
C         get other parameters
          XPOS = CHRINT (I,INBUFF(6))
          YPOS = CHRINT (I,INBUFF(8))
          XLEN = CHRINT (I,INBUFF(10))
          YLEN = CHRINT (I,INBUFF(12))
          FCOL = CHRINT (I,INBUFF(14))
          BCOL = CHRINT (I,INBUFF(16))
          WRITE(WNDNAM,1000) (INBUFF(I),I=18,38)
C
          IF (OXLEN .GT. 0 .AND.
     1        (XPOS.NE.OXPOS .OR. YPOS.NE.OYPOS .OR.
     2         XLEN.NE.OXLEN .OR. YLEN.NE.OYLEN)) THEN
C           tutor box moved, refresh
            CALL ZRFRSH(I0)
          END IF
          CALL COLGET (TFCOL,TBCOL)
          CALL COLSET (FCOL,BCOL)
          CALL ZDRWDW (ZITYPE,WNDNAM,YPOS-1,XPOS-1,
     1                               YLEN+2,XLEN+2,' ')
          DO 80 I= 1,YLEN
C           get YLEN lines of text and put 'em in the window
            READ (TERIFL,1000) (LNBUFF(J),J=1,XLEN)
            CALL ZWRSCR (TBUFF(1:XLEN),YPOS+I-1,XPOS)
 80       CONTINUE
          CALL COLSET (TFCOL,TBCOL)
        END IF
        DONFG = 0
 90     CONTINUE
C         get keyboard response until desired response is entered
          CALL GETKEY (GROUP,CODE)
          IF (GROUP.EQ.1 .AND. (CODE.GE.97 .AND. CODE.LE.122)) THEN
C           convert character to upper case
            CODE= CODE- 32
          END IF
C
          IF (GROUP.EQ.2 .AND. CODE.EQ.27 .AND.
     1        PGROUP.NE.GROUP .AND. PCODE.NE.CODE) THEN
C           user wants out
            DONLOG= 1
            DONFG = 1
            ERRFLG= 1
C           refresh screen
            CALL ZRFRSH(I0)
          ELSE IF (PGROUP.EQ.0 .OR.
     1       (PGROUP.EQ.GROUP .AND. PCODE.EQ.CODE) .OR.
     2       (CODE.EQ.IBACK .AND. SCRCNT.GT.0)) THEN
C           met desired response or backing up when allowed
            DONFG= 1
          END IF
        IF (DONFG .EQ. 0) GO TO 90
C
        IF (DONLOG .EQ. 0) THEN
C         how about backing up?
          IF (CODE.EQ.IBACK .AND. SCRCNT.GT.0) THEN
C           turn on back up flag
            BCKSCR= 1
C           decrement number of screens allowed to back up
            SCRCNT= SCRCNT- 1
C           back up in log file number of lines for this screen
            DO 100 J= 1,YLEN
              BACKSPACE (TERIFL)
 100        CONTINUE
C
 110        CONTINUE
C             back up through file until # directive found
              BACKSPACE (TERIFL)
              BACKSPACE (TERIFL)
              I= 80
              READ (TERIFL,1000) (INBUFF(J),J=1,I)
              ILEN= LENSTR(I,INBUFF)
            IF (INBUFF(1).NE.'#' .OR. INBUFF(2).NE.'6') GO TO 110
            GROUP= 6
C           stay in this loop
            ERRFLG= 1
          ELSE IF (PGROUP.EQ.0) THEN
C           increment number of screens allowed to back up
            SCRCNT= SCRCNT+ 1
C           stay in this loop
            ERRFLG= 1
          ELSE
C           reset flag and screens allowed to back up
            ERRFLG= 0
            SCRCNT= 0
          END IF
          ILEN= 0
        ELSE
C         dont want to backup, want out
          BCKSCR= 0
          SCRCNT= 0
        END IF
      IF (BCKSCR .NE. 0) GO TO 70
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZRFRSH
     I                   (REFOPT)
C
C     + + + PURPOSE + + +
C     refresh all or part of screen
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   REFOPT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REFOPT - what to refresh - 0:all, 1:box1, 2:box2, 3:box3
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZDRWDW,ZWRTMN,ZWRTB2
C
C     + + + END SPECIFICATIONS + + +
C
      IF (REFOPT .EQ. 0 .OR. REFOPT .EQ. 1) THEN
C       top window (box 1)
        CALL ZDRWDW(ZITYPE,ZSCNAM,ZB1F-1,1,ZB1N+2,80,ZAPNAM)
C       write text
        CALL ZWRTMN (ZB1F,ZB1N,1,ZMNTXT(ZMNCSL),
     M               ZMNLEN(ZMNCSL))
      END IF
C
      IF ((REFOPT.EQ.0 .OR. REFOPT.EQ.2) .AND. ZWN2ID.NE.0) THEN
C       redraw middle window (box2)
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN2ID),ZB2F-1,1,ZB2N+2,80,' ')
C       write the text
        IF (ZWN2ID.EQ.7 .OR. ZWN2ID.EQ.8 .OR. ZWN2ID.EQ.10) THEN
C         help or limits or command
          CALL ZWRTB2 (ZHLTXT,ZHLPLN)
        ELSE IF (ZWN2ID .EQ. 11) THEN
C         xpad
          CALL ZWRTB2(ZXPTXT(ZXPCSL),ZXPLEN(ZXPCSL))
        ELSE IF (ZWN2ID .EQ. 6) THEN
C         status
          CALL ZWRTB2(ZSTTXT(ZSTCSL),ZSTLEN(ZSTCSL))
        END IF
      END IF
C
      IF (REFOPT .EQ. 0 .OR. REFOPT .EQ. 3) THEN
C       redraw bottom window (box 3)
        CALL ZDRWDW(ZITYPE,ZWNNAM(ZWN3ID),ZB3F-1,1,ZB3N+2,80,' ')
C       write the text
        CALL ZWRTMN (ZB3F,ZB3N,0,ZMSTXT,
     M               ZMSLEN)
      END IF
C
      RETURN
      END
