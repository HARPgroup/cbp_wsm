C
C     4.0
C
      SUBROUTINE OSUPER
     I                  (FILES,FILNAM)
CGaryShenk     I                  (FILES)
C
C     + + + PURPOSE + + +
C     This module is the operations supervisor.  it calls the various
C     operating modules in the correct sequence and ensures that the
C     various input and output time series are moved into and out of
C     the inpad.  it also reads the osvs from disc and writes them
C     back to disc when an operation terminates or is interrupted
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FILES(15)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FILES  - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION OSV + + +
      INCLUDE      'cmosv.inc'
      INCLUDE      'cmpad.inc'
      INCLUDE      'cosupm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      COUNT,DELT,EXGINF(4),EXKEY,FSTCAL,GRP,INGINF(7),
     $             INKEY,LAST,LSTCAL,MESSU,NGRPS,NOPNS,NXGRPS,OMCODE,
     $             OPINF(8),OPKEY,OPN,OPTNO,OSUPKY,I11,I4,I7,I8,
     $             OSVKND,OSVKST,REPEAT,REPLST,RUNINF(11),TSGKND,
     $             TSGKST,TSPKND,TSPKST,WID,WIDTH,WLAST,XCOUNT,
     $             XDELT,XGRP,XREPET,XWIDTH,FOPKEY,LOPKEY,GKEY,PKEY,
     #             IW,IVLS,RUNWID,EXTFG,STATFG,STIVL,I1
      character*64 FILNAM
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (RUNINF(1),NXGRPS)
C
      EQUIVALENCE  (EXGINF(1),XREPET), (EXGINF(2),XWIDTH),
     $             (EXGINF(3),XDELT),  (EXGINF(4),NGRPS)
C
      EQUIVALENCE  (INGINF(1),REPEAT), (INGINF(2),REPLST),
     $             (INGINF(3),WID),    (INGINF(4),WLAST),
     $             (INGINF(5),DELT),   (INGINF(6),NOPNS),
     $             (INGINF(7),RUNWID)
C
      EQUIVALENCE  (OPINF(1),OMCODE),  (OPINF(2),OPTNO),
     $             (OPINF(3),TSGKST),  (OPINF(4),TSGKND),
     $             (OPINF(5),TSPKST),  (OPINF(6),TSPKND),
     $             (OPINF(7),OSVKST),  (OPINF(8),OSVKND)
C
C     + + + INTRINSICS + + +
      INTRINSIC    MIN
C
C     + + + EXTERNALS + + +
      EXTERNAL     HSPSTA,TSGET,GETOSV,PERLND,IMPLND,RCHRES,COPY
      EXTERNAL     HDISPL,DURANL,GENER,MUTSIN,PUTOSV,TSPUT,PLTGEN
      EXTERNAL     COPYI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'COMMENCING EXECUTION')
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize dummy variables
      I11   = 11
      I1    = 1
      I4    = 4
      I7    = 7
      I8    = 8
      MESSU = FILES(1)
      FSTCAL= 1
C     first record
      OSUPKY= 1
C
      WRITE (MESSU,2000)
C     get information about this run
      CALL COPYI (I11,OSUPM(1,OSUPKY),
     O            RUNINF)
C     find the starting key for the first exgroup
      EXKEY= OSUPKY +1
C
C     Exgroup loop
      DO 170 XGRP= 1,NXGRPS
C       write(99,*) 'xgrp,nxgrps',xgrp,nxgrps
C       get information about this exgroup
        CALL COPYI (I4,OSUPM(1,EXKEY),
     O              EXGINF)
C
C       exspan loop
        DO 160 XCOUNT= 1,XREPET
C         find key for first ingroup
          INKEY= EXKEY+ 1
C
C         ingroup loop
          DO 150 GRP= 1,NGRPS
C           write(99,*) 'osuper:',GRP
            CALL COPYI (I7,OSUPM(1,INKEY),
     O                  INGINF)
C           WRITE(99,*)' runwid',RUNWID
C           find the number of inspans in this exspan
            IF (XCOUNT.NE.XREPET) GO TO 10
              LAST= REPLST
              GO TO 20
 10         CONTINUE
              LAST= REPEAT
 20         CONTINUE
C
C           inspan loop
            DO 140 COUNT= 1,LAST
C
C             find the width of this inspan
              IF (COUNT.NE.LAST) GO TO 70
                WIDTH= WLAST
                GO TO 80
 70           CONTINUE
                WIDTH= WID
 80           CONTINUE
C
C             get ready to perform the operations in this ingroup
              OPKEY= INKEY+ 1
C
C             set up runwid loop
              FOPKEY= OPKEY
              LOPKEY= OPKEY+ NOPNS- 1
              STATFG= 0
              STIVL= 1
C
              IF (RUNWID .GT. 0) THEN
C               get all external time series before running any
C               WRITE(99,*)' getting all time series'
                EXTFG= 1
                DO 82 GKEY= FOPKEY, LOPKEY
                  CALL COPYI (I8,OSUPM(1,GKEY),
     O                        OPINF)
                  IF (TSGKST.GT.0) CALL TSGET (FILES,TSGKST,TSGKND,
     I                             DELT,I1,WIDTH,FSTCAL,EXTFG)
 82             CONTINUE
              END IF
C
              IVLS= WIDTH
C             WRITE(99,*)' set ivls',IVLS
C
C             begin do-until loop for runwid
 85           CONTINUE
C
                OPKEY= FOPKEY
                IF (RUNWID .GT. 0) THEN
C                 only run runwid or remaining intervals
                  IW= MIN (RUNWID,IVLS)
                ELSE
C                 run entire width
                  IW= WIDTH
                END IF
C               WRITE(99,*)'width,ivls,runwid,iw',WIDTH,IVLS,RUNWID,IW
C
C               determine whether or not this is the first or last
C               time through operation loop
                FSTCAL= 0
                LSTCAL= 0
C               WRITE(99,*)'xcount,xrepet,count,last,stivl,width,iw',
C     #                      XCOUNT,XREPET,COUNT,LAST,STIVL,WIDTH,IW
                IF ( (XCOUNT .EQ. 1) .AND. (COUNT .EQ. 1) .AND.
     #               (STIVL .EQ. 1) ) THEN
                  FSTCAL= 1
C                 WRITE(99,*)'fstcal'
                END IF
                IF ( (XCOUNT .EQ. XREPET) .AND.(COUNT .EQ. LAST) .AND.
     #               ((STIVL+ IW) .GT. WIDTH) ) THEN
                  LSTCAL= 1
C                 WRITE(99,*)'lstcal'
                END IF
C             
!             write(*,*)'super, test 6:',  cssss, dvvhr
!                    pause    ! pw 
C               opn loop
                DO 130 OPN= 1,NOPNS
C
C                 write (99,*) '  opn,opkey:',OPN,OPKEY
                  CALL COPYI (I8,OSUPM(1,OPKEY),
     O                        OPINF)
C
                  STATFG= STATFG+ 1
                  IF (STATFG .LE. NOPNS) THEN
C                   show status for pc version
                    CALL HSPSTA
     I                        (NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO,FILNAM)
CGaryShenk     I                         (NOPNS,LAST,COUNT,OPN,OMCODE,OPTNO)
C                   write(99,*)'out of hspsta'
                  END IF
C
                  IF (RUNWID .EQ. 0) THEN
C                   need to get all time series for operation
                    EXTFG= 0
                  ELSE
C                   external done, just get internal
                    EXTFG= 2
                  END IF
C                 get time series data not already done
                  IF (TSGKST.GT.0) CALL TSGET (FILES,TSGKST,TSGKND,
     I                             DELT,STIVL,IW,FSTCAL,EXTFG)
C                 write(99,*)'out of single tsget, tsgkst',tsgkst
C
C                 read the osv from disc
                  CALL GETOSV (OSVKST,OSVKND,MAXOSV,
     O                         OSV)
C
C                 call the appropriate operating module
C                 casentry omcode
C                 write(99,*)'omcode,opn,nopns',omcode,opn,nopns
! pw  write(*,*)'test 1:', cssss,dvvhr! WORKSP(11),worksp(22),worksp(33)
                  GO TO (90,100,110,111,112,113,114,115,116),OMCODE
C                   case 1
 90                   CONTINUE
                        CALL PERLND (STIVL,IW)
                      GO TO 120
C
C                   case 2
 100                  CONTINUE
                        CALL IMPLND (STIVL,IW)
                      GO TO 120
C
C                   case 3
 110                  CONTINUE
                        CALL RCHRES (STIVL,IW,FSTCAL)
!             write(*,*)'super, test 2:',  cssss, dvvhr
!                    pause    ! pw 
                      GO TO 120
C
C                   case 4
 111                  CALL COPY (STIVL,IW)
                      GO TO 120
C
C                   case 5
 112                  CALL PLTGEN (STIVL,IW)
                      GO TO 120
C
C                   case 6
 113                  CALL HDISPL (STIVL,IW,LSTCAL)
                      GO TO 120
C
C                   case 7
 114                  CALL DURANL (STIVL,IW,FSTCAL,LSTCAL)
                      GO TO 120
C
C                   case 8
 115                  CALL GENER (STIVL,IW)
!             write(*,*)'super test 3:',  cssss, dvvhr
!                    pause ! pw 
                      GO TO 120
C
C                   case 9
 116                  CALL MUTSIN (STIVL,IW)
 120              CONTINUE
C                 endcase
C
C                 write the osv to disc
                  CALL PUTOSV (OSVKST,OSVKND,MAXOSV,OSV)
C
                  IF (RUNWID .EQ. 0) THEN
C                   put all time series
                    EXTFG= 0
                  ELSE
C                   only put internal time series
                    EXTFG= 2
                  END IF
C                 output time series from the inpad
C                 WRITE(99,*)' putting single time series',TSPKST
                  IF (TSPKST.GT.0) CALL TSPUT (FILES,TSPKST,TSPKND,
     I                             DELT,STIVL,IW,FSTCAL,LSTCAL,EXTFG)
C
C                 increment key for osupfl
                  OPKEY= OPKEY+ 1
C
 130            CONTINUE
C
                IVLS= IVLS- IW
                STIVL= STIVL+ IW
C               WRITE (99,*)'updated ivls,stivl',IVLS,STIVL
C
C             end of do-until loop
              IF (IVLS .GT. 0) GO TO 85
C
              IF (RUNWID .GT. 0) THEN
C               put all external time series
C               WRITE(99,*)' putting all external time series'
                EXTFG= 1
                DO 135 PKEY= FOPKEY, LOPKEY
                  CALL COPYI (I8,OSUPM(1,PKEY),
     O                        OPINF)
                  IF (TSPKST.GT.0) CALL TSPUT (FILES,TSPKST,TSPKND,
     $                             DELT,I1,WIDTH,FSTCAL,LSTCAL,EXTFG)
 135            CONTINUE
              END IF
C
 140        CONTINUE
C           get key for start of next ingroup
            INKEY= OPKEY
C
 150      CONTINUE
C
 160    CONTINUE
C       get key for start of next exgroup
        EXKEY= OPKEY
C
 170  CONTINUE
C
      RETURN
      END
