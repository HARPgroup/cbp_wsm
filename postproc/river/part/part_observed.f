      PROGRAM PARTCC
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      character*100 watershedfile
c
c     jeff p. raffensperger
c     uses part as a subroutine
c     partitions flow into baseflow and quickflow for a single reach
c	and both observed and simulated streamflow
c
      character*13 catcode
      integer i
      logical its
c
C      write(*,*) 'Enter a 13-digit reach identifier: '
      read*, rscen,catcode
      fnam = '/model/p5_observed/flow/'//catcode//'.OFLOW'

      do i = 1,25
        if (rscen(i:i).ne.' ') last=i
      end do

      call readcontrol_Rgeoscen(rscen,last,
     O                          geoscen)
      do i = 1,25
        if (geoscen(i:i).ne.' ') last=i
      end do

      watershedfile = catdir//'geo/'//geoscen(:last)//
     .              '/watershed_area.csv'
c
c     if fnam exists,
      inquire(file=fnam,exist=its)
c
      if (its) call part(fnam,watershedfile)
c
   17 FORMAT (A13)
c
      stop
      end
c
c
c     ===================================================================
      SUBROUTINE PART(ofile,watershedfile)
C
C     BY AL RUTLEDGE, USGS           2002 VERSION
C
C     THIS PROGRAM PERFORMS STREAMFLOW PARTITIONING (A FORM OF HYDROGRAPH
C     SEPARATION) USING A DAILY-VALUES RECORD OF STREAMFLOW.  THIS AND
C     OTHER PROGRAMS ARE DOCUMENTED IN USGS WRIR 98-4148. THIS 2002
C     VERSION OF THE PROGRAM READS DAILY-VALUES FROM A FLAT-FILE.
C     (THE ORIGINAL VERSION READ FROM A "Z-FILE".
C  
C     THIS PROGRAM ALSO READS INFORMATION ABOUT STATIONS FROM
C     FILE "watershed_area.csv" (DRAINAGE AREA).
C
C     SEVERAL DECLARATION STATEMENTS PERTAIN TO ARRAY SIZES:
C          MAXIMUM NUMBER OF YEARS = 120.
C          MAXIMUM NUMBER OF DAYS = 44000.
C          (ALSO NOTE INITIALIZING STATEMENT AFTER LABEL 10)
C          RETRIEVE DAILY VALUES AFTER THE YEAR 1890.
c
      INTEGER MODAYS(120,12)
      INTEGER IYR1D(44000),IMO1D(44000),IDA1D(44000)
      INTEGER ITBASE,TBASE1,IYEAR,IMONTH,IDAY,IHR,IMIN
c
      REAL BFMO(3,120,12),SFMO(120,12)
      REAL BFMONTH(120,12)
      CHARACTER*1 FLAG(120,12)
      REAL SFMEAN(3),BFMEAN(3)
      REAL Q(120,12,31),QRD
      REAL Q1D(44000),B1D(44000,3)
      REAL QMININT,QMAXPI,QMINPI,DA
c
      CHARACTER*80 OPTION1
      CHARACTER*1 EST(120,12,31),EST1D(44000),ALLGW(44000)
      character*13 catcode,ccode
      CHARACTER*100 ofile,fname1,fname2,fnam,watershedfile
      CHARACTER*90 LINE
      CHARACTER*1 BLANK(80)
c
      IBEFORE=1890
C
C-------------------------- INITIALIZE VARIABLES : -------------------
C 
      DO 10 IYEAR=1,120
      DO 10 IMONTH=1,12
      DO 10 IDAY=1,31
   10 Q(IYEAR,IMONTH,IDAY)=-999.0
      DO 11 I=1,44000
      ALLGW(I)= ' '
      Q1D(I)= -999.0
      DO 11 IBASE=1,3
      B1D(I,IBASE)= -888.0
   11 CONTINUE
      DO 12 I=1,80
   12 BLANK(I)= ' '
      OPTION1= 't'
C
   14 FORMAT (F8.0)
   15 FORMAT (I6,',',I3,',',I3,',','24,0',',',E14.5)
   16 FORMAT (A13,1X,1F6.2,2x,1I4,'-',1I4,4F8.2,
     $ 1F6.1)
   17 FORMAT (A13,1F8.0)
   18 FORMAT (A26, 1F7.1, 33X, 1F7.1)
   19 FORMAT (1F10.5,1F15.3,3F8.1,10X,1I6,2I3)
   20 FORMAT (1I12, 2F10.4, 1I12, 3I8)
   21 FORMAT (A6)
   22 FORMAT (2X,I4)
   23 FORMAT (1F12.0)
   24 FORMAT (80X)
   25 FORMAT (I4)
   26 FORMAT (A4,1X,A8,1X,I4,1X,I2,1X,I2,1X,A)
   27 FORMAT (12F10.2)
   28 FORMAT (1I6, 2I3, 8F10.3)
   29 FORMAT (1I6, 2I3, 8(1F10.3,1A1) )
C
C -------------- READ THE FILES OF STREAMFLOW: ----------------------
C
c      OPEN (UNIT=10,FILE='partsum.txt')
c      OPEN (UNIT=13,FILE='partmon.txt')
c      OPEN (UNIT=15,FILE='partqrt.txt')
c      OPEN (UNIT=16,FILE='partWY.txt')
c
c     determine filenames to open for baseflow and quickflow
c     observed input file is 'catcode.OFLOW'
c     simulated input flow file is 'catcode.flow'

      OPEN (UNIT=9,FILE=ofile)

      do i = 1,100
        if (ofile(i:i).ne.' ') last = i
      end do

      fnam = ofile(:last-6)
      catcode = ofile(last-18:last-6)
      fname1 = fnam(:last-6)//'.OBFLW'
      fname2 = fnam(:last-6)//'.OQFLW'
      fname1(last-23:last-20) = 'bflw'
      fname2(last-23:last-20) = 'qflw'
c
      OPEN (UNIT = 21,FILE = fname1)
      OPEN (UNIT = 22,FILE = fname2)
c
c    1 READ (10,21,END=2)
c      GO TO 1
c    2 CONTINUE
c
      IYEAR=1
      IFRSTYR=0
c
   35 READ (9,'(A90)',END = 37) LINE
      do i=1,90
	if (LINE(i:i).eq.CHAR(13)) LINE(i:i)=' '
      end do
c
      READ (LINE,*) IYEAR,IMONTH,IDAY,IHR,IMIN,QRD
c
      IF(IFRSTYR.EQ.0) IFRSTYR=IYEAR
      IYEAR = IYEAR-IBEFORE
c      DO 36 J=1,8
c        IF(FLOCHAR(J:J).EQ.CHAR(9)) FLOCHAR(J:8)= ' '
c   36   CONTINUE
c      READ(FLOCHAR,14) FLONUM
c
      Q(IYEAR,IMONTH,IDAY) = QRD
      GO TO 35
   37 CONTINUE
c
      ILSTYR= IYEAR+IBEFORE
c      WRITE (*,*) 'FIRST YEAR IN RECORD = ', IFRSTYR
c      WRITE (*,*) ' LAST YEAR IN RECORD = ', ILSTYR

c
c --- flag nonexistent dates with flow=-9999 -----
c
      do 38 IYEAR=IFRSTYR, ILSTYR
      DO 38 IMONTH=1,12
      DO 38 IDAY=1,31
         IF((IMONTH.EQ.2).AND.(IDAY.GT.29)) THEN
            Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
          END IF
         IF((IMONTH.EQ.2).AND.(IDAY.EQ.29)) THEN
           IDIV=INT((IYEAR)/4.0)
           XDIV=(IYEAR)/4.0
           DIFFER=ABS(IDIV-XDIV)
           IF(DIFFER.GT.0.1) THEN
               Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
             END IF
          END IF
         IF(IDAY.EQ.31) THEN
              IF((IMONTH.EQ.4).OR.(IMONTH.EQ.6).OR.(IMONTH.EQ.9)
     $         .OR.(IMONTH.EQ.11)) THEN
                    Q(IYEAR-IBEFORE,IMONTH,IDAY)= -9999.0
               END IF
          END IF
   38 CONTINUE

c      WRITE (*,*) '                 MONTH  '
c      WRITE (*,*) ' YEAR   J F M A M J J A S O N D'

      IFRSTYR= IFRSTYR-IBEFORE
      ILSTYR= ILSTYR-IBEFORE

      do 72 IYEAR=IFRSTYR, ILSTYR

           DO 55 IMONTH=1,12
   55      FLAG(IYEAR,IMONTH)='.'
           DO 70 IMONTH=1,12
             DO 60 IDAY=1,31
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-999) FLAG(IYEAR,IMONTH)='X'
               IF(Q(IYEAR,IMONTH,IDAY).EQ.-99) FLAG(IYEAR,IMONTH)='X'
   60         CONTINUE
   70         CONTINUE
c            WRITE (*,73) IYEAR+IBEFORE, (FLAG(IYEAR,IMONTH),IMONTH=1,12)

   72 continue

c        WRITE (*,*) ' '
c        WRITE (*,*) ' COMPLETE RECORD = .      INCOMPLETE = X  '
c        WRITE (*,*) ' '
   73 FORMAT (1I6, 2X, 12A2)

      CLOSE (9,STATUS='KEEP')
C
C   ---------------   SELECT TIME PERIOD OF INTEREST:  ----------------------
C
      IYEARST = IFRSTYR + IBEFORE
      IYEAREN = ILSTYR + IBEFORE
c
      IIMAX=0
      DO 80 IIYR=IYEARST,IYEAREN
           IDIV=INT(IIYR/4.0)
           XDIV=IIYR/4.0
           DIFFER=ABS(IDIV-XDIV)
           IF(DIFFER.LT.0.1) THEN
               IIMAX=IIMAX+366
             ELSE
               IIMAX=IIMAX+365
            END IF
   80 CONTINUE
      IYEARST= IYEARST-IBEFORE
      IYEAREN= IYEAREN-IBEFORE
C
C ------ ASSIGN VALUES TO 1-DIMENSIONAL ARRAYS OF DISCHARGE AND DATE: ----
C
      ICOUNT= 0
      IBREAK= 0
      ITESTX=0
      DO 100 IYEAR= IYEARST, IYEAREN
      DO 100 IMONTH= 1,12
      DO 100 IDAY= 1, 31
          SFLOW= Q(IYEAR,IMONTH,IDAY)
          IF(SFLOW.EQ.-9999) GO TO 100
          IF(SFLOW.EQ.-99.OR.SFLOW.EQ.-999) THEN
                 ITEST=0
            ELSE
                 ITEST=1
          ENDIF
          IF(ITEST.EQ.1.AND.ITESTX.EQ.0) THEN
                 IBREAK= IBREAK+1
C                 IF(IBREAK.GT.1) THEN
C                   WRITE (*,*) '***************************************'
C                   WRITE (*,*) '*** THERE IS A BREAK IN THE STREAM- ***'
C                   WRITE (*,*) '*** FLOW RECORD WITHIN THE PERIOD OF **'
C                   WRITE (*,*) '*** INTEREST.  PROGRAM TERMINATION. ***'
C                   WRITE (*,*) '***************************************'
C                   GO TO 1500
C                 ENDIF
          ENDIF
          ITESTX= ITEST
          IF(SFLOW.EQ.-99.OR.SFLOW.EQ.-999) GO TO 100
          ICOUNT = ICOUNT + 1
          Q1D(ICOUNT)= SFLOW
          IYR1D(ICOUNT)= IYEAR
          IMO1D(ICOUNT)= IMONTH
          IDA1D(ICOUNT)= IDAY
          EST1D(ICOUNT)= EST(IYEAR,IMONTH,IDAY)
  100 CONTINUE
c      WRITE (*,*) 'NUMBER OF DAYS (WITH DATA) COUNTED =            ', 
c     $ ICOUNT
c      WRITE (*,*) 'NUMBER OF DAYS THAT SHOULD BE IN THIS INTERVAL =',
c     $ IIMAX
c      WRITE (*,*) ' '

C
C ------------- READ LIST FILE GIVING STATION PROPERTIES:  -------------------
C OBTAIN DRAINAGE AREA(DA), THEN CALCULATE THE REQUIREMENT OF ANTECEDENT
C                          RECESSION (DA**0.2)
C
c      WRITE (*,*) 'READING FILE NAMED watershed_area.csv'
      OPEN (UNIT=11,FILE=watershedfile)

  120 READ (11,'(A90)',END = 121) LINE
      do i=1,90
	if (LINE(i:i).eq.CHAR(13)) LINE(i:i)=' '
      end do
c
      READ (LINE,*) ccode,DA
      IF(ccode.NE.catcode) GO TO 120
c      WRITE (*,*) 'FILE NAME:', catcode
c      WRITE (*,*) 'DRAINAGE AREA:', DA
  121 CLOSE (11,STATUS='KEEP')
c
      IF (DA.LT.1.0) THEN
         WRITE (*,*) '*** DRAINAGE AREA IS SMALLER THAN RECOMMENDED.***'
         WRITE (*,*) '*** THIS CAUSES THE REQUIREMENT OF ANTECEDENT ***'
         WRITE (*,*) '*** RECESSION TO BE LESS THAN TIME INCREMENT  ***'
         WRITE (*,*) '*** OF THE DATA!  RESULTS WILL BE QUESTIONABLE.**'
         WRITE (*,*) ' '
       ENDIF
      IF (DA.GT.500.0) THEN
         WRITE (*,*) ' '
         WRITE (*,*) '*** DRAINAGE AREA IS LARGER THAN RECOMMENDED ***'
         WRITE (*,*) '*********  USE RESULTS WITH CAUTION ************'
         WRITE (*,*) ' '
       ENDIF
C
C  ----DETERMINE THREE INTEGER VALUES FOR THE REQMT. OF ANTEC. RECESSION ----
C  -----(ONE THAT IS LESS THAN, AND TWO THAT ARE MORE THAN DA**0.2 )---------
C
c      WRITE (*,*) '     '
      TBASE1= INT(DA**0.2)
      IF(TBASE1.GT.DA**0.2) THEN
         TBASE1=TBASE1-1
       END IF
      IF(DA**0.2-TBASE1.GT.1.0) THEN
         WRITE (*,*) 'PROBLEMS WITH CALCULATION OF REQUIREMENT OF  '
         WRITE (*,*) 'ANTECEDENT RECESSION. '
         GO TO 1500
        END IF
C
      THRESH= 0.1

C
C --------  REPEAT FROM HERE TO LINE 500 FOR EACH OF THE 3 VALUES  ----------
C --------  FOR THE REQUIREMENT OF ANTECEDENT RECESSION        --------------
C
      ITBASE= TBASE1-1
      DO 500 IBASE=1,3
      ITBASE= ITBASE+1

      DO 185 I=1,ICOUNT
  185 ALLGW(I)= ' '
C
C---DESIGNATE BASEFLOW=STREAMFLOW ON DATES PRECEEDED BY A RECESSION PERIOD ---
C---AND ON DATES OF ZERO STREAMFLOW. SET VARIABLE ALLGW='*' ON THESE DATES.---
C
      DO 200 I= ITBASE+1, ICOUNT
         INDICAT=1
         IBACK= 0
  190    IBACK= IBACK + 1
            IF(Q1D(I-IBACK).LT.Q1D(I-IBACK+1)) INDICAT=0
            IF(IBACK.LT.ITBASE) GO TO 190
            IF(INDICAT.EQ.1) THEN
                   B1D(I,IBASE)= Q1D(I)
                   ALLGW(I)= '*'
               ELSE
                   B1D(I,IBASE)= -888.0
                   ALLGW(I)= ' '
             END IF
  200 CONTINUE
      DO 220 I=1,ITBASE
          IF(Q1D(I).EQ.0.0) THEN
             B1D(I,IBASE)= 0.0
             ALLGW(I)= '*'
           END IF
  220 CONTINUE
C
C ------ DURING THESE RECESSION PERIODS, SET ALLGW = ' ' IF THE DAILY  ------
C ----------  DECLINE OF LOG Q EXCEEDS THE THRESHOLD VALUE: -----------------
C
      I=1
  221 I=I+1
      IF (I.EQ.ICOUNT) GO TO 224
      IF (ALLGW(I).NE.'*') GO TO 221
      IF (ALLGW(I+1).NE.'*') GO TO 221
      IF (Q1D(I).EQ.0.0.OR.Q1D(I+1).EQ.0.0) GO TO 221
      IF (ALLGW(I+1).NE.'*') GO TO 221
      IF(Q1D(I).LT.0.0.OR.Q1D(I+1).LT.0.0) GO TO 221
      DIFF= ( LOG(Q1D(I)) - LOG(Q1D(I+1)) ) / 2.3025851
      IF (DIFF.GT.THRESH) ALLGW(I)= ' '
      IF (I+1.LE.ICOUNT) GO TO 221
  224 CONTINUE

  225 CONTINUE
C
C ----EXTRAPOLATE BASEFLOW IN THE FIRST FEW DAYS OF THE PERIOD OF INTEREST:----
C
      J=0
  230 J=J+1
      IF(ALLGW(J).EQ.' ') GO TO 230
      STARTBF= B1D(J,IBASE)
      DO 240 I=1,J
  240 B1D(I,IBASE)= STARTBF
C
C ---- EXTRAPOLATE BASEFLOW IN LAST FEW DAYS OF PERIOD OF INTEREST: ----------
C
      J=ICOUNT+1
  250 J=J-1
      IF(ALLGW(J).EQ.' ') GO TO 250
      ENDBF= B1D(J,IBASE)
      DO 260 I=J,ICOUNT
  260 B1D(I,IBASE)= ENDBF

C
C ---------  INTERPOLATE DAILY VALUES OF BASEFLOW FOR PERIODS WHEN   ----------
C -------------------------  BASEFLOW IS NOT KNOWN:   -------------------------
C
C                  FIND VERY FIRST INCIDENT OF BASEFLOW DATA:
  280 CONTINUE
      I= 0
  290 CONTINUE
      I=I + 1
      IF(ALLGW(I).EQ.' ') GO TO 290


C                 NOW THAT A DAILY BASEFLOW IS FOUND, MARCH
C                        AHEAD TO FIRST GAP IN DATA:
  300 CONTINUE
      IF(B1D(I,IBASE).EQ.0.0) THEN
          BASE1= -999.0
        ELSE
          IF (B1D(I,IBASE).LT.0.0) THEN
C           WRITE (*,*) '(B)ARG OF LOG FUNC<0 AT I=', I
                BASE1= -999.0
            ELSE
                BASE1= LOG(B1D(I,IBASE)) / 2.3025851
           END IF
       END IF
      I= I + 1
      IF(I.GT.ICOUNT) GO TO 350
      IF(ALLGW(I).NE.' ') GO TO 300
      ITSTART= I-1
C
C                  MARCH THROUGH GAP IN BASEFLOW DATA:
      ITIME= 1
  320 ITIME= ITIME + 1
      IF(ITIME+ITSTART.GT.ICOUNT) GO TO 350
      IF(ALLGW(ITSTART+ITIME).EQ.' ') GO TO 320
      IDAYS= ITIME-1
      T2= ITIME
       IF (B1D(I+IDAYS,IBASE).EQ.0.0) THEN
          BASE2= -999.0
         ELSE
          IF(B1D(I+IDAYS,IBASE).LT.0.0) THEN
C              WRITE (*,*) '(C)ARG OF LOG FUNC<0 AT I=', I+DAYS
               BASE2= -999.0
             ELSE
               BASE2= LOG(B1D(I+IDAYS,IBASE)) / 2.3025851
           END IF
        END IF
C
C                     FILL IN ESTIMATED BASEFLOW DATA:
C
      IF(BASE1.EQ.-999.0.OR.BASE2.EQ.-999.0) THEN
         DO 330 J=1,IDAYS
             B1D(J+ITSTART,IBASE)=0.0
  330      CONTINUE
       ELSE
         DO 340 J=1,IDAYS
            T=J
            Y= BASE1 + (BASE2-BASE1) * T / T2
            B1D(J+ITSTART,IBASE)= 10**Y
  340     CONTINUE
      END IF
C
      I= I + IDAYS
      IF(I.LE.ICOUNT) GO TO 300
C
  350 CONTINUE
C
C ------ TEST TO FIND OUT IF BASE FLOW EXCEEDS STREAMFLOW ON ANY DAY: -----
C
      QLOW=0
      DO 395 I=1,ICOUNT
         IF(B1D(I,IBASE).GT.Q1D(I)) QLOW=1
  395 CONTINUE
      IF(QLOW.EQ.0) GO TO 420
C
C ------- IF ANY DAYS EXIST WHEN BASE FLOW > STREAMFLOW AND SF=0, ASSIGN
C ------- BF=SF, THEN RUN THROUGH INTERPOLATION (ABOVE): 
C
      QLOW2=0
      DO 397 I=1,ICOUNT
        IF (B1D(I,IBASE).GT.Q1D(I).AND.Q1D(I).EQ.0.0) THEN
             QLOW2=1
             B1D(I,IBASE)= 0.0
             ALLGW(I)= '*'
          END IF
  397 CONTINUE
      IF (QLOW2.EQ.1) GO TO 225
C
C --------  LOCATE INTERVALS OF INTERPOLATED BASEFLOW IN WHICH AT LEAST ------
C --------  ONE BASEFLOW EXCEEDS STREAMFLOW ON THE CORRESPONDING DAY. --------
C --------  LOCATE THE DAY ON THIS INTERVAL OF THE MAXIMUM "BF MINUS SF", ----
C --------  AND ASSIGN BASEFLOW=STREAMFLOW ON THIS DAY. THEN RUN THROUGH -----
C --------  THE INTERPOLATION SCHEME (ABOVE) AGAIN.  -------------------------
C
      I=0
  400 CONTINUE
      I=I+1
      IF(B1D(I,IBASE).GT.Q1D(I)) THEN
           QMININT= Q1D(I)
           IF (B1D(I,IBASE).LT.0.0.OR.Q1D(I).LT.0.0) THEN
C               WRITE (*,*) '(D)ARG OF LOG FUNC<0 AT I=', I
                DELMAX=-999.0
              ELSE
                DELMAX= LOG(B1D(I,IBASE)) - LOG(Q1D(I))
            END IF
           ISEARCH= I
           IMIN=I
  402      CONTINUE
           ISEARCH= ISEARCH + 1
           IF(ALLGW(ISEARCH).NE.' '.OR.B1D(ISEARCH,IBASE).LE.
     $        Q1D(ISEARCH).OR.ISEARCH.GT.ICOUNT) GO TO 405
           IF (B1D(ISEARCH,IBASE).LT.0.0.OR.Q1D(ISEARCH).LT.0.0) THEN
C              WRITE (*,*) '(E)ARG OF LOG FUNC<0 AT I=', ISEARCH
               DEL= -999.0
             ELSE
               DEL= LOG(B1D(ISEARCH,IBASE)) - LOG(Q1D(ISEARCH))
            END IF
           IF(DEL.GT.DELMAX) THEN
               DELMAX=DEL
               QMININT= Q1D(ISEARCH)
               IMIN= ISEARCH
            END IF
           IF(ALLGW(ISEARCH).EQ.' ') GO TO 402
  405      CONTINUE
           B1D(IMIN,IBASE)= QMININT
           ALLGW(IMIN)= '*'
           I= ISEARCH+1
        END IF
      IF(I.LT.ICOUNT) GO TO 400

      IF(QLOW.EQ.1) GO TO 225

  420 CONTINUE
C
C
C ---------- DETERMINE RANGE OF VALUES FOR STREAMFLOW AND BASEFLOW:  ----------
C
            QMINPI= Q1D(1)
            QMAXPI= Q1D(1)
            BMINPI= B1D(1,IBASE)
            BMAXPI= B1D(1,IBASE)
            DO 470 I=1,ICOUNT
                IF(Q1D(I).LT.QMINPI) QMINPI=Q1D(I)
                IF(Q1D(I).GT.QMAXPI) QMAXPI=Q1D(I)
                IF(B1D(I,IBASE).LT.BMINPI) BMINPI= B1D(I,IBASE)
                IF(B1D(I,IBASE).GT.BMAXPI) BMAXPI= B1D(I,IBASE)
  470        CONTINUE
C
  500 CONTINUE
c      WRITE (*,*) ' '

C
C  ------- WRITE DAILY STREAMFLOW AND BASEFLOW FOR ONE OR MORE YEARS: ---

           IYRSEL1= IYEARST+IBEFORE
           IYRSEL2= IYEAREN+IBEFORE

           NUMDAYS=0
           DO 825 IIYR=IYRSEL1,IYRSEL2
               IDIV=INT(IIYR/4.0)
               XDIV=IIYR/4.0
               DIFFER=ABS(IDIV-XDIV)
               IF(DIFFER.LT.0.1) THEN
                   NUMDAYS=NUMDAYS+366
                 ELSE
                   NUMDAYS=NUMDAYS+365
                END IF
  825      CONTINUE
           IYRSEL1=IYRSEL1-IBEFORE
           IYRSEL2=IYRSEL2-IBEFORE

           I=0
  830      I=I+1
           IF(IYR1D(I).NE.IYRSEL1) GO TO 830
           ISTRT=I
c           WRITE (12,*) 'THIS IS FILE '//ofile(1:8)//'.obflw'//' WHICH 
c     $     GIVES DAILY OUTPUT OF PROGRAM PART. '
c           WRITE (12,*) 'NOTE -- RESULTS AT THIS SMALL TIME SCALE ARE PR
c     $OVIDED FOR '
c           WRITE (12,*) 'THE PURPOSES OF PROGRAM SCREENING AND FOR GRAPH
c     $ICS, BUT '
c           WRITE (12,*) 'SHOULD NOT BE REPORTED OR USED QUANTITATIVELY '
c
c           WRITE (12,*) ' INPUT FILE = ', ofile
c           WRITE (12,*) ' STARTING YEAR = ', IYRSEL1+IBEFORE
c           WRITE (12,*) ' ENDING YEAR =   ', IYRSEL2+IBEFORE
c           WRITE (12,*) '                         BASE FLOW FOR EACH'
c           WRITE (12,*) '                            REQUIREMENT OF  '
c           WRITE (12,*) '          STREAM         ANTECEDENT RECESSION '
c           WRITE (12,*) ' DAY #     FLOW        #1         #2         #3
c     $          DATE '
           ICNT=0
           DO 835 I=ISTRT, ISTRT+NUMDAYS-1
              IF(Q1D(I).GE.0.0.AND.IYR1D(I).LE.IYRSEL2) THEN
                 ICNT=ICNT+1
              END IF
  835       CONTINUE
           NUMDAYS=ICNT
           DO 840 I=ISTRT, ISTRT+NUMDAYS-1
              JULDATE= I-ISTRT+1
                 WRITE (21,15) IYR1D(I)+IBEFORE,IMO1D(I),
     $			IDA1D(I),B1D(I,1)
                 WRITE (22,15) IYR1D(I)+IBEFORE,IMO1D(I),
     $			IDA1D(I),Q1D(I)-B1D(I,1)
  840       CONTINUE
c           WRITE (*,*) ' '
c           WRITE (*,*) 'DAILY RECORDS WRITTEN TO FILE '//ofile(1:8)//
c     $         '.obflow'
c           WRITE (*,*) ' '
  845  CONTINUE
c
C -------------  GO HERE FOR MAKING ALL FINAL CALCULATIONS:  -------------
C
C ------------------ FIRST DETERMINE MONTHLY RESULTS:  -------------------

C  INITIALIZE ARRAYS:

            DO 850 IYEAR=1,120
            DO 850 IMONTH=1,12
            SFMO(IYEAR,IMONTH)=0.0
            MODAYS(IYEAR,IMONTH)=0
            BFMONTH(IYEAR,IMONTH)=0.0
  850       CONTINUE

            DO 860 IBASE=1,3
            DO 860 IYEAR=1,120
            DO 860 IMONTH=1,12
  860       BFMO(IBASE,IYEAR,IMONTH)=0.0
        
C  DETERMINE MONTHLY STREAMFLOW AND DAYS IN EACH MONTH:
            DO 870 I=1,ICOUNT
              SFMO(IYR1D(I),IMO1D(I))= SFMO(IYR1D(I),IMO1D(I)) + Q1D(I)
              MODAYS(IYR1D(I),IMO1D(I))= MODAYS(IYR1D(I),IMO1D(I)) + 1
  870        CONTINUE

C  MONTHLY STREAMFLOW (IN CFS):
            DO 880 IYEAR=1,120
            DO 880 IMONTH=1,12
            IF (MODAYS(IYEAR,IMONTH).EQ.0) THEN
                 SFMO(IYEAR,IMONTH)= -99.99
              ELSE
                 SFMO(IYEAR,IMONTH)=
     $                SFMO(IYEAR,IMONTH)/MODAYS(IYEAR,IMONTH)
             END IF
  880       CONTINUE


C   DETERMINE MONTHLY STREAMFLOW IN INCHES AND FLAG AS -99.99 IF MONTH 
C   IS INCOMPLETE.  ALSO DETERMINE TOTAL OF MONTHLY AMOUNTS:
            TOTXX=0.0
            DO 890 I=IYEARST, IYEAREN
            DO 890 J=1,12
              SFMO(I,J)= SFMO(I,J)*MODAYS(I,J) / (26.888889*DA)
              IF (FLAG(I,J).EQ.'X') THEN
                     SFMO(I,J)= -99.99
                    ELSE
                     TOTXX=TOTXX + SFMO(I,J)
                   ENDIF
  890       CONTINUE

C   DETERMINE YEARLY STREAMFLOW IN INCHES AND FLAG AS -99.99 IF
C   YEAR IS INCOMPLETE.  WRITE MONTHLY AND YEARLY AMOUNTS.
c      WRITE (13,*) '  '
c      WRITE (13,*) ' THIS IS FILE PARTMON.TXT FOR INPUT FILE: ', ofile
c      WRITE (13,*) ' '
c      WRITE (13,*) '                       MONTHLY STREAMFLOW (INCHES):'
c      WRITE (13,*) '         J     F     M     A     M     J     J     A
c     $     S     O     N     D   YEAR'

c      WRITE (15,*) '  '
c      WRITE (15,*) ' THIS IS FILE PARTQRT.TXT FOR INPUT FILE: ', ofile
c      WRITE (15,*) '  '
c      WRITE (15,*) '       QUARTER-YEAR STREAMFLOW IN INCHES         '
c      WRITE (15,*) '       --------------------------------          '
c      WRITE (15,*) '         JAN-    APR-    JULY-   OCT-    YEAR    '
c      WRITE (15,*) '         MAR     JUNE    SEPT    DEC     TOTAL   '

            DO 910 I=IYEARST,IYEAREN
               SUMYR=0.0
               IMISS=0
               DO 900 J=1,12
                 SUMYR=SUMYR + SFMO(I,J)
                 IF (SFMO(I,J).LT.-99.0) IMISS=1
  900          CONTINUE
                 IF(IMISS.EQ.1) SUMYR= -99.99
c              WRITE (13,913) I+IBEFORE, (SFMO(I,J),J=1,12), SUMYR

              AA = SFMO(I,1)+SFMO(I,2)+SFMO(I,3)
                 IF (SFMO(I,1).LT.-99) AA= -99.99
                 IF (SFMO(I,2).LT.-99) AA= -99.99
                 IF (SFMO(I,3).LT.-99) AA= -99.99
              BB = SFMO(I,4)+SFMO(I,5)+SFMO(I,6)
                 IF (SFMO(I,4).LT.-99) BB= -99.99
                 IF (SFMO(I,5).LT.-99) BB= -99.99
                 IF (SFMO(I,6).LT.-99) BB= -99.99
              CC = SFMO(I,7)+SFMO(I,8)+SFMO(I,9)
                 IF (SFMO(I,7).LT.-99) CC= -99.99
                 IF (SFMO(I,8).LT.-99) CC= -99.99
                 IF (SFMO(I,9).LT.-99) CC= -99.99
              DD = SFMO(I,10)+SFMO(I,11)+SFMO(I,12)
                 IF (SFMO(I,10).LT.-99) DD= -99.99
                 IF (SFMO(I,11).LT.-99) DD= -99.99
                 IF (SFMO(I,12).LT.-99) DD= -99.99

c              WRITE (15,1053) I+IBEFORE, AA, BB, CC, DD, SUMYR



  910       CONTINUE


c            WRITE (13,*) ' '
c            WRITE (13,*) '                TOTAL OF MONTHLY AMOUNTS  = ',
c     $                                                             TOTXX
c
  913       FORMAT (1I6, 13F6.2)


C  DETERMINE SUM OF DAILY BASE FLOWS FOR EACH MONTH (IN CFS),
C  FOR EACH REQMT. OF ANTEC. RECESSION:
            DO 950 IBASE=1,3
             DO 940 I=1,ICOUNT
                BFMO(IBASE,IYR1D(I),IMO1D(I))=
     $                    BFMO(IBASE,IYR1D(I),IMO1D(I)) + B1D(I,IBASE)
  940        CONTINUE
  950       CONTINUE

C  NOW DIVIDE EACH BY THE NUMBER OF DAYS IN THE MONTH, TO OBTAIN MEAN
C  FLOW IN EACH MONTH IN CFS, FOR EACH OF THE THREE VALUES OF
C  THE REQUIREMENT OF ANTECEDENT RECESSION....
            DO 960 IBASE=1,3
            DO 960 IYEAR=1,120
            DO 960 IMONTH=1,12
              IF (MODAYS(IYEAR,IMONTH).EQ.0) THEN
                   BFMO(IBASE,IYEAR,IMONTH) = -99.99
                ELSE
                   BFMO(IBASE,IYEAR,IMONTH)=BFMO(IBASE,IYEAR,IMONTH)/
     $                          MODAYS(IYEAR,IMONTH)
               END IF
  960       CONTINUE

C  DETERMINE MONTHLY BASE FLOW (IN CFS) BY INTERPOLATION BETWEEN
C  BASE FLOW FOR TWO DIFFERENT REQUIREMENTS OF ANT. RECESSION:
            DO 1000 IYEAR=1,120
            DO 1000 IMONTH=1,12
            IF (FLAG(IYEAR,IMONTH).NE.'X') THEN
               X= (DA**0.2) - TBASE1 + 1     
               BFMONTH(IYEAR,IMONTH) = BFMO(1,IYEAR,IMONTH)  +
     $           (X-1) * ( BFMO(2,IYEAR,IMONTH)-BFMO(1,IYEAR,IMONTH) )
             ELSE
              BFMONTH(IYEAR,IMONTH)= -99.99
            ENDIF
 1000       CONTINUE


C   DETERMINE MONTHLY BASE FLOW IN INCHES AND FLAG AS -99.99 IF
C   MONTH IS INCOMPLETE.  ALSO DETERMINE TOTAL OF MONTHLY AMOUNTS:
            TOTXX=0.0
            DO 1030 I=IYEARST, IYEAREN
            DO 1030 J=1,12
              BFMONTH(I,J)= BFMONTH(I,J)*MODAYS(I,J) / (26.888889*DA)
              IF (FLAG(I,J).EQ.'X') THEN
                     BFMONTH(I,J)= -99.99
                    ELSE
                     TOTXX=TOTXX + BFMONTH(I,J)
                   ENDIF
 1030       CONTINUE

C   DETERMINE YEARLY BASE FLOW IN INCHES AND FLAG AS -99.99 IF
C   YEAR IS INCOMPLETE.  WRITE MONTHLY AND YEARLY AMOUNTS.
c      WRITE (13,*) ' '
c      WRITE (13,*) ' '
c      WRITE (13,*) '                        MONTHLY BASE FLOW (INCHES):'
c      WRITE (13,*) '         J     F     M     A     M     J     J     A
c     $     S     O     N     D   YEAR'
c
c      WRITE (15,*) '  '
c      WRITE (15,*) '  '
c      WRITE (15,*) '       QUARTER-YEAR BASE FLOW IN INCHES          '
c      WRITE (15,*) '       --------------------------------          '
c      WRITE (15,*) '         JAN-    APR-    JULY-   OCT-    YEAR    '
c      WRITE (15,*) '         MAR     JUNE    SEPT    DEC     TOTAL   '
c      WRITE (16,*) ' '
c      WRITE (16,*) 'Results on the basis of the '
c      WRITE (16,*) 'water year (Oct 1 to Sept 30) '
c      WRITE (16,*) ' '
c      WRITE (16,*) '        Year              Total '
c      WRITE (16,*) '--------------------      ----- '
c
            DO 1050 I=IYEARST,IYEAREN
               SUMYR=0.0
               IMISS=0
               DO 1040 J=1,12
                 SUMYR=SUMYR + BFMONTH(I,J)
                 IF (BFMONTH(I,J).LT.-99.0) IMISS=1
 1040          CONTINUE
                 IF(IMISS.EQ.1) SUMYR= -99.99
c              WRITE (13,913) I+IBEFORE, (BFMONTH(I,J),J=1,12), SUMYR

              AA = BFMONTH(I,1)+BFMONTH(I,2)+BFMONTH(I,3)
                 IF (BFMONTH(I,1).LT.-99) AA= -99.99
                 IF (BFMONTH(I,2).LT.-99) AA= -99.99
                 IF (BFMONTH(I,3).LT.-99) AA= -99.99
              BB = BFMONTH(I,4)+BFMONTH(I,5)+BFMONTH(I,6)
                 IF (BFMONTH(I,4).LT.-99) BB= -99.99
                 IF (BFMONTH(I,5).LT.-99) BB= -99.99
                 IF (BFMONTH(I,6).LT.-99) BB= -99.99
              CC = BFMONTH(I,7)+BFMONTH(I,8)+BFMONTH(I,9)
                 IF (BFMONTH(I,7).LT.-99) CC= -99.99
                 IF (BFMONTH(I,8).LT.-99) CC= -99.99
                 IF (BFMONTH(I,9).LT.-99) CC= -99.99

                 if (I.gt.IYEARST) then
                     WY=DD+AA+BB+CC
                     IF(DD.LT.-99) WY= -99.99
                     IF(AA.LT.-99) WY= -99.99
                     IF(BB.LT.-99) WY= -99.99
                     IF(CC.LT.-99) WY= -99.99
c                     WRITE (16,1055) I+IBEFORE-1,I+IBEFORE,WY
                   end if

              DD = BFMONTH(I,10)+BFMONTH(I,11)+BFMONTH(I,12)
                 IF (BFMONTH(I,10).LT.-99) DD= -99.99
                 IF (BFMONTH(I,11).LT.-99) DD= -99.99
                 IF (BFMONTH(I,12).LT.-99) DD= -99.99

c              WRITE (15,1053) I+IBEFORE, AA, BB, CC, DD, SUMYR
              


 1050       CONTINUE

 1053 FORMAT (1I6, 5F8.2)
 1055 format ('Oct ',1i4,' to Sept ',1i4,3x,5f8.2)


C----------------------------

c            WRITE (13,*) ' '
c            WRITE (13,*) '                 TOTAL OF MONTHLY AMOUNTS = ',
c     $                                                             TOTXX
c
c      WRITE (13,*) ' '
c      WRITE (13,*) 'RESULTS ON THE MONTHLY TIME SCALE SHOULD BE USED WIT
c     $H CAUTION. '
c      WRITE (13,*) 'FILES PARTQRT.TXT AND PARTSUM.TXT GIVE RESULT AT THE
c     $           '
c      WRITE (13,*) 'CORRECT TIME SCALES (QUARTER YEAR, YEAR, OR MORE). '

c            WRITE (*,*) ' '
c            WRITE (*,*) 'QUARTER-YEAR OUTPUT WRITTEN TO "partqrt.txt" '
c            WRITE (*,*) ' '
c            WRITE (*,*) 'MONTHLY OUTPUT WRITTEN TO "partmon.txt" '
c            WRITE (*,*) ' '

C  -----------------   NOW DETERMINE LONG-TERM RESULTS:  ----------------
C
            IF (IIMAX.EQ.ICOUNT) THEN

c             WRITE (*,*) 'LONG-TERM CALCULATIONS FOLLOW: '
c             WRITE (*,*) ' '
c             WRITE (*,*) '          REQUIREMENT                       '
c             WRITE (*,*) '              OF                            '
c             WRITE (*,*) ' CALCU-   ANTECEDENT                        '
c             WRITE (*,*) ' LATION   RECESSION   STREAMFLOW  BASE-FLOW '
c             WRITE (*,*) ' NUMBER     (DAYS)      (CFS)       (CFS)   '

              DO 1200 IBASE=1,3
              TOTSF=0.0
              TOTBF=0.0
               DO 1100 I=1,ICOUNT
                  TOTSF= TOTSF + Q1D(I)
                  TOTBF= TOTBF + B1D(I,IBASE)
 1100          CONTINUE
              SFMEAN(IBASE)= TOTSF/ICOUNT
              BFMEAN(IBASE)= TOTBF/ICOUNT
c             WRITE (*,1201) IBASE,IBASE+TBASE1-1,SFMEAN(IBASE),
c    $                    BFMEAN(IBASE)
 1200         CONTINUE
 
 1201 format (1I5, 1I11, 3x, 2f12.4)

              IF(SFMEAN(1).NE.SFMEAN(2)) THEN
                WRITE (*,*) 'STREAMFLOW VARIES BETWEEN DIFFERENT '
                WRITE (*,*) 'VALUES OF THE REQMT ANT. RECESSION !!!'
               END IF

              SF= ( SFMEAN(1) + SFMEAN(2) + SFMEAN(3) ) / 3.0
c              WRITE (*,*) ' '
c
C  CURVE INTERPOLATION USING RESULTS FROM THE THREE REQMTS OF ANT. RECESSION
c
c             WRITE (*,*) 'The requirement of antecedent recession,'
c             WRITE (*,*) 'expressed as a real number, is.... '
c             WRITE (*,*) DA**0.2, ' days '
c             WRITE (*,*) 'Because this program works with daily  '
c             WRITE (*,*) 'streamflow, time is expressed in integers.'
c             WRITE (*,*) 'Interpolation is required.....'
C
      A = ( BFMEAN(1)-BFMEAN(2)-BFMEAN(2)+BFMEAN(3) ) / 2.0
      B = BFMEAN(2) - BFMEAN(1) - (3.0*A)
      C = BFMEAN(1) - A - B
      X = (DA**0.2) - TBASE1 + 1
      BF = (A*X**2) + (B*X) + C
c      WRITE (*,*) ' '
c      WRITE (*,*) 'RESULTS USING NONLINEAR INTERPOLATION....'
c      WRITE (*,*) ' '
c      WRITE (*,*) 'MEAN BASE FLOW (CFS) = ', BF
c      WRITE (*,*) '             (IN/YR) = ', BF*13.5837/DA
c      WRITE (*,*) ' '
c
C   LINEAR INTERPOLATION BETWEEN RESULTS FOR THE FIRST AND SECOND VALUES
C   OF THE REQUIREMENT OF ANTECEDENT RECESSION.....
c
              X= (DA**0.2) - TBASE1 + 1
              BFLINE  = BFMEAN(1)  +
     $           (X-1) * ( BFMEAN(2) - BFMEAN(1) ) 
c             WRITE (*,*) ' '
c             WRITE (*,*) 'Results using linear interpolation.... '
c             WRITE (*,*) ' '
c             WRITE (*,*) 'mean base flow (cfs) = ', BFLINE
c             WRITE (*,*) '             (in/yr) = ', BFLINE*
c    $                                               13.5837/DA
c             WRITE (*,*) ' '


c              WRITE (10,16) catcode,DA,IYEARST+IBEFORE,
c     $               IYEAREN+IBEFORE, SF, SF*13.5837/DA,
c     $               BF, BF*13.5837/DA, 100*BF/SF
c              WRITE (*,*) ' '
c              WRITE (*,*) 'SUMMARY OUTPUT WRITTEN TO "partsum.txt" '
              GO TO 1500

           ELSE
c              WRITE (*,*) 'SUMMARY RESULTS ARE NOT WRITTEN TO FILE '
c              WRITE (*,*) 'PARTSUM.TXT BECAUSE RECORD IS NOT COMPLETE '
c              WRITE (*,*) 'THROUGH THE PERIOD OF INTEREST. '
c              WRITE (10,1203) catcode,DA,IYEARST+IBEFORE,
c     $               IYEAREN+IBEFORE,
c     $               '******** record incomplete ********'
 1203 FORMAT (A12,1X,1F6.2,2X,1I4,'-',1I4, A35)
           ENDIF
c
 1500 CONTINUE
c
c      CLOSE (10,STATUS='KEEP')
c      CLOSE (13,STATUS='KEEP')
c      CLOSE (15,STATUS='KEEP')
c      CLOSE (16,STATUS='KEEP')
      CLOSE (21,STATUS='KEEP')
      CLOSE (22,STATUS='KEEP')
c
      RETURN
      END
