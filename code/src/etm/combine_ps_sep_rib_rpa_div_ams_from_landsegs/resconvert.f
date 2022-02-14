C
C     CONOWINGO DAM REGULATION MODEL.
C     ADAPTED FROM U.S.G.S. CONVOLUTION ROUTING MODEL OF THE LOWER
C     SUSQUEHANNA RIVER.
C
C     INPUT: DAILY TIME SERIES OF FLOW MO/DAY/YR/QIN.  THIS DATA HAS BEEN
C            CONVERTED FROM AN HOURLY TIME SERIES TO A DAILY SERIES BY AN
C            INTERMEDIATE SAS JOB OUTSIDE THIS PROGRAM UNIT
C     OUTPUT: DAYLY TIME SERIES OF FLOW MO/DAY/YR/QOUT ON A DISK IN A
C             FORMAT SUITABLE FOR USE BY THE LOADINGS RELEASE SECTION OF
C             RECEIV SUBMODEL.
C     OPERATION: THE REGULATION MODEL USES THE MONDAY INFLOW TO DETERMINE
C                A SET OF COEFFICIENTS WHICH ARE APPLIED TO THE APPROPRIATE
C                DAY OF THE WEEK.  THE WEEKLY INFLOW/OUTFLOW BALANCE IS
C                THEN COMPUTED AND THE DIFFERENCE SPREAD OVER THE FIRST
C                FIVE DAYS OF THE WEEK TO MAINTAIN A CONSTANT LAKE LEVEL.
C
C      ********************  MODIFIED BY JLKITTLE TO READ HSPF PLOT
C      ********************  FILES AND WRITE THEM, 11/21/85
C      ********************  9/15/89 JLK, OUTPUT INFLOW AND STORAGE
C      ********************  11/13/90 BRBICKNELL - ALLOW TWO SEASONAL RULE CURVES
C      ********************    AND MINIMUM 5000 CFS FROM 4/15 TO 9/15
C      ********************  turned into a subroutine for Phase5 CBP model
C      ********************    by Gary Shenk 10/2003
C
      subroutine resconvert(rscen,rseg,conin,conout,nvals,ny1,nm1,nd1)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      real conin(ndaymax),conout(ndaymax)  !  inflow and outflow
      integer nvals,nv                     ! number of values to read
      integer ny1,nm1,nd1                  ! start date
      integer fnum              ! file number
C      integer conindex

      INTEGER MO(7),DAY(7),YR(7),DAYI,YRI,MOI,TEST,I,J,K,N,SUMMER,L,M
      REAL    QIN(7),QOUT(7),QROUT(7,6),QPRCNT(7,6),Q(7),
     1        QREGU,TOTIN,TOTOUT,DIFFQ,QST,QINI,
     1        QROSUM(7,6),QROWIN(7,6),QPRSUM(7,6),QPRWIN(7,6)
C
      INTEGER IWKDAY, IWKDAY_V2
      REAL    TABLE
      external IWKDAY,TABLE, IWKDAY_V2
C
 1000 FORMAT (6(F8.2,F5.2))
 1010 FORMAT (8X,I2,2I3,6X,G14.5)
 2000 FORMAT (6X,'19',I2,2I3,' 24  0',3G14.5)
 2010 FORMAT ('123456789012345678901234567890')
 2020 FORMAT (8X,I2)
C
      DATA  MO/7*0/,DAY/7*0/,YR/7*0/,QIN/7*0./,QOUT/7*0./
C
       print*,rscen,rseg,nvals,ny1,nm1,nd1
      TOTIN= 0.0
      TOTOUT= 0.0
      TEST= 0
      K= 7
C
C     READ IN TWO TABLES OF OPERATING RULES
C
CGSHENK  previously read default files, modified to use names
Cgshenk      OPEN(8,FILE='RESIN.PLT',STATUS='OLD')
Cgshenk      OPEN(9,FILE='RESOUT.PLT',STATUS='NEW')
      call lencl(rscen,lenrscen)
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      call lencl(rseg,lenrseg)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/reservoir_rules/'//rseg(:lenrseg)//'_R1.DAT'
      call findopen(fnum)
      OPEN(fnum,FILE=fnam,STATUS='OLD',iostat=err)
      if (err.ne.0) go to 991
      DO 10 I= 1, 7
        READ (fnum,1000) (QROSUM(I,J),QPRSUM(I,J),J=1,6)
 10   CONTINUE
      close(fnum)

      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/reservoir_rules/'//rseg(:lenrseg)//'_R2.DAT'
      OPEN(fnum,FILE=fnam,STATUS='OLD',iostat=err)
      if (err.ne.0) go to 991
      DO 20 I= 1, 7
        READ (fnum,1000) (QROWIN(I,J),QPRWIN(I,J),J=1,6)
 20   CONTINUE
      close(fnum)

C
C     READ FIRST DAY OF SIMULATION
C
Cgshenk      DO 30 I= 1, 24
Cgshenk        READ (8,1010) 
Cgshenk        WRITE (9,2020) I
Cgshenk 30   CONTINUE
Cgshenk      READ (8,1010) 
Cgshenk      WRITE (9,2010)

Cgshenk      READ (8,1010)  YRI,MOI,DAYI,QINI
Cgshenk      WRITE (9,2000) YRI,MOI,DAYI,QINI
      nv = 1
      conout(nv) = conin(nv)  ! in = out day 1
      YRI = ny1
      MOI = nm1
      DAYI = nd1
      QST= 0
C
Cgshenk      READ (8,1010) YRI,MOI,DAYI,QINI
      print*,'BHATT resconvert a',YRI,MOI,DAYI
      call tomorrow(YRI,MOI,DAYI)  ! increment day
      print*,'BHATT resconvert b',YRI,MOI,DAYI
      nv = nv + 1
      QINI = conin(nv)
      ! I= IWKDAY(MOI,DAYI,YRI) - 2
      I= IWKDAY_V2(MOI,DAYI,YRI) ! BHATT added
      print*,'my I= ',I
      IF (QINI .GE. 50000.) QREGU= 50000.0
Cgshenk      BACKSPACE 8
      nv = nv - 1
      call yesterday(YRI,MOI,DAYI)

Cgbhatt      I= IWKDAY(01,02,1984)
Cgbhatt      I= IWKDAY(02,02,1984)
Cgbhatt      I= IWKDAY(03,02,1984)
Cgbhatt      I= IWKDAY(04,02,1984)
Cgbhatt      I= IWKDAY(05,02,1984)
Cgbhatt      I= IWKDAY(01,01,2000)
Cgbhatt      I= IWKDAY(05,02,2000)
Cgbhatt      I= IWKDAY(03,02,2010)
Cgbhatt      I= IWKDAY(05,10,2013)
Cgbhatt      stop

C
C     READ IN A WEEK OF INFLOW
C
 40   CONTINUE
Cgbhatt      print*,'A ',I,YRI,MOI,DAYI
      DO 50 J= I, 7
Cgshenk        READ (8,1010,END=100) YR(J),MO(J),DAY(J),QIN(J)
        nv = nv + 1
        call tomorrow(YRI,MOI,DAYI)
        if (nv.gt.nvals) go to 100 ! done
        YR(J) = YRI
        MO(J) = MOI
        DAY(J) = DAYI
        QIN(J) = conin(nv)
        K= J
 50   CONTINUE
C
C     assign appropriate seasonal rule curve based on 
C     first day
      IF ((MO(1) .GT. 4 .AND. MO(1) .LT. 9) .OR.
     1    (MO(1) .EQ. 4 .AND. DAY(1) .GE. 15) .OR. 
     2    (MO(1) .EQ. 9 .AND. DAY(1) .LE. 15))  THEN
C       use summer operating rule
        DO 70 L= 1, 7
          DO 60 M= 1, 6
            QROUT(L,M) = QROSUM(L,M)
            QPRCNT(L,M)= QPRSUM(L,M)
 60       CONTINUE
 70     CONTINUE
        SUMMER = 1
      ELSE
C       use winter operating rule
        DO 90 L= 1, 7
          DO 80 M= 1, 6
            QROUT(L,M) = QROWIN(L,M)
            QPRCNT(L,M)= QPRWIN(L,M)
 80       CONTINUE
 90     CONTINUE
        SUMMER = 0
      END IF
C
      GO TO 110
 100  CONTINUE
      nv = nv - 1
      TEST= 1
 110  CONTINUE
      IF (QIN(I) .LT. 50000.) THEN
        QREGU= QIN(I)
      END IF
      IF (TEST .EQ. 1 .AND. K .EQ. 7) GO TO 200
C
C     THIS LOOP APPLIES THE CONOWINGO DAM REGULATION MODEL
C
      DO 120 J= I, K
         IF (QIN(J) .LE. 50000.) THEN
           Q(J)= TABLE(J,QREGU,QROUT,QPRCNT)
           QOUT(J)= Q(J)*QIN(J)
         ELSE
           QOUT(J)= QIN(J)
         END IF
C         IF (SUMMER .EQ. 1 .AND. QOUT(J) .LT. 5000.) QOUT(J) = 5000.
         IF (SUMMER.EQ.1.AND.QOUT(J).LT.9000.) 
     .     QOUT(J) = 7000.-(7000.-QOUT(J))/2.0
         TOTIN= TOTIN + QIN(J)
         TOTOUT= TOTOUT + QOUT(J)
 120   CONTINUE
C
C     THIS LOOP BALANCES THE INFLOWS AND OUTFLOWS
C
      N= 5
      IF (K .LE. 4) N= K
      IF (I .GT. 1 .AND. I .LE. 5) N= 6-I
      IF (I .LT. 6) THEN
        DIFFQ= (TOTIN - TOTOUT)/N
        DO 130 J=I, 5
          QOUT(J)= QOUT(J) + DIFFQ
 130    CONTINUE
      END IF
C
C     OUTPUT THE RESULTS  
C
      DO 140 J= I, K
        QST= QST + QIN(J) - QOUT(J)
Cgbhatt        if (QST > 1.0) print*,'BHATT date qst ',YRI,MOI,DAYI,QST
Cgshenk        WRITE (9,2000) YR(J),MO(J),DAY(J),QOUT(J),QIN(J),QST
        conout(nv-k+j) = QOUT(J)
C        conindex = nv - k + j
C       WRITE (99,*) conindex,conin(conindex),conout(conindex),
C     .               YR(J),MO(J),DAY(J),QIN(J),QOUT(J),QST
 140  CONTINUE
Cgbhatt      if (QST > 1.0) print*,'BHATT date qst ',YRI,MOI,DAYI,QST
      IF (TEST .EQ. 1) GO TO 200
      I= 1
      TOTIN= 0.0
      TOTOUT= 0.0
C
C     RETURN AND PICK UP ANOTHER WEEK OF INFLOW
C
      GO TO 40
  200 CONTINUE
C
      return

991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*)'Error = ',err
      go to 999

999   call stopreport(report)
 
      END
C
C
      INTEGER FUNCTION IWKDAY
     I                        (tm,td,ty)
C
      INTEGER   M,IDY,IYR
      REAL      X
      integer   tm,td,ty  ! added by gary shenk so that input vars
                          ! do not change

C     FUNCTION IWKDAY DETERMINES THE DAY OF THE WEEK FOR ANY DATE FROM
C     JAN1,1901 TO DEC31,1999 AND RETURNS A CODE NUMBER FROM 1 TO 7.
C     MONDAY IS CODE NO. 1.  INPUT: MONTH,DAY, AND LAST TWO DIGITS OF YEAR.
C     INPUT:INTEGER. OUTPUT:INTEGER.
C
      M = tm
      IDY = td
      IYR = ty

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

C******** BHATT added IWKDAY_V2 funtion
C******** Function fixes the computation of IWKDAY
      INTEGER FUNCTION IWKDAY_V2
     I                        (tm,td,ty)
C
      INTEGER   M,IDY,IYR
      REAL      X
      integer   tm,td,ty  ! added by gary shenk so that input vars
                          ! do not change

C     MONDAY IS CODE NO. 1.  INPUT: MONTH,DAY, AND LAST TWO DIGITS OF YEAR.
C     INPUT:INTEGER. OUTPUT:INTEGER.
C
      integer Y12, Y34, ID, IM, IY, IC, var  ! BHATT added
      integer mtable(2,12)
      data mtable/ 0,-1,3,2,3,3,6,6,1,1,4,4,6,6,2,2,5,5,0,0,3,3,5,5 /

      M = tm
      IDY = td
      IYR = ty

      print*,'IWKDAY a',tm,td,ty

      Y12 = INT(IYR / 100)
      Y34 = IYR - (Y12 * 100)

      ID = MOD(IDY, 7)

      LEAPYR = 0
      IF ( MOD(IYR,4).EQ.0) LEAPYR = 1
      IF ( MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).NE.0 ) LEAPYR = 0 
      LEAPYR = LEAPYR + 1

      IM = MTABLE(LEAPYR,M)

      IY = Y34 + INT(Y34 / 4)

      C  = MOD((39 - Y12), 4) * 2

      IWKDAY_V2 = MOD(ID + IM + IY + C, 7.0) 
      IF (IWKDAY_V2.EQ.0) IWKDAY_V2 = 7

      print*, ID, IM, IY, C, LEAPYR-1

      print*,'IWKDAY = ',IWKDAY_V2
C
      RETURN
      END
C
C
      REAL FUNCTION TABLE 
     I                    (I3,YO,Y,Z)
      INTEGER   I3,I
      REAL      YO,Y(7,6),Z(7,6)
C
C     FUNCTION TABLE PERFORMS STRAIGHT LINE INTERPOLATION AND RETURNS
C     A RATING % TO BE APPLIED TO INFLOW
C
      DO 10 I= 2, 6
        IF(YO .LT. Y(I3,I)) GO TO 20
 10   CONTINUE
      I= 6
 20   CONTINUE
      TABLE= Z(I3,I-1)+(YO-Y(I3,I-1))*(Z(I3,I)-Z(I3,I-1))/
     1       (Y(I3,I)-Y(I3,I-1))
C
      RETURN

      END
