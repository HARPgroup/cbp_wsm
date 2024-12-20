************************************************************************
** subroutine to get pltgen file for each output variable of interest **
**  this code was verified by spreadsheed 3/17/2005                   **
************************************************************************
      implicit none

      integer:: file1          ! file number

      integer:: year,month,day,hour,zero
      integer:: year1            ! the first and last simulation year

      integer:: maxvals, nval, n, i, np
      parameter (maxvals = 900000)
      
      real:: value,annvalue,allval(maxvals)
      real:: aveann, aveload

      integer npcnt
      parameter (npcnt = 43)
      real   pcntle(npcnt),taupctle(npcnt)
      data pcntle /100,99,98,97,96,95,94,93,92,91,90,88,86,84,82,80,75,
     .              70,65,60,55,50,45,40,35,30,25,20,18,16,14,12,10,9,
     .               8,7,6,5,4,3,2,1,0/

      integer:: ord(maxvals)

********** END DECLARATIONS  *******************************************

      fnam = 'myfile'
      open(file1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nval = 1
      do
        read(file1,*,err=994,end=111) allval(nval)
        nval = nval + 1
        if (nval.gt.maxvals) go to 992
      end do

111   close(file1)
      nval = nval - 1

      call qsortr(ord,nval,allval)

      do np = 1,npcnt-1
        taupctle(np) = allval(ord(int(real(nval)*pcntle(np)/100.)))
      end do
      taupctle(npcnt) = allval(ord(1))

      print 11, 'pcnt',(pcntle(np),np=1,npcnt)
      print 12, rseg,(taupctle(np),np=1,npcnt)
11    format (A4,50(',',F10.3))
12    format (A13,50(',',e11.4))

      return

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'too many values in file'
      report(2) = 'increase maxvals variable in '
      report(3) = 
     .     './pp/src/calibration_utils/get_pltgen_percentiles/main.f'
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,zero,value
      go to 999

999   call stopreport(report)
      end

************************************************************************
**     real quicksort -- fast- returns a pointer array                **
************************************************************************
C From HDK@psuvm.psu.edu Thu Dec  8 15:27:16 MST 1994
C
C The following was converted from Algol recursive to Fortran iterative
C by a colleague at Penn State (a long time ago - Fortran 66, please
C excuse the GoTo's). The following code also corrects a bug in the
C Quicksort algorithm published in the ACM (see Algorithm 402, CACM,
C Sept. 1970, pp 563-567; also you younger folks who weren't born at
C that time might find interesting the history of the Quicksort
C algorithm beginning with the original published in CACM, July 1961,
C pp 321-322, Algorithm 64). Note that the following algorithm sorts
C integer data; actual data is not moved but sort is affected by sorting
C a companion index array (see leading comments). The data type being
C sorted can be changed by changing one line; see comments after
C declarations and subsequent one regarding comparisons(Fortran
C 77 takes care of character comparisons of course, so that comment
C is merely historical from the days when we had to write character
C compare subprograms, usually in assembler language for a specific
C mainframe platform at that time). But the following algorithm is
C good, still one of the best available.


      SUBROUTINE QSORTR (ORD,N,A)
C
C==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
C   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
C   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
C   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .
C   
C
C     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
C                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
C                                 CENTER FOR ACADEMIC COMPUTING
C                                 THE PENNSYLVANIA STATE UNIVERSITY
C                                 UNIVERSITY PARK, PA.  16802
C
      IMPLICIT none
C
      integer n
      integer ORD(N),POPLST(2,20)
      real X,XX,Z,ZZ,Y
C
C     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
C     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
C     USE THE FOLLOWING:  CHARACTER *(*) A(N)
C
      real A(N)
C
      integer ndeep,u1,l1,i,u,l,p,q,iz,ix,yp,ip,iq

      NDEEP=0
      U1=N
      L1=1  
      DO 1  I=1,N
    1 ORD(I)=I
    2 IF (U1.LE.L1) RETURN
C   
    3 L=L1
      U=U1
C 
C PART
C   
    4 P=L
      Q=U 
C     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
C     X = ORD(P)
C     Z = ORD(Q)
C     IF (A(X) .LE. A(Z)) GO TO 2
C     
C     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
C     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
C     CHARACTERS.
C
      X=A(ORD(P))
      Z=A(ORD(Q)) 
      IF (X.LE.Z) GO TO 5
      Y=X
      X=Z
      Z=Y
      YP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=YP
    5 IF (U-L.LE.1) GO TO 15
      XX=X
      IX=P
      ZZ=Z
      IZ=Q
C 
C LEFT
C   
    6 P=P+1
      IF (P.GE.Q) GO TO 7
      X=A(ORD(P))
      IF (X.GE.XX) GO TO 8
      GO TO 6
    7 P=Q-1 
      GO TO 13
C 
C RIGHT
C   
    8 Q=Q-1
      IF (Q.LE.P) GO TO 9
      Z=A(ORD(Q))
      IF (Z.LE.ZZ) GO TO 10
      GO TO 8
    9 Q=P
      P=P-1
      Z=X
      X=A(ORD(P))
C
C DIST
C
   10 IF (X.LE.Z) GO TO 11
      Y=X
      X=Z
      Z=Y
      IP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=IP
   11 IF (X.LE.XX) GO TO 12
      XX=X
      IX=P
   12 IF (Z.GE.ZZ) GO TO 6
      ZZ=Z
      IZ=Q
      GO TO 6
C
C OUT
C
   13 CONTINUE
      IF (.NOT.(P.NE.IX.AND.X.NE.XX)) GO TO 14
      IP=ORD(P)
      ORD(P)=ORD(IX)
      ORD(IX)=IP
   14 CONTINUE
      IF (.NOT.(Q.NE.IZ.AND.Z.NE.ZZ)) GO TO 15
      IQ=ORD(Q)
      ORD(Q)=ORD(IZ)
      ORD(IZ)=IQ
   15 CONTINUE
      IF (U-Q.LE.P-L) GO TO 16
      L1=L
      U1=P-1
      L=Q+1
      GO TO 17
   16 U1=U
      L1=Q+1
      U=P-1
   17 CONTINUE
      IF (U1.LE.L1) GO TO 18
C
C START RECURSIVE CALL
C
      NDEEP=NDEEP+1
      POPLST(1,NDEEP)=U
      POPLST(2,NDEEP)=L
      GO TO 3
   18 IF (U.GT.L) GO TO 4
C
C POP BACK UP IN THE RECURSION LIST
C
      IF (NDEEP.EQ.0) GO TO 2
      U=POPLST(1,NDEEP)
      L=POPLST(2,NDEEP)
      NDEEP=NDEEP-1
      GO TO 18
C
C END SORT
C END QSORT
C
      END

