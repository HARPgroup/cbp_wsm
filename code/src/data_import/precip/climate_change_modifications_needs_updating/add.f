
         real  raindyB(366), raindyU(366), raindyN(366)
         integer iyr, idyiny, iyd, N10, N30 
         integer ord(22*366) 
         real    sumB, sumU
         character*80 cdum
         real  baserain(22*366), uniformrain(22*366) 
         real  TraindyB(366)

100       format(a80)
123       format(i8, 2(1x, f10.2)) 
c           open(11,file='junk.prn', status='old') 
           open(11,file='junk.out', status='old') 
           open(22,file='out.dat', status='unknown') 
c            read(11,*)  ! cdum
           do i= 1, 999999
c             read(11,*, end=899) idat, idm, baserain(i) ! , uniformrain(i)
              read(11,*, end=899) idat, baserain(i), uniformrain(i)
c             write(*,*) idat, baserain(i), uniformrain(i)
c            write(21,123) idat, baserain(i), baserain(i)*1.1 ! uniformrain(i)
           enddo
899       continue 

          N10 = int(366./10.) 
C         N30 = int(366./3.33333)
       !     do iyr = 1985, 1989   ! 1984, 2005 
c         do iyr = sdate(1), edate(1)   ! 1984 to 2005 
            idinyr = 365
            if(mod(iyr, 4).eq.0) idinyr=366
            sumB = 0.
            sumU = 0.
               idinyr = 20  !!!!!!!!!111
               n10    = 10  !!!!!!!!!111
            do iyd=1, idinyr
               raindyB(iyd) = baserain(iyd)
               if(raindyB(idy).lt.0.000001)raindyB(idy)=0.
               sumB = sumB + raindyB(iyd)

               raindyU(iyd) = uniformrain(iyd)
               if(raindyU(idy).lt.0.000001)raindyU(idy)=0.
               sumU = sumU + raindyU(iyd)
            enddo
*           write(*,*) sumB, sumU
            diff = (sumU - sumB)  
            call QSORTR(ord, idinyr, raindyB) 
c            do iyd=idinyr, 1, -1 
c               if(iyd .le. N10) then 
c               TraindyB(iyd) = raindyB(ord(iyd)) 
                sumN10 = 0.0
             do iyd=1,idinyr
                if(iyd .le. N10) then 
                TraindyB(iyd) = raindyB(ord(idinyr-iyd+1))
                sumN10 = sumN10 + TraindyB(iyd)
                else 
                TraindyB(iyd) = raindyB(ord(idinyr-iyd+1)) 
                endif 
             enddo 
             write(*,*) sumN10, diff
c              now for the upper 10% 
                ratio = 1. + diff / sumN10    !!!!!!  
               do ik = 1, N10
                TraindyB(ik) = TraindyB(ik) * ratio
               enddo
             do iyd = 1, idinyr
               raindyN(ord(idinyr-iyd+1)) = TraindyB(iyd)
             enddo 
             do iyd = 1, idinyr
               write(22,123) iyd, baserain(iyd), raindyN(iyd) 
             enddo 
         !     enddo    ! 1984-2005

999        continue
         end


************************************************************************
**  integer bubble sort -- slow but simple                            **
************************************************************************

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

