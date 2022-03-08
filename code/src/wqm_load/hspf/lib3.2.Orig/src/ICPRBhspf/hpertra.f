C
C
C
      SUBROUTINE   PTRACR
C
C     + + + PURPOSE + + +
C     Process input for section tracer
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER1 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I4,J,N,RETCOD,K
      REAL      RVAL(3),R0
C
C     + + + EXTERNALS + + +
      EXTERNAL  RTABLE,ITABLE
      EXTERNAL  ZIPR,MDATBL
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION TRACER')
 2010 FORMAT (/,' SEGMENT-WIDE STORAGES')
 2020 FORMAT (/,' STORAGES IN BLOCK',I3)
 2030 FORMAT (/,'  SURFACE    UPPER   INTERFLOW')
 2040 FORMAT (  '   LAYER     LAYER    STORAGE')
 2050 FORMAT (  ' ',1P3E10.3)
 2060 FORMAT (/,' TOTAL STORAGE IN THE SEGMENT: ',1PE10.3)
 2070 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION TRACER')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      R0 = 0.0
C
      IF (OUTLEV.GT.1) THEN
C       processing message
        WRITE (MESSU,2000)
      END IF
C
C     initialize month-data input
      I= 24
      CALL ZIPR (I,R0,
     O           TRAFXM)
      CALL ZIPR (I,R0,
     O           TRACNM)
C
C     initialize atmospheric deposition fluxes
      I= 10
      CALL ZIPR (I,R0,
     O           TRCFX3)
      CALL ZIPR (I,R0,
     O           TRCFX4)
C
C     warning message counter initialization
      TRWCNT(1)= 0
C
C     get atmospheric deposition flags - table-type trac-ad-flags
      I2= 132
      I4= 4
      CALL ITABLE (I2,I1,I4,UUNITS,
     M             TRADFG)
C
C     read in month-data tables where necessary
      DO 40 J= 1, 2
        N= 2*(J- 1)+ 1
        IF (TRADFG(N) .GT. 0) THEN
C         monthly flux must be read
          CALL MDATBL
     I                (TRADFG(N),
     O                 TRAFXM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
C         from lb/ac.day to lb/ac.ivl or from kg/ha.day to kg/ha.ivl
          DO 10 K= 1, 12
            TRAFXM(K,J)= TRAFXM(K,J)*DELT60/24.0
 10       CONTINUE
        END IF
        IF (TRADFG(N+1) .GT. 0) THEN
C         monthly ppn conc must be read
          CALL MDATBL
     I                (TRADFG(N+1),
     O                 TRACNM(1,J),RETCOD)
C         convert units to internal - not done by MDATBL
          IF (UUNITS .EQ. 1) THEN
C           convert from mg/l to lb/ac.in
            DO 20 K= 1, 12
              TRACNM(K,J)= TRACNM(K,J)*0.226635
 20         CONTINUE
            ELSE IF (UUNITS .EQ. 2) THEN
C             convert from mg/l to kg/ha.in
              DO 30 K= 1, 12
                TRACNM(K,J)= TRACNM(K,J)*0.01
 30           CONTINUE
            END IF
        END IF
 40   CONTINUE
C
C     table-type tracer-id
      I2= 133
      I4= 5
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             TRACID)
C
C     initial storages
      IF (NBLKS.EQ.1) THEN
C       topsoil layers have not been subdivided into blocks
        IF (OUTLEV.GT.2) THEN
C         segment wide message
          WRITE (MESSU,2010)
        END IF
        I2= 134
        I4= 3
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               TRST1(1))
      ELSE
C       topsoil layers have been subdivided into blocks
C       initialize segment-wide variables
        DO 50 J= 1,3
          TRST1(J)= 0.0
 50     CONTINUE
        DO 70 N= 1,NBLKS
          IF (OUTLEV.GT.2) THEN
C           block message
            WRITE (MESSU,2020)  N
          END IF
C
          I2= 134
          I4= 3
          CALL RTABLE (I2,N,I4,UUNITS,
     M                 RVAL)
C
          STRSUB(N)= RVAL(1)
          UTRSUB(N)= RVAL(2)
          ITRSUB(N)= RVAL(3)
C
C         add to segment-wide variables
          DO 60 J= 1,3
            TRST1(J)= TRST1(J)+ (RVAL(J)*NBLKSI)
 60       CONTINUE
 70     CONTINUE
C
        IF (OUTLEV.GT.2) THEN
          WRITE (MESSU,2010)
          WRITE (MESSU,2030)
          WRITE (MESSU,2040)
          WRITE (MESSU,2050)  (TRST1(J),J=1,3)
        END IF
      END IF
C
C     lower and active groundwater layer storages - table-type
C     tra-substor
      I2= 135
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             TRST1(4))
C
C     determine total storage in the system
      TRSU= 0.0
      DO 80 I= 1,5
        TRSU= TRSU+ TRST1(I)
 80   CONTINUE
C
      IF (OUTLEV.GT.2) THEN
C       storage message
        WRITE (MESSU,2060)  TRSU
      END IF
C
      IF (OUTLEV.GT.1) THEN
C       finished processing message
        WRITE (MESSU,2070)
      END IF
C
      RETURN
      END
C
C     4.2(1).12
C
      SUBROUTINE TRACER
C
C     + + + PURPOSE + + +
C     Simulate movement of a tracer
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE   'cpltr.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,N
      REAL      TSTRSB(5)
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  TOPMOV,SUBMOV,DAYVAL
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MSTLFG.EQ.0.AND.PESTFG.EQ.0.AND.NITRFG.EQ.0.AND.PHOSFG.EQ.0)
     #    THEN
C       read time series supplied by mstlay
        IF (NBLKS.EQ.1) THEN
C         surface and upper layers of the land segment have not
C         been subdivided into blocks
          DO 10 J= 1,5
            MST(J)= PAD(MSTFP(J)+IVL1)
 10       CONTINUE
C
          DO 20 J= 1,8
            FRAC(J)= PAD(FRACFP(J)+IVL1)
 20       CONTINUE
        ELSE
C         surface and upper layers of the land segment have
C         been subdivided into blocks
          DO 60 N= 1,NBLKS
            DO 40 J= 1,3
              MSTB(J,N)= PAD(MSTBFP(J,N)+IVL1)
 40         CONTINUE
C
            DO 50 J= 1,5
              FRACB(J,N)= PAD(FRACBX(J,N)+IVL1)
 50         CONTINUE
C
 60       CONTINUE
C
C         get time series for subsoil layers
          MST(4) = PAD(MSTFP(4)+IVL1)
          MST(5) = PAD(MSTFP(5)+IVL1)
          FRAC(6)= PAD(FRACFP(6)+IVL1)
          FRAC(7)= PAD(FRACFP(7)+IVL1)
          FRAC(8)= PAD(FRACFP(8)+IVL1)
C
        END IF
      ELSE
C       the above time series are available from mstlay or
C       other agri-chemical sections
C
      END IF
      PREC= PAD(PRECFP+IVL1)
C
C     compute atmospheric deposition influx
      DO 70 I= 1, 2
        N= 2*(I-1)+ 1
C       dry deposition
        IF (TRADFG(N) .LE. -1) THEN
          TRADDR(I)= PAD(TRAFFP(I)+IVL1)
        ELSE IF (TRADFG(N) .GE. 1) THEN
          TRADDR(I)= DAYVAL(TRAFXM(MON,I),TRAFXM(NXTMON,I),DAY,NDAYS)
        ELSE
          TRADDR(I)= 0.0
        END IF
C       wet deposition
        IF (TRADFG(N+1) .LE. -1) THEN
          TRADWT(I)= PREC*PAD(TRACFP(I)+IVL1)
        ELSE IF (TRADFG(N+1) .GE. 1) THEN
          TRADWT(I)= PREC*DAYVAL(TRACNM(MON,I),TRACNM(NXTMON,I),DAY,
     I                           NDAYS)
        ELSE
          TRADWT(I)= 0.0
        END IF
 70   CONTINUE
C
      IF (NBLKS.EQ.1) THEN
C       surface and upper layers of the land segment have not
C       been subdivided into blocks
C
C       update storages for atmospheric deposition
        STRSU= STRSU+ TRADDR(1)+ TRADWT(1)
        UTRSU= UTRSU+ TRADDR(2)+ TRADWT(2)
C
C       move tracer with water in the topsoil layers
        CALL TOPMOV (FRAC,
     M               STRSU,UTRSU,ITRSU,
     O               TSTRS)
      ELSE
C       surface and upper layers of the land segment have
C       been subdivided into blocks
C       initialize segment-wide variables
C
C       fluxes
        DO 80 J= 1,5
          TSTRS(J)= 0.0
 80     CONTINUE
C
C       storage
        STRSU= 0.0
        UTRSU= 0.0
        ITRSU= 0.0
C
        DO 100 I= 1,NBLKS
C
C         update storages for atmospheric deposition
          STRSUB(I)= STRSUB(I)+ (TRADDR(1)+ TRADWT(1))*NBLKSI
          UTRSUB(I)= UTRSUB(I)+ (TRADDR(2)+ TRADWT(2))*NBLKSI
C
C         transport tracer in the topsoil layers
          CALL TOPMOV (FRACB(1,I),
     M                 STRSUB(I),UTRSUB(I),ITRSUB(I),
     O                 TSTRSB)
C
C         cumulate block fluxes
          DO 90 J= 1,5
            TSTRS(J)= TSTRS(J)+ TSTRSB(J)
 90       CONTINUE
C
C         cumulate block storage in the surface layer
          STRSU= STRSU+ STRSUB(I)
C
C         cumulate block storage in the upper layer
          UTRSU= UTRSU+ UTRSUB(I)
C
C         cumulate block storage in the upper layer
C         transitory (interflow) storage
          ITRSU= ITRSU+ ITRSUB(I)
C
 100    CONTINUE
C
C       average sum of block storage and fluxes for the surface
C       and upper layer to get segment-wide values
C
C       fluxes
        DO 110 J= 1,5
          TSTRS(J)= TSTRS(J)*NBLKSI
 110    CONTINUE
C
C       storages
        STRSU= STRSU*NBLKSI
        UTRSU= UTRSU*NBLKSI
        ITRSU= ITRSU*NBLKSI
C
      END IF
C
C     transport tracer in the subsurface layers
      CALL SUBMOV (TSTRS(3),FRAC(6),FRAC(7),FRAC(8),
     M             LTRSU,ATRSU,
     O             SSTRS)
C
C     find total tracer outflow
      POTRS= TSTRS(1)+ TSTRS(5)+ SSTRS(3)
C
C     total tracer in storage
      TRSU= STRSU+ UTRSU+ ITRSU+ LTRSU+ ATRSU
C
      RETURN
      END
C
C     4.2(1).15.1.9
C
      SUBROUTINE TRACC
     I                 (FRMROW,TOROW)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for section tracer
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FRMROW,TOROW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL   ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I5=5
C
      CALL ACCVEC (I5,TRCFX1(1,FRMROW),
     M             TRCFX1(1,TOROW))
C
      CALL ACCVEC (I3,TRCFX2(1,FRMROW),
     M             TRCFX2(1,TOROW))
C
      DO 10 I= 1, 2
        TRCFX3(I,TOROW)= TRCFX3(I,TOROW)+ TRCFX3(I,FRMROW)
        TRCFX4(I,TOROW)= TRCFX4(I,TOROW)+ TRCFX4(I,FRMROW)
 10   CONTINUE
C
      RETURN
      END
C
C     4.2(1).14.9
C
      SUBROUTINE TRACPB
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section tracer
      DO 10 J= 1,5
        IF (TSTRFP(J).GE.1)  PAD(TSTRFP(J)+IVL1)= TSTRS(J)
 10   CONTINUE
C
      DO 20 J= 1,3
        IF (SSTRFP(J).GE.1)  PAD(SSTRFP(J)+IVL1)= SSTRS(J)
 20   CONTINUE
C
      DO 30 J= 1, 2
        IF (TRADDX(J) .GE. 1) THEN
          PAD(TRADDX(J)+IVL1)= TRADDR(J)
        END IF
        IF (TRADWX(J) .GE. 1) THEN
          PAD(TRADWX(J)+IVL1)= TRADWT(J)
        END IF
 30   CONTINUE
C
      IF (POTRFP.GE.1)  PAD(POTRFP+IVL1)= POTRS
C
      RETURN
      END
C
C     4.2(1).13.12
C
      SUBROUTINE TRACPT
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE 'cpltr.inc'
      INCLUDE 'cmpad.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section tracer
      IF (STRSFP.GE.1)  PAD(STRSFP+IVL1)= STRSU
      IF (UTRSFP.GE.1)  PAD(UTRSFP+IVL1)= UTRSU
      IF (ITRSFP.GE.1)  PAD(ITRSFP+IVL1)= ITRSU
      IF (LTRSFP.GE.1)  PAD(LTRSFP+IVL1)= LTRSU
      IF (ATRSFP.GE.1)  PAD(ATRSFP+IVL1)= ATRSU
      IF (TRSUFP.GE.1)  PAD(TRSUFP+IVL1)= TRSU
C
      RETURN
      END
C
C     4.2(1).15.2.12
C
      SUBROUTINE TRAPRT
     I                  (LEV,PRINTU,AGMAID,MFACTA,MFACTB,UNITFG)
C
C     + + + PURPOSE + + +
C     Convert quantities from internal to external units, and
C     produce printout
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEV,PRINTU,UNITFG
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGMAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU - fortran unit number on which to print output
C     AGMAID - ???
C     MFACTA - ???
C     MFACTB - ???
C     UNITFG - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I1,I3,I5,I,N,ADFG
      REAL        MATDIF,PCFLX1(5),PCFLX2(3),PPOTRS,PSTAT(5),PSTOR,
     $            PSTORS,TOTAL,PCFLX3(2),PCFLX4(2),PADTOT(2),PADALL
      CHARACTER*8 UNITID
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC,BALCHK
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** TRACER ***')
 2010 FORMAT (/,1H ,'  SEGMENT WIDE VALUES:')
 2020 FORMAT (/,1H ,'  STATE VARIABLES   ',A8)
 2030 FORMAT (' ','    STORAGES BY LAYERS',11X,'SURFACE',5X,
     $        'UPPER PRINCIPAL  UPPER TRANS(INTER)     LOWER',
     $        '  ACTIVE GROUNDWATER',15X,'TOTAL')
 2040 FORMAT (' ',6X,5A4,4X,F10.3,10X,F10.3,10X,2F10.3,
     $        2(10X,F10.3))
 2050 FORMAT (/,1H ,'  FLUXES',12X,A8)
 2060 FORMAT (  '     ATMOSPHERIC DEPOSITION    <-------SURFACE',
     #          ' LAYER--------><------UPPER LAYER PRIN------>')
 2070 FORMAT (  31X,'       DRY       WET     TOTAL       DRY',
     #          '       WET     TOTAL')
 2080 FORMAT (  31X,6(1PE10.3))
 2090 FORMAT (' ','    FLOWS (ALL IN SOLUTION)   <--SURFACE LAYER--->',
     $  '<-UPPER LAYER PRIN-> INTERFLOW<---LOWER LAYER----> GRNDWATER',
     $        15X,'TOTAL')
 2100 FORMAT (' ',30X,'   OUTFLOW      PERC      PERC  TO TRANS   ',
     $        'OUTFLOW      PERC DEEP PERC   OUTFLOW',13X,'OUTFLOW')
 2110 FORMAT (' ',6X,5A4,9X,'SOTRS     SPTRS     UPTRS     IITRS',
     $        5X,'IOTRS     LPTRS    LDPTRS     AOTRS',15X,'POTRS')
 2120 FORMAT (' ',30X,8(1PE10.3),10X,(1PE10.3))
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      I3= 3
      I5= 5
      WRITE (PRINTU,2000)
C
C     print headings on unit printu
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)  AGMAID
      WRITE (PRINTU,2030)
C
      CALL TRNVEC (I5,TRST1,MFACTA,MFACTB,
     O             PSTAT)
      TOTAL= PSTAT(1)+ PSTAT(2)+ PSTAT(3)+ PSTAT(4)+ PSTAT(5)
      WRITE (PRINTU,2040)  TRACID, PSTAT, TOTAL
C
      WRITE (PRINTU,2050)  AGMAID
C
      ADFG= 0
      DO 10 I= 1, 2
        N= 2*(I- 1)+ 1
        IF ( (TRADFG(N) .NE. 0) .OR. (TRADFG(N+1) .NE. 0) ) THEN
          ADFG= 1
        END IF
 10   CONTINUE
C
      PADALL= 0.0
      IF (ADFG .EQ. 1) THEN
        DO 20 I= 1, 2
          N= 2*(I- 1)+ 1
          IF (TRADFG(N) .NE. 0) THEN
            PCFLX3(I)= TRCFX3(I,LEV)*MFACTA
          ELSE
            PCFLX3(I)= 0.0
          END IF
          IF (TRADFG(N+1) .NE. 0) THEN
            PCFLX4(I)= TRCFX4(I,LEV)*MFACTA
          ELSE
            PCFLX4(I)= 0.0
          END IF
          PADTOT(I)= PCFLX3(I)+ PCFLX4(I)
          PADALL= PADALL+ PADTOT(I)
 20     CONTINUE
C
        WRITE (PRINTU,2060)
        WRITE (PRINTU,2070)
        WRITE (PRINTU,2080) PCFLX3(1),PCFLX4(1),PADTOT(1),
     #                      PCFLX3(2),PCFLX4(2),PADTOT(2)
C
      END IF
C
      WRITE (PRINTU,2090)
      WRITE (PRINTU,2100)
      WRITE (PRINTU,2110)  TRACID
      CALL TRNVEC (I5,TRCFX1(1,LEV),MFACTA,MFACTB,
     O             PCFLX1)
C
      CALL TRNVEC (I3,TRCFX2(1,LEV),MFACTA,MFACTB,
     O             PCFLX2)
C
      PPOTRS= PCFLX1(1)+ PCFLX1(5)+ PCFLX2(3)
C
      WRITE (PRINTU,2120)  PCFLX1, PCFLX2, PPOTRS
C
C     tracer balance check and report
      IF (UNITFG .EQ. 1) THEN
C       english
        UNITID= '   LB/AC'
      ELSE
C       metric
        UNITID= '   KG/HA'
      END IF
C
C     convert storages to external units for balance
      PSTORS= TOTTR(LEV)*MFACTA+ MFACTB
      PSTOR = TOTTR(1)*MFACTA+ MFACTB
C
C     find the net output of tracer from the pls
      MATDIF= PADALL- PPOTRS- PCFLX2(2)
C
      CALL BALCHK (I1,LSNO,DATIM,MESSU,PRINTU,MSGFL,
     $             PSTORS,PSTOR,PADALL,MATDIF,UNITID,I1,
     M             TRWCNT(1))
C
      RETURN
      END
C
C     4.2(1).15.3.9
C
      SUBROUTINE TRRST
     I                 (LEV)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variablesc
C     used in material balance check for section tracer
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    LEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION TRACER2 + + +
      INCLUDE    'cpltr.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I3,I5,I
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I5=5
C     set flux accumulators to zero
C
      CALL SETVEC (I5,0.0,
     O             TRCFX1(1,LEV))
C
      CALL SETVEC (I3,0.0,
     O             TRCFX2(1,LEV))
C
      DO 10 I= 1, 2
        TRCFX3(I,LEV)= 0.0
        TRCFX4(I,LEV)= 0.0
 10   CONTINUE
C
C     keep storage in state variable used for
C     material balance check
C
      TOTTR(LEV)= TOTTR(1)
C
      RETURN
      END
