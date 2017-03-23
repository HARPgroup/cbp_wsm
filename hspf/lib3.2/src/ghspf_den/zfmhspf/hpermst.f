C
C
C
      SUBROUTINE   PMSTLA
C
C     + + + PURPOSE + + +
C     Process input for section mstlay
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY1 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'crin2.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I4,IVAL(1),BLK
      REAL      RVAL(8)
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' PROCESSING INPUT FOR SECTION MSTLAY')
 2010 FORMAT (/,' SEGMENT-WIDE TOPSOIL MOISTURE STORAGES')
 2020 FORMAT (/,' SEGMENT-WIDE FRACTIONAL FLUXES THROUGH TOPSOIL ',
     $            'LAYERS')
 2030 FORMAT (/,' TOPSOIL MOISTURE STORAGES IN BLOCK',I3)
 2040 FORMAT (/,' FRACTIONAL FLUXES THROUGH TOPSOIL LAYERS ',
     $            'IN BLOCK',I3)
 2050 FORMAT (/,' FINISHED PROCESSING INPUT FOR SECTION MSTLAY')
 2060 FORMAT (/,' SEGMENT-WIDE SUBSOIL MOISTURE STORAGES')
 2070 FORMAT (/,' SEGMENT-WIDE FRACTIONAL FLUXES THROUGH SUBSOIL')
C
C     + + + END SPECIFICATIONS + + +
C
      I1= 1
      IF (OUTLEV.GT.1) THEN
C       processing section message
        WRITE (MESSU,2000)
      END IF
C
C     factor to convert water from inches to mass/area
      IF (UUNITS.EQ.1) THEN
        CFINMA= 2.264E05
      ELSE
        CFINMA= 2.54E05
      END IF
C
      IF (PWATFG.EQ.0) THEN
C       vuzfg obtained here from user's control input table-type vuzfg
        I2= 64
        I4= 1
        CALL ITABLE (I2,I1,I4,UUNITS,
     M               IVAL)
        VUZFG= IVAL(1)
C
C       uzsn, lzsn and initial surface storages read in table-type uzsn-lzsn
        I2= 65
        I4= 8
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               RVAL)
        UZSN= RVAL(1)
        LZSN= RVAL(2)
        SURS= RVAL(3)
        DO 25 I= 1,5
          SURSB(I)= RVAL(I+3)
 25     CONTINUE
        IF (VUZFG.EQ.1) THEN
C         table-type mon-uzsn
          I2= 18
          I4= 12
          CALL RTABLE (I2,I1,I4,UUNITS,
     M                 UZSNM)
        END IF
      END IF
C
C     table-type mst-parm
      I2= 66
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             MSTPM)
C
      IF (NBLKS.EQ.1) THEN
C       topsoil layers have not been subdivided into blocks
        IF (OUTLEV.GT.2) THEN
C         processing message
          WRITE (MESSU,2010)
        END IF
C       table-type mst-topstor
        I2= 67
        I4= 3
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               MST)
C
        IF (OUTLEV.GT.2) THEN
C         processing message
          WRITE (MESSU,2020)
        END IF
C
C       table-type mst-topflx
        I2= 68
        I4= 5
        CALL RTABLE (I2,I1,I4,UUNITS,
     M               FRAC)
      ELSE
C       topsoil layers have been subdivided into blocks
        DO 60 BLK= 1,NBLKS
          IF (OUTLEV.GT.2) THEN
C           processing block message
            WRITE (MESSU,2030) BLK
          END IF
C
C         table-type mst-topstor
          I2= 67
          I4= 3
          CALL RTABLE (I2,BLK,I4,UUNITS,
     M                 MSTB(1,BLK))
C
          IF (OUTLEV.GT.2) THEN
C           processing block message
            WRITE (MESSU,2040) BLK
          END IF
C
C         table-type mst-topflx
          I2= 68
          I4= 5
          CALL RTABLE (I2,BLK,I4,UUNITS,
     M                 FRACB(1,BLK))
 60     CONTINUE
      END IF
C
C     process input for subsoil layers
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2060)
      END IF
C     table-type mst-substor
      I2= 69
      I4= 2
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             MST(4))
C
      IF (OUTLEV.GT.2) THEN
C       processing message
        WRITE (MESSU,2070)
      END IF
C     table-type mst-subflx
      I2= 70
      I4= 3
      CALL RTABLE (I2,I1,I4,UUNITS,
     M             FRAC(6))
C
      IF (OUTLEV.GT.1) THEN
C       end processing message
        WRITE (MESSU,2050)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSTLAY
C
C     + + + PURPOSE + + +
C     Estimate the moisture storages and solute fluxes as
C     fractions of stored material
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + FUNCTIONS + + +
      REAL       DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYVAL,TOPLAY,SUBLAY
C
C     + + + END SPECIFICATIONS + + +
C
      IF (PWATFG.NE.0) GO TO 70
C
        IF (DAYFG.NE.1) GO TO 30
C         it is the first interval of the day
          IF (VUZFG.NE.1) GO TO 10
C           uzsn is allowed to vary throughout the year
C           interpolate for the daily value
C           linearly interpolate uzsn between two values from the
C           monthly array uzsnm(12)
            UZSN= DAYVAL(UZSNM(MON),UZSNM(NXTMON),DAY,NDAYS)
            GO TO 20
 10       CONTINUE
C           uzsn does not vary throughout the year.
C           uzsn value has been supplied by the run interpreter
 20       CONTINUE
C
 30     CONTINUE
C
C       read time series supplied by pwater
        IF (NBLKS.NE.1) GO TO 40
C         topsoil layers of the land segment have not
C         been subdivided into blocks
          SURO = PAD(SOFP+IVL1)
          SURSS= SURS
          SURS = PAD(SSFP+IVL1)
          INFIL= PAD(INFFP+IVL1)
          IFWI = PAD(IIFP+IVL1)
          UZI  = PAD(UZIFP+IVL1)
          UZS  = PAD(UZSFP+IVL1)
          PERC = PAD(PCFP+IVL1)
          IFWS = PAD(ISFP+IVL1)
          IFWO = PAD(IOFP+IVL1)
          GO TO 60
C
 40     CONTINUE
C         surface and upper layers of the land segment have
C         been subdivided into blocks
          DO 50 J= 1,NBLKS
            SUROB(J) = PAD(SOBFP(J)+IVL1)
            SURSSB(J)= SURSB(J)
            SURSB(J) = PAD(SSBFP(J)+IVL1)
            INFILB(J)= PAD(INFBFP(J)+IVL1)
            UZIB(J)  = PAD(UZIBFP(J)+IVL1)
            IFWIB(J) = PAD(IIBFP(J)+IVL1)
            UZSB(J)  = PAD(UZSBFP(J)+IVL1)
            PERCB(J) = PAD(PCBFP(J)+IVL1)
            IFWSB(J) = PAD(ISBFP(J)+IVL1)
            IFWOB(J) = PAD(IOBFP(J)+IVL1)
 50       CONTINUE
C
 60     CONTINUE
C
        SURI= PAD(SURIFP+IVL1)
        LZS = PAD(LZSFP+IVL1)
        IGWI= PAD(IGIFP+IVL1)
        AGWI= PAD(AIFP+IVL1)
        AGWS= PAD(AGWSFP+IVL1)
        AGWO= PAD(AOFP+IVL1)
        GO TO 80
C
 70   CONTINUE
C       the above time series and uzsn are available
C       from pwater
C
 80   CONTINUE
C
C     estimate for surface and upper layers
      IF (NBLKS.NE.1) GO TO 90
C       surface and upper layers of the land segment have not
C       been subdivided into blocks
        CALL TOPLAY (SURSS,SURI,CFINMA,SURO,INFIL,
     $               IFWI,UZI,SLMPF,UZS,ULPF,PERC,UZSN,
     $               IFWS,IFWO,
     O               MST,FRAC)
        GO TO 110
C
 90   CONTINUE
C       surface and upper layers of the land segment have
C       been subdivided into blocks
        DO 100 I= 1,NBLKS
          CALL TOPLAY (SURSSB(I),SURI,CFINMA,SUROB(I),INFILB(I),
     $                 IFWIB(I),UZIB(I),SLMPF,UZSB(I),ULPF,PERCB(I),
     $                 UZSN,IFWSB(I),IFWOB(I),
     O                 MSTB(1,I),FRACB(1,I))
 100    CONTINUE
C
 110  CONTINUE
C
C     estimate for lower and groundwater layers
      CALL SUBLAY (LZS,IGWI,AGWI,CFINMA,LZSN,
     $             LLPF,AGWS,AGWO,
     O             MST,FRAC)
C
      RETURN
      END
C
C
C
      SUBROUTINE   MSTLPT
C
C     + + + PURPOSE + + +
C     ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J
C
C     + + + END SPECIFICATIONS + + +
C
C     handle section mstlay
      IF (NBLKS.EQ.1) THEN
        DO 10 J= 1,5
          IF (MSTFP(J).GE.1)  PAD(MSTFP(J)+IVL1)  = MST(J)
 10     CONTINUE
C
        DO 20 J= 1,8
          IF (FRACFP(J).GE.1)  PAD(FRACFP(J)+IVL1)= FRAC(J)
 20     CONTINUE
      ELSE
        DO 50 I= 1,NBLKS
          DO 30 J= 1,3
            IF (MSTBFP(J,I).GE.1)
     $        PAD(MSTBFP(J,I)+IVL1)= MSTB(J,I)
 30       CONTINUE
C
          DO 40 J= 1,5
            IF (FRACBX(J,I).GE.1)
     $        PAD(FRACBX(J,I)+IVL1)= FRACB(J,I)
 40       CONTINUE
C
 50     CONTINUE
C
        DO 60 J= 4,5
          IF (MSTFP(J).GE.1)  PAD(MSTFP(J)+IVL1)  = MST(J)
 60     CONTINUE
C
        DO 70 J= 6,8
          IF (FRACFP(J).GE.1)  PAD(FRACFP(J)+IVL1)= FRAC(J)
 70     CONTINUE
C
      END IF
C
      RETURN
      END
C
C     4.2(1).15.2.8
C
      SUBROUTINE MSTPRT
     I                  (PRINTU,AGMAID,MFACTA,MFACTB)
C
C     + + + PURPOSE + + +
C     Convert quanities from internal to external units and print out
C     results.  the array pstat2 has identical structure to mst
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     PRINTU
      REAL        MFACTA,MFACTB
      CHARACTER*8 AGMAID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PRINTU - fortran unit number on which to print output
C     AGMAID - ???
C     MFACTA - ???
C     MFACTB - ???
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MSTLAY2 + + +
      INCLUDE    'cplms.inc'
      INCLUDE    'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I3,I5,J
      REAL       PSTAT1(5),PSTAT2,PSTAT3
C
C     + + + EXTERNALS + + +
      EXTERNAL   TRNVEC
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,1H ,'*** MSTLAY ***')
 2010 FORMAT (/,1H ,'  STATE VARIABLES',13X,'<--SURFACE LAYER--->',
     $  '<---UPPER LAYER----><----INTERFLOW-----><---LOWER LAYER---->',
     $        '<---GROUNDWATER---->')
 2020 FORMAT (' ',33X,'OUTFLOW    PERCOL  TO INTER    PERCOL',
     $  10X,'   OUTFLOW      PERC DEEP PERC',13X,'OUTFLOW')
 2030 FORMAT (' ','    SOLUTE FRACTIONS',17X,'FSO       FSP',
     $        7X,'FII       FUP',17X,'FIO       FLP      FLDP',
     $        17X,'FAO')
 2040 FORMAT (' ',6X,'SEGMENT WIDE',12X,4F10.4,10X,3F10.4,10X,F10.4)
 2050 FORMAT (' ',6X,'SEGMENT WIDE',72X,2F10.4,10X,F10.4)
 2060 FORMAT (' ',8X,'BLOCK',I2,15X,4F10.4,10X,F10.4)
 2070 FORMAT (/,1H ,'    MOISTURE ',A8,9X,5(13X,'STORAGE'))
 2080 FORMAT (' ',45X,'SMSTM',15X,'UMSTM',15X,'IMSTM',15X,'LMSTM',15X,
     $        'AMSTM')
 2090 FORMAT (' ',6X,'SEGMENT WIDE',12X,5(10X,F10.1))
 2100 FORMAT (' ',6X,'SEGMENT WIDE',72X,2(10X,F10.1))
 2110 FORMAT (' ',8X,'BLOCK',I2,15X,5(10X,F10.1))
C
C     + + + END SPECIFICATIONS + + +
C
      I3=3
      I5=5
C     mstlay has no fluxes only state variables
C
C     write heading to unit printu
      WRITE (PRINTU,2000)
      WRITE (PRINTU,2010)
      WRITE (PRINTU,2020)
C
C     write fractions to unit printu
      WRITE (PRINTU,2030)
      IF (NBLKS.NE.1) GO TO 10
        WRITE (PRINTU,2040)  FRAC
        GO TO 30
 10   CONTINUE
C       write subsurface layer fraction to unit printu
        WRITE (PRINTU,2050)  FRAC(6), FRAC(7), FRAC(8)
        DO 20 I= 1,NBLKS
          WRITE (PRINTU,2060)  I, (FRACB(J,I),J=1,5)
 20     CONTINUE
 30   CONTINUE
C
      WRITE (PRINTU,2070)  AGMAID
      WRITE (PRINTU,2080)
C
      IF (NBLKS.NE.1) GO TO 40
C       surface and near surface zones of the land
C       have not been subdivided into blocks
C
C       convert moisture storages to external units
        CALL TRNVEC (I5,MST,MFACTA,MFACTB,
     O               PSTAT1)
C
        WRITE (PRINTU,2090)  PSTAT1
        GO TO 60
C
 40   CONTINUE
C       surface and near surface zones of the land
C       have been subdivided into blocks
C
C       write subsurface layer moisture to unit printu
        PSTAT2= MST(4)*MFACTA+ MFACTB
        PSTAT3= MST(5)*MFACTA+ MFACTB
        WRITE (PRINTU,2100)  PSTAT2, PSTAT3
C
        DO 50 I= 1,NBLKS
C         convert moisture storages to external units
          CALL TRNVEC (I3,MSTB(1,I),MFACTA,MFACTB,
     O                 PSTAT1)
C
          WRITE (PRINTU,2110)  I, (PSTAT1(J),J=1,3)
 50     CONTINUE
C
 60   CONTINUE
C
      RETURN
      END
C
C     4.2(1).8.2
C
      SUBROUTINE SUBLAY
     I                  (LZS,IGWI,AGWI,CFINMA,LZSN,
     $                   LLPF,AGWS,AGWO,
     O                   MST,FRAC)
C
C     + + + PURPOSE + + +
C     Estimate the moisture and the fraction of solutes being
C     transported in the subsurface layers (lower layer and
C     groundwater layer)
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       AGWI,AGWO,AGWS,CFINMA,FRAC(8),IGWI,LLPF,LZS,
     $           LZSN,MST(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LZS    - ???
C     IGWI   - ???
C     AGWI   - ???
C     CFINMA - ???
C     LZSN   - ???
C     LLPF   - ???
C     AGWS   - ???
C     AGWO   - ???
C     MST    - ???
C     FRAC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       AMST,AMSTM,FAO,FLDP,FLP,LLMPF,LMST,LMSTM
C
C     + + + END SPECIFICATIONS + + +
C
C     note that this subroutine only affects elements 4 & 5 of mst,
C     elements 6-8 of frac
C
C     ******************************************************
C     warning,  the equations in this subroutine are based
C     on those in the arm model.  some of the equations are
C     not dimensionally homogeneous.  for example:
C           amst( )= agws(inches) + agwo(inches/interval)
C     thus, the results obtained for solute movement,
C     particularly in the surface & upper layers, will
C     probably be highly dependent on the simulation time
C     interval (delt).  the arm model used delt of 5 mins
C     and (occasionally) of 15 mins.
C     ******************************************************
C
C     find the lower layer moisture storage
      LMST= LZS+ (IGWI+AGWI)
C
      IF (LMST.LE.0.0) GO TO 10
C       there is lower layer moisture storage
C       convert lower layer moisture storage to mass/area units
        LMSTM= LMST*CFINMA
C
C       calculate the percolating solutes retardation factor
C       for lzs < (lzsn*llpf)
        LLMPF= LZS/(LZSN*LLPF)
        IF (LLMPF.GT.1.0)  LLMPF= 1.0
C
C       determine the fraction of the lower layer solute
C       in storage being percolated
C       to active groundwater
        FLP= LLMPF*AGWI/LMST
C       to inactive groundwater by deep percolation
        FLDP= LLMPF*IGWI/LMST
        GO TO 20
C
 10   CONTINUE
C       there is no lower layer moisture storage so zero variables
        LMSTM= 0.0
        FLP  = 0.0
        FLDP = 0.0
C
 20   CONTINUE
C
C     find the active groundwater moisture storage
      AMST= AGWS+ AGWO
C
      IF (AMST.LE.0.0) GO TO 30
C       there is active groundwater moisture storage
C       convert to mass units
        AMSTM= AMST*CFINMA
C
C       determine the fraction of the active groundwater solute
C       in storage being removed by groundwater outflow
        FAO= AGWO/AMST
        GO TO 40
C
 30   CONTINUE
C       there is no active groundwater moisture storage
        AMSTM= 0.0
        FAO  = 0.0
C
 40   CONTINUE
C
C     place computed values in groups
      MST(4) = LMSTM
      MST(5) = AMSTM
      FRAC(6)= FLP
      FRAC(7)= FLDP
      FRAC(8)= FAO
C
      RETURN
      END
C
C     4.2(1).8.1
C
      SUBROUTINE TOPLAY
     I                  (SURSS,SURI,CFINMA,SURO,INFIL,
     $                   IFWI,UZI,SLMPF,UZS,ULPF,PERC,UZSN,
     $                   IFWS,IFWO,
     O                   MST,FRAC)
C
C     + + + PURPOSE + + +
C     Estimate the moisture and the fraction of solutes being
C     transported in the topsoil (surface layer and upper
C     layer)
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       CFINMA,FRAC(5),IFWI,IFWO,IFWS,INFIL,MST(3),
     $           PERC,SLMPF,SURI,SURO,SURSS,ULPF,UZI,UZS,UZSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SURSS  - ???
C     SURI   - ???
C     CFINMA - ???
C     SURO   - surface output
C     INFIL  - ???
C     IFWI   - ???
C     UZI    - ???
C     SLMPF  - ???
C     UZS    - initial upper zone storage
C     ULPF   - ???
C     PERC   - ???
C     UZSN   - upper zone nominal storage
C     IFWS   - ???
C     IFWO   - ???
C     MST    - ???
C     FRAC   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       FII,FIO,FSO,FSP,FUP,IMSTM,ISMST,SDOWN,SMST,SMSTM,
     $           UDOWN,ULMPF,UMST,UMSTM
C
C     + + + END SPECIFICATIONS + + +
C
C     note that this subroutine only affects elements 1-3 of mst and
C     elements 1-5 of frac
C
C     ******************************************************
C     warning,  the equations in this subroutine are based
C     on those in the arm model.  some of the equations are
C     not dimensionally homogeneous.  for example:
C           smst( )= surss(inches) + suri(inches/interval)
C     thus, the results obtained for solute movement,
C     particularly in the surface & upper layers, will
C     probably be highly dependent on the simulation time
C     interval (delt).  the arm model used delt of 5 mins
C     and (occasionally) of 15 mins.
C     ******************************************************
C
C     find the surface layer moisture storage
      SMST= SURSS+ SURI
C
      IF (SMST.LE.0.0) GO TO 10
C       there is surface layer moisture storage
C       convert surface layer moisture storage to mass/area units
        SMSTM= SMST*CFINMA
C
C       determine the fraction of the surface layer solute in
C       storage being removed in surface runoff
        FSO= SURO/SMST
C
C       total downward water flux from the surface layer
        SDOWN= INFIL+ IFWI+ UZI
C
C       determine the fraction of the surface layer solute in
C       storage being percolated
C       the percolation multiplier factor (slmpf) is inputed not
C       calculated
        FSP= SLMPF*SDOWN/SMST
        GO TO 20
C
 10   CONTINUE
C       there is no surface moisture storage, so zero variables
        SMSTM= 0.0
        FSO  = 0.0
        FSP  = 0.0
C
 20   CONTINUE
C
C     find the upper layer principal moisture storage
      UMST= UZS+ (IFWI+PERC+INFIL)
C
      IF (UMST.LE.0.0) GO TO 30
C       there is upper layer principal moisture storage
C       convert moisture storage to mass/area units
        UMSTM= UMST*CFINMA
C
C       determine the fraction of the upper layer solute in principal
C       storage going to upper layer transitory (interflow) storage
        FII= IFWI/UMST
C
C       total downward water flux from the upper layer
        UDOWN= INFIL+ PERC
C
C       calculate the percolating solutes retardation factor
C       for uzs < (uzsn*ulpf)
        ULMPF= UZS/(UZSN*ULPF)
        IF (ULMPF.GT.1.0)  ULMPF= 1.0
C
C       determine the fraction of the upper layer solute in storage
C       being percolated
        FUP= ULMPF*UDOWN/UMST
        GO TO 40
C
 30   CONTINUE
C       there is no upper layer moisture storage, so zero variables
        UMSTM= 0.0
        FII  = 0.0
        FUP  = 0.0
C
 40   CONTINUE
C
C     find upper layer transitory (interflow) moisture storage
      ISMST= IFWS+ IFWO
      IMSTM= ISMST*CFINMA
C
      IF (ISMST.LE.0.0) GO TO 50
C       there is upper layer transitory (interflow) moisture storage
C       determine the fraction of the upper layer transitory
C       (interflow) solute in storage being removed by interflow
C       outflow
        FIO= IFWO/ISMST
        GO TO 60
C
 50   CONTINUE
        FIO= 0.0
C
 60   CONTINUE
C
C     place computed values in groups
      MST(1) = SMSTM
      MST(2) = UMSTM
      MST(3) = IMSTM
      FRAC(1)= FSO
      FRAC(2)= FSP
      FRAC(3)= FII
      FRAC(4)= FUP
      FRAC(5)= FIO
C
      RETURN
      END
