C
C     common block CTSDIN/C from include file CTSDIR
C
      INCLUDE  'pmxts.inc'
C
      COMMON /CTSDRN/ CTSWDM,CTSBDS,CTSSAV,CTSAVA,CTSGPS,
     $                CTSTU,CTSTS,CTSSDT,CTSEDT,
     $                CSPAVA,CTSSEL,CTSILC,CPARFG
      INTEGER         CTSWDM,CTSBDS,CTSSAV(MXTS),CTSAVA(MXTS),
     $                CTSGPS(MXTS),CTSTU(MXTS),CTSTS(MXTS),
     $                CTSSDT(6,MXTS),CTSEDT(6,MXTS),
     $                CSPAVA,CTSSEL(MXTS),CTSILC(MXTS),CPARFG
C
      COMMON /CTSDRC/ CTSSEN,CTSLOC,CTSCON,CSPSEN,CSPLOC,CSPCON
      CHARACTER*8     CTSSEN(MXTS),CTSLOC(MXTS),CTSCON(MXTS),
     $                CSPSEN,CSPLOC,CSPCON
C
C     CTSAVA - available flag - -1: no dataset,
C                                1: label only,
C                                2: label and data
C     CSPAVA - specification for data available
C     CPARFG - flag for allowing partial matches of current specs
C              0 - no partial matches
C              1 - partial matches allowed
C
