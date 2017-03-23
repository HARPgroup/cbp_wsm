C     from CSPEC.INC   
C     run interpreter common
C     special action version
C     start byte:      0 word:    0
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxopn.inc'
      INCLUDE 'pmxvnm.inc'
C
      COMMON/CRINTP/FILE,DUMT1,DUMT2,DUMT3,WCOUNT,ECOUNT,OUTLEV,RESMFG,
     $              NIVLS,IVLLIB,NXGRPS,EXGTAB,NGRPS,GRPTAB,NOPNS,
     $              OPNO,OPNTAB,VDIM,VLOC,LOSPFL,LOSPST,LOSPKY,LONAM,
     $              LONUM,DUM
      INTEGER       FILE(15),DUMT1,DUMT2,DUMT3,WCOUNT(10),ECOUNT,OUTLEV,
     $              RESMFG,NIVLS,IVLLIB(20),NXGRPS,EXGTAB(5,10),NGRPS,
     $              GRPTAB(5,10),NOPNS,OPNO,OPNTAB(20,MAXOPN),
     $              VDIM(3,MAXVNM),VLOC(MAXVNM),LOSPFL(10),LOSPST(10),
     $              LOSPKY(10),LONAM(10),LONUM(10)
      INTEGER       DUM(794)
C     end byte:  44000 word:11000
C
      COMMON/VNAMES/VNAMLB
      CHARACTER*6   VNAMLB(MAXVNM)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (VNAMLB,VNAML1)
      CHARACTER*1  VNAML1(MAXVNM*6)
C
