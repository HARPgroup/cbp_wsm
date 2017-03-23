C
C     include file CSPINS.INC - common block to store special action
C     instructions
      COMMON /CSPINS/SPOFST,SPOUT,SPINS
C
      INTEGER      SPOFST,SPOUT,SPINS(LENSPI,MXSPIN)
C
      EQUIVALENCE (SPINS,SPINSR)
      REAL         SPINSR(LENSPI,MXSPIN)
C
C     SPOUT  - Special Action output level: 0-none;  1-action; 2-block;
C                                           3-chain; 4-condition
C     SPINS  - Special Action instructions
C
