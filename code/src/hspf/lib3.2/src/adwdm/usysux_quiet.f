C
C
C
      SUBROUTINE   XOSVAR
     I                   (WDNAME,
     O                    FILNAM)
C
C     + + + PURPOSE + + +
C     dummy routine to check for operating system variable.
C     just returns name of wdm file in this version.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64 WDNAME,FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDNAME - name of the WDM file
C     FILNAM - output name from operating system
C
C     + + + END SPECIFICATIONS + + +
C
      FILNAM = WDNAME
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTARG
     M                   (FNAME)
C
C     + + + PURPOSE + + +
C     Get the name of the input file from the user.  If not
C     entered, use default.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64  FNAME
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FNAME  - Name of input file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  TNAME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A64)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(//,' Enter the name of your input file: ')
C
C     + + + END SPECIFICATIONS + + +
C
C      WRITE(*,2000)
      READ (*,1000) TNAME
      IF (TNAME .NE. ' ') THEN
C       use user-supplied file name
        FNAME= TNAME
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTCHR
     O                   (CH1,CH2,CH3)
C
C     + + + PURPOSE + + +
C     get characters for screen display
C     unix specific
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1   CH1,CH2,CH3
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CH1    - character for hspf screen output
C     CH2    - character for hspf screen output
C     CH3    - character for hspf screen output
C
C     + + + END SPECIFICATIONS + + +
C
      CH1= '-'
      CH2= 'X'
      CH3= '.'
C
      RETURN
      END
C
C
C
      SUBROUTINE   XCLRSC
C
C     + + + PURPOSE + + +
C     clear screen for hspf run
C     dummy in unix version
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTRCL
     I                   (BASE,
     O                    RCL)
C
C     + + + PURPOSE + + +
C     get record length
C     unix specific
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BASE,RCL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BASE - base number of record length
C     RCL  - total record length
C
C     + + + END SPECIFICATIONS + + +
C
      RCL = 4*BASE
C
      RETURN
      END
C
C
C
      SUBROUTINE   XPLFRM
     O                   (FRM)
C
C     + + + PURPOSE + + +
C     get frm for pltgen
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*30   FRM
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FRM    - format needed for pltgen
C
C     + + + END SPECIFICATIONS + + +
C
      FRM= 'FORMATTED'
C
      RETURN
      END
