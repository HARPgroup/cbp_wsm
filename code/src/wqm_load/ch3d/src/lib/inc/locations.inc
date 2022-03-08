************************************************************************
***** PARAMETER FILE  -- changing this file changes the parameters in **
***** the entire pre- and post-processors ( recompile necessary )     **
************************************************************************
      integer lentree
      parameter (lentree = 15)

      character tree*(lentree)     ! location from the temp directory
      parameter (tree   = '../../../../../')

********* parameters giving the locations of input and output files
      character ucidir*(lentree+8)  ! location of ucis
      parameter (ucidir = tree//'tmp/uci/')

      character outwdmdir*(lentree+8)  ! location of output wdms
      parameter (outwdmdir = tree//'tmp/wdm/')

      character sumoutdir*(lentree+7)   !  location of permanent output
      parameter (sumoutdir = tree//'sumout/')

      character outdir*(lentree+7)   !  location of temporary output
      parameter (outdir = tree//'output/')

      character pardir*(lentree+12)  ! location of parameter files
      parameter (pardir = tree//'input/param/')

      character calibdir*(lentree+12)    ! location of calibration files
      parameter (calibdir = tree//'input/calib/')

      character ScenDatDir*(lentree+15)        ! location scenario data 
      parameter (ScenDatDir = tree//'input/scenario/')

      character seglistdir*(lentree+16)  ! location of catalog files
      parameter (seglistdir = tree//'config/seglists/')

      character controldir*(lentree+15)   ! location of control files
      parameter (controldir = tree//'config/control/')

      character catdir*(lentree+15)        ! location of catalog files
      parameter (catdir = tree//'config/catalog/')

      character dummyWDMname*(lentree+26)
      parameter (dummyWDMname = tree//'config/blank_wdm/dummy.wdm')


************** variables for scenarios under the above directories
      character*25 ioscen  ! name of the directory for the I/O variables
      integer lenioscen    ! length of the variable

      character*25 geoscen  ! directory for the geometry variables
      integer lengeoscen    ! length of the variable

      character*25 paramscen  ! name of the directory for parameters
      integer lenparamscen


