************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialPOTFW(POTFW ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real POTFW,initialPOTFW
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialPOTFW = 0.05
      else if (nq.eq.kno3) then
        initialPOTFW = 0.5
      else if (nq.eq.klon) then
        initialPOTFW = 0.2
      else if (nq.eq.kron) then
        initialPOTFW = 0.2
      else if (nq.eq.kpo4) then
        initialPOTFW = 0.2
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .     './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialSQO(SQO ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQO,initialSQO
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialSQO = Tsnh4/365.0*10.0
      else if (nq.eq.kno3) then
        initialSQO = Tsno3/365.0*10.0
      else if (nq.eq.klon) then
        initialSQO = Tslon/365.0*10.0
      else if (nq.eq.kron) then
        initialSQO = Tsron/365.0*10.0
      else if (nq.eq.kpo4) then
        initialSQO = Tspo4/365.0*10.0
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialACQOP(ACQOP ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real ACQOP,initialACQOP
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialACQOP = Tsnh4/365.0
      else if (nq.eq.kno3) then
        initialACQOP = Tsno3/365.0
      else if (nq.eq.klon) then
        initialACQOP = Tslon/365.0
      else if (nq.eq.kron) then
        initialACQOP = Tsron/365.0
      else if (nq.eq.kpo4) then
        initialACQOP = Tspo4/365.0
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialSQOLIM(SQOLIM ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQOLIM,initialSQOLIM
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialSQOLIM = Tsnh4/365.0*10.0
      else if (nq.eq.kno3) then
        initialSQOLIM = Tsno3/365.0*10.0
      else if (nq.eq.klon) then
        initialSQOLIM = Tslon/365.0*10.0
      else if (nq.eq.kron) then
        initialSQOLIM = Tsron/365.0*10.0
      else if (nq.eq.kpo4) then
        initialSQOLIM = Tspo4/365.0*10.0
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      initialSQOLIM = max(initialSQOLIM,0.000001)
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialWSQOP(WSQOP ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real WSQOP,initialWSQOP
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialWSQOP = 0.5
      else if (nq.eq.kno3) then
        initialWSQOP = 0.5
      else if (nq.eq.klon) then
        initialWSQOP = 0.5
      else if (nq.eq.kron) then
        initialWSQOP = 0.5
      else if (nq.eq.kpo4) then
        initialWSQOP = 0.5
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      initialWSQOP = max(0.01,initialWSQOP)
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialIOQC(IOQC ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real IOQC,initialIOQC
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialIOQC = 0.00005
      else if (nq.eq.kno3) then
        initialIOQC = 0.0005
      else if (nq.eq.klon) then
        initialIOQC = 0.0002
      else if (nq.eq.kron) then
        initialIOQC = 0.0002
      else if (nq.eq.kpo4) then
        initialIOQC = 0.0002
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function initialAOQC(AOQC ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real AOQC,initialAOQC
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      if (nq.eq.knh3) then
        initialAOQC = 0.0001
      else if (nq.eq.kno3) then
        initialAOQC = 0.0005
      else if (nq.eq.klon) then
        initialAOQC = 0.0001
      else if (nq.eq.kron) then
        initialAOQC = 0.0001
      else if (nq.eq.kpo4) then
        initialAOQC = 0.0000001
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      return
      end

