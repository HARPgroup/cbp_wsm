************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function newPOTFW(POTFW ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real POTFW,newPOTFW
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newPOTFW = RatioRelax(POTFW,Tsnh4,Xdnh3,Xsnh3,Xinh3,relax)
      else if (nq.eq.kno3) then
        newPOTFW = POTFW
      else if (nq.eq.klon) then
        newPOTFW = RatioRelax(POTFW,Tslon,Xdlon,Xslon,Xilon,relax)
      else if (nq.eq.kron) then
        newPOTFW = RatioRelax(POTFW,Tsron,Xdron,Xsron,Xiron,relax)
      else if (nq.eq.kpo4) then
        newPOTFW = RatioRelax(POTFW,Tspo4,Xdpo4,Xspo4,Xipo4,relax)
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
      function newSQO(SQO ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQO,newSQO
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newSQO = RatioRelax(SQO,Tsnh4,Xdnh3,Xsnh3,Xinh3,relax)
      else if (nq.eq.kno3) then
        newSQO = RatioRelax(SQO,Tsno3,Xzero,Xsno3,Xino3,relax)
      else if (nq.eq.klon) then
        newSQO = RatioRelax(SQO,Tslon,Xdlon,Xslon,Xilon,relax)
      else if (nq.eq.kron) then
        newSQO = RatioRelax(SQO,Tsron,Xdron,Xsron,Xiron,relax)
      else if (nq.eq.kpo4) then
        newSQO = RatioRelax(SQO,Tspo4,Xdpo4,Xspo4,Xipo4,relax)
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
      function newACQOP(ACQOP ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real ACQOP,newACQOP
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newACQOP = RatioRelax(ACQOP,Tsnh4,Xdnh3,Xsnh3,Xinh3,relax)
      else if (nq.eq.kno3) then
        newACQOP = RatioRelax(ACQOP,Tsno3,Xzero,Xsno3,Xino3,relax)
      else if (nq.eq.klon) then
        newACQOP = RatioRelax(ACQOP,Tslon,Xdlon,Xslon,Xilon,relax)
      else if (nq.eq.kron) then
        newACQOP = RatioRelax(ACQOP,Tsron,Xdron,Xsron,Xiron,relax)
      else if (nq.eq.kpo4) then
        newACQOP = RatioRelax(ACQOP,Tspo4,Xdpo4,Xspo4,Xipo4,relax)
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
      function newSQOLIM(SQOLIM ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQOLIM,newSQOLIM
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newSQOLIM = RatioRelax(SQOLIM,Tsnh4,Xdnh3,Xsnh3,Xinh3,relax)
      else if (nq.eq.kno3) then
        newSQOLIM = RatioRelax(SQOLIM,Tsno3,Xzero,Xsno3,Xino3,relax)
      else if (nq.eq.klon) then
        newSQOLIM = RatioRelax(SQOLIM,Tslon,Xdlon,Xslon,Xilon,relax)
      else if (nq.eq.kron) then
        newSQOLIM = RatioRelax(SQOLIM,Tsron,Xdron,Xsron,Xiron,relax)
      else if (nq.eq.kpo4) then
        newSQOLIM = RatioRelax(SQOLIM,Tspo4,Xdpo4,Xspo4,Xipo4,relax)
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      newSQOLIM = max(newSQOLIM,0.000001)
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function newWSQOP(WSQOP ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real WSQOP,newWSQOP
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newWSQOP = WSQOP
      else if (nq.eq.kno3) then
        newWSQOP = WSQOP
      else if (nq.eq.klon) then
        newWSQOP = WSQOP
      else if (nq.eq.kron) then
        newWSQOP = WSQOP
      else if (nq.eq.kpo4) then
        newWSQOP = WSQOP
      else
        write(report(1),*) 'programming error, nquals = ',nq
        report(2) = 'check ./code/src/lib/inc/piqual.inc'
        report(3) = 
     .    './code/src/calibration_utils/change_param/calib_iter/IQUAL/'
        call stopreport(report)
      end if
      newWSQOP = max(0.01,newWSQOP)
      return
      end

************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function newIOQC(IOQC ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real IOQC,newIOQC
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newIOQC = RatioRelax(IOQC,Tsnh4,Xdnh3,Xsnh3,Xinh3,relax)
      else if (nq.eq.kno3) then
        newIOQC = RatioRelax(IOQC,Tsno3,Xzero,Xsno3,Xino3,relax)
      else if (nq.eq.klon) then
        newIOQC = RatioRelax(IOQC,Tslon,Xdlon,Xslon,Xilon,relax)
      else if (nq.eq.kron) then
        newIOQC = RatioRelax(IOQC,Tsron,Xdron,Xsron,Xiron,relax)
      else if (nq.eq.kpo4) then
        newIOQC = RatioRelax(IOQC,Tspo4,Xdpo4,Xspo4,Xipo4,relax)
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
      function newAOQC(AOQC ,nq,
     I      Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I      Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I      Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I      Xino3,Xano3,Xdlon,Xslon,Xilon,
     I      Xalon,Xdron,Xsron,Xiron,Xaron,
     I      Xdpo4,Xspo4,Xipo4,Xapo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real AOQC,newAOQC
      real  Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real  Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real  Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,Xino3,Xano3,Xdlon,Xslon,Xilon
      real  Xalon,Xdron,Xsron,Xiron,Xaron,Xdpo4,Xspo4,Xipo4,Xapo4
      real Xzero,relax,RatioRelax
      data Xzero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newAOQC = RatioRelax(AOQC,Tbnh4,Xanh3,Xzero,Xzero,relax)
      else if (nq.eq.kno3) then
        newAOQC = RatioRelax(AOQC,Tbno3,Xano3,Xzero,Xzero,relax)
      else if (nq.eq.klon) then
        newAOQC = RatioRelax(AOQC,Tblon,Xalon,Xzero,Xzero,relax)
      else if (nq.eq.kron) then
        newAOQC = RatioRelax(AOQC,Tbron,Xaron,Xzero,Xzero,relax)
      else if (nq.eq.kpo4) then
        newAOQC = RatioRelax(AOQC,Tbpo4,Xapo4,Xzero,Xzero,relax)
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
** standard ratio parameter update with undershooting                 **
************************************************************************
      function RatioRelax(oldparam,target,X1,X2,X3,relax)
      implicit none
      real RatioRelax,oldparam,target,X1,X2,X3,relax
      RatioRelax = oldparam * (1.0+ (target/(X1+X2+X3) - 1.0) * relax)
      return
      end

