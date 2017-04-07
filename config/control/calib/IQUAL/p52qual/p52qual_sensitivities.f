************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function newACQOP(ACQOP ,nq,
     I    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I    Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real ACQOP,newACQOP
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      real zero,relax,RatioRelax
      data zero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newACQOP = RatioRelax(ACQOP,Tsnh4,Xsnh3,Xdnh3,relax)
      else if (nq.eq.kno3) then
        newACQOP = RatioRelax(ACQOP,Tsno3,Xsno3,zero,relax)
      else if (nq.eq.klon) then
        newACQOP = RatioRelax(ACQOP,Tslon,Xslon,Xdlon,relax)
      else if (nq.eq.kron) then
        newACQOP = RatioRelax(ACQOP,Tsron,Xsron,Xdron,relax)
      else if (nq.eq.kpo4) then
        newACQOP = RatioRelax(ACQOP,TSpo4,Xspo4,Xdpo4,relax)
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
      function newPOTFW(POTFW ,nq,
     I    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I    Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real POTFW,newPOTFW
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      real zero,relax,RatioRelax
      data zero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newPOTFW = RatioRelax(POTFW,Tsnh4,Xsnh3,Xdnh3,relax)
      else if (nq.eq.kno3) then
        newPOTFW = POTFW
      else if (nq.eq.klon) then
        newPOTFW = RatioRelax(POTFW,Tslon,Xslon,Xdlon,relax)
      else if (nq.eq.kron) then
        newPOTFW = RatioRelax(POTFW,Tsron,Xsron,Xdron,relax)
      else if (nq.eq.kpo4) then
        newPOTFW = RatioRelax(POTFW,Tspo4,Xspo4,Xdpo4,relax)
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
     I    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I    Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQO,newSQO
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      real zero,relax,RatioRelax
      data zero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newSQO = RatioRelax(SQO,Tsnh4,Xsnh3,Xdnh3,relax)
      else if (nq.eq.kno3) then
        newSQO = RatioRelax(SQO,Tsno3,Xsno3,zero,relax)
      else if (nq.eq.klon) then
        newSQO = RatioRelax(SQO,Tslon,Xslon,Xdlon,relax)
      else if (nq.eq.kron) then
        newSQO = RatioRelax(SQO,Tsron,Xsron,Xdron,relax)
      else if (nq.eq.kpo4) then
        newSQO = RatioRelax(SQO,TSpo4,Xspo4,Xdpo4,relax)
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
     I    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I    Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real SQOLIM,newSQOLIM
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      real zero,relax,RatioRelax
      data zero /0.0/
      external RatioRelax
      relax = 0.99
      if (nq.eq.knh3) then
        newSQOLIM = RatioRelax(SQOLIM,Tsnh4,Xsnh3,Xdnh3,relax)
      else if (nq.eq.kno3) then
        newSQOLIM = RatioRelax(SQOLIM,Tsno3,Xsno3,zero,relax)
      else if (nq.eq.klon) then
        newSQOLIM = RatioRelax(SQOLIM,Tslon,Xslon,Xdlon,relax)
      else if (nq.eq.kron) then
        newSQOLIM = RatioRelax(SQOLIM,Tsron,Xsron,Xdron,relax)
      else if (nq.eq.kpo4) then
        newSQOLIM = RatioRelax(SQOLIM,TSpo4,Xspo4,Xdpo4,relax)
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
      function newWSQOP(WSQOP ,nq,
     I    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I    Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/calibration_utils/change_param/ca
     .lib_iter/PQUAL/piqual.inc'
      include '../../../../../code/src/lib/inc/standard.inc'
      real WSQOP,newWSQOP
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      real zero,relax,RatioRelax
      data zero /0.0/
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
      return
      end

************************************************************************
** standard ratio parameter update with undershooting                 **
************************************************************************
      function RatioRelax(oldparam,target,X1,X2,relax)
      implicit none
      real RatioRelax,oldparam,target,X1,X2,relax
      RatioRelax = oldparam * (1.0+ (target/(X1+X2) - 1.0) * relax)
      return
      end

