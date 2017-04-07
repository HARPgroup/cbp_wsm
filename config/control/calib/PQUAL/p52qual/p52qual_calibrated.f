************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function calibrated(
     I                    Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I                    Tbnh4,Tbno3,Tblon,Tbron,Tbpo4,
     I                    Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3,
     I                    Xino3,Xano3,Xdlon,Xslon,Xilon,
     I                    Xalon,Xdron,Xsron,Xiron,Xaron,
     I                    Xdpo4,Xspo4,Xipo4,Xapo4)

      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      real                Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real                Tbnh4,Tbno3,Tblon,Tbron,Tbpo4
      real                Xdnh3,Xsnh3,Xinh3,Xanh3,Xsno3
      real                Xino3,Xano3,Xdlon,Xslon,Xilon
      real                Xalon,Xdron,Xsron,Xiron,Xaron
      real                Xdpo4,Xspo4,Xipo4,Xapo4
      logical calibrated
      real tol
      parameter (tol = 0.05)
      calibrated = .true.
      if (abs((Xdnh3+Xsnh3+Xinh3-Tsnh4)/Tsnh4).gt.tol) then
        calibrated = .false.
      end if
      if (abs((      Xsno3+Xino3-Tsno3)/Tsno3).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xdlon+Xslon+Xilon-Tslon)/Tslon).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xdron+Xsron+Xiron-Tsron)/Tsron).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xdpo4+Xspo4+Xipo4-Tspo4)/Tspo4).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xanh3-Tbnh4)/Tbnh4).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xano3-Tbno3)/Tbno3).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xalon-Tblon)/Tblon).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xaron-Tbron)/Tbron).gt.tol) then
        calibrated = .false.
      end if
      if (abs((Xapo4-Tbpo4)/Tbpo4).gt.tol) then
        calibrated = .false.
      end if
      return
      end

