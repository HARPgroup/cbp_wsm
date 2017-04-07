************************************************************************
** the user-defined senstivities of inputs to outputs are defined here**
************************************************************************
      function calibrated(
     I     Tsnh4,Tsno3,Tslon,Tsron,Tspo4,
     I     Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      real Tsnh4,Tsno3,Tslon,Tsron,Tspo4
      real Xdnh3,Xsnh3,Xsno3,Xdlon,Xslon,Xdron,Xsron,Xdpo4,Xspo4
      logical calibrated
      real tol
      parameter (tol = 0.01)
      calibrated = .true.
      if (abs((Xsnh3+Xdnh3-Tsnh4)/Tsnh4).gt.tol) calibrated = .false.
      if (abs((Xsno3      -Tsno3)/Tsno3).gt.tol) calibrated = .false.
      if (abs((Xslon+Xdlon-Tslon)/Tsnh4).gt.tol) calibrated = .false.
      if (abs((Xsron+Xdron-Tsron)/Tsron).gt.tol) calibrated = .false.
      if (abs((Xspo4+Xdpo4-Tspo4)/Tspo4).gt.tol) calibrated = .false.
      return
      end

