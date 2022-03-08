************************************************************************
**  function to get a value of y given two points that define a line  **
**   and a value of x                                                 **
************************************************************************
      function interpolate(x,xlow,ylow,xhigh,yhigh)
      implicit none
      real x,xlow,ylow,xhigh,yhigh
      real m,b
      real interpolate

      if (abs(xlow-xhigh).lt.0.001) then
        interpolate = ylow
        return
      end if

      m = (yhigh-ylow)/(xhigh-xlow)
      b = ylow - m*xlow

      interpolate = m*x + b
      return
      end

