************************************************************************
** subroutine to perform regression between two vectors               **
************************************************************************
      subroutine regress(x,y,n,vsize,
     O                   slope,intercept,correl,limit,err)
      
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real correl  !  correlation coefficient
      real slope,intercept,SSE,S2

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,sumy,sumxy,sumx2,sumy2,denom,realn  ! calculation variables
      double precision:: Sxx, Sxy, Syy
      integer i    ! index
      
      real:: tvalue
      parameter (tvalue = 1.282) 

      real:: limit

******************** END DECLARATION ****************************************
      realn = real(n)

      sumx  = 0.
      sumy  = 0.
      sumxy = 0.
      sumx2 = 0.
      sumy2 = 0.

      do i = 1,n
        sumx  = sumx  + x(i)
        sumy  = sumy  + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumx2 = sumx2 + x(i)*x(i)
        sumy2 = sumy2 + y(i)*y(i)
      end do

      denom = (realn*sumx2 - sumx*sumx)*(realn*sumy2 - sumy*sumy)
      if (denom.lt.0.0) then
        err = 1
        correl = 0.0
      else

        denom = denom**0.5

        if (abs(denom).lt.1.0e-10) then
          err = 1
          correl = 0.0
        else
          err = 0
          correl = (realn*sumxy - sumx*sumy) / denom
        end if

      end if

      Sxx = sumx2 - (sumx*sumx/realn)
      Syy = sumy2 - (sumy*sumy/realn)
      Sxy = sumxy - (sumx*sumy/realn)

      if (abs(Sxx).lt.1.0e-10) then
        err = 1
        slope = 0.0
      else
        err = 0
        slope = Sxy/Sxx
      end if

      intercept = sumy/realn - slope*sumx/realn

      SSE = Syy - slope*Sxy
      S2  = SSE/real((n-2))
      limit = tvalue*sqrt(S2)/sqrt(Sxx)

      return

      end

************************************************************************
** subroutine to perform regression between two vectors NO INTERCEPT  **
************************************************************************
      subroutine noIregress(x,y,n,vsize,
     O                      slope,correl,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real correl  !  correlation coefficient
      real slope

      integer err   ! 0=OK, 1=divide by zero

      double precision sumy,sumxy,sumx2,sumy2,meany,yybar2,R2  ! calculation variables
      integer i    ! index


      sumy  = 0.
      sumxy = 0.
      sumx2 = 0.
      sumy2 = 0.

      do i = 1,n
        sumy  = sumy  + y(i)
      end do
      meany = sumy/real(n)

      do i = 1,n
        yybar2 = yybar2 + (y(i)-meany)*(y(i)-meany)
        sumxy = sumxy + x(i)*y(i)
        sumx2 = sumx2 + x(i)*x(i)
        sumy2 = sumy2 + y(i)*y(i)
      end do

      if (abs(yybar2).lt.1.0e-10) then
        err = 1
        correl = 0.0
      else
        err = 0
        R2 = 1.0 - (sumy2-(sumxy*sumxy/sumx2))/yybar2
        correl = sqrt(R2) 
      end if

      slope = sumxy/sumx2
      if (abs(sumx2).lt.1.0e-10) then
        err = 1
        slope = 0.0
      else
        err = 0
        slope = sumxy/sumx2
      end if

      return
      end

************************************************************************
**  function to find the correlation coefficient for two vectors      **
************************************************************************
      function correl(x,y,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real correl  !  correlation coefficient

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,sumy,sumxy,sumx2,sumy2,denom,realn  ! calculation variables
      integer i    ! index

      realn = real(n)

      sumx  = 0.
      sumy  = 0.
      sumxy = 0.
      sumx2 = 0.
      sumy2 = 0.

      do i = 1,n
        sumx  = sumx  + x(i)
        sumy  = sumy  + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumx2 = sumx2 + x(i)*x(i)
        sumy2 = sumy2 + y(i)*y(i)
      end do

      denom = (realn*sumx2 - sumx*sumx)*(realn*sumy2 - sumy*sumy)
      if (denom.lt.0.0) then
        err = 1
        correl = 0.0
      else

        denom = denom**0.5
        if (abs(denom).lt.1.0e-10) then
          err = 2
          correl = 0.0
        else
          err = 0
          correl = (realn*sumxy - sumx*sumy) / denom
        end if

      end if

      end

************************************************************************
**  function to find the slope of the regression line given 2 vectors **
************************************************************************
      function slope(x,y,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real slope  ! slope

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,sumy,sumxy,sumx2,sumy2,Sxy,Sxx,realn  ! calculation variables
      integer i    ! index

      realn = real(n)

      sumx  = 0.
      sumy  = 0.
      sumxy = 0.
      sumx2 = 0.
      sumy2 = 0.

      do i = 1,n
        sumx  = sumx  + x(i)
        sumy  = sumy  + y(i)
        sumxy = sumxy + x(i)*y(i)
        sumx2 = sumx2 + x(i)*x(i)
        sumy2 = sumy2 + y(i)*y(i)
      end do

      Sxx = sumx2 - (sumx*sumx/realn)
      Sxy = sumxy - (sumx*sumy/realn)

      if (abs(Sxx).lt.1.0e-10) then
        err = 1
        slope = 0.0
      else
        err = 0
        slope = Sxy/Sxx
      end if

      end

************************************************************************
**  function to find the interceptthe regression line given 2 vectors **
************************************************************************
      function intercept(x,y,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real slope  ! slope
      real intercept  ! intercept

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,sumy,sumxy,sumx2,sumy2,Sxy,Sxx,realn  ! calculation variables
      integer i    ! index

      realn = real(n)

      sumx  = 0.
      sumy  = 0.
      sumxy = 0.
      sumx2 = 0.
      sumy2 = 0.

      do i = 1,n
	sumx  = sumx  + x(i)
	sumy  = sumy  + y(i)
	sumxy = sumxy + x(i)*y(i)
	sumx2 = sumx2 + x(i)*x(i)
	sumy2 = sumy2 + y(i)*y(i)
      end do

      Sxx = sumx2 - (sumx*sumx/realn)
      Sxy = sumxy - (sumx*sumy/realn)

      if (abs(Sxx).lt.1.0e-10) then
	err = 1
	slope = 0.0
      else
	err = 0
	slope = Sxy/Sxx
      end if

      intercept = sumy/realn - slope*sumx/realn

      end


************************************************************************
**  function to find the mean of a vector                             **
************************************************************************
      function average(x,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize)  ! vectors
      real average

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,realn  ! calculation variables
      integer i    ! index

      realn = real(n)

      sumx = 0.0
      do i = 1,n
        sumx = sumx + x(i)
      end do
      average = sumx/real(n)
      return
      end


************************************************************************
**  function to find the variance of a vector                         **
************************************************************************
      function variance(x,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize)  ! vectors
      real variance  !  correlation coefficient

      integer err   ! 0=OK, 1=divide by zero

      double precision sumx,sumx2  ! calculation variables
      integer denom
      integer i    ! index

      sumx  = 0.
      sumx2 = 0.

      do i = 1,n
        sumx  = sumx  + x(i)
        sumx2 = sumx2 + x(i)*x(i)
      end do

      denom = n*(n-1)
      if (denom.eq.0) then
        err = 1
        variance = 0.0
      else
        err = 0
        variance = (real(n)*sumx2 - sumx*sumx) / real(denom)
      end if

      end
      
************************************************************************
**  function to find the error variance of two vectors                **
************************************************************************
      function errorvar(x,y,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),y(vsize)  ! vectors
      real errorvar  !  correlation coefficient

      integer err   ! 0=OK, 1=divide by zero

      double precision sume2  ! calculation variables
      integer denom
      integer i    ! index

      sume2 = 0.

      do i = 1,n
        sume2 = sume2 + (x(i)-y(i))**2
      end do

      denom = (n-1)
      if (denom.le.0) then
        err = 1
        errorvar = 0.0
      else
        err = 0
        errorvar = sume2 / real(denom)
      end if

      end

************************************************************************
**  function to find the mean of a vector                             **
************************************************************************
      function mean(x,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize)  ! vector
      real mean      ! mean
      double precision sumx  ! calculation variables
      integer i    ! index
      integer err   ! 0=OK, 1=divide by zero

      if (n.le.0) then
        err = 1
        mean = 0.0
      else
        err = 0
        sumx = 0.0
        do i = 1,n
          sumx = sumx + x(i)
        end do
        mean = sumx / real(n)
      end if
      end

************************************************************************
**  function to find the median of a vector                           **
************************************************************************
      function median(x,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      real x(vsize),Tx(vsize)  ! vector
      real median      ! median
      integer i    ! index
      integer err   ! 0=OK, 1=NO DATA

      do i = 1, n
        Tx(i) = x(i)
      end do

      if (n.eq.0) then
        err = 1
        median = -9.0
        return
      end if

      err = 0
      call rbubble(Tx,n,vsize)
      if (mod(n,2).eq.1) then
        median = Tx(n/2+1)
      else
        median = (Tx(n/2)+Tx(n/2+1))/2.0
      end if
      end

************************************************************************
** function to return the mode of an integer vector                   **
**   if more than one mode exists, the one closest to the average is  **
**     used.  If two are equidistant, the lower one is used           **
************************************************************************
      function imode(invector,n,vsize,err)
      implicit none
      integer n,vsize   ! number of points, total vector size
      integer invector(vsize) ! input vector (does not change)
      integer iv(vsize)   ! sorted input vector
      integer modes(n)  ! possible modes
      integer nmodes    ! number of possible modes
      integer maxoccur,occur  ! maximum occurance, current occurances
      integer i ! index
      integer lastiv  ! previous number in sorted vector
      integer imode   ! output
      integer err  ! = 1 if multi-modal
      integer nearmode ! index of current mode nearest to average
      real riv(vsize)  ! real invector
      real aveiv      ! average of invector
      real mindist    ! minimum distance between mode and average
      real mean
      external mean

***************** END DECLARATIONS

      do i = 1,n  ! put into sortable vector
        iv(i) = invector(i)
      end do

      call ibubble(iv,n,vsize)   ! sort

*********** find mode(s)
      maxoccur = 0
      occur = 1
      lastiv = iv(1)
      do i = 2,n
        if (iv(i).eq.lastiv) then
          occur = occur + 1
        else
          if (occur.eq.maxoccur) then
            nmodes = nmodes + 1
            modes(nmodes) = lastiv
          else if (occur.gt.maxoccur) then
            nmodes = 1
            modes(nmodes) = lastiv
          end if
          lastiv = iv(i)
        end if
      end do
      if (occur.eq.maxoccur) then
        nmodes = nmodes + 1
        modes(nmodes) = lastiv
      else if (occur.gt.maxoccur) then
        nmodes = 1
        modes(nmodes) = lastiv
      end if

********** if multiple modes, find one nearest average
      if (nmodes.gt.1) then
        err = 1
        do i = 1,n
          riv(i) = real(iv(i))
        end do
        aveiv = mean(riv,n,vsize,err)
        mindist = abs(aveiv-modes(1))
        nearmode = 1
        do i = 2,n
          if (abs(aveiv-modes(i)).lt.mindist) then
            mindist = abs(aveiv-modes(i))
            nearmode = i
          end if
        end do
        modes(1) = modes(nearmode)
      end if

      imode = modes(1)
      return

      end
