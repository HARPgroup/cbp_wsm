************************************************************************
** finds next value of parm                                           **
**   simple optimization routine that searches for the maximum NSE    **
**    works well for monotonic first derivatives, won't work for      **
**    multiple minima, test sensitivity first                         **
** strategy:                                                          **
**   if the first iteration, just pick a new value one step to middle **
**   calculate the difference between the last 2 iterations and move  **
**   in the implied direction.                                        **
**   if that puts you close to another point, cut the step by half    **
**   If the step drops below minstep, and efficiency change is small  **
**     convergence is reached                                         **
**   If a step puts you beyond the limits, go the other direction     **
************************************************************************
      subroutine updateparm(
     I                        eff,maxit,it,limitsparm,
     M                        parm,
     O                        converge)
      implicit none
      integer maxit,it,i
      real eff(maxit)
      real parm(maxit)
      logical converge
      real step
      real limitsparm(4)

      real critstep, critE ! criteria for convergence
      parameter (critstep = 0.1)
      parameter (critE = 0.01)

      logical oob  ! Out Of Bounds
      external oob

      integer highit, previt  ! highest and previous iterations
      real testE  ! test efficiency

      logical beenthere

      integer safety  ! iteration counter to get out of infinite loop 

      converge = .false.

      if (it.eq.1) then  !  if first time through, go toward middle
        step = 2.0
        if (parm(1).lt.9.0) then
          parm(2) = parm(1) + step
        else
          parm(2) = parm(1) - step
        end if
        return
      end if

      testE = -99999.0
      do i = 1,it                   ! find highest iteration
        if (eff(i).gt.testE) then
          testE = eff(i)
          highit = i
        end if
      end do

      testE = -99999.0
      do i = 1,it        ! find next highest
        if (eff(i).gt.testE.and.i.ne.highit) then
          testE = eff(i)
          previt = i
        end if
      end do

      step = parm(highit)-parm(previt)

      parm(it+1) = parm(highit) + step  ! go further in direction

      if (oob(parm(it+1),limitsparm)) then  ! check for out of bounds
        parm(it+1) = parm(highit) - step/2.0 ! go half back other way
      end if

      beenthere = .true.
      safety = 0
      do while (beenthere)
        beenthere = .false.
        do i = 1,it  ! check for oscillation
          if (abs(parm(it+1)-parm(i)).lt.0.001) then  ! if been there
            beenthere = .true.
            exit
          end if
        end do
        if (beenthere) then
          step = - step/2.0
          parm(it+1) = parm(highit) + step
        end if
        if (oob(parm(it+1),limitsparm)) then  ! check for out of bounds
          parm(it+1) = parm(highit) - step ! go half back other way
        end if
        safety = safety + 1
        if (safety.eq.20) exit
      end do

      if (abs(step).lt.critstep.and.abs(eff(it-1)-eff(it)).lt.critE)
     .    converge = .true.

      return
      end

************************************************************************
** tell if the value is Out Of Bounds                                 **
************************************************************************
      function oob(value,limits)
      real value,limits(4)
      logical oob
      oob = .false.
      if (value.lt.limits(1)) oob = .true.
      if (value.gt.limits(2)) oob = .true.
      return
      end

