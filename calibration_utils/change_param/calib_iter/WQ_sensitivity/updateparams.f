************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program determines the next parameter to be updated and calls **
**  a subroutine to update the correct file.  When moving to a new    **
**  variable, it sets the previous variable back to the nomimal value **
************************************************************************
      subroutine updateparams(
     I                        paramscen,uniqindex,lakeflags,
     I                        parLKflag,parModule,parTable,parName,
     I                        parAorM,parstart,parmin,parmax,npar,
     M                        parval,done)

      implicit none
      include 'sens.inc'
      logical done
      integer oldnp,oldnv

********* find next parameter to check
      done = .true.
      do np = 1,npar   
        do nv = 1,nval
          if (abs(parval(np,nv)+9.0).lt.0.001) then
            done = .false.
            exit
          end if
        end do
        if (.not.done) exit
      end do
      print*,np,',',nv

********* if switching params, reset old
      if (nv.eq.1) then
        if (np.ne.1) then
          oldnp = np - 1
          print*,parName(oldnp),parstart(oldnp)
          call change_river(
     I                      paramscen,uniqindex,lakeflags,
     I                      parLKflag(oldnp),parModule(oldnp),
     I                      parTable(oldnp),parName(oldnp),
     I                      parstart(oldnp))
        end if
      end if

********* if done, reset last parameter
      if (done) then
        oldnp = npar
        print*,parName(oldnp),parstart(oldnp)
        call change_river(
     I                    paramscen,uniqindex,lakeflags,
     I                    parLKflag(oldnp),parModule(oldnp),
     I                    parTable(oldnp),parName(oldnp),
     I                    parstart(oldnp))

********** if not done, set new parameter
      else
        if (nv.eq.1) then
          parval(np,nv) = parmin(np)
C        else if (nv.eq.2) then
C          if (parAorM(np).eq.'A') then
C            parval(np,nv) = (parstart(np)+parmin(np))/2.0
C          else
C           parval(np,nv) = exp((log(parstart(np))+log(parmin(np)))/2.0)
C          end if
C        else if (nv.eq.3) then
C          parval(np,nv) = parstart(np)
C        else if (nv.eq.4) then
C          if (parAorM(np).eq.'A') then
C            parval(np,nv) = (parstart(np)+parmax(np))/2.0
C          else
C           parval(np,nv) = exp((log(parstart(np))+log(parmax(np)))/2.0)
C          end if
        else if (nv.eq.2) then
          parval(np,nv) = parmax(np)
        end if
        print*,parName(np),parval(np,nv)
        call change_river(
     I                    paramscen,uniqindex,lakeflags,
     I                    parLKflag(np),parModule(np),
     I                    parTable(np),parName(np),
     I                    parval(np,nv))
      end if


************* store np and nv for sens matrix program
      call storenpnv(
     I               paramscen,np,nv)

      return
********************** ERROR SPACE *************************************


      end


