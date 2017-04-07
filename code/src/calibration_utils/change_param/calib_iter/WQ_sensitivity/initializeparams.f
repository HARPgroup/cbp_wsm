************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program sets the starting values for the parameters that are  **
**   specified in the param_list.csv file. If this step is not done   **
**   then the param_list.csv must exactly match the parameter files   **
**   Failure of this condition would result in false sensitvities     **
************************************************************************
      subroutine initializeparams(
     I                        paramscen,uniqindex,lakeflags,
     I                        parLKflag,parModule,parTable,parName,
     I                        parAorM,parstart,parmin,parmax,npar,
     M                        parval)

      implicit none
      include 'sens.inc'

********* find next parameter to check
      do np = 1,npar   
        print*,parTable(np),' ',parName(np),' ',parstart(np)
        call change_river(
     I                      paramscen,uniqindex,lakeflags,
     I                      parLKflag(np),parModule(np),
     I                      parTable(np),parName(np),
     I                      parstart(np))
      end do

      return
********************** ERROR SPACE *************************************


      end


