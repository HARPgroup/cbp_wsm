************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program read the file of parameters to test and finds the     **
**  next one on the list.  It then updates the appropriate parameter  **
**  file so that the rug can be re-run                                **
************************************************************************
      implicit none
      include 'sens.inc'

      integer Tnb  ! temp reading variable
      character*1 dummy  ! temp reading variable

      integer nr

************* END DECLARATIONS *****************************************
      read*,rscen
      call lencl(rscen,lenrscen)
      print*,' setting initial parameters'

*********** get rsegs
      call getrsegs(rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs) 

********** get paramscen
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

********** get all lake flags
      call getlakeflags(
     I                  paramscen,uniqindex,
     O                  lakeflags)

*********** read and store parameter list
      call readparams(
     I                paramscen,
     O                parLKflag,parModule,parTable,parName,parAorM,
     O                parstart,parmin,parmax,parval,header,npar)

************ put starting parameters into the parameter files
      call initializeparams(
     I                  paramscen,uniqindex,lakeflags,
     I                  parLKflag,parModule,parTable,parName,parAorM,
     I                  parstart,parmin,parmax,npar,
     M                  parval)

      return

********************** ERROR SPACE *************************************
996   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

999   call stopreport(report)

      end




