************************************************************************
** Program to find the delivery factors by multiplying all downstream **
**   transport factors for each delivered constituent.                **
**                                                                    **
** Delivery Factors can be calculated for different scales            **
**   The segment-specific delivery factor may be appropriate for N as **
**   there is the reduction mechanism of denitrification              **
**   For P and S, it is not clear that there are long-term removal    **
**   mechanisms other than burial, which can be positive or negative  **
**   over the relatively long-term (100 years or so)                  **
**                                                                    **
** This program calculates deliver factors three different ways       **
**   1. segment specific (multiply TFs down to the pour point)        **
**   2. whole basin (outputs / inputs upstream of the pour point)     **
**   3. cut at reservoirs ( outputs / inputs  upstream of a pour      **
**        point or reservoir and downstream of any upstream reservoir)**
**        multiplied by any downstrem deliver factors                 **
**                                                                    **
**   For segment specific, the general strategy is to start with the  **
**     current segment transport factors, then work downstream to the **
**     next segment and make it the current segment.  When the pour   **
**     point is reached, stop                                         **
**                                                                    **
**   For whole basin, find the downstream pour point, find all        **
**     segments upstream of the downstream pour point, then calculate **
**     the delivery factor as pour point output over the sum of       **
**     upstream EOS loads                                             **
**                                                                    **
**   For reservoirs, find the downstream pour point. develop a        **
**     transport factor for that pour point based on outputs/inputs   **
*      for all upstream segments that are also downstream of any      **
**     reservoirs other than one that may be at the pour point. If    ****     this basin does not include the current segment, continue      **
**     developing transport factors upstream until you reach the      **
**     basins defined by a downstream reservoir that contains the     **
**     current segment.  When you have transport factors for all      **
**     downstream reservoir basins and the pour point, multiply them  **
**     together to get the delivery factor.                           **
**                                                                    **
**   This code is inefficient and complicated in order to run on a    **
**     segment basis.  It could be simplified by running on the whole **
**     basin at once.  It is kept on a segment-specific run so that   **
**     it can be run on any geography.  Otherwise you would have to   **
**     make sure that any segment list was exhaustive of upstream     **
**     segments or that all segments in the model domain were run.    **
**                                                                    **
************************************************************************
      implicit none
      include 'dfs.inc'

      integer i,nd,ny

      character*13 downseg

      logical pourpoint             ! true if this is the pourpoint

*********** output flags
      integer imonthly,iannual,iaveann
      logical monthly,annual,aveann

*************************** END DECLARATIONS ***************************
      read(*,*,err=995,end=996) rscen,rseg,
     .                          imonthly,iannual,iaveann,
     .                          year1,year2

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)      !GY

      print*,'Delivery factors for ',rscen(:lenrscen),' ',rseg

      call readcontrol_time(rscen,lenrscen,
     O                      sdate,edate)
      if (sdate(1).lt.y1) go to 991
      if (edate(1).gt.y2) go to 992

********** set time flags and report what you are doing
      monthly = .false.
      if (imonthly.ne.0) then
        monthly = .true.
        print*,'   monthly from ',sdate(1),' to ',edate(1)
      end if
      annual = .false.
      if (iannual.ne.0) then
        annual = .true.
        print*,'   annual from ',sdate(1),' to ',edate(1)
      end if
      aveann = .false.
      if (iaveann.ne.0) then
        aveann = .true.
        print*,'   average annual for ',year1,' to ',year2
      end if

*************** read the control file for specifications
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      print *, 'readcontrol_Rioscen DONE!'
      call lencl(ioscen,lenioscen)
      print *, 'lencl DONE!'
      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)
      print *, 'readcontrol_modules DONE!'
************** read the iovars directory for variable specifications
      call getRvars(              ! get active Rvars
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rname)
      print *, 'getRvars DONE!'
      call getloads(            ! get loads for active Rvars
     I              ioscen,lenioscen,nRvar,Rname,
     O              nloads,loadname)
      print *, 'getloads DONE!'
      call delinfo(
     I             ioscen,lenioscen,nloads,loadname,
     O             ndel,delname,load2del)  ! number of delivery factors
      print *, 'delinfo DONE!'
************ get the list of river segments
      call getrsegs(rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs)
      print *, 'getrsegs DONE!'

*********** separate routines for each temporal averaging period
      if (monthly) then

       call CalcSegDFmon(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    sdate,edate,ndel,delname,
     O                    segDFmon)
        call CalcBasinDFmon(
     I                      rscen,lenrscen,
     I                      rseg,rsegs,uniqid,dsid,
     I                      uniqindex,nrsegs,
     I                      sdate,edate,ndel,delname,
     O                      basinDFmon)
        call CalcResDFmon(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    sdate,edate,ndel,delname,
     O                    resDFmon)
        call writemondfs(                                 !GY
     I                  rseg,lenrseg,rscen,lenrscen,      !GY
     I                  ndel,delname,sdate,edate,
     I                  segDFmon,BasinDFmon,ResDFmon)
      end if

      if (annual) then
        call CalcSegDFann(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    sdate,edate,ndel,delname,
     O                    segDFann)
        call CalcBasinDFann(
     I                      rscen,lenrscen,
     I                      rseg,rsegs,uniqid,dsid,
     I                      uniqindex,nrsegs,
     I                      sdate,edate,ndel,delname,
     O                      basinDFann)
        call CalcResDFann(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    sdate,edate,ndel,delname,
     O                    resDFann)
        call WriteAnnDFs(
     I                  rseg,lenrseg,rscen,lenrscen,
     I                  ndel,delname,sdate,edate,
     I                  segDFann,BasinDFann,ResDFann)
      end if

      if (aveann) then
        call CalcSegDFave(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    year1,year2,ndel,delname,
     O                    segDFave)
        print *, 'CalcSegDFave DONE!'
        call CalcBasinDFave(
     I                      rscen,lenrscen,
     I                      rseg,rsegs,uniqid,dsid,
     I                      uniqindex,nrsegs,
     I                      year1,year2,ndel,delname,
     O                      basinDFave)
        print *, 'CalcBasinDFave DONE!'
        call CalcResDFave(
     I                    rscen,lenrscen,
     I                    rseg,rsegs,uniqid,dsid,
     I                    uniqindex,nrsegs,
     I                    year1,year2,ndel,delname,
     O                    resDFave)
        print *, 'CalcResDFave DONE!'
        call WriteAveDFs(
     I                  rseg,rscen,lenrscen,
     I                  ndel,delname,year1,year2,
     I                  segDFave,BasinDFave,ResDFave)
        print *, 'WriteAveDFs DONE!'
      end if

      stop

************************ ERROR SPACE *************************
991   report(1) = 'problem in transport factor program'
      report(2) = ' model run is earlier than allowed, change '//
     .         char(39)//'y1'//char(39)//' in file'
      report(3) = ' pp/src/postproc/del/dfs/dfs.inc'
      go to 999

992   report(1) = 'problem in transport factor program'
      report(2) = ' model run is later than allowed, change '//
     .         char(39)//'y2'//char(39)//' in file'
      report(3) = ' pp/src/postproc/del/dfs/dfs.inc'
      go to 999

995   report(1) = 'Error initializing tf postprocessor'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and run_postproc.csh'
      go to 999

996   report(1) = 'Error initializing tf postprocessor'
      report(2) = 'not enough input data in run_postproc.csh'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
