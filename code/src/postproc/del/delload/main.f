************************************************************************
**  This program multiplies all the edge-of-stream loads for a land   **
***    segment by the delivery factor for the river segment with      **
***    which it is associated                                         **
**                                                                    **
**  The strategy for this program is to loop over all lsegs in a seg  **
**    opening all the eos and data files and rewriting them after     **
**    multiplying by the delivery factors                             **
************************************************************************
      implicit none
      include 'delload.inc'
      integer ns,numsegs  ! number of segments in a river

      integer nl ! index for loads

      logical doatdep,dops,dosep

*********** output flags
      integer imonthly,iannual,iaveann
      logical monthly,annual,aveann

********************* END DECLARATIONS *********************************
********* get and process input
      read(*,*,err=995,end=996) rscen,rseg,
     .                          imonthly,iannual,iaveann,
     .                          year1,year2

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      print*,'Delivered loads for ',rscen(:lenrscen),' ',rseg

************ start book keeping
      call readcontrol_dat(rscen,lenrscen,
     O                     doatdep,dops,dosep)

      call readcontrol_time(rscen,lenrscen,
     O                      sdate,edate)
      if (sdate(1).lt.y1) go to 991
      if (edate(1).gt.y2) go to 992

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

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

      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)

      call getRvars(              ! get active Rvars
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rname)

      call getloads(            ! get loads for active Rvars
     I              ioscen,lenioscen,nRvar,Rname,
     O              nloads,loadname)
      do nl = 1,nloads
        segLoadName(nl) = 'SEG_'//loadname(nl)
        basinLoadName(nl) = 'BASIN_'//loadname(nl)
        resLoadName(nl) = 'RES_'//loadname(nl)
      end do

      call delinfo(
     I             ioscen,lenioscen,nloads,loadname,
     O             ndel,delname,load2del)  ! number of delivery factors

      call getl2rlist(rseg,rscen,lenrscen,
     O                numsegs,l2r)  ! find land segs for this river

*************** all book keeping done

      if (monthly) then
        call getmondfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,sdate,edate,
     O                 segDFmon,basinDFmon,resDFmon)
        do ns = 1,numsegs
          lseg = l2r(ns)
          call lencl(lseg,lenlseg)
          call dellsegmon
     I                (rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 sdate,edate,segDFmon,basinDFmon,resDFmon)
          call deldatmon(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 sdate,edate,segDFmon,basinDFmon,resDFmon)
          call delatdepmon(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 sdate,edate,segDFmon,basinDFmon,resDFmon)
        end do
      end if

      if (annual) then
        call getanndfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,sdate,edate,
     O                 segDFann,basinDFann,resDFann)
        do ns = 1,numsegs
          lseg = l2r(ns)
          call lencl(lseg,lenlseg)
          call dellsegann
     I                (rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 sdate,edate,segDFann,basinDFann,resDFann)
          call deldatann(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 sdate,edate,segDFann,basinDFann,resDFann)
          call delatdepann(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 sdate,edate,segDFann,basinDFann,resDFann)
        end do
      end if

      if (aveann) then
        call getavedfs(rseg,lenrseg,rscen,lenrscen,
     I                 ndel,delname,year1,year2,
     O                 segDFave,basinDFave,resDFave)
        do ns = 1,numsegs
          lseg = l2r(ns)
          call lencl(lseg,lenlseg)
          call dellsegave
     I                (rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 year1,year2,segDFave,basinDFave,resDFave)
          call deldatave(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 year1,year2,segDFave,basinDFave,resDFave)
          call delatdepave(
     I                 rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 year1,year2,segDFave,basinDFave,resDFave)
        end do
      end if

      stop

************************ ERROR SPACE *************************
991   report(1) = 'problem in transport factor program'
      report(2) = ' model run is earlier than allowed, change '//
     .         char(39)//'y1'//char(39)//' in file'
      report(3) = ' pp/src/postproc/del/tfs/tfs.inc'
      go to 999

992   report(1) = 'problem in transport factor program'
      report(2) = ' model run is later than allowed, change '//
     .         char(39)//'y2'//char(39)//' in file'
      report(3) = ' pp/src/postproc/del/tfs/tfs.inc'
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
