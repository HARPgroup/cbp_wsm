************************************************************************
**  This program finds transport factors for each river segment.  The **
***    transport factor is a property of a river segment, not of a    **
***    land segment. It is equal to the total output divided by the   **
***    total input.  By multiplying transport factors downstream, a   **
**     total delivery factor can be found.                            **
**                                                                    **
**  The strategy for this program is to loop over all lsegs in a seg  **
**    adding the associated eos and data inputs, then add the upstream**
**    river segment loads.  All calculations are on load variables in **
**    pp/catalog/iovars/rchres_out_to_load.  This calculation is done **
**    for collective variables, (TN, TP, etc) and then applied to     **
**    constituents.  The file pp/catalog/iovars/delivery_ratio_calc   **
**    contains the relationships between collective and constituents  **
************************************************************************
      implicit none

      include 'tfs.inc'

      integer i,ndl,ny,nm,nm1,nm2

      integer nlrsegs,nlr    ! number of land-river segments
      integer numsegs        ! number of land segs associated with river

*********** output flags
      integer imonthly,iannual,iaveann
      logical monthly,annual,aveann

      logical doatdep,dops,dosep

      logical IamRiver,rivercheck
      external rivercheck

      logical found

*************************** END DECLARATIONS ***************************
      read(*,*,err=995,end=996) rscen,rseg,
     .                          imonthly,iannual,iaveann,
     .                          year1,year2

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      print*,'Transport factors for ',rscen(:lenrscen),' ',rseg

*********** read control file to determine if atdep, ps, sep are used
      call readcontrol_dat(rscen,lenrscen,
     O                     doatdep,dops,dosep)  

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

******* POPULATE variables
      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)

      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call getRvars(              ! get active Rvars
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rname)

      call getloads(            ! get loads for active Rvars
     I              ioscen,lenioscen,nRvar,Rname,
     O              nloads,loadname)

      call delinfo(
     I             ioscen,lenioscen,nloads,loadname,
     O             ndel,delname,load2del)  ! number of delivery factors

      call getl2rlist(                                 ! get the exhaustive list of all land-ricer segments
     I                rseg,rscen,lenrscen,
     O                numsegs,l2r)

      IamRiver = rivercheck(rseg)

*********** start to calculate transport factor
      if (monthly) then
        call montf(
     I             rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I             doatdep,dops,dosep,sdate,edate,
     I             ndel,delname,nloads)
      end if

      if (annual) then
        call anntf(
     I             rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I             doatdep,dops,dosep,sdate,edate,
     I             ndel,delname,nloads)
      end if

      if (aveann) then
        call avetf(
     I             rseg,lenrseg,IamRiver,numsegs,l2r,rscen,lenrscen,
     I             doatdep,dops,dosep,year1,year2,
     I             ndel,delname,nloads)
      end if

      return

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

994   write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',rseg
      report(2) = 'in list'
      report(3) = ' '
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
