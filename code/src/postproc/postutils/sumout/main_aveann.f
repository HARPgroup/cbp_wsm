************************************************************************
** program to summarize output in the ./sumout/ diretory              **
**  output is initially by lrseg and land use or data type            **
**  this program takes the input of a river basin and summarizes by   **
**  land use type, by data type, by lseg, by river seg, and by basin  **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/rsegs.inc'
      include '../../../lib/inc/lsegs.inc'
      include '../../../lib/inc/masslinks.inc'
      include '../../../lib/inc/modules.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'

******** define load variables
      integer nloadmax             !
      parameter (nloadmax = 20)    ! maximum number of loads
      integer nloads               ! number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')

*********** state variables
      integer nstatemax,nstate,nstates
      parameter (nstatemax = 10) ! number of jurisdictions
      character*2 states(nstatemax)  ! state code (24,42,36, ect)

*********** segment variables
      integer numlrsegs,nlr
      character*13 allrsegs(maxrsegs)
      character*6 alllsegs(maxlsegs)

********* reading variables for loads
      real EOFload(nloadmax+1) 
      real EOFwwtp(nloadmax) 
      real EOFindus(nloadmax)
      real EOFcso(nloadmax)
      real EOFsep(nloadmax) 
      real EOFatdep(nloadmax) 

      real EOSload(nloadmax+1) 
      real EOSwwtp(nloadmax)
      real EOSindus(nloadmax)
      real EOScso(nloadmax)
      real EOSsep(nloadmax) 
      real EOSatdep(nloadmax) 

      real DELload(nloadmax+1) 
      real DELwwtp(nloadmax)
      real DELindus(nloadmax)
      real DELcso(nloadmax)
      real DELsep(nloadmax) 
      real DELatdep(nloadmax) 

********** accumulations of load
      real allEOF(nloadmax+1) 
      real allEOFrseg(maxrsegs,nloadmax+1) 
      real allEOFlseg(maxlsegs,nloadmax+1)
      real allEOFlrseg(maxrsegs*maxL2R,nloadmax+1) 
      real allEOFlu(nlu,nloadmax+1) 
      real allEOFstate(nstatemax,nloadmax+1) 
      real allEOFwwtp(nloadmax+1)
      real allEOFindus(nloadmax+1)
      real allEOFcso(nloadmax+1) 
      real allEOFsep(nloadmax+1) 
      real allEOFatdep(nloadmax+1) 
      real allEOFstatelu(nstatemax,nlu,nloadmax+1)
      real allEOFstatewwtp(nstatemax,nloadmax+1)
      real allEOFstateindus(nstatemax,nloadmax+1)
      real allEOFstatecso(nstatemax,nloadmax+1)
      real allEOFstatesep(nstatemax,nloadmax+1)
      real allEOFstateatdep(nstatemax,nloadmax+1)
      real acres

      real allEOS(nloadmax+1) 
      real allEOSrseg(maxrsegs,nloadmax+1) 
      real allEOSlseg(maxlsegs,nloadmax+1)
      real allEOSlrseg(maxrsegs*maxL2R,nloadmax+1) 
      real allEOSlu(nlu,nloadmax+1) 
      real allEOSstate(nstatemax,nloadmax+1) 
      real allEOSwwtp(nloadmax+1)
      real allEOSindus(nloadmax+1)
      real allEOScso(nloadmax+1) 
      real allEOSsep(nloadmax+1) 
      real allEOSatdep(nloadmax+1) 
      real allEOSstatelu(nstatemax,nlu,nloadmax+1)
      real allEOSstatewwtp(nstatemax,nloadmax+1)
      real allEOSstateindus(nstatemax,nloadmax+1)
      real allEOSstatecso(nstatemax,nloadmax+1)
      real allEOSstatesep(nstatemax,nloadmax+1)
      real allEOSstateatdep(nstatemax,nloadmax+1)

      real allDEL(nloadmax+1) 
      real allDELrseg(maxrsegs,nloadmax+1) 
      real allDELlseg(maxlsegs,nloadmax+1)
      real allDELlrseg(maxrsegs*maxL2R,nloadmax+1) 
      real allDELlu(nlu,nloadmax+1) 
      real allDELstate(nstatemax,nloadmax+1) 
      real allDELwwtp(nloadmax+1)
      real allDELindus(nloadmax+1)
      real allDELcso(nloadmax+1) 
      real allDELsep(nloadmax+1) 
      real allDELatdep(nloadmax+1) 
      real allDELstatelu(nstatemax,nlu,nloadmax+1)
      real allDELstatewwtp(nstatemax,nloadmax+1)
      real allDELstateindus(nstatemax,nloadmax+1)
      real allDELstatecso(nstatemax,nloadmax+1)
      real allDELstatesep(nstatemax,nloadmax+1)
      real allDELstateatdep(nstatemax,nloadmax+1)

********** indices and accounting variables
      integer nr,ns,nl,lu,nls,nlseg,nrseg,nlrseg,nload   ! indices

      integer numsegs ! number of lsegs associated with an rseg

      integer nlrsegs  ! total land river segs in basin
      character*13 lrrsegs(maxrsegs*maxL2R)
      character*6 lrlsegs(maxrsegs*maxL2R) 

      character*200 basin

      integer lenbasin

      logical found

      character*3 EOF,EOS,DEL
      data EOF,EOS,DEL /'eof','eos','del'/

      integer year1,year2
      character*4 cy1,cy2

      integer iEOF,iEOS,iDEL  ! input flags 
      logical doEOF,doEOS,doDEL  ! logical values
      data doEOF,doEOS,doDEL /.false.,.false.,.false./

      integer fnAllLoads   ! all data file
      parameter (fnAllLoads=14)

************ several different schemes for calculating Delivery
******** user specifies which one to use
      character*3 DFmethod(nloadmax)  ! possible values 'seg','bas','res'

************ END DECLARATIONS ******************************************

      read(*,*,err=997,end=998) rscen,basin,year1,year2,
     .                          iEOF,iEOS,iDEL

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      if (iEOF.ne.0) doEOF = .true.
      if (iEOS.ne.0) doEOS = .true.
      if (iDEL.ne.0) doDEL = .true.
      call lencl(basin,lenbasin)
      call lencl(rscen,lenrscen) 

      print*,'summarizing average annual output for scenario ',
     .       rscen(:lenrscen),' river basin ',basin(:lenbasin)

*********** open all data files to feed scenario builder postprocessor
      fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .       '/AllLoads_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .       rscen(:lenrscen)//'.csv'
      open(fnAllLoads,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

************* read in river segments
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)

      call masslinksmall(rscen,lenrscen,modules,nmod,
     O                   nRvar,Rname)

      call loadinfosmall(rscen,lenrscen,nRvar,Rname,
     O                   nloads,loadname)

*********** read in DF method for each load type
      call getDFmethod(rscen,lenrscen,nloads,loadname,
     O                 DFmethod)

      do nl = 1, nloads 
        call lowercase(DFmethod(nl))                  ! check for correct DFmethod
        if (DFmethod(nl).ne.'seg'.and.DFmethod(nl).ne.'bas'
     .                 .and.DFmethod(nl).ne.'res') go to 992

      end do
************ initialize variables
************ initialize variables
      acres = 0.0

      do nl = 1,nloadmax+1
        allEOFwwtp(nl) = 0.0
        allEOFindus(nl) = 0.0
        allEOFcso(nl) = 0.0
        allEOFsep(nl) = 0.0
        allEOFatdep(nl) = 0.0
        allEOF(nl) = 0.0

        allEOSwwtp(nl) = 0.0
        allEOSindus(nl) = 0.0
        allEOScso(nl) = 0.0
        allEOSsep(nl) = 0.0
        allEOSatdep(nl) = 0.0
        allEOS(nl) = 0.0

        allDELwwtp(nl) = 0.0
        allDELindus(nl) = 0.0
        allDELcso(nl) = 0.0
        allDELsep(nl) = 0.0
        allDELatdep(nl) = 0.0
        allDEL(nl) = 0.0

        do ns = 1,maxrsegs
          allEOFrseg(ns,nl) = 0.0
          allEOSrseg(ns,nl) = 0.0
          allDELrseg(ns,nl) = 0.0
        end do

        do ns = 1,maxlsegs
          allEOFlseg(ns,nl)= 0.0
          allEOSlseg(ns,nl)= 0.0
          allDELlseg(ns,nl)= 0.0
        end do

        do ns = 1,maxrsegs*maxL2R
          allEOFlrseg(ns,nl) = 0.0
          allEOSlrseg(ns,nl) = 0.0
          allDELlrseg(ns,nl) = 0.0
        end do

        do ns = 1,nlu
          allEOFlu(ns,nl) = 0.0
          allEOSlu(ns,nl) = 0.0
          allDELlu(ns,nl) = 0.0
        end do

        do ns = 1,nstatemax
          allEOFstate(ns,nl) = 0.0
          allEOSstate(ns,nl) = 0.0
          allDELstate(ns,nl) = 0.0
          do lu = 1,nlu
            allEOFstatelu(ns,lu,nl) = 0.0
            allEOSstatelu(ns,lu,nl) = 0.0
            allDELstatelu(ns,lu,nl) = 0.0
          end do
          allEOFstatewwtp(ns,nl) = 0.0
          allEOFstateindus(ns,nl) = 0.0
          allEOFstatecso(ns,nl) = 0.0
          allEOFstatesep(ns,nl) = 0.0
          allEOFstateatdep(ns,nl) = 0.0

          allEOSstatewwtp(ns,nl) = 0.0
          allEOSstateindus(ns,nl) = 0.0
          allEOSstatecso(ns,nl) = 0.0
          allEOSstatesep(ns,nl) = 0.0
          allEOSstateatdep(ns,nl) = 0.0

          allDELstatewwtp(ns,nl) = 0.0
          allDELstateindus(ns,nl) = 0.0
          allDELstatecso(ns,nl) = 0.0
          allDELstatesep(ns,nl) = 0.0
          allDELstateatdep(ns,nl) = 0.0
        end do

      end do

*********** populate variables by reading ASCII output
************ loop over river segs
      nlsegs = 0
      nstates = 0
      nlrsegs = 0
      do nrseg = 1,nrsegs
        call ttyput(rsegs(nrseg)//' ')

********** get the segment in this river
        call getl2rlist(                              ! get the exhaustive list of all land-river segments
     I                  rsegs(nrseg),rscen,lenrscen,
     O                  numsegs,l2r)

************** loop over land segs in river seg
        do ns = 1,numsegs

************ find index for land segment
          found = .false.
          do nls = 1,nlsegs
            if (l2r(ns).eq.lsegs(nls)) then
              nlseg = nls
              found = .true.
              exit
            end if
          end do
          if (.not.found) then
            nlsegs = nlsegs + 1
            lsegs(nlsegs) = l2r(ns)
            nlseg = nlsegs
          end if

************ define lrseg
          nlrsegs = nlrsegs + 1
          lrrsegs(nlrsegs) = rsegs(nrseg) 
          lrlsegs(nlrsegs) = l2r(ns)
          nlrseg = nlrsegs

************* find index for state
          found = .false.
          do nls = 1,nstates
            if (l2r(ns)(2:3).eq.states(nls)) then
              nstate = nls
              found = .true.
              exit
            end if
          end do
          if (.not.found) then
            nstates = nstates + 1
            states(nstates) = l2r(ns)(2:3)
            nstate = nstates
          end if

***************** EOF ***********************        
          if (doEOF) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOF,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOFload,acres)

              do nload = 1,nloads  ! add loads
                allEOF(nload) = allEOF(nload) + EOFload(nload)
                allEOFrseg(nrseg,nload) = allEOFrseg(nrseg,nload) 
     .                                  + EOFload(nload)
                allEOFlseg(nlseg,nload) = allEOFlseg(nlseg,nload) 
     .                                  + EOFload(nload)
                allEOFstate(nstate,nload) = allEOFstate(nstate,nload) 
     .                                    + EOFload(nload)
                allEOFlrseg(nlrseg,nload) = allEOFlrseg(nlrseg,nload) 
     .                                    + EOFload(nload)
                allEOFlu(nl,nload) = allEOFlu(nl,nload) + EOFload(nload)
                allEOFstatelu(nstate,nl,nload) = 
     .                   allEOFstatelu(nstate,nl,nload) + EOFload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),EOF,loadname(nload),
     .                                 EOFload(nload)
              end do

              allEOF(nloads+1) = allEOF(nloads+1) + acres
              allEOFrseg(nrseg,nloads+1) = allEOFrseg(nrseg,nloads+1) 
     .                                   + acres
              allEOFlseg(nlseg,nloads+1) = allEOFlseg(nlseg,nloads+1) 
     .                                   + acres
              allEOFstate(nstate,nloads+1)=allEOFstate(nstate,nloads+1) 
     .                                    + acres
              allEOFlrseg(nlrseg,nloads+1)=allEOFlrseg(nlrseg,nloads+1) 
     .                                    + acres
              allEOFlu(nl,nloads+1) = allEOFlu(nl,nloads+1) + acres
              allEOFstatelu(nstate,nl,nloads+1) = 
     .                    allEOFstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),EOF,'acres',
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOF,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EOFwwtp,EOFindus,EOFcso,EOFsep,EOFatdep,acres)

            do nload = 1,nloads
              allEOF(nload) = allEOF(nload)
     .                      + EOFwwtp(nload)
     .                      + EOFindus(nload)
     .                      + EOFcso(nload)
     .                      + EOFsep(nload)
     .                      + EOFatdep(nload)
              allEOFrseg(nrseg,nload) = allEOFrseg(nrseg,nload) 
     .                                + EOFwwtp(nload)
     .                                + EOFindus(nload)
     .                                + EOFcso(nload)
     .                                + EOFsep(nload)
     .                                + EOFatdep(nload)
              allEOFlseg(nlseg,nload) = allEOFlseg(nlseg,nload) 
     .                                + EOFwwtp(nload)
     .                                + EOFindus(nload)
     .                                + EOFcso(nload)
     .                                + EOFsep(nload)
     .                                + EOFatdep(nload)
              allEOFstate(nstate,nload) = allEOFstate(nstate,nload) 
     .                                  + EOFwwtp(nload)
     .                                  + EOFindus(nload)
     .                                  + EOFcso(nload)
     .                                  + EOFsep(nload)
     .                                  + EOFatdep(nload)
              allEOFlrseg(nlrseg,nload) = allEOFlrseg(nlrseg,nload) 
     .                                  + EOFwwtp(nload)
     .                                  + EOFindus(nload)
     .                                  + EOFcso(nload)
     .                                  + EOFsep(nload)
     .                                  + EOFatdep(nload)
              allEOFwwtp(nload) = allEOFwwtp(nload) + EOFwwtp(nload)
              allEOFindus(nload) = allEOFindus(nload) + EOFindus(nload)
              allEOFcso(nload) = allEOFcso(nload) + EOFcso(nload)
              allEOFsep(nload) = allEOFsep(nload) + EOFsep(nload)
              allEOFatdep(nload) = allEOFatdep(nload) + EOFatdep(nload)
              allEOFstatewwtp(nstate,nload) = 
     .              allEOFstatewwtp(nstate,nload) + EOFwwtp(nload)
              allEOFstateindus(nstate,nload) =
     .              allEOFstateindus(nstate,nload) + EOFindus(nload)
              allEOFstatecso(nstate,nload) =
     .              allEOFstatecso(nstate,nload) + EOFcso(nload)
              allEOFstatesep(nstate,nload) = 
     .              allEOFstatesep(nstate,nload) + EOFsep(nload)
              allEOFstateatdep(nstate,nload) = 
     .              allEOFstateatdep(nstate,nload) + EOFatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               EOF,loadname(nload),EOFwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               EOF,loadname(nload),EOFindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               EOF,loadname(nload),EOFcso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               EOF,loadname(nload),EOFsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOF,loadname(nload),EOFatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),'atdep',
     .                             EOF,'acres',acres
            allEOFatdep(nloads+1) = allEOFatdep(nloads+1) + acres
            allEOFstateatdep(nstate,nloads+1) =
     .              allEOFstateatdep(nstate,nloads+1) + acres
          end if

***************** EOS ***********************
          if (doEOS) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOS,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOSload,acres)

              do nload = 1,nloads  ! add loads
                allEOS(nload) = allEOS(nload) + EOSload(nload)
                allEOSrseg(nrseg,nload) = allEOSrseg(nrseg,nload) 
     .                                  + EOSload(nload)
                allEOSlseg(nlseg,nload) = allEOSlseg(nlseg,nload) 
     .                                  + EOSload(nload)
                allEOSstate(nstate,nload) = allEOSstate(nstate,nload) 
     .                                    + EOSload(nload)
                allEOSlrseg(nlrseg,nload) = allEOSlrseg(nlrseg,nload) 
     .                                    + EOSload(nload)
                allEOSlu(nl,nload) = allEOSlu(nl,nload) + EOSload(nload)
                allEOSstatelu(nstate,nl,nload) = 
     .                   allEOSstatelu(nstate,nl,nload) + EOSload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),EOS,loadname(nload),
     .                                 EOSload(nload)
              end do

              allEOS(nloads+1) = allEOS(nloads+1) + acres
              allEOSrseg(nrseg,nloads+1) = allEOSrseg(nrseg,nloads+1) 
     .                                   + acres
              allEOSlseg(nlseg,nloads+1) = allEOSlseg(nlseg,nloads+1) 
     .                                   + acres
              allEOSstate(nstate,nloads+1)=allEOSstate(nstate,nloads+1) 
     .                                    + acres
              allEOSlrseg(nlrseg,nloads+1)=allEOSlrseg(nlrseg,nloads+1) 
     .                                    + acres
              allEOSlu(nl,nloads+1) = allEOSlu(nl,nloads+1) + acres
              allEOSstatelu(nstate,nl,nloads+1) = 
     .                    allEOSstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),EOS,'acres',
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOS,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EOSwwtp,EOSindus,EOScso,EOSsep,EOSatdep,acres)

            do nload = 1,nloads
              allEOS(nload) = allEOS(nload)
     .                      + EOSwwtp(nload)
     .                      + EOSindus(nload)
     .                      + EOScso(nload)
     .                      + EOSsep(nload)
     .                      + EOSatdep(nload)
              allEOSrseg(nrseg,nload) = allEOSrseg(nrseg,nload)
     .                                + EOSwwtp(nload)
     .                                + EOSindus(nload)
     .                                + EOScso(nload)
     .                                + EOSsep(nload)
     .                                + EOSatdep(nload)
              allEOSlseg(nlseg,nload) = allEOSlseg(nlseg,nload)
     .                                + EOSwwtp(nload)
     .                                + EOSindus(nload) 
     .                                + EOScso(nload)
     .                                + EOSsep(nload)
     .                                + EOSatdep(nload)
              allEOSstate(nstate,nload) = allEOSstate(nstate,nload)
     .                                  + EOSwwtp(nload)
     .                                  + EOSindus(nload) 
     .                                  + EOScso(nload)
     .                                  + EOSsep(nload)
     .                                  + EOSatdep(nload)
              allEOSlrseg(nlrseg,nload) = allEOSlrseg(nlrseg,nload)
     .                                  + EOSwwtp(nload)
     .                                  + EOSindus(nload) 
     .                                  + EOScso(nload)
     .                                  + EOSsep(nload)
     .                                  + EOSatdep(nload)
              allEOSwwtp(nload) = allEOSwwtp(nload) + EOSwwtp(nload)
              allEOSindus(nload) = allEOSindus(nload) + EOSindus(nload)
              allEOScso(nload) = allEOScso(nload) + EOScso(nload)
              allEOSsep(nload) = allEOSsep(nload) + EOSsep(nload)
              allEOSatdep(nload) = allEOSatdep(nload) + EOSatdep(nload)
              allEOSstatewwtp(nstate,nload) = 
     .              allEOSstatewwtp(nstate,nload) + EOSwwtp(nload)
              allEOSstateindus(nstate,nload) =
     .              allEOSstateindus(nstate,nload) + EOSindus(nload)
              allEOSstatecso(nstate,nload) =
     .              allEOSstatecso(nstate,nload) + EOScso(nload)
              allEOSstatesep(nstate,nload) = 
     .              allEOSstatesep(nstate,nload) + EOSsep(nload)
              allEOSstateatdep(nstate,nload) = 
     .              allEOSstateatdep(nstate,nload) + EOSatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               EOS,loadname(nload),EOSwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               EOS,loadname(nload),EOSindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               EOS,loadname(nload),EOScso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               EOS,loadname(nload),EOSsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOS,loadname(nload),EOSatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),'atdep',
     .                             EOS,'acres',acres
            allEOSatdep(nloads+1) = allEOSatdep(nloads+1) + acres
            allEOSstateatdep(nstate,nloads+1) =
     .              allEOSstateatdep(nstate,nloads+1) + acres
          end if

***************** DEL ***********************
          if (doDEL) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),DEL,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          DELload,acres)

              do nload = 1,nloads  ! add loads
                allDEL(nload) = allDEL(nload) + DELload(nload)
                allDELrseg(nrseg,nload) = allDELrseg(nrseg,nload) 
     .                                  + DELload(nload)
                allDELlseg(nlseg,nload) = allDELlseg(nlseg,nload) 
     .                                  + DELload(nload)
                allDELstate(nstate,nload) = allDELstate(nstate,nload) 
     .                                  + DELload(nload)
                allDELlrseg(nlrseg,nload) = allDELlrseg(nlrseg,nload) 
     .                                    + DELload(nload)
                allDELlu(nl,nload) = allDELlu(nl,nload) + DELload(nload)
                allDELstatelu(nstate,nl,nload) = 
     .                   allDELstatelu(nstate,nl,nload) + DELload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),DEL,loadname(nload),
     .                                 DELload(nload)
              end do

              allDEL(nloads+1) = allDEL(nloads+1) + acres
              allDELrseg(nrseg,nloads+1) = allDELrseg(nrseg,nloads+1) 
     .                                   + acres
              allDELlseg(nlseg,nloads+1) = allDELlseg(nlseg,nloads+1) 
     .                                   + acres
              allDELstate(nstate,nloads+1)=allDELstate(nstate,nloads+1) 
     .                                    + acres
              allDELlrseg(nlrseg,nloads+1)=allDELlrseg(nlrseg,nloads+1) 
     .                                    + acres
              allDELlu(nl,nloads+1) = allDELlu(nl,nloads+1) + acres
              allDELstatelu(nstate,nl,nloads+1) = 
     .                    allDELstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),DEL,'acres',
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         DEL,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    DELwwtp,DELindus,DELcso,DELsep,DELatdep,acres)

            do nload = 1,nloads
              allDEL(nload) = allDEL(nload)
     .                      + DELwwtp(nload)
     .                      + DELindus(nload)
     .                      + DELcso(nload)
     .                      + DELsep(nload)
     .                      + DELatdep(nload)
              allDELrseg(nrseg,nload) = allDELrseg(nrseg,nload)
     .                                + DELwwtp(nload)
     .                                + DELindus(nload) 
     .                                + DELcso(nload)
     .                                + DELsep(nload)
     .                                + DELatdep(nload)
              allDELlseg(nlseg,nload) = allDELlseg(nlseg,nload)
     .                                + DELwwtp(nload)
     .                                + DELindus(nload) 
     .                                + DELcso(nload)
     .                                + DELsep(nload)
     .                                + DELatdep(nload)
              allDELstate(nstate,nload) = allDELstate(nstate,nload)
     .                                  + DELwwtp(nload)
     .                                  + DELindus(nload) 
     .                                  + DELcso(nload)
     .                                  + DELsep(nload)
     .                                  + DELatdep(nload)
              allDELlrseg(nlrseg,nload) = allDELlrseg(nlrseg,nload) 
     .                                  + DELwwtp(nload)
     .                                  + DELindus(nload)
     .                                  + DELcso(nload)
     .                                  + DELsep(nload)
     .                                  + DELatdep(nload)
              allDELwwtp(nload) = allDELwwtp(nload) + DELwwtp(nload)
              allDELindus(nload) = allDELindus(nload) + DELindus(nload)
              allDELcso(nload) = allDELcso(nload) + DELcso(nload)
              allDELsep(nload) = allDELsep(nload) + DELsep(nload)
              allDELatdep(nload) = allDELatdep(nload) + DELatdep(nload)
              allDELstatewwtp(nstate,nload) =
     .              allDELstatewwtp(nstate,nload) + DELwwtp(nload)
              allDELstateindus(nstate,nload) =
     .              allDELstateindus(nstate,nload) + DELindus(nload)
              allDELstatecso(nstate,nload) = 
     .              allDELstatecso(nstate,nload) + DELcso(nload)
              allDELstatesep(nstate,nload) = 
     .              allDELstatesep(nstate,nload) + DELsep(nload)
              allDELstateatdep(nstate,nload) = 
     .              allDELstateatdep(nstate,nload) + DELatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               DEL,loadname(nload),DELwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               DEL,loadname(nload),DELindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               DEL,loadname(nload),DELcso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               DEL,loadname(nload),DELsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               DEL,loadname(nload),DELatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),'atdep',
     .                             DEL,'acres',acres
            allDELatdep(nloads+1) = allDELatdep(nloads+1) + acres
            allDELstateatdep(nstate,nloads+1) =
     .              allDELstateatdep(nstate,nloads+1) + acres
          end if

        end do   ! loop over lsegs in rseg
      end do   ! loop over rsegs in basin

      print*,' '
      close (fnAllLoads)
      print*,fnam(:78)
           
****************** write output ****************************************
************ write EOF output
      if (doEOF) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_EOF.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951)'specifier',
     .                 (',',loadname(nload),nload=1,nloads),',','acre'

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allEOFlu(nl,nload),nload=1,nloads+1)
        end do
        write(11,1234,err=951) wwtp,
     .                         (',',allEOFwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allEOFindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allEOFcso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allEOFsep(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allEOFatdep(nload),nload=1,nloads+1)
  
        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allEOFrseg(nrseg,nload),nload=1,nloads+1)
        end do

        do nlseg = 1,nlsegs
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allEOFlseg(nlseg,nload),nload=1,nloads+1)
        end do

        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allEOFlrseg(nlrseg,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allEOFstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allEOFstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allEOFstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allEOFstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allEOFstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allEOFstateatdep(nstate,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allEOFstate(nstate,nload),nload=1,nloads+1)
        end do

        write(11,1234,err=951) 'total',
     .                         (',',allEOF(nload),nload=1,nloads+1)

        close(11)

      end if

************ write EOS output
      if (doEOS) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_EOS.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                          ',','acre'

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allEOSlu(nl,nload),nload=1,nloads+1)
        end do
        write(11,1234,err=951) wwtp,
     .                         (',',allEOSwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allEOSindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allEOScso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allEOSsep(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allEOSatdep(nload),nload=1,nloads+1)
  
        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allEOSrseg(nrseg,nload),nload=1,nloads+1)
        end do

        do nlseg = 1,nlsegs
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allEOSlseg(nlseg,nload),nload=1,nloads+1)
        end do

        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allEOSlrseg(nlrseg,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allEOSstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allEOSstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allEOSstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allEOSstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allEOSstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allEOSstateatdep(nstate,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allEOSstate(nstate,nload),nload=1,nloads+1)
        end do

        write(11,1234,err=951) 'total',
     .                         (',',allEOS(nload),nload=1,nloads+1)

        close(11)

      end if

************ write DEL output
      if (doDEL) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_DEL.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                           ',','acre'

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allDELlu(nl,nload),nload=1,nloads+1)
        end do
        write(11,1234,err=951) wwtp,
     .                         (',',allDELwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allDELindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allDELcso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allDELsep(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allDELatdep(nload),nload=1,nloads+1)
  
        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allDELrseg(nrseg,nload),nload=1,nloads+1)
        end do

        do nlseg = 1,nlsegs
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allDELlseg(nlseg,nload),nload=1,nloads+1)
        end do

        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allDELlrseg(nlrseg,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allDELstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allDELstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allDELstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allDELstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allDELstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allDELstateatdep(nstate,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allDELstate(nstate,nload),nload=1,nloads+1)
        end do

        write(11,1234,err=951) 
     .        'total',(',',allDEL(nload),nload=1,nloads+1)

        close(11)

      end if

      stop

1232  format(A6,',',A13,',',A6,',',A3,',',A5,',',E14.7)
1233  format(a,20(a1,a4))
1234  format(a,20(a1,e14.7))

************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Delivery Factor method must be'
      report(2) = 'res - reservoir, seg - segment, bas - basin'
      report(3) = 'method read was '//DFmethod(nl)
      go to 999

997   report(1) = 'Error initializing sumout program'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and calling script'
      go to 999

998   report(1) = 'Error initializing land postprocessor'
      report(2) = 'not enough input data in calling script'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


