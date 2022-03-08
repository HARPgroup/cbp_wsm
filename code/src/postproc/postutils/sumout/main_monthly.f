************************************************************************
** mini main program to pass years and other inputs                   **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      character*200 basin
      integer year1,year2
      integer iEOF,iEOS,iDEL  ! input flags

      read(*,*,err=997,end=998) rscen,basin,year1,year2,
     .                          iEOF,iEOS,iDEL
      call sumout(rscen,basin,year1,year2,iEOF,iEOS,iDEL)

      stop

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

************************************************************************
** program to summarize output in the ./sumout/ diretory              **
**  output is initially by lrseg and land use or data type            **
**  this program takes the input of a river basin and summarizes by   **
**  land use type, by data type, by lseg, by river seg, and by basin  **
************************************************************************
      subroutine sumout(rscen,basin,year1,year2,iEOF,iEOS,iDEL)
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/rsegs.inc'
      include '../../../lib/inc/lsegs.inc'
      include '../../../lib/inc/masslinks.inc'
      include '../../../lib/inc/modules.inc'
      include '../../../lib/inc/land_use.inc'
      include '../../../lib/inc/ps_septic_atdep.inc'

*********** passed parameters
      integer year1,year2
      integer nmonths
      parameter (nmonths=12)

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
      real EOFload(nloadmax+1,year1:year2,nmonths) 
      real EOFwwtp(nloadmax,year1:year2,nmonths) 
      real EOFindus(nloadmax,year1:year2,nmonths)
      real EOFcso(nloadmax,year1:year2,nmonths)
      real EOFsep(nloadmax,year1:year2,nmonths) 
      real EOFatdep(nloadmax,year1:year2,nmonths) 

      real EOSload(nloadmax+1,year1:year2,nmonths) 
      real EOSwwtp(nloadmax,year1:year2,nmonths)
      real EOSindus(nloadmax,year1:year2,nmonths)
      real EOScso(nloadmax,year1:year2,nmonths) 
      real EOSsep(nloadmax,year1:year2,nmonths) 
      real EOSatdep(nloadmax,year1:year2,nmonths) 

      real DELload(nloadmax+1,year1:year2,nmonths) 
      real DELwwtp(nloadmax,year1:year2,nmonths) 
      real DELindus(nloadmax,year1:year2,nmonths)
      real DELcso(nloadmax,year1:year2,nmonths)
      real DELsep(nloadmax,year1:year2,nmonths) 
      real DELatdep(nloadmax,year1:year2,nmonths) 

********** accumulations of load
********* lrseg, lseg, and rseg cause seg faults
      real allEOF(nloadmax+1,year1:year2,nmonths) 
Crseg      real allEOFrseg(maxrsegs,nloadmax+1,year1:year2,nmonths) 
Clseg      real allEOFlseg(maxlsegs,nloadmax+1,year1:year2,nmonths)
Clrseg      real allEOFlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2,nmonths) 
      real allEOFlu(nlu,nloadmax+1,year1:year2,nmonths) 
      real allEOFstate(nstatemax,nloadmax+1,year1:year2,nmonths) 
      real allEOFwwtp(nloadmax+1,year1:year2,nmonths) 
      real allEOFindus(nloadmax+1,year1:year2,nmonths)
      real allEOFcso(nloadmax+1,year1:year2,nmonths)
      real allEOFsep(nloadmax+1,year1:year2,nmonths) 
      real allEOFatdep(nloadmax+1,year1:year2,nmonths) 
      real acres(year1:year2,nmonths)

      real allEOS(nloadmax+1,year1:year2,nmonths) 
Crseg      real allEOSrseg(maxrsegs,nloadmax+1,year1:year2,nmonths) 
Clseg      real allEOSlseg(maxlsegs,nloadmax+1,year1:year2,nmonths)
Clrseg      real allEOSlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2,nmonths) 
      real allEOSlu(nlu,nloadmax+1,year1:year2,nmonths) 
      real allEOSstate(nstatemax,nloadmax+1,year1:year2,nmonths) 
      real allEOSwwtp(nloadmax+1,year1:year2,nmonths)
      real allEOSindus(nloadmax+1,year1:year2,nmonths)
      real allEOScso(nloadmax+1,year1:year2,nmonths) 
      real allEOSsep(nloadmax+1,year1:year2,nmonths) 
      real allEOSatdep(nloadmax+1,year1:year2,nmonths) 

      real allDEL(nloadmax+1,year1:year2,nmonths) 
Crseg      real allDELrseg(maxrsegs,nloadmax+1,year1:year2,nmonths) 
Clseg      real allDELlseg(maxlsegs,nloadmax+1,year1:year2,nmonths)
Clrseg      real allDELlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2,nmonths) 
      real allDELlu(nlu,nloadmax+1,year1:year2,nmonths) 
      real allDELstate(nstatemax,nloadmax+1,year1:year2,nmonths) 
      real allDELwwtp(nloadmax+1,year1:year2,nmonths) 
      real allDELindus(nloadmax+1,year1:year2,nmonths)
      real allDELcso(nloadmax+1,year1:year2,nmonths)
      real allDELsep(nloadmax+1,year1:year2,nmonths) 
      real allDELatdep(nloadmax+1,year1:year2,nmonths) 

********** indices and accounting variables
      integer nr,ns,nl,nls,nlseg,nrseg,nlrseg,nload,ny,nm ! indices

      integer numsegs ! number of lsegs associated with an rseg

Clrseg      integer nlrsegs  ! total land river segs in basin
Clrseg      character*13 lrrsegs(maxrsegs*maxL2R)
Clrseg      character*6 lrlsegs(maxrsegs*maxL2R) 

      character*(*) basin
      integer lenbasin

      logical found

      character*3 EOF,EOS,DEL
      data EOF,EOS,DEL /'eof','eos','del'/

      character*4 cy1,cy2

      integer iEOF,iEOS,iDEL  ! input flags 
      logical doEOF,doEOS,doDEL  ! logical values
      data doEOF,doEOS,doDEL /.false.,.false.,.false./

      character*300 header

************ several different schemes for calculating Delivery
******** user specifies which one to use
      character*3 DFmethod(nloadmax)  ! poss values 'seg','bas','res'

************ END DECLARATIONS ******************************************

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      if (iEOF.ne.0) doEOF = .true.
      if (iEOS.ne.0) doEOS = .true.
      if (iDEL.ne.0) doDEL = .true.
      call lencl(basin,lenbasin)
      call lencl(rscen,lenrscen) 

      print*,'summarizing monthly output for scenario ',
     .       rscen(:lenrscen),' river basin ',basin(:lenbasin)

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
        call lowercase(DFmethod(nl)) ! check for correct DFmethod
        if (DFmethod(nl).ne.'seg'.and.DFmethod(nl).ne.'bas'
     .                 .and.DFmethod(nl).ne.'res') go to 992
      end do

************ initialize variables
      do nm = 1,nmonths
        do ny = year1,year2

          acres(ny,nm) = 0.0

          do nl = 1,nloadmax+1
            allEOFwwtp(nl,ny,nm) = 0.0
            allEOFindus(nl,ny,nm) = 0.0
            allEOFcso(nl,ny,nm) = 0.0
            allEOFsep(nl,ny,nm) = 0.0
            allEOFatdep(nl,ny,nm) = 0.0
            allEOF(nl,ny,nm) = 0.0

            allEOSwwtp(nl,ny,nm) = 0.0
            allEOSindus(nl,ny,nm) = 0.0
            allEOScso(nl,ny,nm) = 0.0
            allEOSsep(nl,ny,nm) = 0.0
            allEOSatdep(nl,ny,nm) = 0.0
            allEOS(nl,ny,nm) = 0.0

            allDELwwtp(nl,ny,nm) = 0.0
            allDELindus(nl,ny,nm) = 0.0
            allDELcso(nl,ny,nm) = 0.0
            allDELsep(nl,ny,nm) = 0.0
            allDELatdep(nl,ny,nm) = 0.0
            allDEL(nl,ny,nm) = 0.0

Crseg            do ns = 1,maxrsegs
Crseg              allEOFrseg(ns,nl,ny,nm) = 0.0
Crseg              allEOSrseg(ns,nl,ny,nm) = 0.0
Crseg              allDELrseg(ns,nl,ny,nm) = 0.0
Crseg            end do

Clseg            do ns = 1,maxlsegs
Clseg              allEOFlseg(ns,nl,ny,nm)= 0.0
Clseg              allEOSlseg(ns,nl,ny,nm)= 0.0
Clseg              allDELlseg(ns,nl,ny,nm)= 0.0
Clseg            end do

Clrseg            do ns = 1,maxrsegs*maxL2R
Clrseg              allEOFlrseg(ns,nl,ny,nm) = 0.0
Clrseg              allEOSlrseg(ns,nl,ny,nm) = 0.0
Clrseg              allDELlrseg(ns,nl,ny,nm) = 0.0
Clrseg            end do

            do ns = 1,nlu
              allEOFlu(ns,nl,ny,nm) = 0.0
              allEOSlu(ns,nl,ny,nm) = 0.0
              allDELlu(ns,nl,ny,nm) = 0.0
            end do

            do ns = 1,nstatemax
              allEOFstate(ns,nl,ny,nm) = 0.0
              allEOSstate(ns,nl,ny,nm) = 0.0
              allDELstate(ns,nl,ny,nm) = 0.0
            end do

          end do
        end do
      end do


*********** populate variables by reading ASCII output
************ loop over river segs
      nlsegs = 0
      nstates = 0
Clrseg      nlrsegs = 0
      do nrseg = 1,nrsegs
        call ttyput(rsegs(nrseg)//' ')

********** get the segment in this river
        call getl2rlist(                              ! get the exhaustive list of all land-ricer segments
     I                  rseg,rscen,lenrscen,
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

Clrseg************ define lrseg
Clrseg          nlrsegs = nlrsegs + 1
Clrseg          lrrsegs(nlrsegs) = rsegs(nrseg) 
Clrseg          lrlsegs(nlrsegs) = l2r(ns)
Clrseg          nlrseg = nlrsegs

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
        
          if (doEOF) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOmonthly(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOF,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOFload,acres)

              do nm = 1,nmonths  ! add loads
                do ny = year1,year2
                  do nload = 1,nloads
                    allEOF(nload,ny,nm) = allEOF(nload,ny,nm) 
     .                           + EOFload(nload,ny,nm)
Crseg                    allEOFrseg(nrseg,nload,ny,nm) = 
Crseg     .                           allEOFrseg(nrseg,nload,ny,nm) 
Crseg     .                           + EOFload(nload,ny,nm)
Clseg                    allEOFlseg(nlseg,nload,ny,nm) = 
Clseg     .                           allEOFlseg(nlseg,nload,ny,nm) 
Clseg     .                           + EOFload(nload,ny,nm)
                    allEOFstate(nstate,nload,ny,nm) = 
     .                           allEOFstate(nstate,nload,ny,nm) 
     .                           + EOFload(nload,ny,nm)
Clrseg                    allEOFlrseg(nlrseg,nload,ny,nm) = 
Clrseg     .                           allEOFlrseg(nlrseg,nload,ny,nm) 
Clrseg     .                           + EOFload(nload,ny,nm)
                    allEOFlu(nl,nload,ny,nm) = allEOFlu(nl,nload,ny,nm) 
     .                           + EOFload(nload,ny,nm)
                  end do
    
                  allEOF(nloads+1,ny,nm) = allEOF(nloads+1,ny,nm) 
     .                                   + acres(ny,nm)
Crseg                  allEOFrseg(nrseg,nloads+1,ny,nm) = 
Crseg     .                       allEOFrseg(nrseg,nloads+1,ny,nm) 
Crseg     .                                   + acres(ny,nm)
Clseg                  allEOFlseg(nlseg,nloads+1,ny,nm) = 
Clseg     .                       allEOFlseg(nlseg,nloads+1,ny,nm) 
Clseg     .                                   + acres(ny,nm)
                  allEOFstate(nstate,nloads+1,ny,nm)=
     .                       allEOFstate(nstate,nloads+1,ny,nm) 
     .                                   + acres(ny,nm)
Clrseg                  allEOFlrseg(nlrseg,nloads+1,ny,nm)=
Clrseg     .                       allEOFlrseg(nlrseg,nloads+1,ny,nm) 
Clrseg     .                                   + acres(ny,nm)
                  allEOFlu(nl,nloads+1,ny,nm) = 
     .                       allEOFlu(nl,nloads+1,ny,nm) + acres(ny,nm)
                end do
              end do
            end do

            call readDATmonthly(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOF,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         EOFwwtp,EOFindus,EOFcso,EOFsep,EOFatdep)

            do nm = 1,nmonths  ! add loads
              do ny = year1,year2
                do nload = 1,nloads
                  allEOF(nload,ny,nm) = allEOF(nload,ny,nm)
     .                                + EOFwwtp(nload,ny,nm)
     .                                + EOFindus(nload,ny,nm)
     .                                + EOFcso(nload,ny,nm)
     .                                + EOFsep(nload,ny,nm)
     .                                + EOFatdep(nload,ny,nm)
Crseg                  allEOFrseg(nrseg,nload,ny,nm) = 
Crseg     .                                  allEOFrseg(nrseg,nload,ny,nm) 
Crseg     .                                + EOFps(nload,ny,nm)
Crseg     .                                + EOFsep(nload,ny,nm)
Crseg     .                                + EOFatdep(nload,ny,nm)
Clseg                  allEOFlseg(nlseg,nload,ny,nm) = 
Clseg     .                                  allEOFlseg(nlseg,nload,ny,nm) 
Clseg     .                                + EOFps(nload,ny,nm)
Clseg     .                                + EOFsep(nload,ny,nm)
Clseg     .                                + EOFatdep(nload,ny,nm)
                  allEOFstate(nstate,nload,ny,nm) = 
     .                                  allEOFstate(nstate,nload,ny,nm) 
     .                                + EOFwwtp(nload,ny,nm)
     .                                + EOFindus(nload,ny,nm)
     .                                + EOFcso(nload,ny,nm)
     .                                + EOFsep(nload,ny,nm)
     .                                + EOFatdep(nload,ny,nm)
Clrseg                  allEOFlrseg(nlrseg,nload,ny,nm) = 
Clrseg     .                                  allEOFlrseg(nlrseg,nload,ny,nm) 
Clrseg     .                                + EOFps(nload,ny,nm)
Clrseg     .                                + EOFsep(nload,ny,nm)
Clrseg     .                                + EOFatdep(nload,ny,nm)
                  allEOFwwtp(nload,ny,nm) = allEOFwwtp(nload,ny,nm) 
     .                                  + EOFwwtp(nload,ny,nm)
                  allEOFindus(nload,ny,nm) = allEOFindus(nload,ny,nm)
     .                                  + EOFindus(nload,ny,nm)
                  allEOFcso(nload,ny,nm) = allEOFcso(nload,ny,nm)
     .                                  + EOFcso(nload,ny,nm)
                  allEOFsep(nload,ny,nm) = allEOFsep(nload,ny,nm) 
     .                                   + EOFsep(nload,ny,nm)
                  allEOFatdep(nload,ny,nm) = allEOFatdep(nload,ny,nm) 
     .                                      + EOFatdep(nload,ny,nm)
                end do
              end do
            end do

          end if

          if (doEOS) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOmonthly(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOS,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOSload,acres)

              do nm = 1,nmonths  ! add loads
                do ny = year1,year2
                  do nload = 1,nloads
                    allEOS(nload,ny,nm) = allEOS(nload,ny,nm)
     .                           + EOSload(nload,ny,nm)
Crseg                    allEOSrseg(nrseg,nload,ny,nm) =
Crseg     .                           allEOSrseg(nrseg,nload,ny,nm)
Crseg     .                           + EOSload(nload,ny,nm)
Clseg                    allEOSlseg(nlseg,nload,ny,nm) =
Clseg     .                           allEOSlseg(nlseg,nload,ny,nm)
Clseg     .                           + EOSload(nload,ny,nm)
                    allEOSstate(nstate,nload,ny,nm) =
     .                           allEOSstate(nstate,nload,ny,nm)
     .                           + EOSload(nload,ny,nm)
Clrseg                    allEOSlrseg(nlrseg,nload,ny,nm) =
Clrseg     .                           allEOSlrseg(nlrseg,nload,ny,nm)
Clrseg     .                           + EOSload(nload,ny,nm)
                    allEOSlu(nl,nload,ny,nm) = allEOSlu(nl,nload,ny,nm)
     .                           + EOSload(nload,ny,nm)
                  end do

                  allEOS(nloads+1,ny,nm) = allEOS(nloads+1,ny,nm) 
     .                                   + acres(ny,nm)
Crseg                  allEOSrseg(nrseg,nloads+1,ny,nm) =
Crseg     .                       allEOSrseg(nrseg,nloads+1,ny,nm) 
Crseg     .                                   + acres(ny,nm)
Clseg                  allEOSlseg(nlseg,nloads+1,ny,nm) =
Clseg     .                       allEOSlseg(nlseg,nloads+1,ny,nm) 
Clseg     .                                   + acres(ny,nm)
                  allEOSstate(nstate,nloads+1,ny,nm)=
     .                       allEOSstate(nstate,nloads+1,ny,nm) 
     .                                   + acres(ny,nm)
Clrseg                  allEOSlrseg(nlrseg,nloads+1,ny,nm)=
Clrseg     .                       allEOSlrseg(nlrseg,nloads+1,ny,nm) 
Clrseg     .                                   + acres(ny,nm)
                  allEOSlu(nl,nloads+1,ny,nm) =
     .                       allEOSlu(nl,nloads+1,ny,nm) + acres(ny,nm)
                end do
              end do
            end do

            call readDATmonthly(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOS,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         EOSwwtp,EOSindus,EOScso,EOSsep,EOSatdep)

            do nm = 1,nmonths  ! add loads
              do ny = year1,year2
                do nload = 1,nloads
                  allEOS(nload,ny,nm) = allEOS(nload,ny,nm)
     .                                + EOSwwtp(nload,ny,nm)
     .                                + EOSindus(nload,ny,nm)
     .                                + EOScso(nload,ny,nm)
     .                                + EOSsep(nload,ny,nm)
     .                                + EOSatdep(nload,ny,nm)
Crseg                  allEOSrseg(nrseg,nload,ny,nm) =
Crseg     .                                  allEOSrseg(nrseg,nload,ny,nm)
Crseg     .                                + EOSps(nload,ny,nm)
Crseg     .                                + EOSsep(nload,ny,nm)
Crseg     .                                + EOSatdep(nload,ny,nm)
Clseg                  allEOSlseg(nlseg,nload,ny,nm) =
Clseg     .                                  allEOSlseg(nlseg,nload,ny,nm)
Clseg     .                                + EOSps(nload,ny,nm)
Clseg     .                                + EOSsep(nload,ny,nm)
Clseg     .                                + EOSatdep(nload,ny,nm)
                  allEOSstate(nstate,nload,ny,nm) =
     .                                  allEOSstate(nstate,nload,ny,nm)
     .                                + EOSwwtp(nload,ny,nm)
     .                                + EOSindus(nload,ny,nm)
     .                                + EOScso(nload,ny,nm)
     .                                + EOSsep(nload,ny,nm)
     .                                + EOSatdep(nload,ny,nm)
Clrseg                  allEOSlrseg(nlrseg,nload,ny,nm) =
Clrseg     .                                  allEOSlrseg(nlrseg,nload,ny,nm)
Clrseg     .                                + EOSps(nload,ny,nm)
Clrseg     .                                + EOSsep(nload,ny,nm)
Clrseg     .                                + EOSatdep(nload,ny,nm)
                  allEOSwwtp(nload,ny,nm) = allEOSwwtp(nload,ny,nm)
     .                                  + EOSwwtp(nload,ny,nm)
                  allEOSindus(nload,ny,nm) = allEOSindus(nload,ny,nm)
     .                                  + EOSindus(nload,ny,nm)
                  allEOScso(nload,ny,nm) = allEOScso(nload,ny,nm)
     .                                  + EOScso(nload,ny,nm)
                  allEOSsep(nload,ny,nm) = allEOSsep(nload,ny,nm)
     .                                   + EOSsep(nload,ny,nm)
                  allEOSatdep(nload,ny,nm) = allEOSatdep(nload,ny,nm)
     .                                      + EOSatdep(nload,ny,nm)
                end do
              end do
            end do

          end if

          if (doDEL) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOmonthly(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),DEL,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          DELload,acres)

              do nm = 1,nmonths  ! add loads
                do ny = year1,year2
                  do nload = 1,nloads
                    allDEL(nload,ny,nm) = allDEL(nload,ny,nm)
     .                           + DELload(nload,ny,nm)
Crseg                    allDELrseg(nrseg,nload,ny,nm) =
Crseg     .                           allDELrseg(nrseg,nload,ny,nm)
Crseg     .                           + DELload(nload,ny,nm)
Clseg                    allDELlseg(nlseg,nload,ny,nm) =
Clseg     .                           allDELlseg(nlseg,nload,ny,nm)
Clseg     .                           + DELload(nload,ny,nm)
                    allDELstate(nstate,nload,ny,nm) =
     .                           allDELstate(nstate,nload,ny,nm)
     .                           + DELload(nload,ny,nm)
Clrseg                    allDELlrseg(nlrseg,nload,ny,nm) =
Clrseg     .                           allDELlrseg(nlrseg,nload,ny,nm)
Clrseg     .                           + DELload(nload,ny,nm)
                    allDELlu(nl,nload,ny,nm) = allDELlu(nl,nload,ny,nm)
     .                           + DELload(nload,ny,nm)
                  end do

                  allDEL(nloads+1,ny,nm) = allDEL(nloads+1,ny,nm) 
     .                                   +acres(ny,nm)
Crseg                  allDELrseg(nrseg,nloads+1,ny,nm) =
Crseg     .                       allDELrseg(nrseg,nloads+1,ny,nm) 
Crseg     .                                   + acres(ny,nm)
Clseg                  allDELlseg(nlseg,nloads+1,ny,nm) =
Clseg     .                       allDELlseg(nlseg,nloads+1,ny,nm) 
Clseg     .                                   + acres(ny,nm)
                  allDELstate(nstate,nloads+1,ny,nm)=
     .                       allDELstate(nstate,nloads+1,ny,nm) 
     .                                   + acres(ny,nm)
Clrseg                  allDELlrseg(nlrseg,nloads+1,ny,nm)=
Clrseg     .                       allDELlrseg(nlrseg,nloads+1,ny,nm) 
Clrseg     .                                   + acres(ny,nm)
                  allDELlu(nl,nloads+1,ny,nm) =
     .                       allDELlu(nl,nloads+1,ny,nm) + acres(ny,nm)
                end do
              end do
            end do

            call readDATmonthly(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         DEL,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         DELwwtp,DELindus,DELcso,DELsep,DELatdep)

            do nm = 1,nmonths  ! add loads
              do ny = year1,year2
                do nload = 1,nloads
                  allDEL(nload,ny,nm) = allDEL(nload,ny,nm)
     .                                + DELwwtp(nload,ny,nm)
     .                                + DELindus(nload,ny,nm)
     .                                + DELcso(nload,ny,nm)
     .                                + DELsep(nload,ny,nm)
     .                                + DELatdep(nload,ny,nm)
Crseg                  allDELrseg(nrseg,nload,ny,nm) =
Crseg     .                                  allDELrseg(nrseg,nload,ny,nm)
Crseg     .                                + DELps(nload,ny,nm)
Crseg     .                                + DELsep(nload,ny,nm)
Crseg     .                                + DELatdep(nload,ny,nm)
Clseg                  allDELlseg(nlseg,nload,ny,nm) =
Clseg     .                                  allDELlseg(nlseg,nload,ny,nm)
Clseg     .                                + DELps(nload,ny,nm)
Clseg     .                                + DELsep(nload,ny,nm)
Clseg     .                                + DELatdep(nload,ny,nm)
                  allDELstate(nstate,nload,ny,nm) =
     .                                  allDELstate(nstate,nload,ny,nm)
     .                                + DELwwtp(nload,ny,nm)
     .                                + DELindus(nload,ny,nm)
     .                                + DELcso(nload,ny,nm)
     .                                + DELsep(nload,ny,nm)
     .                                + DELatdep(nload,ny,nm)
Clrseg                  allDELlrseg(nlrseg,nload,ny,nm) =
Clrseg     .                                  allDELlrseg(nlrseg,nload,ny,nm)
Clrseg     .                                + DELps(nload,ny,nm)
Clrseg     .                                + DELsep(nload,ny,nm)
Clrseg     .                                + DELatdep(nload,ny,nm)
                  allDELwwtp(nload,ny,nm) = allDELwwtp(nload,ny,nm)
     .                                  + DELwwtp(nload,ny,nm)
                  allDELindus(nload,ny,nm) = allDELindus(nload,ny,nm)
     .                                  + DELindus(nload,ny,nm)
                  allDELcso(nload,ny,nm) = allDELcso(nload,ny,nm)
     .                                  + DELcso(nload,ny,nm)
                  allDELsep(nload,ny,nm) = allDELsep(nload,ny,nm)
     .                                   + DELsep(nload,ny,nm)
                  allDELatdep(nload,ny,nm) = allDELatdep(nload,ny,nm)
     .                                      + DELatdep(nload,ny,nm)
                end do
              end do
            end do

          end if

        end do   ! loop over lsegs in rseg
      end do   ! loop over rsegs in basin
      print*,' '
           
************ write output
      write(header,1233,err=951)'year,mon,',
     .                  (',',loadname(nload),nload=1,nloads),',','acre'

************** EOF 
      if (doEOF) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_EOF_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .                 (',',allEOFlu(nl,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close (11)
        end do

************* point source -wwtp
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOFwwtp(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -industrial
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOFindus(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -cso
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOFcso(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOFsep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOFatdep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

Crseg************* rsegs  
Crseg        do nrseg = 1,nrsegs
Crseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
Crseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Crseg     .          rsegs(nrseg)//'.csv'
Crseg          print*,fnam(:78)
Crseg          open(11,file=fnam,status='unknown',iostat=err)
Crseg          if (err.ne.0) go to 991
Crseg          write(11,'(a)',err=951) header
Crseg          do ny = year1,year2
Crseg            do nm = 1,nmonths
Crseg              write(11,1234,err=951) ny,',',nm,
Crseg     .             (',',allEOFrseg(nrseg,nload,ny,nm),nload=1,nloads+1)
Crseg            end do
Crseg          end do
Crseg          close(11)
Crseg        end do
Crseg
Clseg************* lsegs  
Clseg        do nlseg = 1,nlsegs
Clseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
Clseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clseg     .          lsegs(nlseg)//'.csv'
Clseg          print*,fnam(:78)
Clseg          open(11,file=fnam,status='unknown',iostat=err)
Clseg          if (err.ne.0) go to 991
Clseg          write(11,'(a)',err=951) header
Clseg          do ny = year1,year2
Clseg            do nm = 1,nmonths
Clseg              write(11,1234,err=951) ny,',',nm,
Clseg     .             (',',allEOFlseg(nlseg,nload,ny,nm),nload=1,nloads+1)
Clseg            end do
Clseg          end do
Clseg          close(11)
Clseg        end do
Clseg
Clrseg************* lrsegs  
Clrseg        do nlrseg = 1,nlrsegs
Clrseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
Clrseg    .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clrseg     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'.csv'
Clrseg          print*,fnam(:78)
Clrseg          open(11,file=fnam,status='unknown',iostat=err)
Clrseg          if (err.ne.0) go to 991
Clrseg          write(11,'(a)',err=951) header
Clrseg          do ny = year1,year2
Clrseg            do nm = 1,nmonths
Clrseg              write(11,1234,err=951) ny,',',nm,
Clrseg     .            (',',allEOFlrseg(nlrseg,nload,ny,nm),nload=1,nloads+1)
Clrseg            end do
Clrseg          end do
Clrseg          close(11)
Clrseg        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_EOF_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .            (',',allEOFstate(nstate,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .           (',',allEOF(nload,ny,nm),nload=1,nloads+1)
          end do
        end do
        close(11)

      end if


************** EOS 
      if (doEOS) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_EOS_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .                 (',',allEOSlu(nl,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close (11)
        end do

************* point source -wwtp
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOSwwtp(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -industrial
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOSindus(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -cso
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOScso(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOSsep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allEOSatdep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

Crseg************* rsegs  
Crseg        do nrseg = 1,nrsegs
Crseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
Crseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Crseg     .          rsegs(nrseg)//'.csv'
Crseg          print*,fnam(:78)
Crseg          open(11,file=fnam,status='unknown',iostat=err)
Crseg          if (err.ne.0) go to 991
Crseg          write(11,'(a)',err=951) header
Crseg          do ny = year1,year2
Crseg            do nm = 1,nmonths
Crseg              write(11,1234,err=951) ny,',',nm,
Crseg     .             (',',allEOSrseg(nrseg,nload,ny,nm),nload=1,nloads+1)
Crseg            end do
Crseg          end do
Crseg          close(11)
Crseg        end do
Crseg
Clseg************* lsegs  
Clseg        do nlseg = 1,nlsegs
Clseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
Clseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clseg     .          lsegs(nlseg)//'.csv'
Clseg          print*,fnam(:78)
Clseg          open(11,file=fnam,status='unknown',iostat=err)
Clseg          if (err.ne.0) go to 991
Clseg          write(11,'(a)',err=951) header
Clseg          do ny = year1,year2
Clseg            do nm = 1,nmonths
Clseg              write(11,1234,err=951) ny,',',nm,
Clseg     .             (',',allEOSlseg(nlseg,nload,ny,nm),nload=1,nloads+1)
Clseg            end do
Clseg          end do
Clseg          close(11)
Clseg        end do
Clseg
Clrseg************* lrsegs  
Clrseg        do nlrseg = 1,nlrsegs
Clrseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
Clrseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clrseg     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'.csv'
Clrseg          print*,fnam(:78)
Clrseg          open(11,file=fnam,status='unknown',iostat=err)
Clrseg          if (err.ne.0) go to 991
Clrseg          write(11,'(a)',err=951) header
Clrseg          do ny = year1,year2
Clrseg            do nm = 1,nmonths
Clrseg              write(11,1234,err=951) ny,',',nm,
Clrseg     .            (',',allEOSlrseg(nlrseg,nload,ny,nm),nload=1,nloads+1)
Clrseg            end do
Clrseg          end do
Clrseg          close(11)
Clrseg        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_EOS_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .            (',',allEOSstate(nstate,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .           (',',allEOS(nload,ny,nm),nload=1,nloads+1)
          end do
        end do
        close(11)
      end if

************** DEL 
      if (doDEL) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_DEL_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .                 (',',allDELlu(nl,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close (11)
        end do

************* point source -wwtp
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allDELwwtp(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -industrial
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allDELindus(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************* point source -cso
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allDELcso(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allDELsep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .                     (',',allDELatdep(nload,ny,nm),nload=1,nloads)
          end do
        end do
        close(11)

Crseg************* rsegs  
Crseg        do nrseg = 1,nrsegs
Crseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
Crseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Crseg     .          rsegs(nrseg)//'.csv'
Crseg          print*,fnam(:78)
Crseg          open(11,file=fnam,status='unknown',iostat=err)
Crseg          if (err.ne.0) go to 991
Crseg          write(11,'(a)',err=951) header
Crseg          do ny = year1,year2
Crseg            do nm = 1,nmonths
Crseg              write(11,1234,err=951) ny,',',nm,
Crseg     .             (',',allDELrseg(nrseg,nload,ny,nm),nload=1,nloads+1)
Crseg            end do
Crseg          end do
Crseg          close(11)
Crseg        end do
Crseg
Clseg************* lsegs  
Clseg        do nlseg = 1,nlsegs
Clseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
Clseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clseg     .          lsegs(nlseg)//'.csv'
Clseg          print*,fnam(:78)
Clseg          open(11,file=fnam,status='unknown',iostat=err)
Clseg          if (err.ne.0) go to 991
Clseg          write(11,'(a)',err=951) header
Clseg          do ny = year1,year2
Clseg            do nm = 1,nmonths
Clseg              write(11,1234,err=951) ny,',',nm,
Clseg     .             (',',allDELlseg(nlseg,nload,ny,nm),nload=1,nloads+1)
Clseg            end do
Clseg          end do
Clseg          close(11)
Clseg        end do

Clrseg************* lrsegs  
Clrseg        do nlrseg = 1,nlrsegs
Clrseg          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
Clrseg     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
Clrseg     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'.csv'
Clrseg          print*,fnam(:78)
Clrseg          open(11,file=fnam,status='unknown',iostat=err)
Clrseg          if (err.ne.0) go to 991
Clrseg          write(11,'(a)',err=951) header
Clrseg          do ny = year1,year2
Clrseg            do nm = 1,nmonths
Clrseg              write(11,1234,err=951) ny,',',nm,
Clrseg     .            (',',allDELlrseg(nlrseg,nload,ny,nm),nload=1,nloads+1)
Clrseg            end do
Clrseg          end do
Clrseg          close(11)
Clrseg        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//
     .          '/monthly_DEL_'//basin(:lenbasin)//'_'//cy1//'_'//cy2//
     .          '_'//states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            do nm = 1,nmonths
              write(11,1234,err=951) ny,',',nm,
     .            (',',allDELstate(nstate,nload,ny,nm),nload=1,nloads+1)
            end do
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'monthly/'//rscen(:lenrscen)//'/monthly_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          do nm = 1,nmonths
            write(11,1234,err=951) ny,',',nm,
     .           (',',allDEL(nload,ny,nm),nload=1,nloads+1)
          end do
        end do
        close(11)
      end if

      return

1234  format(i4,a1,i3,20(a1,e14.7))
1233  format(a,20(a1,a14))

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

994   write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',rseg
      report(2) = 'in list'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


