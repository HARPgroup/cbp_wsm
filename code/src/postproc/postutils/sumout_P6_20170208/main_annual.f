************************************************************************
** mini main program to pass years and other inputs                   **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      character*200 basin
      integer year1,year2
      integer iEOF,iEOS,iDEL  ! input flags
************ several different schemes for calculating Delivery
******** user specifies which one to use
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

******** define load variables
      integer nloadmax             !
      parameter (nloadmax = 20)    ! maximum number of loads
      integer nloads               ! number of loads
      character*4 loadname(nloadmax)  ! name of each load (e.g. 'totn')

*********** state variables
      integer nstatemax,nstate,nstates
      parameter (nstatemax = 10) ! number of jurisdictions
      character*2 states(nstatemax)  ! state code (24,42,36, ect)

********* reading variables for loads
      real EOFload(nloadmax+1,year1:year2) 
      real EOFwwtp(nloadmax,year1:year2)
      real EOFindus(nloadmax,year1:year2)
      real EOFcso(nloadmax,year1:year2)
      real EOFsep(nloadmax,year1:year2) 
      real EOFatdep(nloadmax,year1:year2) 

      real EOSload(nloadmax+1,year1:year2) 
      real EOSwwtp(nloadmax,year1:year2)
      real EOSindus(nloadmax,year1:year2)
      real EOScso(nloadmax,year1:year2)
      real EOSsep(nloadmax,year1:year2) 
      real EOSatdep(nloadmax,year1:year2) 

      real DELload(nloadmax+1,year1:year2) 
      real DELwwtp(nloadmax,year1:year2)
      real DELindus(nloadmax,year1:year2)
      real DELcso(nloadmax,year1:year2)
      real DELsep(nloadmax,year1:year2) 
      real DELatdep(nloadmax,year1:year2) 

********** accumulations of load
******** lrsegs cause seg faults so they are commented out
      real allEOF(nloadmax+1,year1:year2) 
      real allEOFrseg(maxrsegs,nloadmax+1,year1:year2) 
      real allEOFlseg(maxlsegs,nloadmax+1,year1:year2)
Clrsegs      real allEOFlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2) 
      real allEOFlu(nlu,nloadmax+1,year1:year2) 
      real allEOFstate(nstatemax,nloadmax+1,year1:year2) 
      real allEOFwwtp(nloadmax+1,year1:year2)
      real allEOFindus(nloadmax+1,year1:year2)
      real allEOFcso(nloadmax+1,year1:year2)
      real allEOFsep(nloadmax+1,year1:year2) 
      real allEOFatdep(nloadmax+1,year1:year2) 
      real acres(year1:year2)

      real allEOS(nloadmax+1,year1:year2) 
      real allEOSrseg(maxrsegs,nloadmax+1,year1:year2) 
      real allEOSlseg(maxlsegs,nloadmax+1,year1:year2)
Clrsegs      real allEOSlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2) 
      real allEOSlu(nlu,nloadmax+1,year1:year2) 
      real allEOSstate(nstatemax,nloadmax+1,year1:year2) 
      real allEOSwwtp(nloadmax+1,year1:year2)
      real allEOSindus(nloadmax+1,year1:year2)
      real allEOScso(nloadmax+1,year1:year2)
      real allEOSsep(nloadmax+1,year1:year2) 
      real allEOSatdep(nloadmax+1,year1:year2) 

      real allDEL(nloadmax+1,year1:year2) 
      real allDELrseg(maxrsegs,nloadmax+1,year1:year2) 
      real allDELlseg(maxlsegs,nloadmax+1,year1:year2)
Clrsegs      real allDELlrseg(maxrsegs*maxL2R,nloadmax+1,year1:year2) 
      real allDELlu(nlu,nloadmax+1,year1:year2) 
      real allDELstate(nstatemax,nloadmax+1,year1:year2) 
      real allDELwwtp(nloadmax+1,year1:year2)
      real allDELindus(nloadmax+1,year1:year2)
      real allDELcso(nloadmax+1,year1:year2)
      real allDELsep(nloadmax+1,year1:year2) 
      real allDELatdep(nloadmax+1,year1:year2) 

*********** segment variables
      integer numlrsegs,nlr
      character*13 allrsegs(maxrsegs)
      character*6 alllsegs(maxlsegs)

********** indices and accounting variables
      integer nr,ns,nl,nls,nlseg,nrseg,nlrseg,nload,ny,nm ! indices

      integer numsegs ! number of lsegs associated with an rseg

      integer nlrsegs  ! total land river segs in basin
      character*13 lrrsegs(maxrsegs*maxL2R)
      character*6 lrlsegs(maxrsegs*maxL2R) 

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

      print*,'summarizing annual output for scenario ',
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
      do ny = year1,year2

        acres(ny) = 0.0

        do nl = 1,nloadmax+1
          allEOFwwtp(nl,ny) = 0.0
          allEOFindus(nl,ny) = 0.0
          allEOFcso(nl,ny) = 0.0
          allEOFsep(nl,ny) = 0.0
          allEOFatdep(nl,ny) = 0.0
          allEOF(nl,ny) = 0.0
          allEOSwwtp(nl,ny) = 0.0
          allEOSindus(nl,ny) = 0.0
          allEOScso(nl,ny) = 0.0
          allEOSsep(nl,ny) = 0.0
          allEOSatdep(nl,ny) = 0.0
          allEOS(nl,ny) = 0.0
          allDELwwtp(nl,ny) = 0.0
          allDELindus(nl,ny) = 0.0
          allDELcso(nl,ny) = 0.0
          allDELsep(nl,ny) = 0.0
          allDELatdep(nl,ny) = 0.0
          allDEL(nl,ny) = 0.0

          do ns = 1,maxrsegs
            allEOFrseg(ns,nl,ny) = 0.0
            allEOSrseg(ns,nl,ny) = 0.0
            allDELrseg(ns,nl,ny) = 0.0
          end do

          do ns = 1,maxlsegs
            allEOFlseg(ns,nl,ny)= 0.0
            allEOSlseg(ns,nl,ny)= 0.0
            allDELlseg(ns,nl,ny)= 0.0
          end do

Clrsegs          do ns = 1,maxrsegs*maxL2R
Clrsegs            allEOFlrseg(ns,nl,ny) = 0.0
Clrsegs            allEOSlrseg(ns,nl,ny) = 0.0
Clrsegs            allDELlrseg(ns,nl,ny) = 0.0
Clrsegs          end do

          do ns = 1,nlu
            allEOFlu(ns,nl,ny) = 0.0
            allEOSlu(ns,nl,ny) = 0.0
            allDELlu(ns,nl,ny) = 0.0
          end do

          do ns = 1,nstatemax
            allEOFstate(ns,nl,ny) = 0.0
            allEOSstate(ns,nl,ny) = 0.0
            allDELstate(ns,nl,ny) = 0.0
          end do

        end do
      end do


*********** populate variables by reading ASCII output
************ loop over river segs
      nlsegs = 0
      nstates = 0
Clrsegs      nlrsegs = 0
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
Clrsegs          nlrsegs = nlrsegs + 1
Clrsegs          lrrsegs(nlrsegs) = rsegs(nrseg) 
Clrsegs          lrlsegs(nlrsegs) = l2r(ns)
Clrsegs          nlrseg = nlrsegs

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
 
              call readEOannual(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOF,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOFload,acres)

              do ny = year1,year2
                do nload = 1,nloads
                  allEOF(nload,ny) = allEOF(nload,ny) 
     .                         + EOFload(nload,ny)
                  allEOFrseg(nrseg,nload,ny) = 
     .                         allEOFrseg(nrseg,nload,ny) 
     .                         + EOFload(nload,ny)
                  allEOFlseg(nlseg,nload,ny) = 
     .                         allEOFlseg(nlseg,nload,ny) 
     .                         + EOFload(nload,ny)
                  allEOFstate(nstate,nload,ny) = 
     .                         allEOFstate(nstate,nload,ny) 
     .                         + EOFload(nload,ny)
Clrseg                  allEOFlrseg(nlrseg,nload,ny) = 
Clrseg     .                         allEOFlrseg(nlrseg,nload,ny) 
Clrseg     .                         + EOFload(nload,ny)
                  allEOFlu(nl,nload,ny) = allEOFlu(nl,nload,ny) 
     .                         + EOFload(nload,ny)
                end do
    
                allEOF(nloads+1,ny) = allEOF(nloads+1,ny) 
     .                                 + acres(ny)
                allEOFrseg(nrseg,nloads+1,ny) = 
     .                     allEOFrseg(nrseg,nloads+1,ny) 
     .                                 + acres(ny)
                allEOFlseg(nlseg,nloads+1,ny) = 
     .                     allEOFlseg(nlseg,nloads+1,ny) 
     .                                 + acres(ny)
                allEOFstate(nstate,nloads+1,ny)=
     .                     allEOFstate(nstate,nloads+1,ny) 
     .                                 + acres(ny)
Clrseg                allEOFlrseg(nlrseg,nloads+1,ny)=
Clrseg     .                     allEOFlrseg(nlrseg,nloads+1,ny) 
Clrseg     .                                 + acres(ny)
                allEOFlu(nl,nloads+1,ny) = 
     .                     allEOFlu(nl,nloads+1,ny) + acres(ny)
              end do
            end do

            call readDATannual(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOF,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         EOFwwtp,EOFindus,EOFcso,EOFsep,EOFatdep)

            do ny = year1,year2
              do nload = 1,nloads
                allEOF(nload,ny) = allEOF(nload,ny)
     .                              + EOFwwtp(nload,ny)
     .                              + EOFindus(nload,ny)
     .                              + EOFcso(nload,ny)
     .                              + EOFsep(nload,ny)
     .                              + EOFatdep(nload,ny)
                allEOFrseg(nrseg,nload,ny) = 
     .                                allEOFrseg(nrseg,nload,ny) 
     .                              + EOFwwtp(nload,ny)
     .                              + EOFindus(nload,ny)
     .                              + EOFcso(nload,ny)
     .                              + EOFsep(nload,ny)
     .                              + EOFatdep(nload,ny)
                allEOFlseg(nlseg,nload,ny) = 
     .                                allEOFlseg(nlseg,nload,ny) 
     .                              + EOFwwtp(nload,ny)
     .                              + EOFindus(nload,ny)
     .                              + EOFcso(nload,ny)
     .                              + EOFsep(nload,ny)
     .                              + EOFatdep(nload,ny)
                allEOFstate(nstate,nload,ny) = 
     .                                allEOFstate(nstate,nload,ny) 
     .                              + EOFwwtp(nload,ny)
     .                              + EOFindus(nload,ny)
     .                              + EOFcso(nload,ny)
     .                              + EOFsep(nload,ny)
     .                              + EOFatdep(nload,ny)
Clrseg                allEOFlrseg(nlrseg,nload,ny) = 
Clrseg     .                                allEOFlrseg(nlrseg,nload,ny) 
Clrseg     .                              + EOFps(nload,ny)
Clrseg     .                              + EOFsep(nload,ny)
Clrseg     .                              + EOFatdep(nload,ny)
                allEOFwwtp(nload,ny) = allEOFwwtp(nload,ny)
     .                                + EOFwwtp(nload,ny)
                allEOFindus(nload,ny) = allEOFindus(nload,ny)
     .                                + EOFindus(nload,ny)
                allEOFcso(nload,ny) = allEOFcso(nload,ny)
     .                                + EOFcso(nload,ny)
                allEOFsep(nload,ny) = allEOFsep(nload,ny) 
     .                                 + EOFsep(nload,ny)
                allEOFatdep(nload,ny) = allEOFatdep(nload,ny) 
     .                                    + EOFatdep(nload,ny)
              end do
            end do

          end if

          if (doEOS) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOannual(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOS,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOSload,acres)

              do ny = year1,year2
                do nload = 1,nloads
                  allEOS(nload,ny) = allEOS(nload,ny)
     .                         + EOSload(nload,ny)
                  allEOSrseg(nrseg,nload,ny) =
     .                         allEOSrseg(nrseg,nload,ny)
     .                         + EOSload(nload,ny)
                  allEOSlseg(nlseg,nload,ny) =
     .                         allEOSlseg(nlseg,nload,ny)
     .                         + EOSload(nload,ny)
                  allEOSstate(nstate,nload,ny) =
     .                         allEOSstate(nstate,nload,ny)
     .                         + EOSload(nload,ny)
Clrseg                  allEOSlrseg(nlrseg,nload,ny) =
Clrseg     .                         allEOSlrseg(nlrseg,nload,ny)
Clrseg     .                         + EOSload(nload,ny)
                  allEOSlu(nl,nload,ny) = allEOSlu(nl,nload,ny)
     .                         + EOSload(nload,ny)
                end do

                allEOS(nloads+1,ny) = allEOS(nloads+1,ny) 
     .                                 + acres(ny)
                allEOSrseg(nrseg,nloads+1,ny) =
     .                     allEOSrseg(nrseg,nloads+1,ny) 
     .                                 + acres(ny)
                allEOSlseg(nlseg,nloads+1,ny) =
     .                     allEOSlseg(nlseg,nloads+1,ny) 
     .                                 + acres(ny)
                allEOSstate(nstate,nloads+1,ny)=
     .                     allEOSstate(nstate,nloads+1,ny) 
     .                                 + acres(ny)
Clrseg                allEOSlrseg(nlrseg,nloads+1,ny)=
Clrseg     .                     allEOSlrseg(nlrseg,nloads+1,ny) 
Clrseg     .                                 + acres(ny)
                allEOSlu(nl,nloads+1,ny) =
     .                     allEOSlu(nl,nloads+1,ny) + acres(ny)
              end do
            end do

            call readDATannual(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOS,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         EOSwwtp,EOSindus,EOScso,EOSsep,EOSatdep)

            do ny = year1,year2
              do nload = 1,nloads
                allEOS(nload,ny) = allEOS(nload,ny)
     .                              + EOSwwtp(nload,ny)
     .                              + EOSindus(nload,ny)
     .                              + EOScso(nload,ny)
     .                              + EOSsep(nload,ny)
     .                              + EOSatdep(nload,ny)
                allEOSrseg(nrseg,nload,ny) =
     .                                allEOSrseg(nrseg,nload,ny)
     .                              + EOSwwtp(nload,ny)
     .                              + EOSindus(nload,ny)
     .                              + EOScso(nload,ny)
     .                              + EOSsep(nload,ny)
     .                              + EOSatdep(nload,ny)
                allEOSlseg(nlseg,nload,ny) =
     .                                allEOSlseg(nlseg,nload,ny)
     .                              + EOSwwtp(nload,ny)
     .                              + EOSindus(nload,ny)
     .                              + EOScso(nload,ny)
     .                              + EOSsep(nload,ny)
     .                              + EOSatdep(nload,ny)
                allEOSstate(nstate,nload,ny) =
     .                                allEOSstate(nstate,nload,ny)
     .                              + EOSwwtp(nload,ny)
     .                              + EOSindus(nload,ny)
     .                              + EOScso(nload,ny)
     .                              + EOSsep(nload,ny)
     .                              + EOSatdep(nload,ny)
Clrseg                allEOSlrseg(nlrseg,nload,ny) =
Clrseg     .                                allEOSlrseg(nlrseg,nload,ny)
Clrseg     .                              + EOSps(nload,ny)
Clrseg     .                              + EOSsep(nload,ny)
Clrseg     .                              + EOSatdep(nload,ny)
                allEOSwwtp(nload,ny) = allEOSwwtp(nload,ny)
     .                                + EOSwwtp(nload,ny)
                allEOSindus(nload,ny) = allEOSindus(nload,ny)
     .                                + EOSindus(nload,ny)
                allEOScso(nload,ny) = allEOScso(nload,ny)
     .                                + EOScso(nload,ny)
                allEOSsep(nload,ny) = allEOSsep(nload,ny)
     .                                 + EOSsep(nload,ny)
                allEOSatdep(nload,ny) = allEOSatdep(nload,ny)
     .                                    + EOSatdep(nload,ny)
              end do
            end do

          end if

          if (doDEL) then

            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOannual(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),DEL,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          DELload,acres)
	print*,luname(nl),' ',DELload(1,year1)

              do ny = year1,year2
                do nload = 1,nloads
                  allDEL(nload,ny) = allDEL(nload,ny)
     .                         + DELload(nload,ny)
                  allDELrseg(nrseg,nload,ny) =
     .                         allDELrseg(nrseg,nload,ny)
     .                         + DELload(nload,ny)
                  allDELlseg(nlseg,nload,ny) =
     .                         allDELlseg(nlseg,nload,ny)
     .                         + DELload(nload,ny)
                  allDELstate(nstate,nload,ny) =
     .                         allDELstate(nstate,nload,ny)
     .                         + DELload(nload,ny)
Clrseg                  allDELlrseg(nlrseg,nload,ny) =
Clrseg     .                         allDELlrseg(nlrseg,nload,ny)
Clrseg     .                         + DELload(nload,ny)
                  allDELlu(nl,nload,ny) = allDELlu(nl,nload,ny)
     .                         + DELload(nload,ny)
                end do

                allDEL(nloads+1,ny) = allDEL(nloads+1,ny) 
     .                                 +acres(ny)
                allDELrseg(nrseg,nloads+1,ny) =
     .                     allDELrseg(nrseg,nloads+1,ny) 
     .                                 + acres(ny)
                allDELlseg(nlseg,nloads+1,ny) =
     .                     allDELlseg(nlseg,nloads+1,ny) 
     .                                 + acres(ny)
                allDELstate(nstate,nloads+1,ny)=
     .                     allDELstate(nstate,nloads+1,ny) 
     .                                 + acres(ny)
Clrseg                allDELlrseg(nlrseg,nloads+1,ny)=
Clrseg     .                     allDELlrseg(nlrseg,nloads+1,ny) 
Clrseg     .                                 + acres(ny)
                allDELlu(nl,nloads+1,ny) =
     .                     allDELlu(nl,nloads+1,ny) + acres(ny)
              end do
            end do
	print*,allDEL(1,year1)

            call readDATannual(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         DEL,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                         DELwwtp,DELindus,DELcso,DELsep,DELatdep)

            do ny = year1,year2
              do nload = 1,nloads
                allDEL(nload,ny) = allDEL(nload,ny)
     .                              + DELwwtp(nload,ny)
     .                              + DELindus(nload,ny)
     .                              + DELcso(nload,ny)
     .                              + DELsep(nload,ny)
     .                              + DELatdep(nload,ny)
                allDELrseg(nrseg,nload,ny) =
     .                                allDELrseg(nrseg,nload,ny)
     .                              + DELwwtp(nload,ny)
     .                              + DELindus(nload,ny)
     .                              + DELcso(nload,ny)
     .                              + DELsep(nload,ny)
     .                              + DELatdep(nload,ny)
                allDELlseg(nlseg,nload,ny) =
     .                                allDELlseg(nlseg,nload,ny)
     .                              + DELwwtp(nload,ny)
     .                              + DELindus(nload,ny)
     .                              + DELcso(nload,ny)
     .                              + DELsep(nload,ny)
     .                              + DELatdep(nload,ny)
                allDELstate(nstate,nload,ny) =
     .                                allDELstate(nstate,nload,ny)
     .                              + DELwwtp(nload,ny)
     .                              + DELindus(nload,ny)
     .                              + DELcso(nload,ny)
     .                              + DELsep(nload,ny)
     .                              + DELatdep(nload,ny)
Clrseg                allDELlrseg(nlrseg,nload,ny) =
Clrseg     .                                allDELlrseg(nlrseg,nload,ny)
Clrseg     .                              + DELps(nload,ny)
Clrseg     .                              + DELsep(nload,ny)
Clrseg     .                              + DELatdep(nload,ny)
                allDELwwtp(nload,ny) = allDELwwtp(nload,ny)
     .                                + DELwwtp(nload,ny)
                allDELindus(nload,ny) = allDELindus(nload,ny)
     .                                + DELindus(nload,ny)
                allDELcso(nload,ny) = allDELcso(nload,ny)
     .                                + DELcso(nload,ny)
                allDELsep(nload,ny) = allDELsep(nload,ny)
     .                                 + DELsep(nload,ny)
                allDELatdep(nload,ny) = allDELatdep(nload,ny)
     .                                    + DELatdep(nload,ny)
              end do
            end do

          end if

        end do   ! loop over lsegs in rseg
      end do   ! loop over rsegs in basin
      print*,' '
           
************ write output
      write(header,1233,err=951)'year,',
     .                  (',',loadname(nload),nload=1,nloads),',','acre'

************** EOF 
      if (doEOF) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .               (',',allEOFlu(nl,nload,ny),nload=1,nloads+1)
          end do
          close (11)
        end do

*************  point source -WWTP
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOFwwtp(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source -Industrial
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOFindus(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source - CSO
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOFcso(nload,ny),nload=1,nloads)
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOFsep(nload,ny),nload=1,nloads)
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOFatdep(nload,ny),nload=1,nloads)
        end do
        close(11)

************* rsegs  
        do nrseg = 1,nrsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          rsegs(nrseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allEOFrseg(nrseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* lsegs  
        do nlseg = 1,nlsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          lsegs(nlseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allEOFlseg(nlseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

C************* lrsegs  
C        do nlrseg = 1,nlrsegs
C          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
C     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
C     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'.csv'
C          print*,fnam(:78)
C          open(11,file=fnam,status='unknown',iostat=err)
C          if (err.ne.0) go to 991
C          write(11,'(a)',err=951) header
C          do ny = year1,year2
C            write(11,1234,err=951) ny,
C     .          (',',allEOFlrseg(nlrseg,nload,ny),nload=1,nloads+1)
C          end do
C          close(11)
C        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .          (',',allEOFstate(nstate,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOF_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .         (',',allEOF(nload,ny),nload=1,nloads+1)
        end do
        close(11)

      end if


************** EOS 
      if (doEOS) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .               (',',allEOSlu(nl,nload,ny),nload=1,nloads+1)
          end do
          close (11)
        end do

************* point source -wwtp
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOSwwtp(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source -industrial
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOSindus(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source -cso
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOScso(nload,ny),nload=1,nloads)
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOSsep(nload,ny),nload=1,nloads)
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allEOSatdep(nload,ny),nload=1,nloads)
        end do
        close(11)

************* rsegs  
        do nrseg = 1,nrsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          rsegs(nrseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allEOSrseg(nrseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* lsegs  
        do nlseg = 1,nlsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          lsegs(nlseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allEOSlseg(nlseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

C************* lrsegs  
C        do nlrseg = 1,nlrsegs
C          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
C     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
C     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'.csv'
C          print*,fnam(:78)
C          open(11,file=fnam,status='unknown',iostat=err)
C          if (err.ne.0) go to 991
C          write(11,'(a)',err=951) header
C          do ny = year1,year2
C            write(11,1234,err=951) ny,
C     .          (',',allEOSlrseg(nlrseg,nload,ny),nload=1,nloads+1)
C          end do
C          close(11)
C        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .          (',',allEOSstate(nstate,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_EOS_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .        'total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .         (',',allEOS(nload,ny),nload=1,nloads+1)
        end do
        close(11)
      end if

************** DEL 
      if (doDEL) then

***************** land use
        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          luname(nl)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .               (',',allDELlu(nl,nload,ny),nload=1,nloads+1)
          end do
          close (11)
        end do

************* point source -wwtp
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//wwtp//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
            write(11,1234,err=951) ny,
     .                 (',',allDELwwtp(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source -industrial
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//indus//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
            write(11,1234,err=951) ny,
     .                 (',',allDELindus(nload,ny),nload=1,nloads)
        end do
        close(11)

************* point source -cso
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//cso//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
            write(11,1234,err=951) ny,
     .                 (',',allDELcso(nload,ny),nload=1,nloads)
        end do
        close(11)

*************** septic
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//sep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allDELsep(nload,ny),nload=1,nloads)
        end do
        close(11)

************** atdep
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//atdep//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .                   (',',allDELatdep(nload,ny),nload=1,nloads)
        end do
        close(11)

************* rsegs  
        do nrseg = 1,nrsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          rsegs(nrseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allDELrseg(nrseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* lsegs  
        do nlseg = 1,nlsegs
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          lsegs(nlseg)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .           (',',allDELlseg(nlseg,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

C************* lrsegs  
C        do nlrseg = 1,nlrsegs
C          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
C     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
C     .          lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg)//'_'//DFmethod//'.csv'
C          print*,fnam(:78)
C          open(11,file=fnam,status='unknown',iostat=err)
C          if (err.ne.0) go to 991
C          write(11,'(a)',err=951) header
C          do ny = year1,year2
C            write(11,1234,err=951) ny,
C     .          (',',allDELlrseg(nlrseg,nload,ny),nload=1,nloads+1)
C          end do
C          close(11)
C        end do

************* states
        do nstate = 1,nstates
          fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .          //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .          states(nstate)//'.csv'
          print*,fnam(:78)
          open(11,file=fnam,status='unknown',iostat=err)
          if (err.ne.0) go to 991
          write(11,'(a)',err=951) header
          do ny = year1,year2
            write(11,1234,err=951) ny,
     .          (',',allDELstate(nstate,nload,ny),nload=1,nloads+1)
          end do
          close(11)
        end do

************* total basin
        fnam =sumoutdir//'annual/'//rscen(:lenrscen)//'/annual_DEL_'
     .        //basin(:lenbasin)//'_'//cy1//'_'//cy2//'_total.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
        write(11,'(a)',err=951) header
        do ny = year1,year2
          write(11,1234,err=951) ny,
     .         (',',allDEL(nload,ny),nload=1,nloads+1)
        end do
        close(11)
      end if

      return

1234  format(i4,20(a1,e14.7))
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

999   call stopreport(report)

      end


