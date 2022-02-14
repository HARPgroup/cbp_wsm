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
      character*4 area
      data area /'ACRE'/

*********** state variables
      integer nstatemax,nstate,nstates
      parameter (nstatemax = 10) ! number of jurisdictions
      character*2 states(nstatemax)  ! state code (24,42,36, ect)

*********** segment variables
      integer numlrsegs,nlr
      character*13 allrsegs(maxrsegs)
      character*6 alllsegs(maxlsegs)

********* reading variables for loads
      real EOPload(nloadmax+1)
      real EOPwwtp(nloadmax)
      real EOPindus(nloadmax)
      real EOPcso(nloadmax)
      real EOPsep(nloadmax)
      real EOPrib(nloadmax)
      real EOPrpa(nloadmax)
      real EOPatdep(nloadmax)

      real EOFload(nloadmax+1) 
      real EOFwwtp(nloadmax) 
      real EOFindus(nloadmax)
      real EOFcso(nloadmax)
      real EOFsep(nloadmax)
      real EOFrib(nloadmax)
      real EOFrpa(nloadmax) 
      real EOFatdep(nloadmax) 

      real EOSload(nloadmax+1) 
      real EOSwwtp(nloadmax)
      real EOSindus(nloadmax)
      real EOScso(nloadmax)
      real EOSsep(nloadmax)
      real EOSrib(nloadmax) 
      real EOSrpa(nloadmax)
      real EOSatdep(nloadmax) 

      real EORload(nloadmax+1)
      real EORwwtp(nloadmax)
      real EORindus(nloadmax)
      real EORcso(nloadmax)
      real EORsep(nloadmax) 
      real EORrib(nloadmax)
      real EORrpa(nloadmax)
      real EORatdep(nloadmax)

      real DELload(nloadmax+1) 
      real DELwwtp(nloadmax)
      real DELindus(nloadmax)
      real DELcso(nloadmax)
      real DELsep(nloadmax)
      real DELrib(nloadmax)
      real DELrpa(nloadmax) 
      real DELatdep(nloadmax) 

      real BAYload(nloadmax+1)
      real BAYwwtp(nloadmax)
      real BAYindus(nloadmax)
      real BAYcso(nloadmax)
      real BAYsep(nloadmax)
      real BAYrib(nloadmax)
      real BAYrpa(nloadmax)
      real BAYatdep(nloadmax)

********** accumulations of load
      real allEOP(nloadmax+1)
      real allEOPrseg(maxrsegs,nloadmax+1)
      real allEOPlseg(maxlsegs,nloadmax+1)
      real allEOPlrseg(maxrsegs*maxL2R,nloadmax+1)
      real allEOPlu(nlu,nloadmax+1)
      real allEOPstate(nstatemax,nloadmax+1)
      real allEOPwwtp(nloadmax+1)
      real allEOPindus(nloadmax+1)
      real allEOPcso(nloadmax+1)
      real allEOPsep(nloadmax+1)
      real allEOPrib(nloadmax+1)
      real allEOPrpa(nloadmax+1)
      real allEOPatdep(nloadmax+1)
      real allEOPstatelu(nstatemax,nlu,nloadmax+1)
      real allEOPstatewwtp(nstatemax,nloadmax+1)
      real allEOPstateindus(nstatemax,nloadmax+1)
      real allEOPstatecso(nstatemax,nloadmax+1)
      real allEOPstatesep(nstatemax,nloadmax+1)
      real allEOPstaterib(nstatemax,nloadmax+1)
      real allEOPstaterpa(nstatemax,nloadmax+1)
      real allEOPstateatdep(nstatemax,nloadmax+1)

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
      real allEOFrib(nloadmax+1)
      real allEOFrpa(nloadmax+1)
      real allEOFatdep(nloadmax+1) 
      real allEOFstatelu(nstatemax,nlu,nloadmax+1)
      real allEOFstatewwtp(nstatemax,nloadmax+1)
      real allEOFstateindus(nstatemax,nloadmax+1)
      real allEOFstatecso(nstatemax,nloadmax+1)
      real allEOFstatesep(nstatemax,nloadmax+1)
      real allEOFstaterib(nstatemax,nloadmax+1)
      real allEOFstaterpa(nstatemax,nloadmax+1)
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
      real allEOSrib(nloadmax+1)
      real allEOSrpa(nloadmax+1)
      real allEOSatdep(nloadmax+1) 
      real allEOSstatelu(nstatemax,nlu,nloadmax+1)
      real allEOSstatewwtp(nstatemax,nloadmax+1)
      real allEOSstateindus(nstatemax,nloadmax+1)
      real allEOSstatecso(nstatemax,nloadmax+1)
      real allEOSstatesep(nstatemax,nloadmax+1)
      real allEOSstaterib(nstatemax,nloadmax+1)
      real allEOSstaterpa(nstatemax,nloadmax+1)
      real allEOSstateatdep(nstatemax,nloadmax+1)

      real allEOR(nloadmax+1)
      real allEORrseg(maxrsegs,nloadmax+1)
      real allEORlseg(maxlsegs,nloadmax+1)
      real allEORlrseg(maxrsegs*maxL2R,nloadmax+1)
      real allEORlu(nlu,nloadmax+1)
      real allEORstate(nstatemax,nloadmax+1)
      real allEORwwtp(nloadmax+1)
      real allEORindus(nloadmax+1)
      real allEORcso(nloadmax+1)
      real allEORsep(nloadmax+1)
      real allEORrib(nloadmax+1)
      real allEORrpa(nloadmax+1)
      real allEORatdep(nloadmax+1)
      real allEORstatelu(nstatemax,nlu,nloadmax+1)
      real allEORstatewwtp(nstatemax,nloadmax+1)
      real allEORstateindus(nstatemax,nloadmax+1)
      real allEORstatecso(nstatemax,nloadmax+1)
      real allEORstatesep(nstatemax,nloadmax+1)
      real allEORstaterib(nstatemax,nloadmax+1)
      real allEORstaterpa(nstatemax,nloadmax+1)
      real allEORstateatdep(nstatemax,nloadmax+1)

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
      real allDELrib(nloadmax+1)
      real allDELrpa(nloadmax+1)
      real allDELatdep(nloadmax+1) 
      real allDELstatelu(nstatemax,nlu,nloadmax+1)
      real allDELstatewwtp(nstatemax,nloadmax+1)
      real allDELstateindus(nstatemax,nloadmax+1)
      real allDELstatecso(nstatemax,nloadmax+1)
      real allDELstatesep(nstatemax,nloadmax+1)
      real allDELstaterib(nstatemax,nloadmax+1)
      real allDELstaterpa(nstatemax,nloadmax+1)
      real allDELstateatdep(nstatemax,nloadmax+1)

      real allBAY(nloadmax+1)
      real allBAYrseg(maxrsegs,nloadmax+1)
      real allBAYlseg(maxlsegs,nloadmax+1)
      real allBAYlrseg(maxrsegs*maxL2R,nloadmax+1)
      real allBAYlu(nlu,nloadmax+1)
      real allBAYstate(nstatemax,nloadmax+1)
      real allBAYwwtp(nloadmax+1)
      real allBAYindus(nloadmax+1)
      real allBAYcso(nloadmax+1)
      real allBAYsep(nloadmax+1)
      real allBAYrib(nloadmax+1)
      real allBAYrpa(nloadmax+1)
      real allBAYatdep(nloadmax+1)
      real allBAYstatelu(nstatemax,nlu,nloadmax+1)
      real allBAYstatewwtp(nstatemax,nloadmax+1)
      real allBAYstateindus(nstatemax,nloadmax+1)
      real allBAYstatecso(nstatemax,nloadmax+1)
      real allBAYstatesep(nstatemax,nloadmax+1)
      real allBAYstaterib(nstatemax,nloadmax+1)
      real allBAYstaterpa(nstatemax,nloadmax+1)
      real allBAYstateatdep(nstatemax,nloadmax+1)

********** indices and accounting variables
      integer nr,ns,nl,lu,nls,nlseg,nrseg,nlrseg,nload   ! indices

      integer numsegs ! number of lsegs associated with an rseg

      integer nlrsegs  ! total land river segs in basin
      character*13 lrrsegs(maxrsegs*maxL2R)
      character*6 lrlsegs(maxrsegs*maxL2R) 

      character*200 basin

      integer lenbasin

      logical found

      character*3 EOP,EOF,EOS,EOR,DEL,BAY
      data EOP,EOF,EOS,EOR,DEL,BAY
     .       /'eop','eof','eos','eor','del','bay'/

      character*3 cEOP,cEOF,cEOS,cEOR,cDEL,cBAY
      data cEOP,cEOF,cEOS,cEOR,cDEL,cBAY
     .       /'EOP','EOF','EOS','EOR','DEL','BAY'/

      integer year1,year2
      character*4 cy1,cy2

      integer iEOF,iEOS,iDEL  ! input flags 
      logical doEOP,doEOF,doEOS,doEOR,doDEL,doBAY  ! logical values
      data doEOF,doEOS,doDEL /.false.,.false.,.false./
      data doEOP,doEOR       /.false.,.false./
      data doBAY             /.false./

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

      if (iEOF.ne.0) doEOP = .true.
      if (iEOF.ne.0) doEOF = .true.
      if (iEOS.ne.0) doEOS = .true.
      if (iEOS.ne.0) doEOR = .true.
      if (iDEL.ne.0) doDEL = .true.
      if (iDEL.ne.0) doBAY = .true.
      call lencl(basin,lenbasin)
      call lencl(rscen,lenrscen) 

      call lencl(atdep,lenatdep)

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
        allEOPwwtp(nl) = 0.0
        allEOPindus(nl) = 0.0
        allEOPcso(nl) = 0.0
        allEOPsep(nl) = 0.0
        allEOPrib(nl) = 0.0
        allEOPrpa(nl) = 0.0
        allEOPatdep(nl) = 0.0
        allEOP(nl) = 0.0

        allEOFwwtp(nl) = 0.0
        allEOFindus(nl) = 0.0
        allEOFcso(nl) = 0.0
        allEOFsep(nl) = 0.0
        allEOFrib(nl) = 0.0
        allEOFrpa(nl) = 0.0
        allEOFatdep(nl) = 0.0
        allEOF(nl) = 0.0

        allEOSwwtp(nl) = 0.0
        allEOSindus(nl) = 0.0
        allEOScso(nl) = 0.0
        allEOSsep(nl) = 0.0
        allEOSrib(nl) = 0.0
        allEOSrpa(nl) = 0.0
        allEOSatdep(nl) = 0.0
        allEOS(nl) = 0.0

        allEORwwtp(nl) = 0.0
        allEORindus(nl) = 0.0
        allEORcso(nl) = 0.0
        allEORsep(nl) = 0.0
        allEORrib(nl) = 0.0
        allEORrpa(nl) = 0.0
        allEORatdep(nl) = 0.0
        allEOR(nl) = 0.0

        allDELwwtp(nl) = 0.0
        allDELindus(nl) = 0.0
        allDELcso(nl) = 0.0
        allDELsep(nl) = 0.0
        allDELrib(nl) = 0.0
        allDELrpa(nl) = 0.0
        allDELatdep(nl) = 0.0
        allDEL(nl) = 0.0

        allBAYwwtp(nl) = 0.0
        allBAYindus(nl) = 0.0
        allBAYcso(nl) = 0.0
        allBAYsep(nl) = 0.0
        allBAYrib(nl) = 0.0
        allBAYrpa(nl) = 0.0
        allBAYatdep(nl) = 0.0
        allBAY(nl) = 0.0

        do ns = 1,maxrsegs
          allEOPrseg(ns,nl) = 0.0
          allEOFrseg(ns,nl) = 0.0
          allEOSrseg(ns,nl) = 0.0
          allEORrseg(ns,nl) = 0.0
          allDELrseg(ns,nl) = 0.0
          allBAYrseg(ns,nl) = 0.0
        end do

        do ns = 1,maxlsegs
          allEOPlseg(ns,nl)= 0.0
          allEOFlseg(ns,nl)= 0.0
          allEOSlseg(ns,nl)= 0.0
          allEORlseg(ns,nl)= 0.0
          allDELlseg(ns,nl)= 0.0
          allBAYlseg(ns,nl)= 0.0
        end do

        do ns = 1,maxrsegs*maxL2R
          allEOPlrseg(ns,nl) = 0.0
          allEOFlrseg(ns,nl) = 0.0
          allEOSlrseg(ns,nl) = 0.0
          allEORlrseg(ns,nl) = 0.0
          allDELlrseg(ns,nl) = 0.0
          allBAYlrseg(ns,nl) = 0.0
        end do

        do ns = 1,nlu
          allEOPlu(ns,nl) = 0.0
          allEOFlu(ns,nl) = 0.0
          allEOSlu(ns,nl) = 0.0
          allEORlu(ns,nl) = 0.0
          allDELlu(ns,nl) = 0.0
          allBAYlu(ns,nl) = 0.0
        end do

        do ns = 1,nstatemax
          allEOPstate(ns,nl) = 0.0
          allEOFstate(ns,nl) = 0.0
          allEOSstate(ns,nl) = 0.0
          allEORstate(ns,nl) = 0.0
          allDELstate(ns,nl) = 0.0
          allBAYstate(ns,nl) = 0.0
          do lu = 1,nlu
            allEOPstatelu(ns,lu,nl) = 0.0
            allEOFstatelu(ns,lu,nl) = 0.0
            allEOSstatelu(ns,lu,nl) = 0.0
            allEORstatelu(ns,lu,nl) = 0.0
            allDELstatelu(ns,lu,nl) = 0.0
            allBAYstatelu(ns,lu,nl) = 0.0
          end do
          allEOPstatewwtp(ns,nl) = 0.0
          allEOPstateindus(ns,nl) = 0.0
          allEOPstatecso(ns,nl) = 0.0
          allEOPstatesep(ns,nl) = 0.0
          allEOPstaterib(ns,nl) = 0.0
          allEOPstaterpa(ns,nl) = 0.0
          allEOPstateatdep(ns,nl) = 0.0

          allEOFstatewwtp(ns,nl) = 0.0
          allEOFstateindus(ns,nl) = 0.0
          allEOFstatecso(ns,nl) = 0.0
          allEOFstatesep(ns,nl) = 0.0
          allEOFstaterib(ns,nl) = 0.0
          allEOFstaterpa(ns,nl) = 0.0
          allEOFstateatdep(ns,nl) = 0.0

          allEOSstatewwtp(ns,nl) = 0.0
          allEOSstateindus(ns,nl) = 0.0
          allEOSstatecso(ns,nl) = 0.0
          allEOSstatesep(ns,nl) = 0.0
          allEOSstaterib(ns,nl) = 0.0
          allEOSstaterpa(ns,nl) = 0.0
          allEOSstateatdep(ns,nl) = 0.0

          allEORstatewwtp(ns,nl) = 0.0
          allEORstateindus(ns,nl) = 0.0
          allEORstatecso(ns,nl) = 0.0
          allEORstatesep(ns,nl) = 0.0
          allEORstaterib(ns,nl) = 0.0
          allEORstaterpa(ns,nl) = 0.0
          allEORstateatdep(ns,nl) = 0.0

          allDELstatewwtp(ns,nl) = 0.0
          allDELstateindus(ns,nl) = 0.0
          allDELstatecso(ns,nl) = 0.0
          allDELstatesep(ns,nl) = 0.0
          allDELstaterib(ns,nl) = 0.0
          allDELstaterpa(ns,nl) = 0.0
          allDELstateatdep(ns,nl) = 0.0

          allBAYstatewwtp(ns,nl) = 0.0
          allBAYstateindus(ns,nl) = 0.0
          allBAYstatecso(ns,nl) = 0.0
          allBAYstatesep(ns,nl) = 0.0
          allBAYstaterib(ns,nl) = 0.0
          allBAYstaterpa(ns,nl) = 0.0
          allBAYstateatdep(ns,nl) = 0.0
        end do

      end do

*********** populate variables by reading ASCII output
************ loop over river segs
      nlsegs = 0
      nstates = 0
      nlrsegs = 0
      do nrseg = 1,nrsegs
      !{
        call ttyput(rsegs(nrseg)//' ')

********** get the segment in this river
        call getl2rlist(                              ! get the exhaustive list of all land-river segments
     I                  rsegs(nrseg),rscen,lenrscen,
     O                  numsegs,l2r)

************** loop over land segs in river seg
        do ns = 1,numsegs
        !{
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

***************** EOP ***********************        
          if (doEOP) then
          !{
            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle

              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOP,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EOPload,acres)

              do nload = 1,nloads  ! add loads
                allEOP(nload) = allEOP(nload) + EOPload(nload)
                allEOPrseg(nrseg,nload) = allEOPrseg(nrseg,nload)
     .                                  + EOPload(nload)
                allEOPlseg(nlseg,nload) = allEOPlseg(nlseg,nload)
     .                                  + EOPload(nload)
                allEOPstate(nstate,nload) = allEOPstate(nstate,nload)
     .                                    + EOPload(nload)
                allEOPlrseg(nlrseg,nload) = allEOPlrseg(nlrseg,nload)
     .                                    + EOPload(nload)
                allEOPlu(nl,nload) = allEOPlu(nl,nload) + EOPload(nload)
                allEOPstatelu(nstate,nl,nload) =
     .                   allEOPstatelu(nstate,nl,nload) + EOPload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),EOP,loadname(nload),
     .                                 EOPload(nload)
              end do

              allEOP(nloads+1) = allEOP(nloads+1) + acres
              allEOPrseg(nrseg,nloads+1) = allEOPrseg(nrseg,nloads+1)
     .                                   + acres
              allEOPlseg(nlseg,nloads+1) = allEOPlseg(nlseg,nloads+1)
     .                                   + acres
              allEOPstate(nstate,nloads+1)=allEOPstate(nstate,nloads+1)
     .                                    + acres
              allEOPlrseg(nlrseg,nloads+1)=allEOPlrseg(nlrseg,nloads+1)
     .                                    + acres
              allEOPlu(nl,nloads+1) = allEOPlu(nl,nloads+1) + acres
              allEOPstatelu(nstate,nl,nloads+1) =
     .                    allEOPstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),EOP,area,
     .                               acres
            end do
            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOP,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EOPwwtp,EOPindus,EOPcso,EOPsep,EOPrib,EOPrpa,
     O                    EOPatdep,acres)

            do nload = 1,nloads
              allEOP(nload) = allEOP(nload)
     .                      + EOPwwtp(nload)
     .                      + EOPindus(nload)
     .                      + EOPcso(nload)
     .                      + EOPsep(nload)
     .                      + EOPrib(nload)
     .                      + EOPrpa(nload)
     .                      + EOPatdep(nload)
              allEOPrseg(nrseg,nload) = allEOPrseg(nrseg,nload)
     .                                + EOPwwtp(nload)
     .                                + EOPindus(nload)
     .                                + EOPcso(nload)
     .                                + EOPsep(nload)
     .                                + EOPrib(nload)
     .                                + EOPrpa(nload)
     .                                + EOPatdep(nload)
              allEOPlseg(nlseg,nload) = allEOPlseg(nlseg,nload)
     .                                + EOPwwtp(nload)
     .                                + EOPindus(nload)
     .                                + EOPcso(nload)
     .                                + EOPsep(nload)
     .                                + EOPrib(nload)
     .                                + EOPrpa(nload)
     .                                + EOPatdep(nload)
              allEOPstate(nstate,nload) = allEOPstate(nstate,nload)
     .                                  + EOPwwtp(nload)
     .                                  + EOPindus(nload)
     .                                  + EOPcso(nload)
     .                                  + EOPsep(nload)
     .                                  + EOPrib(nload)
     .                                  + EOPrpa(nload)
     .                                  + EOPatdep(nload)
              allEOPlrseg(nlrseg,nload) = allEOPlrseg(nlrseg,nload)
     .                                  + EOPwwtp(nload)
     .                                  + EOPindus(nload)
     .                                  + EOPcso(nload)
     .                                  + EOPsep(nload)
     .                                  + EOPrib(nload)
     .                                  + EOPrpa(nload)
     .                                  + EOPatdep(nload)
              allEOPwwtp(nload) = allEOPwwtp(nload) + EOPwwtp(nload)
              allEOPindus(nload) = allEOPindus(nload) + EOPindus(nload)
              allEOPcso(nload) = allEOPcso(nload) + EOPcso(nload)
              allEOPsep(nload) = allEOPsep(nload) + EOPsep(nload)
              allEOPrib(nload) = allEOPrib(nload) + EOPrib(nload)
              allEOPrpa(nload) = allEOPrpa(nload) + EOPrpa(nload)
              allEOPatdep(nload) = allEOPatdep(nload) + EOPatdep(nload)
              allEOPstatewwtp(nstate,nload) =
     .              allEOPstatewwtp(nstate,nload) + EOPwwtp(nload)
              allEOPstateindus(nstate,nload) =
     .              allEOPstateindus(nstate,nload) + EOPindus(nload)
              allEOPstatecso(nstate,nload) =
     .              allEOPstatecso(nstate,nload) + EOPcso(nload)
              allEOPstatesep(nstate,nload) =
     .              allEOPstatesep(nstate,nload) + EOPsep(nload)
              allEOPstaterib(nstate,nload) =
     .              allEOPstaterib(nstate,nload) + EOPrib(nload)
              allEOPstaterpa(nstate,nload) =
     .              allEOPstaterpa(nstate,nload) + EOPrpa(nload)
              allEOPstateatdep(nstate,nload) =
     .              allEOPstateatdep(nstate,nload) + EOPatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               EOP,loadname(nload),EOPwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               EOP,loadname(nload),EOPindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               EOP,loadname(nload),EOPcso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               EOP,loadname(nload),EOPsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               EOP,loadname(nload),EOPrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               EOP,loadname(nload),EOPrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOP,loadname(nload),EOPatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),EOP,area,acres
            allEOP(nloads+1) = allEOP(nloads+1) + acres
            allEOPatdep(nloads+1) = allEOPatdep(nloads+1) + acres
            allEOPstateatdep(nstate,nloads+1) =
     .              allEOPstateatdep(nstate,nloads+1) + acres
          !}
          end if

***************** EOF ***********************        
          if (doEOF) then
          !{
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
     .                               luname(nl),EOF,area,
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOF,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EOFwwtp,EOFindus,EOFcso,EOFsep,EOFrib,EOFrpa,
     O                    EOFatdep,acres)

            do nload = 1,nloads
              allEOF(nload) = allEOF(nload)
     .                      + EOFwwtp(nload)
     .                      + EOFindus(nload)
     .                      + EOFcso(nload)
     .                      + EOFsep(nload)
     .                      + EOFrib(nload)
     .                      + EOFrpa(nload)
     .                      + EOFatdep(nload)
              allEOFrseg(nrseg,nload) = allEOFrseg(nrseg,nload) 
     .                                + EOFwwtp(nload)
     .                                + EOFindus(nload)
     .                                + EOFcso(nload)
     .                                + EOFsep(nload)
     .                                + EOFrib(nload)
     .                                + EOFrpa(nload)
     .                                + EOFatdep(nload)
              allEOFlseg(nlseg,nload) = allEOFlseg(nlseg,nload) 
     .                                + EOFwwtp(nload)
     .                                + EOFindus(nload)
     .                                + EOFcso(nload)
     .                                + EOFsep(nload)
     .                                + EOFrib(nload)
     .                                + EOFrpa(nload)
     .                                + EOFatdep(nload)
              allEOFstate(nstate,nload) = allEOFstate(nstate,nload) 
     .                                  + EOFwwtp(nload)
     .                                  + EOFindus(nload)
     .                                  + EOFcso(nload)
     .                                  + EOFsep(nload)
     .                                  + EOFrib(nload)
     .                                  + EOFrpa(nload)
     .                                  + EOFatdep(nload)
              allEOFlrseg(nlrseg,nload) = allEOFlrseg(nlrseg,nload) 
     .                                  + EOFwwtp(nload)
     .                                  + EOFindus(nload)
     .                                  + EOFcso(nload)
     .                                  + EOFsep(nload)
     .                                  + EOFrib(nload)
     .                                  + EOFrpa(nload)
     .                                  + EOFatdep(nload)
              allEOFwwtp(nload) = allEOFwwtp(nload) + EOFwwtp(nload)
              allEOFindus(nload) = allEOFindus(nload) + EOFindus(nload)
              allEOFcso(nload) = allEOFcso(nload) + EOFcso(nload)
              allEOFsep(nload) = allEOFsep(nload) + EOFsep(nload)
              allEOFrib(nload) = allEOFrib(nload) + EOFrib(nload)
              allEOFrpa(nload) = allEOFrpa(nload) + EOFrpa(nload)
              allEOFatdep(nload) = allEOFatdep(nload) + EOFatdep(nload)
              allEOFstatewwtp(nstate,nload) = 
     .              allEOFstatewwtp(nstate,nload) + EOFwwtp(nload)
              allEOFstateindus(nstate,nload) =
     .              allEOFstateindus(nstate,nload) + EOFindus(nload)
              allEOFstatecso(nstate,nload) =
     .              allEOFstatecso(nstate,nload) + EOFcso(nload)
              allEOFstatesep(nstate,nload) = 
     .              allEOFstatesep(nstate,nload) + EOFsep(nload)
              allEOFstaterib(nstate,nload) =
     .              allEOFstaterib(nstate,nload) + EOFrib(nload)
              allEOFstaterpa(nstate,nload) =
     .              allEOFstaterpa(nstate,nload) + EOFrpa(nload)
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
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               EOF,loadname(nload),EOFrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               EOF,loadname(nload),EOFrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOF,loadname(nload),EOFatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),EOF,area,acres
            allEOF(nloads+1) = allEOF(nloads+1) + acres
            allEOFatdep(nloads+1) = allEOFatdep(nloads+1) + acres
            allEOFstateatdep(nstate,nloads+1) =
     .              allEOFstateatdep(nstate,nloads+1) + acres
          !}
          end if

***************** EOS ***********************
          if (doEOS) then
          !{
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
     .                               luname(nl),EOS,area,
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOS,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EOSwwtp,EOSindus,EOScso,EOSsep,EOSrib,EOSrpa,
     O                    EOSatdep,acres)

            do nload = 1,nloads
              allEOS(nload) = allEOS(nload)
     .                      + EOSwwtp(nload)
     .                      + EOSindus(nload)
     .                      + EOScso(nload)
     .                      + EOSsep(nload)
     .                      + EOSrib(nload)
     .                      + EOSrpa(nload)
     .                      + EOSatdep(nload)
              allEOSrseg(nrseg,nload) = allEOSrseg(nrseg,nload)
     .                                + EOSwwtp(nload)
     .                                + EOSindus(nload)
     .                                + EOScso(nload)
     .                                + EOSsep(nload)
     .                                + EOSrib(nload)
     .                                + EOSrpa(nload)
     .                                + EOSatdep(nload)
              allEOSlseg(nlseg,nload) = allEOSlseg(nlseg,nload)
     .                                + EOSwwtp(nload)
     .                                + EOSindus(nload)
     .                                + EOScso(nload)
     .                                + EOSsep(nload)
     .                                + EOSrib(nload)
     .                                + EOSrpa(nload)
     .                                + EOSatdep(nload)
              allEOSstate(nstate,nload) = allEOSstate(nstate,nload)
     .                                  + EOSwwtp(nload)
     .                                  + EOSindus(nload)
     .                                  + EOScso(nload)
     .                                  + EOSsep(nload)
     .                                  + EOSrib(nload)
     .                                  + EOSrpa(nload)
     .                                  + EOSatdep(nload)
              allEOSlrseg(nlrseg,nload) = allEOSlrseg(nlrseg,nload)
     .                                  + EOSwwtp(nload)
     .                                  + EOSindus(nload)
     .                                  + EOScso(nload)
     .                                  + EOSsep(nload)
     .                                  + EOSrib(nload)
     .                                  + EOSrpa(nload)
     .                                  + EOSatdep(nload)
              allEOSwwtp(nload) = allEOSwwtp(nload) + EOSwwtp(nload)
              allEOSindus(nload) = allEOSindus(nload) + EOSindus(nload)
              allEOScso(nload) = allEOScso(nload) + EOScso(nload)
              allEOSsep(nload) = allEOSsep(nload) + EOSsep(nload)
              allEOSrib(nload) = allEOSrib(nload) + EOSrib(nload)
              allEOSrpa(nload) = allEOSrpa(nload) + EOSrpa(nload)
              allEOSatdep(nload) = allEOSatdep(nload) + EOSatdep(nload)
              allEOSstatewwtp(nstate,nload) =
     .              allEOSstatewwtp(nstate,nload) + EOSwwtp(nload)
              allEOSstateindus(nstate,nload) =
     .              allEOSstateindus(nstate,nload) + EOSindus(nload)
              allEOSstatecso(nstate,nload) =
     .              allEOSstatecso(nstate,nload) + EOScso(nload)
              allEOSstatesep(nstate,nload) =
     .              allEOSstatesep(nstate,nload) + EOSsep(nload)
              allEOSstaterib(nstate,nload) =
     .              allEOSstaterib(nstate,nload) + EOSrib(nload)
              allEOSstaterpa(nstate,nload) =
     .              allEOSstaterpa(nstate,nload) + EOSrpa(nload)
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
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               EOS,loadname(nload),EOSrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               EOS,loadname(nload),EOSrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOS,loadname(nload),EOSatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),EOS,area,acres
            allEOS(nloads+1) = allEOS(nloads+1) + acres
            allEOSatdep(nloads+1) = allEOSatdep(nloads+1) + acres
            allEOSstateatdep(nstate,nloads+1) =
     .              allEOSstateatdep(nstate,nloads+1) + acres
          !}
          end if


***************** EOR ***********************
          if (doEOR) then
          !{
            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle
 
              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),EOR,year1,year2, !TODO EOS -> EOR
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          EORload,acres)

              do nload = 1,nloads  ! add loads
                allEOR(nload) = allEOR(nload) + EORload(nload)
                allEORrseg(nrseg,nload) = allEORrseg(nrseg,nload) 
     .                                  + EORload(nload)
                allEORlseg(nlseg,nload) = allEORlseg(nlseg,nload) 
     .                                  + EORload(nload)
                allEORstate(nstate,nload) = allEORstate(nstate,nload) 
     .                                    + EORload(nload)
                allEORlrseg(nlrseg,nload) = allEORlrseg(nlrseg,nload) 
     .                                    + EORload(nload)
                allEORlu(nl,nload) = allEORlu(nl,nload) + EORload(nload)
                allEORstatelu(nstate,nl,nload) = 
     .                   allEORstatelu(nstate,nl,nload) + EORload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),EOR,loadname(nload),
     .                                 EORload(nload)
              end do

              allEOR(nloads+1) = allEOR(nloads+1) + acres
              allEORrseg(nrseg,nloads+1) = allEORrseg(nrseg,nloads+1) 
     .                                   + acres
              allEORlseg(nlseg,nloads+1) = allEORlseg(nlseg,nloads+1) 
     .                                   + acres
              allEORstate(nstate,nloads+1)=allEORstate(nstate,nloads+1) 
     .                                    + acres
              allEORlrseg(nlrseg,nloads+1)=allEORlrseg(nlrseg,nloads+1) 
     .                                    + acres
              allEORlu(nl,nloads+1) = allEORlu(nl,nloads+1) + acres
              allEORstatelu(nstate,nl,nloads+1) = 
     .                    allEORstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),EOR,area,
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         EOR,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    EORwwtp,EORindus,EORcso,EORsep,EORrib,EORrpa,
     O                    EORatdep,acres)

            do nload = 1,nloads
              allEOR(nload) = allEOR(nload)
     .                      + EORwwtp(nload)
     .                      + EORindus(nload)
     .                      + EORcso(nload)
     .                      + EORsep(nload)
     .                      + EORrib(nload)
     .                      + EORrpa(nload)
     .                      + EORatdep(nload)
              allEORrseg(nrseg,nload) = allEORrseg(nrseg,nload)
     .                                + EORwwtp(nload)
     .                                + EORindus(nload)
     .                                + EORcso(nload)
     .                                + EORsep(nload)
     .                                + EORrib(nload)
     .                                + EORrpa(nload)
     .                                + EORatdep(nload)
              allEORlseg(nlseg,nload) = allEORlseg(nlseg,nload)
     .                                + EORwwtp(nload)
     .                                + EORindus(nload) 
     .                                + EORcso(nload)
     .                                + EORsep(nload)
     .                                + EORrib(nload)
     .                                + EORrpa(nload)
     .                                + EORatdep(nload)
              allEORstate(nstate,nload) = allEORstate(nstate,nload)
     .                                  + EORwwtp(nload)
     .                                  + EORindus(nload) 
     .                                  + EORcso(nload)
     .                                  + EORsep(nload)
     .                                  + EORrib(nload)
     .                                  + EORrpa(nload)
     .                                  + EORatdep(nload)
              allEORlrseg(nlrseg,nload) = allEORlrseg(nlrseg,nload)
     .                                  + EORwwtp(nload)
     .                                  + EORindus(nload) 
     .                                  + EORcso(nload)
     .                                  + EORsep(nload)
     .                                  + EORrib(nload)
     .                                  + EORrpa(nload)
     .                                  + EORatdep(nload)
              allEORwwtp(nload) = allEORwwtp(nload) + EORwwtp(nload)
              allEORindus(nload) = allEORindus(nload) + EORindus(nload)
              allEORcso(nload) = allEORcso(nload) + EORcso(nload)
              allEORsep(nload) = allEORsep(nload) + EORsep(nload)
              allEORrib(nload) = allEORrib(nload) + EORrib(nload)
              allEORrpa(nload) = allEORrpa(nload) + EORrpa(nload)
              allEORatdep(nload) = allEORatdep(nload) + EORatdep(nload)
              allEORstatewwtp(nstate,nload) = 
     .              allEORstatewwtp(nstate,nload) + EORwwtp(nload)
              allEORstateindus(nstate,nload) =
     .              allEORstateindus(nstate,nload) + EORindus(nload)
              allEORstatecso(nstate,nload) =
     .              allEORstatecso(nstate,nload) + EORcso(nload)
              allEORstatesep(nstate,nload) = 
     .              allEORstatesep(nstate,nload) + EORsep(nload)
              allEORstaterib(nstate,nload) =
     .              allEORstaterib(nstate,nload) + EORrib(nload)
              allEORstaterpa(nstate,nload) =
     .              allEORstaterpa(nstate,nload) + EORrpa(nload)
              allEORstateatdep(nstate,nload) = 
     .              allEORstateatdep(nstate,nload) + EORatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               EOR,loadname(nload),EORwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               EOR,loadname(nload),EORindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               EOR,loadname(nload),EORcso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               EOR,loadname(nload),EORsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               EOR,loadname(nload),EORrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               EOR,loadname(nload),EORrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               EOR,loadname(nload),EORatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),EOR,area,acres
            allEOR(nloads+1) = allEOR(nloads+1) + acres
            allEORatdep(nloads+1) = allEORatdep(nloads+1) + acres
            allEORstateatdep(nstate,nloads+1) =
     .              allEORstateatdep(nstate,nloads+1) + acres
          !}
          end if

***************** DEL ***********************
          if (doDEL) then
          !{
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
     .                               luname(nl),DEL,area,
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         DEL,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    DELwwtp,DELindus,DELcso,DELsep,DELrib,DELrpa,
     O                    DELatdep,acres)

            do nload = 1,nloads
              allDEL(nload) = allDEL(nload)
     .                      + DELwwtp(nload)
     .                      + DELindus(nload)
     .                      + DELcso(nload)
     .                      + DELsep(nload)
     .                      + DELrib(nload)
     .                      + DELrpa(nload)
     .                      + DELatdep(nload)
              allDELrseg(nrseg,nload) = allDELrseg(nrseg,nload)
     .                                + DELwwtp(nload)
     .                                + DELindus(nload) 
     .                                + DELcso(nload)
     .                                + DELsep(nload)
     .                                + DELrib(nload)
     .                                + DELrpa(nload)
     .                                + DELatdep(nload)
              allDELlseg(nlseg,nload) = allDELlseg(nlseg,nload)
     .                                + DELwwtp(nload)
     .                                + DELindus(nload) 
     .                                + DELcso(nload)
     .                                + DELsep(nload)
     .                                + DELrib(nload)
     .                                + DELrpa(nload)
     .                                + DELatdep(nload)
              allDELstate(nstate,nload) = allDELstate(nstate,nload)
     .                                  + DELwwtp(nload)
     .                                  + DELindus(nload) 
     .                                  + DELcso(nload)
     .                                  + DELsep(nload)
     .                                  + DELrib(nload)
     .                                  + DELrpa(nload)
     .                                  + DELatdep(nload)
              allDELlrseg(nlrseg,nload) = allDELlrseg(nlrseg,nload) 
     .                                  + DELwwtp(nload)
     .                                  + DELindus(nload)
     .                                  + DELcso(nload)
     .                                  + DELsep(nload)
     .                                  + DELrib(nload)
     .                                  + DELrpa(nload)
     .                                  + DELatdep(nload)
              allDELwwtp(nload) = allDELwwtp(nload) + DELwwtp(nload)
              allDELindus(nload) = allDELindus(nload) + DELindus(nload)
              allDELcso(nload) = allDELcso(nload) + DELcso(nload)
              allDELsep(nload) = allDELsep(nload) + DELsep(nload)
              allDELrib(nload) = allDELrib(nload) + DELrib(nload)
              allDELrpa(nload) = allDELrpa(nload) + DELrpa(nload)
              allDELatdep(nload) = allDELatdep(nload) + DELatdep(nload)
              allDELstatewwtp(nstate,nload) =
     .              allDELstatewwtp(nstate,nload) + DELwwtp(nload)
              allDELstateindus(nstate,nload) =
     .              allDELstateindus(nstate,nload) + DELindus(nload)
              allDELstatecso(nstate,nload) = 
     .              allDELstatecso(nstate,nload) + DELcso(nload)
              allDELstatesep(nstate,nload) = 
     .              allDELstatesep(nstate,nload) + DELsep(nload)
              allDELstaterib(nstate,nload) =
     .              allDELstaterib(nstate,nload) + DELrib(nload)
              allDELstaterpa(nstate,nload) =
     .              allDELstaterpa(nstate,nload) + DELrpa(nload)
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
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               DEL,loadname(nload),DELrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               DEL,loadname(nload),DELrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               DEL,loadname(nload),DELatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),DEL,area,acres
            allDEL(nloads+1) = allDEL(nloads+1) + acres
            allDELatdep(nloads+1) = allDELatdep(nloads+1) + acres
            allDELstateatdep(nstate,nloads+1) =
     .              allDELstateatdep(nstate,nloads+1) + acres
          !}
          end if

***************** BAY ***********************
          if (doBAY) then
          !{
            do nl = 1,nlu

              if (luname(nl).eq.'wat') cycle

              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),DEL,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          DELload,acres)
              call readEOaveann(
     I                          rscen,lsegs(nlseg),rsegs(nrseg),
     I                          luname(nl),BAY,year1,year2,
     I                          nloads,loadname,nloadmax,DFmethod,
     O                          BAYload,acres)

              do nload = 1,nloads  ! add loads

                BAYload(nload) = DELload(nload) - BAYload(nload)
                allBAY(nload) = allBAY(nload)
     .                                 + BAYload(nload)
                allBAYrseg(nrseg,nload) = allBAYrseg(nrseg,nload)
     .                                 + BAYload(nload)
                allBAYlseg(nlseg,nload) = allBAYlseg(nlseg,nload)
     .                                 + BAYload(nload)
                allBAYstate(nstate,nload) = allBAYstate(nstate,nload)
     .                                 + BAYload(nload)
                allBAYlrseg(nlrseg,nload) = allBAYlrseg(nlrseg,nload)
     .                                 + BAYload(nload)
                allBAYlu(nl,nload) = allBAYlu(nl,nload)
     .                                 + BAYload(nload)
                allBAYstatelu(nstate,nl,nload) =
     .                   allBAYstatelu(nstate,nl,nload)
     .                                 + BAYload(nload)
                write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                                 luname(nl),BAY,loadname(nload),
     .                                 BAYload(nload)
              end do

              allBAY(nloads+1) = allBAY(nloads+1) + acres
              allBAYrseg(nrseg,nloads+1) = allBAYrseg(nrseg,nloads+1)
     .                                   + acres
              allBAYlseg(nlseg,nloads+1) = allBAYlseg(nlseg,nloads+1)
     .                                   + acres
              allBAYstate(nstate,nloads+1)=allBAYstate(nstate,nloads+1)
     .                                    + acres
              allBAYlrseg(nlrseg,nloads+1)=allBAYlrseg(nlrseg,nloads+1)
     .                                    + acres
              allBAYlu(nl,nloads+1) = allBAYlu(nl,nloads+1) + acres
              allBAYstatelu(nstate,nl,nloads+1) =
     .                    allBAYstatelu(nstate,nl,nloads+1) + acres
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .                               luname(nl),BAY,area,
     .                               acres
            end do

            call readDATaveann(
     I                         rscen,lsegs(nlseg),rsegs(nrseg),
     I                         DEL,year1,year2,
     I                         nloads,loadname,nloadmax,DFmethod,
     O                    DELwwtp,DELindus,DELcso,DELsep,DELrib,DELrpa,
     O                    DELatdep,acres)

            do nload = 1,nloads

              BAYwwtp(nload)  = DELwwtp(nload)
              BAYindus(nload) = DELindus(nload)
              BAYcso(nload)   = DELcso(nload)
              BAYsep(nload)   = DELsep(nload)
              BAYrib(nload)   = DELrib(nload)
              BAYrpa(nload)   = DELrpa(nload)
              BAYatdep(nload) = DELatdep(nload)

              allBAY(nload) = allBAY(nload)
     .                      + BAYwwtp(nload)
     .                      + BAYindus(nload)
     .                      + BAYcso(nload)
     .                      + BAYsep(nload)
     .                      + BAYrib(nload)
     .                      + BAYrpa(nload)
     .                      + BAYatdep(nload)
              allBAYrseg(nrseg,nload) = allBAYrseg(nrseg,nload)
     .                                + BAYwwtp(nload)
     .                                + BAYindus(nload)
     .                                + BAYcso(nload)
     .                                + BAYsep(nload)
     .                                + BAYrib(nload)
     .                                + BAYrpa(nload)
     .                                + BAYatdep(nload)
              allBAYlseg(nlseg,nload) = allBAYlseg(nlseg,nload)
     .                                + BAYwwtp(nload)
     .                                + BAYindus(nload)
     .                                + BAYcso(nload)
     .                                + BAYsep(nload)
     .                                + BAYrib(nload)
     .                                + BAYrpa(nload)
     .                                + BAYatdep(nload)
              allBAYstate(nstate,nload) = allBAYstate(nstate,nload)
     .                                  + BAYwwtp(nload)
     .                                  + BAYindus(nload)
     .                                  + BAYcso(nload)
     .                                  + BAYsep(nload)
     .                                  + BAYrib(nload)
     .                                  + BAYrpa(nload)
     .                                  + BAYatdep(nload)
              allBAYlrseg(nlrseg,nload) = allBAYlrseg(nlrseg,nload)
     .                                  + BAYwwtp(nload)
     .                                  + BAYindus(nload)
     .                                  + BAYcso(nload)
     .                                  + BAYsep(nload)
     .                                  + BAYrib(nload)
     .                                  + BAYrpa(nload)
     .                                  + BAYatdep(nload)
              allBAYwwtp(nload) = allBAYwwtp(nload) + BAYwwtp(nload)
              allBAYindus(nload) = allBAYindus(nload) + BAYindus(nload)
              allBAYcso(nload) = allBAYcso(nload) + BAYcso(nload)
              allBAYsep(nload) = allBAYsep(nload) + BAYsep(nload)
              allBAYrib(nload) = allBAYrib(nload) + BAYrib(nload)
              allBAYrpa(nload) = allBAYrpa(nload) + BAYrpa(nload)
              allBAYatdep(nload) = allBAYatdep(nload) + BAYatdep(nload)
              allBAYstatewwtp(nstate,nload) =
     .              allBAYstatewwtp(nstate,nload) + BAYwwtp(nload)
              allBAYstateindus(nstate,nload) =
     .              allBAYstateindus(nstate,nload) + BAYindus(nload)
              allBAYstatecso(nstate,nload) =
     .              allBAYstatecso(nstate,nload) + BAYcso(nload)
              allBAYstatesep(nstate,nload) =
     .              allBAYstatesep(nstate,nload) + BAYsep(nload)
              allBAYstaterib(nstate,nload) =
     .              allBAYstaterib(nstate,nload) + BAYrib(nload)
              allBAYstaterpa(nstate,nload) =
     .              allBAYstaterpa(nstate,nload) + BAYrpa(nload)
              allBAYstateatdep(nstate,nload) =
     .              allBAYstateatdep(nstate,nload) + BAYatdep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),wwtp,
     .                               BAY,loadname(nload),BAYwwtp(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),indus,
     .                               BAY,loadname(nload),BAYindus(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),cso,
     .                               BAY,loadname(nload),BAYcso(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),sep,
     .                               BAY,loadname(nload),BAYsep(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rib,
     .                               BAY,loadname(nload),BAYrib(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),rpa,
     .                               BAY,loadname(nload),BAYrpa(nload)
              write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),atdep,
     .                               BAY,loadname(nload),BAYatdep(nload)
            end do

            write(fnAllLoads,1232) lsegs(nlseg),rsegs(nrseg),
     .            atdep(:lenatdep),BAY,area,acres
            allBAY(nloads+1) = allBAY(nloads+1) + acres
            allBAYatdep(nloads+1) = allBAYatdep(nloads+1) + acres
            allBAYstateatdep(nstate,nloads+1) =
     .              allBAYstateatdep(nstate,nloads+1) + acres
          !}
          end if
        !}
        end do   ! loop over lsegs in rseg
      !}
      end do   ! loop over rsegs in basin

      print*,' '
      close (fnAllLoads)
      print*,fnam(:78)


****************** write output ****************************************
************ write EOP output
      if (doEOP) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_'//cEOP//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951)'specifier',
     .                 (',',loadname(nload),nload=1,nloads),',',area

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allEOPlu(nl,nload),nload=1,nloads+1)
        end do
        write(11,1234,err=951) wwtp,
     .                         (',',allEOPwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allEOPindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allEOPcso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allEOPsep(nload),nload=1,nloads)
        write(11,1234,err=951) rib,
     .                         (',',allEOPrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allEOPrpa(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allEOPatdep(nload),nload=1,nloads+1)

        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allEOPrseg(nrseg,nload),nload=1,nloads+1)
        end do

        do nlseg = 1,nlsegs
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allEOPlseg(nlseg,nload),nload=1,nloads+1)
        end do

        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allEOPlrseg(nlrseg,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allEOPstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allEOPstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allEOPstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allEOPstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allEOPstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allEOPstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allEOPstaterpa(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allEOPstateatdep(nstate,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allEOPstate(nstate,nload),nload=1,nloads+1)
        end do

        write(11,1234,err=951) 'total',
     .                         (',',allEOP(nload),nload=1,nloads+1)

        close(11)

      end if

************ write EOF output
      if (doEOF) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_'//cEOF//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951)'specifier',
     .                 (',',loadname(nload),nload=1,nloads),',',area

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
        write(11,1234,err=951) rib,
     .                         (',',allEOFrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allEOFrpa(nload),nload=1,nloads)
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
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allEOFstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allEOFstaterpa(nstate,nload),nload=1,nloads)
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
     .         rscen(:lenrscen)//'_'//cEOS//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                          ',',area

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
        write(11,1234,err=951) rib,
     .                         (',',allEOSrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allEOSrpa(nload),nload=1,nloads)
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
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allEOSstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allEOSstaterpa(nstate,nload),nload=1,nloads)
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

************ write EOR output
      if (doEOR) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_'//cEOR//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                          ',',area

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allEORlu(nl,nload),nload=1,nloads+1)
        end do 
        write(11,1234,err=951) wwtp,
     .                         (',',allEORwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allEORindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allEORcso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allEORsep(nload),nload=1,nloads)
        write(11,1234,err=951) rib,
     .                         (',',allEORrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allEORrpa(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allEORatdep(nload),nload=1,nloads+1)
          
        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allEORrseg(nrseg,nload),nload=1,nloads+1)
        end do                 
        
        do nlseg = 1,nlsegs    
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allEORlseg(nlseg,nload),nload=1,nloads+1)
        end do
     
        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allEORlrseg(nlrseg,nload),nload=1,nloads+1)
        end do
          
        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allEORstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do    
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allEORstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allEORstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allEORstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allEORstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allEORstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allEORstaterpa(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allEORstateatdep(nstate,nload),nload=1,nloads+1)
        end do
     
        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allEORstate(nstate,nload),nload=1,nloads+1)
        end do
     
        write(11,1234,err=951) 'total',
     .                         (',',allEOR(nload),nload=1,nloads+1)
          
        close(11) 
          
      end if      
        
************ write DEL output
      if (doDEL) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_'//cDEL//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                           ',',area

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
        write(11,1234,err=951) rib,
     .                         (',',allDELrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allDELrpa(nload),nload=1,nloads)
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
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allDELstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allDELstaterpa(nstate,nload),nload=1,nloads)
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

************ write BAY output
      if (doBAY) then
        fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .         '/'//basin(:lenbasin)//'_'//cy1//'_'//cy2//'_'//
     .         rscen(:lenrscen)//'_'//cBAY//'.csv'
        print*,fnam(:78)
        open(11,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

        write(11,1233,err=951) 'specifier',
     .                         (',',loadname(nload),nload=1,nloads),
     .                           ',',area

        do nl = 1,nlu
          if (luname(nl).eq.'wat') cycle
          write(11,1234,err=951) luname(nl),
     .                 (',',allBAYlu(nl,nload),nload=1,nloads+1)
        end do
        write(11,1234,err=951) wwtp,
     .                         (',',allBAYwwtp(nload),nload=1,nloads)
        write(11,1234,err=951) indus,
     .                         (',',allBAYindus(nload),nload=1,nloads)
        write(11,1234,err=951) cso,
     .                         (',',allBAYcso(nload),nload=1,nloads)
        write(11,1234,err=951) sep,
     .                         (',',allBAYsep(nload),nload=1,nloads)
        write(11,1234,err=951) rib,
     .                         (',',allBAYrib(nload),nload=1,nloads)
        write(11,1234,err=951) rpa,
     .                         (',',allBAYrpa(nload),nload=1,nloads)
        write(11,1234,err=951) atdep,
     .                         (',',allBAYatdep(nload),nload=1,nloads+1)

        do nrseg = 1,nrsegs
          write(11,1234,err=951) rsegs(nrseg),
     .                 (',',allBAYrseg(nrseg,nload),nload=1,nloads+1)
        end do

        do nlseg = 1,nlsegs
          write(11,1234,err=951) lsegs(nlseg),
     .              (',',allBAYlseg(nlseg,nload),nload=1,nloads+1)
        end do

        do nlrseg = 1,nlrsegs
          write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .              (',',allBAYlrseg(nlrseg,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          do nl = 1,nlu
            if (luname(nl).eq.'wat') cycle
            write(11,1234,err=951) states(nstate)//'-'//luname(nl),
     .           (',',allBAYstatelu(nstate,nl,nload),nload=1,nloads+1)
          end do
          write(11,1234,err=951) states(nstate)//'-'//wwtp,
     .            (',',allBAYstatewwtp(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//indus,
     .            (',',allBAYstateindus(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//cso,
     .            (',',allBAYstatecso(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//sep,
     .            (',',allBAYstatesep(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rib,
     .            (',',allBAYstaterib(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//rpa,
     .            (',',allBAYstaterpa(nstate,nload),nload=1,nloads)
          write(11,1234,err=951) states(nstate)//'-'//atdep,
     .            (',',allBAYstateatdep(nstate,nload),nload=1,nloads+1)
        end do

        do nstate = 1,nstates
          write(11,1234,err=951) states(nstate),
     .              (',',allBAYstate(nstate,nload),nload=1,nloads+1)
        end do

        write(11,1234,err=951)
     .        'total',(',',allBAY(nload),nload=1,nloads+1)

        close(11)

      end if

      stop

1232  format(A6,',',A13,',',A3,',',A3,',',A4,',',E14.7)
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


