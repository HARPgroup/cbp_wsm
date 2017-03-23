************************************************************************
** program to summarize input in the ./output/input diretory          **
**  the input is reflected by lseg                                    **
**                                                                    **
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


******** define load variables, only do TN and TP
      integer nloads
      parameter (nloads = 2)    ! maximum number of loads
      character*4 loadname(nloads)  ! name of each load (e.g. 'totn')
      data loadname / 'TOTN','TOTP'/

*********** state variables
      integer nstatemax,nstate,nstates
      parameter (nstatemax = 10) ! number of jurisdictions
      character*2 states(nstatemax)  ! state code (24,42,36, ect)

********* land scenarios
      character*25 lscens(nlu)

********* reading variables for loads in lb/ac
      real fert(nloads) 
      real manure(nloads) 
      real legume(nloads) 
      real uptake(nloads) 
      real atdep(nloads)

********* reading variable for land use acres by type
      real acres(nlu)

********** accumulations of load
      real allfertlu(nlu,nloads)
      real allmanurelu(nlu,nloads)
      real alllegumelu(nlu,nloads)
      real alluptakelu(nlu,nloads)
      real allatdeplu(nlu,nloads)
      real allacreslu(nlu)
      real allfertrseg(maxrsegs,nloads)
      real allmanurerseg(maxrsegs,nloads)
      real alllegumerseg(maxrsegs,nloads)
      real alluptakerseg(maxrsegs,nloads)
      real allatdeprseg(maxrsegs,nloads)
      real allacresrseg(maxrsegs)
      real allfertlseg(maxlsegs,nloads)
      real allmanurelseg(maxlsegs,nloads)
      real alllegumelseg(maxlsegs,nloads)
      real alluptakelseg(maxlsegs,nloads)
      real allatdeplseg(maxlsegs,nloads)
      real allacreslseg(maxlsegs)
      real allfertlrseg(maxrsegs*maxL2R,nloads)
      real allmanurelrseg(maxrsegs*maxL2R,nloads)
      real alllegumelrseg(maxrsegs*maxL2R,nloads)
      real alluptakelrseg(maxrsegs*maxL2R,nloads)
      real allatdeplrseg(maxrsegs*maxL2R,nloads)
      real allacreslrseg(maxrsegs*maxL2R)
      real allfertstate(nstatemax,nloads)
      real allmanurestate(nstatemax,nloads)
      real alllegumestate(nstatemax,nloads)
      real alluptakestate(nstatemax,nloads)
      real allatdepstate(nstatemax,nloads)
      real allacresstate(nstatemax)
      real allfert(nloads)
      real allmanure(nloads)
      real alllegume(nloads)
      real alluptake(nloads)
      real allatdep(nloads)
      real allacres

********** indices and accounting variables
      integer nr,ns,nl,nls,nlseg,nrseg,nlrseg,nload   ! indices

      integer numsegs ! number of lsegs associated with an rseg

      integer nlrsegs  ! total land river segs in basin
      character*13 lrrsegs(maxrsegs*maxL2R)
      character*6 lrlsegs(maxrsegs*maxL2R) 

      character*200 basin

      integer lenbasin

      logical found

      integer year1,year2
      character*4 cy1,cy2

      integer wdmrch
      parameter (wdmrch=dfile+5)    ! file number for fake wdm

************ END DECLARATIONS ******************************************

      read(*,*,err=997,end=998) rscen,basin,year1,year2
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      call lencl(basin,lenbasin)
      call lencl(rscen,lenrscen) 

      print*,'summarizing average annual input for scenario ',
     .       rscen(:lenrscen),' river basin ',basin(:lenbasin)

********  open FAKE WDM file, used ONLY to solve the problem of
*******       opening and closing WDM file, never close this file
      fnam = dummyWDMname
      call wdbopnlong(wdmrch,fnam,0,err)
      if (err .ne. 0) go to 996

******** get land scenarios
      call readcontrol_lscen(rscen,
     O                       lscens)
      
************* read in river segments
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

************ initialize variables 
      do nload = 1,nloads
        do nl = 1,nlu
          allfertlu(nl,nload) = 0.0
          allmanurelu(nl,nload) = 0.0
          alllegumelu(nl,nload) = 0.0
          alluptakelu(nl,nload) = 0.0
          allatdeplu(nl,nload) = 0.0
          allacreslu(nl) = 0.0
        end do
  
        do nrseg = 1,nrsegs
          allfertrseg(nrseg,nload) = 0.0
          allmanurerseg(nrseg,nload) = 0.0
          alllegumerseg(nrseg,nload) = 0.0
          alluptakerseg(nrseg,nload) = 0.0
          allatdeprseg(nrseg,nload) = 0.0
          allacresrseg(nrseg) = 0.0
        end do

        do nlseg = 1,nlsegs
          allfertlseg(nlseg,nload) = 0.0
          allmanurelseg(nlseg,nload) = 0.0
          alllegumelseg(nlseg,nload) = 0.0
          alluptakelseg(nlseg,nload) = 0.0
          allatdeplseg(nlseg,nload) = 0.0
          allacreslseg(nlseg) = 0.0
        end do

        do nlrseg = 1,nlrsegs
          allfertlrseg(nlrseg,nload) = 0.0
          allmanurelrseg(nlrseg,nload) = 0.0
          alllegumelrseg(nlrseg,nload) = 0.0
          alluptakelrseg(nlrseg,nload) = 0.0
          allatdeplrseg(nlrseg,nload) = 0.0
          allacreslrseg(nlrseg) = 0.0
        end do

        do nstate = 1,nstates
          allfertstate(nstate,nload) = 0.0
          allmanurestate(nstate,nload) = 0.0
          alllegumestate(nstate,nload) = 0.0
          alluptakestate(nstate,nload) = 0.0
          allatdepstate(nstate,nload) = 0.0
          allacresstate(nstate) = 0.0
        end do

        allfert(nload) = 0.0
        allmanure(nload) = 0.0
        alllegume(nload) = 0.0
        alluptake(nload) = 0.0
        allatdep(nload) = 0.0
        allacres = 0.0
      end do

*********** populate variables by reading ASCII output
************ loop over river segs
      nlsegs = 0
      nstates = 0
      nlrsegs = 0
      do nrseg = 1,nrsegs
        call ttyput(rsegs(nrseg)//' ')
        call getl2r(
     I              rsegs(nrseg),rscen,lenrscen,
     O              numsegs,l2r)

************** loop over land segs in river seg
*********** find indices for lseg, rseg, nlseg
************ get atdep for all, get acres, get input loads by lu
************* store in appropriate variables
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

********** got indices, now get data        
************* get land use acres by lu type
          call getacres(
     I                  rscen,lsegs(nlseg),rsegs(nrseg),year1,year2,
     O                  acres)

************ get atmospheric deposition by load type
          call getAtdepAveann(
     I                        rscen,lsegs(nlseg),year1,year2,
     I                        nloads,loadname,
     O                        atdep)

          do nl = 1,nlu

            call readInAveann(
     I                        lscens(nl),lsegs(nlseg),
     I                        luname(nl),year1,year2,
     I                        nloads,loadname,
     O                        fert,manure,legume,uptake)

            do nload = 1,nloads  ! add loads
              alluptake(nload) = alluptake(nload) 
     .                         + uptake(nload) * acres(nl)
              alluptakerseg(nrseg,nload) = alluptakerseg(nrseg,nload) 
     .                                 + uptake(nload) * acres(nl)
              alluptakelseg(nlseg,nload) = alluptakelseg(nlseg,nload) 
     .                                 + uptake(nload) * acres(nl)
              alluptakestate(nstate,nload)=alluptakestate(nstate,nload) 
     .                                   + uptake(nload) * acres(nl)
              alluptakelrseg(nlrseg,nload)=alluptakelrseg(nlrseg,nload) 
     .                                   + uptake(nload) * acres(nl)
              alluptakelu(nl,nload) = alluptakelu(nl,nload)
     .                              + uptake(nload) * acres(nl)

              alllegume(nload) = alllegume(nload) 
     .                         + legume(nload) * acres(nl)
              alllegumerseg(nrseg,nload) = alllegumerseg(nrseg,nload) 
     .                                 + legume(nload) * acres(nl)
              alllegumelseg(nlseg,nload) = alllegumelseg(nlseg,nload) 
     .                                 + legume(nload) * acres(nl)
              alllegumestate(nstate,nload)=alllegumestate(nstate,nload) 
     .                                   + legume(nload) * acres(nl)
              alllegumelrseg(nlrseg,nload)=alllegumelrseg(nlrseg,nload) 
     .                                   + legume(nload) * acres(nl)
              alllegumelu(nl,nload) = alllegumelu(nl,nload)
     .                              + legume(nload) * acres(nl)
 
              allmanure(nload) = allmanure(nload) 
     .                         + manure(nload) * acres(nl)
              allmanurerseg(nrseg,nload) = allmanurerseg(nrseg,nload) 
     .                                 + manure(nload) * acres(nl)
              allmanurelseg(nlseg,nload) = allmanurelseg(nlseg,nload) 
     .                                 + manure(nload) * acres(nl)
              allmanurestate(nstate,nload)=allmanurestate(nstate,nload) 
     .                                   + manure(nload) * acres(nl)
              allmanurelrseg(nlrseg,nload)=allmanurelrseg(nlrseg,nload) 
     .                                   + manure(nload) * acres(nl)
              allmanurelu(nl,nload) = allmanurelu(nl,nload)
     .                              + manure(nload) * acres(nl)

              allfert(nload) = allfert(nload) + fert(nload) * acres(nl)
              allfertrseg(nrseg,nload) = allfertrseg(nrseg,nload) 
     .                                 + fert(nload) * acres(nl)
              allfertlseg(nlseg,nload) = allfertlseg(nlseg,nload) 
     .                                 + fert(nload) * acres(nl)
              allfertstate(nstate,nload) = allfertstate(nstate,nload) 
     .                                   + fert(nload) * acres(nl)
              allfertlrseg(nlrseg,nload) = allfertlrseg(nlrseg,nload) 
     .                                   + fert(nload) * acres(nl)
              allfertlu(nl,nload) = allfertlu(nl,nload) 
     .                            + fert(nload) * acres(nl)

              allatdep(nload) = allatdep(nload) 
     .                        + atdep(nload) * acres(nl)
              allatdeprseg(nrseg,nload) = allatdeprseg(nrseg,nload) 
     .                                 + atdep(nload) * acres(nl)
              allatdeplseg(nlseg,nload) = allatdeplseg(nlseg,nload) 
     .                                 + atdep(nload) * acres(nl)
              allatdepstate(nstate,nload) = allatdepstate(nstate,nload) 
     .                                   + atdep(nload) * acres(nl)
              allatdeplrseg(nlrseg,nload) = allatdeplrseg(nlrseg,nload) 
     .                                   + atdep(nload) * acres(nl)
              allatdeplu(nl,nload) = allatdeplu(nl,nload) 
     .                            + atdep(nload) * acres(nl)
            end do  ! loop over loads

            allacres = allacres + acres(nl)
            allacresrseg(nrseg) = allacresrseg(nrseg) + acres(nl)
            allacreslseg(nlseg) = allacreslseg(nlseg) + acres(nl)
            allacresstate(nstate)=allacresstate(nstate) + acres(nl)
            allacreslrseg(nlrseg)=allacreslrseg(nlrseg) + acres(nl)
            allacreslu(nl) = allacreslu(nl) + acres(nl)

          end do   ! end loop over land uses
        end do   ! loop over lsegs in rseg
      end do   ! loop over rsegs in basin
      print*,' '
           
************ write output
      fnam = sumoutdir//'aveann/'//rscen(:lenrscen)//
     .     '/'//basin(:lenbasin)//'_inputs_'//
     .     rscen(:lenrscen)//'_'//cy1//'_'//cy2//'.csv'
      print*,fnam(:78)
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,1233,err=951)'specifier',
     .                (', fert ',loadname(nload),nload=1,nloads),
     .                (', manure ',loadname(nload),nload=1,nloads),
     .                (', legume ',loadname(nload),nload=1,nloads),
     .                (', uptake ',loadname(nload),nload=1,nloads),
     .                (', atdep ',loadname(nload),nload=1,nloads),
     .                 ',','acres'

      do nl = 1,nlu
        write(11,1234,err=951) luname(nl),
     .                (',',allfertlu(nl,nload),nload=1,nloads),
     .                (',',allmanurelu(nl,nload),nload=1,nloads),
     .                (',',alllegumelu(nl,nload),nload=1,nloads),
     .                (',',alluptakelu(nl,nload),nload=1,nloads),
     .                (',',allatdeplu(nl,nload),nload=1,nloads),
     .                 ',',allacreslu(nl)
      end do
  
      do nrseg = 1,nrsegs
        write(11,1234,err=951) rsegs(nrseg),
     .                (',',allfertrseg(nrseg,nload),nload=1,nloads),
     .                (',',allmanurerseg(nrseg,nload),nload=1,nloads),
     .                (',',alllegumerseg(nrseg,nload),nload=1,nloads),
     .                (',',alluptakerseg(nrseg,nload),nload=1,nloads),
     .                (',',allatdeprseg(nrseg,nload),nload=1,nloads),
     .                 ',',allacresrseg(nrseg)
      end do

      do nlseg = 1,nlsegs
        write(11,1234,err=951) lsegs(nlseg),
     .                (',',allfertlseg(nlseg,nload),nload=1,nloads),
     .                (',',allmanurelseg(nlseg,nload),nload=1,nloads),
     .                (',',alllegumelseg(nlseg,nload),nload=1,nloads),
     .                (',',alluptakelseg(nlseg,nload),nload=1,nloads),
     .                (',',allatdeplseg(nlseg,nload),nload=1,nloads),
     .                 ',',allacreslseg(nlseg)
      end do

      do nlrseg = 1,nlrsegs
        write(11,1234,err=951) lrrsegs(nlrseg)//'-'//lrlsegs(nlrseg),
     .                (',',allfertlrseg(nlrseg,nload),nload=1,nloads),
     .                (',',allmanurelrseg(nlrseg,nload),nload=1,nloads),
     .                (',',alllegumelrseg(nlrseg,nload),nload=1,nloads),
     .                (',',alluptakelrseg(nlrseg,nload),nload=1,nloads),
     .                (',',allatdeplrseg(nlrseg,nload),nload=1,nloads),
     .                 ',',allacreslrseg(nlrseg)
      end do

      do nstate = 1,nstates
        write(11,1234,err=951) states(nstate),
     .                (',',allfertstate(nstate,nload),nload=1,nloads),
     .                (',',allmanurestate(nstate,nload),nload=1,nloads),
     .                (',',alllegumestate(nstate,nload),nload=1,nloads),
     .                (',',alluptakestate(nstate,nload),nload=1,nloads),
     .                (',',allatdepstate(nstate,nload),nload=1,nloads),
     .                 ',',allacresstate(nstate)
      end do

      write(11,1234,err=951) 'total',
     .                (',',allfert(nload),nload=1,nloads),
     .                (',',allmanure(nload),nload=1,nloads),
     .                (',',alllegume(nload),nload=1,nloads),
     .                (',',alluptake(nload),nload=1,nloads),
     .                (',',allatdep(nload),nload=1,nloads),
     .                 ',',allacres

      close(11)

      stop

1233  format(a,10(a,a4),a1,a5)
1234  format(a,11(a1,e14.7))

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

996   report(1) = 'Error: opening wdm= '
      report(2) = fnam
      if (err.lt.0) then
        report(3) = 'Error = '
        write(report(3)(9:11),'(i3)') err
      else
        report(3) = 'Not a wdm file'
      end if
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


