************************************************************************
** Program to generate output from the land simulation                **
**     and to populate eos wdms with river input variables            **
**  wdms are stored in ./wdm/river/$scen/eos/$seg.wdm                 **
**  this program is dependent on the binary etm files that are        **
**  generated during the ETM run                                      **
**    general strategy:                                               **
**      Run the program once for each river or bfl subseg             **
**      For each land segment in the river                            **
**         for each land use in the segment                           **
**           determine the dsns for each river output variable        **
**           extracting relevant dsns                                 **
**           add to the appropriate river variable                    **
**      write appropriate output files (eos, wqm)                     **
************************************************************************

      implicit none
      include 'land.inc'
      integer year,month,day

****************  daily load
      real dEOPrvar(ndaymax,maxRvar) ! daily by river var
      real dEOFrvar(ndaymax,maxRvar)
      real dEOSrvar(ndaymax,maxRvar) ! daily by river var
      real dEORrvar(ndaymax,maxRvar)
      real dBAYrvar(ndaymax,maxRvar)

      real dEOPload(ndaymax,nloadmax)     ! daily by load var
      real dEOFload(ndaymax,nloadmax)
      real dEOSload(ndaymax,nloadmax)     ! daily by load var
      real dEORload(ndaymax,nloadmax)
      real dBAYload(ndaymax,nloadmax)

************ read binary etm file
      real tlufac
      real lufac(ndaymax,nlu)            ! land use acres
      real bmpfac(ndaymax,maxBMPcon,nlu)
c      real tranfac(maxBMPcon,nlu)
      real tranfacL2W(maxBMPcon,nlu)
      real tranfacS2R(maxBMPcon,nlu)
      real dailypounds(ndaymax,maxBMPcon,nlu)
      real dailypoundsEOT(ndaymax,maxBMPcon,nlu)
      character*50 LStest             ! test the etm file

      real acres(ndaymax)  ! time series of acres for EOS files
      real fac1,fac2      ! convenience factors
      real hourload

      character*3 c3varid
      real varfac ! variable factor for land to river

************** PS only variables
      integer npssegs
      character*6 psl2r(maxL2R)

************** for wdm generation
      real hvaltemp(ndaymax*24,maxRvar) ! temp variable for river

      integer l, Rvar, Lvar, nv, hour, nc, nl, ns          ! indices
      integer i, nlconi, nbi, nb
      integer inv, nv1, nv2
      integer ndaysinmonth
      external  ndaysinmonth

      integer wdmrch,wdmlnd,etmfil,outfil  ! file numbers
      parameter (etmfil=dfile+1)
      parameter (wdmrch=etmfil+1)
      parameter (wdmlnd=wdmrch+1)
      parameter (outfil=wdmlnd+nlu+1)

      integer sdate(ndate),edate(ndate) ! start and end dates
      integer jday,jdaytot,ndays    ! julian day determined from hours

      character*3 cRvar     ! character representation of integers
      character*6 cfacL2W,cfacS2R
 
      logical dothislu(maxL2R,nlu)     ! decide to use this land use
      logical isastrlu(nlu)
      logical lus_annload(nlu)
      logical lus_avgload(nlu)
      logical lus_xlbsbmp(nlu)
      logical lus_nonphys(nlu)

      integer iETM  ! flag to generate river input
      integer iEOFdaily,iEOFmonthly,iEOFannual,iEOFaveann ! output flags
      integer iEOSdaily,iEOSmonthly,iEOSannual,iEOSaveann ! output flags
      logical etm,OneLandUse ! flag for single land use
      logical EOFdaily,EOFmonthly,EOFannual,EOFaveann
      logical EOSdaily,EOSmonthly,EOSannual,EOSaveann
      logical anyEOF,anyEOS,anyEO
      integer iloud
      logical loud   ! variables to squelch standard output
      data etm,OneLandUse,loud,
     .     EOFdaily,EOFmonthly,EOFannual,EOFaveann,
     .     EOSdaily,EOSmonthly,EOSannual,EOSaveann,
     .     anyEOF,anyEOS,anyEO
     .     /14*.true./

      integer year1,year2

      integer julian
      external julian

      logical found

      character*3 EOP,EOF,EOS,EOR,BAY
      data EOP, EOF, EOS, EOR, BAY /'eop','eof','eos','eor','bay'/

      character*3 clu  ! land use to process

      integer reqSRTy,reqSRTm,reqSRTd  ! requested start time 
      integer reqENDy,reqENDm,reqENDd  ! requested stop time 
      integer etmSRTy,etmSRTm,etmSRTd  ! start time of etm file
      integer etmENDy,etmENDm,etmENDd  ! stop time of etm file
      integer day1  ! first day of requested data relative to etm

************** variables to calcuate pound BMPs
      double precision annEORbcon(EarliestYear:LatestYear)
      real AnnualPoundFactor(EarliestYear:LatestYear)
      double precision AnnualPounds(EarliestYear:LatestYear,
     .                              MaxBMPcon,nlu,MaxL2R)
      real RsegAnnualPounds(EarliestYear:LatestYear,MaxBMPcon)
      double precision AnnHvalTemp(EarliestYear:LatestYear)

      real AnnualPoundFactorEOT(EarliestYear:LatestYear)
      double precision AnnualPoundsEOT(EarliestYear:LatestYear,
     .                              MaxBMPcon,nlu,MaxL2R)




      real CVP2N, CVN2BOD
      integer iNO3D, iNH3D, iNH3A, iNH3I, iNH3C, iRORN,
     .        iPO4D, iPO4A, iPO4I, iPO4C, iRORP, iBODA, iLORN, iLORP
      real fNO3D(ndaymax*24), fNH3D(ndaymax*24), fNH3A(ndaymax*24),
     .     fNH3I(ndaymax*24), fNH3C(ndaymax*24), fRORN(ndaymax*24),
     .     fPO4D(ndaymax*24), fPO4A(ndaymax*24), fPO4I(ndaymax*24),
     .     fPO4C(ndaymax*24), fRORP(ndaymax*24), fBODA(ndaymax*24),
     .     fLORN(ndaymax*24), fLORP(ndaymax*24)

      real AVG_NO3D,AVG_NH3D,AVG_NH3A,AVG_NH3I,AVG_NH3C
      real AVG_RORN,AVG_LORN
      real AVG_TOTN
      real NOXfrac,NO3Dfac,NH3Xfac,ORGNfac

      integer iSAND, iSILT, iCLAY
      real fSAND(ndaymax*24), fSILT(ndaymax*24), fCLAY(ndaymax*24)
      integer iTOTN, iTOTP, iSEDM
      real fTOTN(ndaymax*24), fTOTP(ndaymax*24), fSEDM(ndaymax*24)
 
************** END DECLARATIONS ****************************************

********* get and process input
      read(*,*,err=997,end=998) rscen,rseg,iloud,iETM,
     .       iEOFdaily,iEOFmonthly,iEOFannual,iEOFaveann,
     .       iEOSdaily,iEOSmonthly,iEOSannual,iEOSaveann,
     .       year1,year2,clu

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      call lowercase(clu)

      if (clu.eq.'all') OneLandUse = .false.
      if (iETM.eq.0) etm = .false.
      if (iloud.eq.0) loud = .false.
      if (iEOFdaily.eq.0) EOFdaily = .false.
      if (iEOFmonthly.eq.0) EOFmonthly = .false.
      if (iEOFannual.eq.0) EOFannual = .false.
      if (iEOFaveann.eq.0) EOFaveann = .false.
      if (iEOSdaily.eq.0) EOSdaily = .false.
      if (iEOSmonthly.eq.0) EOSmonthly = .false.
      if (iEOSannual.eq.0) EOSannual = .false.
      if (iEOSaveann.eq.0) EOSaveann = .false.
      if (EOFdaily.or.EOFmonthly.or.EOFannual.or.EOFaveann)
     .  anyEOF = .true.
      if (EOSdaily.or.EOSmonthly.or.EOSannual.or.EOSaveann) 
     .  anyEOS = .true.
      if (anyEOF.or.anyEOS) 
     .  anyEO = .true.

      if (etm.and.OneLandUse) go to 988

      if (anyEO.and.etm) then
        print*,'Land Loads and transfer wdms for '
      else
        if (anyEO) print*,'Land Loads for '
        if (etm) print*,'Transfer wdms for '
      end if
      if (OneLandUse) then
        print*, rscen(:lenrscen),' ',rseg,' ',year1,' ',year2,' ',clu
      else
        print*, rscen(:lenrscen),' ',rseg,' ',year1,' ',year2
      end if

******** OPEN A WDM FILE THAT NEVER GETS CLOSED.  IF DOING ETM
********* OPEN THE RIVER WDM FILE FOR WRITING OTHERWISE OPEN
*********** THE DUMMY WDM
      call lencl(rseg,lenrseg)
      if (etm) then
        wdmfnam=rseg(:lenrseg)//'.wdm'
        call wdbopnlong(wdmrch,wdmfnam,0,err)    ! open river read/write
      else
        wdmfnam = dummyWDMname
        call wdbopnlong(wdmrch,wdmfnam,0,err)
      endif
      if (err .ne. 0) go to 992
      
********* FIRST GET THE SEGMENTS IN THIS RIVER
      call getl2r(rseg,rscen,lenrscen,
     O            numsegs,l2r)
  
******** READ THE CONTROL FILE FOR START AND STOP TIME and LAND SCENARIO
      call readcontrol_tmsce(rscen,lenrscen,
     O                      reqSRTy,reqSRTm,reqSRTd,
     O                      reqENDy,reqENDm,reqENDd,
     O                      LandScen)
      
******* POPULATE nRvar, Rdsn, nLvar, Ldsn, Lname, Lfactor
      call readcontrol_modules(rscen,lenrscen,
     O                         modules,nmod)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call masslink(
     I              ioscen,lenioscen,modules,nmod,
     O              nRvar,Rdsn,Rname,RvarBMP,
     O              nLvar,Ldsn,Lname,Lfactor)

      call getBMPcons(
     I                ioscen,lenioscen,
     O                BMPconname,nbcon)

c      call getBMPconsEOT(
c     I                ioscen,lenioscen,
c     I                BMPconname,nBmpCon,
c     O                C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT)

      call getLvar(ioscen,lenioscen,
     O                I_Lvar,C_Lvar,F_Lvar) 

      if (anyEO) then
        call loadinfo(
     I                ioscen,lenioscen,nRvar,Rname,
     O                nloads,loadname,unit,ncons,con,confactor)
      end if

******* GET START AND END DATE IN WDM FORMAT
      sdate(1) = reqSRTy
      sdate(2) = reqSRTm
      sdate(3) = reqSRTd
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = reqENDy
      edate(2) = reqENDm
      edate(3) = reqENDd
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))
     
********** initialize temp variable
      if (etm) then
        do Rvar = 1,maxRvar  
          do nv = 1,ndaymax*24
            hvaltemp(nv,Rvar) = 0.
          end do
        end do
      end if


******* GET THE LIST OF STREAM LOAD LAND-USES
      call getstrlus(ioscen,lenioscen,isastrlu)
      call get_lus_annload(ioscen,lenioscen,lus_annload)
      call get_lus_avgload(ioscen,lenioscen,lus_avgload)
      call get_lus_xlbsbmp(ioscen,lenioscen,lus_xlbsbmp)
      call get_lus_nonphys(ioscen,lenioscen,lus_nonphys)

********* LOOP OVER SEGMENTS AND LAND USES, EXTRACT VARIABLES, 
************ GET ETM COEFFS, AND WRITE OUTPUT FILES
      do ns = 1,numsegs
      !{

*********  OPEN AND READ ETM file
        call lencl(l2r(ns),lenlseg)

        call getldf(rscen,lenrscen,l2r(ns),lenlseg,
     .               I_Lvar,C_Lvar,F_Lvar,
     .               F_ldf)

        if (.not.loud) call ttyput(l2r(ns)(:lenlseg))
        if (.not.loud) call ttyput(' ')

        fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .         l2r(ns)(:lenlseg)//'to'//rseg(:lenrseg)//'.etm'
        open(etmfil,file=fnam,status='old',
     .       form='unformatted',iostat=err)
        if (err.ne.0) go to 991

        read (etmfil)
     .       etmSRTy,etmSRTm,etmSRTd,
     .       etmENDy,etmENDm,etmENDd,
     .       lufac,bmpfac,tranfacL2W,tranfacS2R,
     .       dailypounds,dailypoundsEOT,
     .       dothislu,LStest
        close(etmfil)

************* test data
        write(*,*)fnam
        print*,'BHATT> ','LStest= ',LStest,'LandScen= ',LandScen(1)
        if (LStest.ne.LandScen(1)) go to 996  ! problem with file
        jday = julian(etmENDy,etmENDm,etmENDd,reqENDy,reqENDm,reqENDd)
        if (jday.gt.1) go to 994
        jday = julian(etmSRTy,etmSRTm,etmSRTd,reqSRTy,reqSRTm,reqSRTd)
        if (jday.lt.1) go to 995

        day1 = jday

******** Set flags for stream load land use to 1
******** TODO: read the list of stream loads land use
c        call getstrlus(ioscen,lenioscen,isastrlu)
        do l = 1,nlu
c           if(luname(l).eq.'dst') then
           if ( isastrlu(l) ) then
              dothislu(ns,l) = .true.
c              print*,'Stream Load Landuse: ',ns,l,' ',luname(l)
c           else
c              dothislu(ns,l) = .false.
           end if
           if ( lus_nonphys(l) ) dothislu(ns,l) = .true.

c           if (luname(l).ne.'stb') dothislu(ns,l) = .false.
        end do
*********** check whether this land use should be processed
        do l = 1,nlu
        !{

          if (luname(l).eq.'wat') cycle
          if (OneLandUse) then
            if (luname(l).ne.clu) cycle
          end if

*********** if land use is processed, find hourly EOS and/or/EOF
          if (loud) then
              print*,' '
              call ttyput(rseg)
              call ttyput(' ')
              call ttyput(l2r(ns))
              call ttyput(', ')
              call ttyput(luname(l))
          else
              call ttyput(luname(l))
              call ttyput(' ')
          end if
          if (dothislu(ns,l)) then  
          !{

c            if (loud) then
c              print*,' '
c              call ttyput(rseg)
c              call ttyput(' ')
c              call ttyput(l2r(ns))
c              call ttyput(', ')
c              call ttyput(luname(l))
c            else
c              call ttyput(luname(l))
c              call ttyput(' ')
c            end if
              
************ READY TO OPEN WDM FILE
            call lencl(LandScen(l),lenls)
            if ( lus_annload(l) .or. lus_avgload(l) ) then
               wdmfnam = outwdmdir//'land/'//luname(l)//'/'//
     .                LandScen(l)(:lenls)//'/'//luname(l)//'_'//
     .                l2r(ns)(:lenlseg)//'_'//rseg(:lenrseg)//'.wdm'
c     .                l2r(ns)(:lenlseg)//'.wdm'
               print*,wdmfnam
            else
               wdmfnam = outwdmdir//'land/'//luname(l)//'/'//
     .                LandScen(l)(:lenls)//'/'//luname(l)//
     .                l2r(ns)(:lenlseg)//'.wdm'
            end if
            call wdbopnlong(wdmlnd,wdmfnam,1,err)     ! open land read only
            if (err .ne. 0) go to 992

            if (anyEO) then
              do Rvar = 1,nRvar 
                do nv = 1,ndaymax
                  dEOPrvar(nv,Rvar) = 0.0
                  dEOFrvar(nv,Rvar) = 0.0
                  dEOSrvar(nv,Rvar) = 0.0
                  dEORrvar(nv,Rvar) = 0.0
                  dBAYrvar(nv,Rvar) = 0.0
                end do
              end do
            end if

            do Rvar = 1,nRvar
            !{
              if (loud) then
                print*,' '
                call ttyput('   River var')
                write(cRvar,'(i3)') Rvar
                call ttyput(cRvar)
                call ttyput(' ')
                call ttyput(Rname(Rvar))
              end if

              if (nLvar(Rvar,l).le.0) cycle

              found = .false.
              do nbi = 1,nbcon
                if (RvarBMP(Rvar).eq.BMPconname(nbi)) then
                  nb=nbi
                  found = .true.
                  exit
                end if
              end do
              if (.not.found) go to 989
              if (loud) then
                call ttyput(' BMP ')
                call ttyput(BMPconname(nb))
                write(cfacL2W,'(f6.3)') tranfacL2W(nb,l)
                write(cfacS2R,'(f6.3)') tranfacS2R(nb,l)
                call ttyput(' '//cfacL2W//' * '//cfacS2R)
              end if

              if (loud) then
                call ttyput(' Land variables => ')
              end if
              do Lvar = 1,nLvar(Rvar,l)
              !{

                if (loud) then
                  call ttyput(' ')
                  call ttyput(Lname(Rvar,l,Lvar))
                end if
                        
************************************************************************
!              for each land variable that goes to a river variable
!              1.  get the hourly variable amount
!              2.  multiply by the Lfactor
!              3.  add to EOF river hourly variable
!              4.  find out what type of variable it is (e.g. tn tp)
!              5.  multiply by land use
!              6.  multiply by appropriate bmp factor
!              7.  multiply by the land to water factor
!              8.  add to river variable temporary variable
************************************************************************
c                if (luname(l).eq.'fnp' .or. luname(l).eq.'fsp') then
c                  call getafoload(rscen,rseg,LandScen(l),l2r(ns),
c     I                         luname(l),sdate,edate,Lname(Rvar,l,Lvar),
c     O                         nvals,hval)
c                else if ( luname(l).eq.'rpa' ) then
c                  call getrpaload(rscen,rseg,LandScen(l),l2r(ns),
c     I                         luname(l),sdate,edate,Lname(Rvar,l,Lvar),
c     O                         nvals,hval)
c                else if ( luname(l).eq.'dst' ) then
                if ( isastrlu(l) ) then
                  call getstrload(rscen,rseg,LandScen(l),l2r(ns),
     I                         luname(l),sdate,edate,Lname(Rvar,l,Lvar),
     O                         nvals,hval)
                else 
                  call gethourdsn(wdmlnd,sdate,edate,   
     I                            Ldsn(Rvar,l,Lvar),
     O                            nvals,hval)                           
                end if
                
                jdaytot = nvals/24
                if (jdaytot.ne.ndays) go to 990


                ! >>
                nv = 0
                fac1 = Lfactor(Rvar,l,Lvar)

****************** check if land factor is a flag for segment-specific
******************** factor, if so, get the factor
c                print*,Lfactor(Rvar,l,Lvar)
                if (Lfactor(Rvar,l,Lvar).lt.0.0) then
c                 print*,int(abs(Lfactor(Rvar,l,Lvar)))
c                 write(*,'(I3)') int(abs(Lfactor(Rvar,l,Lvar)))
                 write(c3varid,'(I3)') int(abs(Lfactor(Rvar,l,Lvar)))
c                 print*,luname(l)
                 call getvarfac2(
     I                          ioscen,lenioscen,c3varid,
     I                          Lname(Rvar,l,Lvar),Rname(Rvar),l2r(ns),
     O                          varfac)
                 fac1 = varfac
                end if

******************** apply land delivery factor (ldf)
                I_ldf = -9
                do iLvar = 1,maxLvar
                   if ( C_Lvar(iLvar) .eq. Lname(Rvar,l,Lvar) ) then
                      I_ldf = iLvar
                      exit
                   end if
                end do
                print*,'... ...',Rname(Rvar),' - ',Lname(Rvar,l,Lvar),
     .                 I_ldf,F_ldf(I_ldf,l)
                fac1 = fac1 * F_ldf(I_ldf,l)


                if (anyEOF) then
                  do jday = 1,jdaytot
                    do hour = 1,24
                      nv = nv + 1
                      dEOPrvar(jday,Rvar) = dEOPrvar(jday,Rvar) 
     .                                    + hval(nv) * fac1
                    end do
                  end do
                end if


                ! >>
                nv = 0
                fac1 = fac1 * tranfacL2W(nb,l)

                if (anyEOF) then
                  do jday = 1,jdaytot
                    do hour = 1,24
                      nv = nv + 1
                      dEOFrvar(jday,Rvar) = dEOFrvar(jday,Rvar)
     .                                    + hval(nv) * fac1
                    end do
                  end do
                end if


                ! >>
                nv = 0

                if (anyEOS) then
                  do jday = 1,jdaytot
                    tlufac = lufac(jday+day1-1,l)
c                    if (luname(l).eq.'dst') tlufac = 1.0
                    if (lus_nonphys(l)) tlufac = 1.0
                    fac2 = fac1 * tlufac
     .                          * bmpfac(jday+day1-1,nb,l)
                    do hour = 1,24
                      nv = nv + 1
                      dEOSrvar(jday,Rvar) = dEOSrvar(jday,Rvar)
     .                                    + hval(nv) * fac2
                    end do
                  end do
                end if


                !>>
                nv = 0
                if (anyEOS.and.etm) then
c                  print*,nb,l,tranfacS2R(nb,l)
                  do jday = 1,jdaytot
                    tlufac = lufac(jday+day1-1,l)
c                    if (luname(l).eq.'dst') tlufac = 1.0
                    if (lus_nonphys(l)) tlufac = 1.0
                    fac2 = fac1 * tlufac
     .                          * bmpfac(jday+day1-1,nb,l)
     .                          * tranfacS2R(nb,l)
                    do hour = 1,24
                      nv = nv + 1
                      hourload = hval(nv) * fac2
                      dEORrvar(jday,Rvar) = dEORrvar(jday,Rvar)
     .                                        + hourload
                      hvaltemp(nv,Rvar)   = hvaltemp(nv,Rvar) + hourload
                    end do
                  end do
                else
                  if (anyEOS) then
                    do jday = 1,jdaytot
                      tlufac = lufac(jday+day1-1,l)
c                      if (luname(l).eq.'dst') tlufac = 1.0
                      if (lus_nonphys(l)) tlufac = 1.0
                      fac2 = fac1 * tlufac
     .                            * bmpfac(jday+day1-1,nb,l)
     .                            * tranfacS2R(nb,l)
                      do hour = 1,24
                        nv = nv + 1
                        hourload = hval(nv) * fac2
                        dEORrvar(jday,Rvar) = dEORrvar(jday,Rvar)
     .                                          + hourload
                      end do
                    end do
                  end if
                  if (etm) then
                    do jday = 1,jdaytot
                      tlufac = lufac(jday+day1-1,l)
c                      if (luname(l).eq.'dst') tlufac = 1.0
                      if (lus_nonphys(l)) tlufac = 1.0
                      fac2 = fac1 * tlufac
     .                            * bmpfac(jday+day1-1,nb,l)
     .                            * tranfacS2R(nb,l)
                      do hour = 1,24
                        nv = nv + 1
                        hourload = hval(nv) * fac2
                        hvaltemp(nv,Rvar) = hvaltemp(nv,Rvar) + hourload
                      end do
                    end do
                  end if
                end if
              !}
              end do       ! end loop over land variables in Rvar
            !}
            end do     ! end loop over river variables in land use

            print*,'closing wdmlnd'
            call wdflcl (wdmlnd,err) 
            if (err.ne.0) go to 993


*************** take the lb reduction BMPs out of the 
*********** daily EOS rvar loads and the hvaltemp loads
********** dealing with a particular LRseg and land use
************** start loop over BMP constituents
            do nb = 1,nbcon
            !{
************ calculate annual pound reduction no matter what
              do year = reqSRTy,reqENDy
                AnnualPounds(year,nb,l,ns) = 0.0
                AnnualPoundsEOT(year,nb,l,ns) = 0.0
              end do
              year = reqSRTy
              month = reqSRTm
              day = reqSRTd
              do jday = 1,jdaytot
                AnnualPounds(year,nb,l,ns) = AnnualPounds(year,nb,l,ns)
     .                                  + dailypounds(jday+day1-1,nb,l)
     .                                  * tranfacS2R(nb,l)
                AnnualPoundsEOT(year,nb,l,ns) =
     .                         AnnualPoundsEOT(year,nb,l,ns)
     .                       + dailypoundsEOT(jday+day1-1,nb,l)
                call tomorrow(year,month,day)
              end do
              if (anyEOS) then
************ calculate annual loads by BMP constituent
                do year = reqSRTy,reqENDy
                  annEORbcon(year) = 0.0
                end do
                do Rvar = 1,nRvar
                  if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle 
                  year = reqSRTy
                  month = reqSRTm
                  day = reqSRTd
                  do jday = 1,jdaytot
                    annEORbcon(year) = annEORbcon(year) 
     .                               + dEORrvar(jday,Rvar)
                    call tomorrow(year,month,day)
                  end do
                end do
c                do year = reqSRTy,reqENDy
c                  print*,'EOR ',nb,year,annEORbcon(year)
c                end do

************* annual factor
********** QUICK FIX kludge.  The sediment is in tons rather than pounds
********** the annual load needs to be converted before the factor is
************ applied.  A better fix would be to put a conversion
************ factor to pounds in the rchres_in file
c                if (BMPconname(nb).eq.'sed') then
c                  do year = reqSRTy,reqENDy
c                    annEORbcon(year) = annEORbcon(year) * 2000.0
c                  end do
c                end if
****************** end QUICK FIX

                do year = reqSRTy,reqENDy
                  AnnualPoundFactor(year) = 
     .                   (annEORbcon(year) - AnnualPounds(year,nb,l,ns))
c                  print*,AnnualPoundFactor(year),annEORbcon(year),
c     .               AnnualPounds(year,nb,l,ns)

                  if (AnnualPoundFactor(year) .le. 0.0) then
                    if (lus_xlbsbmp(l) .eqv. .false.) then
                      ! this improves the accuracy but will result in
                      ! inconsistency in hourly river loads if anyEOS is set to false
                     AnnualPounds(year,nb,l,ns) = annEORbcon(year) ! BHATT added
                     AnnualPoundFactor(year)    = 0.0
                    else
                     if ( annEORbcon(year) .ne. 0 ) then
                       AnnualPoundFactor(year) = AnnualPoundFactor(year)
     .                                      /  annEORbcon(year)
                     else
                       print*,year,BMPconname(nb),annEORbcon(year)
                       if (BMPconname(nb).eq.'tnx' .or. 
     .                     BMPconname(nb).eq.'tpx' .or.
     .                     BMPconname(nb).eq.'sed' ) go to 800
                       AnnualPoundFactor(year) = 1.0
                     end if
c                     print*,BMPconname(nb),annEORbcon(year),
c     .                AnnualPounds(year,nb,l,ns),AnnualPoundFactor(year)
                    end if
                  else
                    AnnualPoundFactor(year) = AnnualPoundFactor(year)
     .                                      /  annEORbcon(year)
                  end if


                  if ( AnnualPoundsEOT(year,nb,l,ns) .eq. 0 ) then
                     AnnualPoundFactorEOT(year) = 1.0
                  else
                     AnnualPoundFactorEOT(year) = annEORbcon(year)
     .                   - AnnualPounds(year,nb,l,ns)
     .                   - AnnualPoundsEOT(year,nb,l,ns)
c                     print*,luname(l),nb,year,
c     .                 AnnualPoundsEOT(year,nb,l,ns),
c     .                 annEORbcon(year)-AnnualPounds(year,nb,l,ns)
                     if ( AnnualPoundFactorEOT(year) .le. 0 ) then
                        if(annEORbcon(year)-AnnualPounds(year,nb,l,ns)
     .                        .ne. 0 ) then
                         AnnualPoundFactorEOT(year) =
     .                     AnnualPoundFactorEOT(year) /
     .                     (annEORbcon(year)-AnnualPounds(year,nb,l,ns))
                        else
                         AnnualPoundFactorEOT(year) = 1.0
                        end if
                     else
                        AnnualPoundFactorEOT(year) = 
     .                     AnnualPoundFactorEOT(year) /
     .                     (annEORbcon(year)-AnnualPounds(year,nb,l,ns))
                     end if
                  end if
c?                  print*,luname(l),BMPconname(nb),year,
c?     .               AnnualPoundsEOT(year,nb,l,ns),
c?     .               annEORbcon(year)-AnnualPounds(year,nb,l,ns),
c?     .               AnnualPoundFactorEOT(year)
                end do
  
****************** apply factor to all EOS 
                do Rvar = 1,nRvar
                  if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle 
                  year = reqSRTy
                  month = reqSRTm
                  day = reqSRTd
                  do jday = 1,jdaytot
                    dEOSrvar(jday,Rvar) = dEOSrvar(jday,Rvar)
     .                                  * AnnualPoundFactor(year)
                    dEORrvar(jday,Rvar) = dEORrvar(jday,Rvar) 
     .                                  * AnnualPoundFactor(year)
                    dBAYrvar(jday,Rvar) = dEORrvar(jday,Rvar)
     .                                  * (1-AnnualPoundFactorEOT(year))
                    call tomorrow(year,month,day)
                  end do
                end do
                
              end if  ! if any EOS
            !}
            end do  ! end loop over BMP constituents


*********** convert EOF and EOS to daily output loads
            if (anyEOF) then
              do nl = 1,nloads
                do nv = 1,jdaytot  ! initialize
                  dEOPload(nv,nl) = 0.0
                  dEOFload(nv,nl) = 0.0
                end do
                do nc = 1,ncons(nl)
                  do Rvar = 1,nRvar        ! get the right dsn
                    if (con(nl,nc).eq.Rname(Rvar)) then
                      do nv = 1,jdaytot
                        dEOPload(nv,nl) = dEOPload(nv,nl)
     .                        + dEOPrvar(nv,Rvar) * confactor(nl,nc)
                        dEOFload(nv,nl) = dEOFload(nv,nl) 
     .                        + dEOFrvar(nv,Rvar) * confactor(nl,nc)
                      end do
                    end if 
                  end do
                end do
              end do
            end if
            if (anyEOS) then
              do nl = 1,nloads
                do nv = 1,jdaytot  ! initialize
                  dEOSload(nv,nl) = 0.0
                  dEORload(nv,nl) = 0.0
                  dBAYload(nv,nl) = 0.0
                end do
                do nc = 1,ncons(nl)
                  do Rvar = 1,nRvar        ! get the right dsn
                    if (con(nl,nc).eq.Rname(Rvar)) then
                      do nv = 1,jdaytot
                        dEOSload(nv,nl) = dEOSload(nv,nl) 
     .                        + dEOSrvar(nv,Rvar) * confactor(nl,nc)
                        dEORload(nv,nl) = dEORload(nv,nl)
     .                        + dEORrvar(nv,Rvar) * confactor(nl,nc)
                        dBAYload(nv,nl) = dBAYload(nv,nl)
     .                        + dBAYrvar(nv,Rvar) * confactor(nl,nc)
                      end do
                    end if 
                  end do
                end do
              end do
            end if

**************** store lufac in acres
            do jday = 1,jdaytot  ! store acres
              acres(jday) = lufac(jday+day1-1,l)
            end do

          !}
          else
          !{
             print*,' x '
          !}
          end if             ! end if dothislu = .true.

***************** output section
          if (EOFdaily) then
            call writeEOdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFmonthly) then
            call writeEOmonthly(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFannual) then
            call writeEOannual(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFaveann) then
            call writeEOaveann(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eop,ndays,nloads,loadname,dEOPload,
     .                        year1,year2)
            call writeEOaveann(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload,
     .                        year1,year2)
          end if
          if (EOSdaily) then
            call writeEOdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
          end if
          if (EOSmonthly) then
            call writeEOmonthly(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
            call writeEOmonthly(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eor,ndays,nloads,loadname,dEORload)
          end if
          if (EOSannual) then
            call writeEOannual(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
          end if
          if (EOSaveann) then
            call writeEOaveann(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload,
     .                        year1,year2)
            call writeEOaveann(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eor,ndays,nloads,loadname,dEORload,
     .                        year1,year2)
            call writeEOaveann(rscen,l2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        bay,ndays,nloads,loadname,dBAYload,
     .                        year1,year2)
          end if

        !}
        end do            ! end loop over land uses in segment
        print*,'done lus'

        if (.not.loud) print*,' '

      !}
      end do    ! end loop over land segments for river
      print*,'done lsegs'

******** DEAL WITH POINT SOURCE ONLY SEGMENTS, CREATE DUMMY OUT FILES
      call getpsonlyl2r(
     I                  rseg,rscen,lenrscen,numsegs,l2r,
     O                  npssegs,psl2r)

      do ns = 1,npssegs
*********  Open and read ETM file
        call lencl(psl2r(ns),lenlseg)
        if (.not.loud) call ttyput(psl2r(ns)(:lenlseg))
        if (.not.loud) call ttyput(' ')

        fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .         psl2r(ns)(:lenlseg)//'to'//rseg(:lenrseg)//'.etm'
        print*,'ps etm'//fnam
        open(etmfil,file=fnam,status='old',
     .       form='unformatted',iostat=err)
        if (err.ne.0) go to 991

        read (etmfil)
     .        etmSRTy,etmSRTm,etmSRTd,
     .        etmENDy,etmENDm,etmENDd,
     .        lufac,bmpfac,tranfacL2W,tranfacS2R,
     .        dailypounds,dailypoundsEOT,
     .        dothislu,LStest
        close(etmfil)

        print*,'done1'
*********** Set dummy EOF/EOS loads
        do l = 1,nlu      
          do nl = 1,nloads
            do nv = 1,jdaytot  
              dEOPload(nv,nl) = 0.0
              dEOFload(nv,nl) = 0.0
              dEOSload(nv,nl) = 0.0
              dEORload(nv,nl) = 0.0
              dBAYload(nv,nl) = 0.0
            end do
          end do
   
          do jday = 1,jdaytot  ! store acres
            acres(jday) = 0.0
          end do

***************** EOF output section
          if (EOFdaily) then
            call writeEOdaily(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFmonthly) then
            call writeEOmonthly(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFannual) then
            call writeEOannual(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload)
          end if
          if (EOFaveann) then
            call writeEOaveann(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eop,ndays,nloads,loadname,dEOPload,
     .                        year1,year2)
            call writeEOaveann(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eof,ndays,nloads,loadname,dEOFload,
     .                        year1,year2)
          end if

***************** EOS output section
          if (EOSdaily) then
            call writeEOdaily(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
          end if
          if (EOSmonthly) then
            call writeEOmonthly(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
            call writeEOmonthly(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eor,ndays,nloads,loadname,dEORload)
          end if
          if (EOSannual) then
            call writeEOannual(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload)
          end if
          if (EOSaveann) then
            call writeEOaveann(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eos,ndays,nloads,loadname,dEOSload,
     .                        year1,year2)
            call writeEOaveann(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        eor,ndays,nloads,loadname,dEORload,
     .                        year1,year2)
            call writeEOaveann(rscen,psl2r(ns),rseg,outfil,sdate,
     .                        luname(l),dothislu(ns,l),acres,
     .                        bay,ndays,nloads,loadname,dBAYload,
     .                        year1,year2)
          end if

        end do            ! end loop over land uses in segment

        if (.not.loud) print*,' '

        print*,'done 2'
      end do    ! end loop over ps ONLY land segments for river

Ctest*********** check code
Ctest      do Rvar = 1,nRvar
Ctest        year = reqSRTy
Ctest        month = reqSRTm
Ctest        day = reqSRTd
Ctest        nv = 0
Ctest        acc(Rvar) = 0.0
Ctest        do jday = 1,jdaytot
Ctest          do hour = 1,24
Ctest            nv = nv + 1
Ctest            if (year.ge.1994) acc(Rvar) = acc(Rvar)+ hvaltemp(nv,Rvar)
Ctest          end do
Ctest          call tomorrow(year,month,day)
Ctest        end do
Ctest      end do
Ctest      print*,(Rname(Rvar),',',Rvar=1,nRvar)
Ctest      print*,(acc(Rvar),',',Rvar=1,nRvar)
Ctest      stop
Ctest************** end check

      if (etm) then  ! generate hourly river output
*************** take the lb reduction BMPs out of the hvaltemp loads
*********** like above process, but add across lrsegs and LUs

********** find total annual pounds for the Rseg
        do nb = 1,maxBMPcon
          do year = EarliestYear,LatestYear
            RsegAnnualPounds(year,nb) = 0.0
          end do
        end do

        do ns = 1,numsegs
          do l = 1,nlu
            do nb = 1,maxBMPcon
              do year = EarliestYear,LatestYear
                RsegAnnualPounds(year,nb) = RsegAnnualPounds(year,nb)
     .                                    + AnnualPounds(year,nb,l,ns)
c                print*,rseg(:lenrseg),' ',l2r(ns)(:lenlseg),' ',
c     .            luname(l),' ',BMPconname(nb),' ',year,' ',
c     .            AnnualPounds(year,nb,l,ns)
              end do
            end do
          end do
        end do

************** start loop over BMP constituents
        do nb = 1,nbcon

************ calculate annual loads by BMP constituent
          do year = reqSRTy,reqENDy
            AnnHvalTemp(year) = 0.0
          end do
          do Rvar = 1,nRvar
            if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle 
            year = reqSRTy
            month = reqSRTm
            day = reqSRTd
            nv = 0
            do jday = 1,jdaytot
              do hour = 1,24
                nv = nv + 1
                AnnHvalTemp(year) = AnnHvalTemp(year) +hvaltemp(nv,Rvar)
              end do
              call tomorrow(year,month,day) 
            end do
          end do

********** QUICK FIX kludge.  The sediment is in tons rather than pounds
********** the annual load needs to be converted before the factor is
************ applied.  A better fix would be to put a conversion
************ factor to pounds in the rchres_in file
c          if (BMPconname(nb).eq.'sed') then
c            do year = reqSRTy,reqENDy
c              AnnHvalTemp(year) = AnnHvalTemp(year) * 2000.0
c            end do
c          end if
****************** end QUICK FIX

************* annual factor
          do year = reqSRTy,reqENDy
            AnnualPoundFactor(year) = 
     .                   (AnnHvalTemp(year) - RsegAnnualPounds(year,nb))
            if (AnnualPoundFactor(year) .le. 0.0) then
              AnnualPoundFactor(year) = 0.0
            else
              AnnualPoundFactor(year) = AnnualPoundFactor(year)
     .                                /  AnnHvalTemp(year)
            end if
          end do

****************** apply factor to all EOS 
          do Rvar = 1,nRvar
            if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle 
            year = reqSRTy
            month = reqSRTm
            day = reqSRTd
            nv = 0
            do jday = 1,jdaytot
              do hour = 1,24
                nv = nv + 1
                hvaltemp(nv,Rvar) = hvaltemp(nv,Rvar) 
     .                            * AnnualPoundFactor(year)
              end do
              call tomorrow(year,month,day)
            end do
          end do

        end do  ! end loop over BMP constituents



************* HANDLING BED/BANK & FLOODPLAIN: START ***********
        print*,'... START: reading bedbank and floodplain loads'
        call readbedbank(rscen,lenrscen,rseg,lenrseg,l2r,numsegs,
     .                   lrbedbank,sYRbb,eYRbb)
        call readfloodplain(rscen,lenrscen,rseg,lenrseg,l2r,numsegs,
     .                   lrfloodplain,sYRfl,eYRfl)
        print*,'... FINISH: reading bedbank and floodplain loads'

        do icstr = 1,ncstr
          write(*,*),'... ',ncstrname(icstr)
          rbedbank(icstr)    = 0.0
          rfloodplain(icstr) = 0.0
          do ns = 1,numsegs

             write(*,'(A,1X,A,A,E14.6)'),rseg(:lenrseg),l2r(ns)
     .    (:lenlseg),' BB',lrbedbank(ns,icstr)*(eYRbb-sYRbb+1)
             write(*,'(A,1X,A,A,E14.6)'),rseg(:lenrseg),l2r(ns)
     .    (:lenlseg),' FL',lrfloodplain(ns,icstr)*(eYRbb-sYRbb+1)

             rbedbank(icstr)    = rbedbank(icstr)    + 
     .                            lrbedbank(ns,icstr)   *(eYRbb-sYRbb+1)
             rfloodplain(icstr) = rfloodplain(icstr) + 
     .                            lrfloodplain(ns,icstr)*(eYRbb-sYRbb+1)

          end do
        end do
        print*,''
        write(*,*)'... LOADS FOR RSEG ',rseg(:lenrseg)
        write(*,'( A, E14.6, E14.6, E14.6, E14.6)')
     .    'BB : ',rbedbank(1),rbedbank(2),rbedbank(3),rbedbank(4)
        write(*,'( A, E14.6, E14.6, E14.6, E14.6)')
     .    'FL : ',rfloodplain(1),rfloodplain(2),rfloodplain(3),
     .            rfloodplain(4)

        rbedbankload(1) = rbedbank(1)
        rbedbankload(2) = rbedbank(2)
        rbedbankload(3) = ( rbedbank(3) + rbedbank(4) ) / 2000.0
        rfloodplainload(1) = rfloodplain(1)
        rfloodplainload(2) = rfloodplain(2)
        rfloodplainload(3) = ( rfloodplain(3) + rfloodplain(4) ) / 2000.0

        write(*,*),' now using TOTN TOTP TSED'
        write(*,*),'---------------------------------------------------'
        write(*,*),'SRC        TOTN         TOTP           TSED'
        write(*,*),'---------------------------------------------------'
        write(*,'( A, E14.6, E14.6, E14.6)')
     .    'BED: ',rbedbankload(1),rbedbankload(2),rbedbankload(3)
        write(*,'( A, E14.6, E14.6, E14.6)')
     .    'FLD: ',rfloodplainload(1),rfloodplainload(2),
     .            rfloodplainload(3)

        iNO3D = -9
        iNH3D = -9
        iNH3A = -9
        iNH3I = -9
        iNH3C = -9
        iRORN = -9
        iLORN = -9
        iPO4D = -9
        iPO4A = -9
        iPO4I = -9
        iPO4C = -9
        iRORP = -9
        iLORP = -9
        iSAND = -9
        iSILT = -9
        iCLAY = -9
        do Rvar = 1,nRvar
           if ( Rname(Rvar) .eq. 'NO3D' ) iNO3D = Rvar
           if ( Rname(Rvar) .eq. 'NH3D' ) iNH3D = Rvar
           if ( Rname(Rvar) .eq. 'NH3A' ) iNH3A = Rvar
           if ( Rname(Rvar) .eq. 'NH3I' ) iNH3I = Rvar
           if ( Rname(Rvar) .eq. 'NH3C' ) iNH3C = Rvar
           if ( Rname(Rvar) .eq. 'RORN' ) iRORN = Rvar
           if ( Rname(Rvar) .eq. 'LORN' ) iLORN = Rvar
           if ( Rname(Rvar) .eq. 'PO4D' ) iPO4D = Rvar
           if ( Rname(Rvar) .eq. 'PO4A' ) iPO4A = Rvar
           if ( Rname(Rvar) .eq. 'PO4I' ) iPO4I = Rvar
           if ( Rname(Rvar) .eq. 'PO4C' ) iPO4C = Rvar
           if ( Rname(Rvar) .eq. 'RORP' ) iRORP = Rvar
           if ( Rname(Rvar) .eq. 'LORP' ) iLORP = Rvar
           if ( Rname(Rvar) .eq. 'SAND' ) iSAND = Rvar
           if ( Rname(Rvar) .eq. 'SILT' ) iSILT = Rvar
           if ( Rname(Rvar) .eq. 'CLAY' ) iCLAY = Rvar
        end do

        do icstr = 1,ncstr
           rPARAload(icstr) = 0.0
        end do
        if ( .not. (iNO3D.eq.-9 .or. iNH3D.eq.-9 .or. iPO4D.eq.-9 .or.
     .              iRORP.eq.-9 .or. iSAND.eq.-9 ) ) then
         do nv = 1,nvals
           fNO3D(nv) = hvaltemp(nv,iNO3D)
           fNH3D(nv) = hvaltemp(nv,iNH3D)
           fNH3A(nv) = hvaltemp(nv,iNH3A)
           fNH3I(nv) = hvaltemp(nv,iNH3I)
           fNH3C(nv) = hvaltemp(nv,iNH3C)
           fRORN(nv) = hvaltemp(nv,iRORN)
           fLORN(nv) = hvaltemp(nv,iLORN)
           fPO4D(nv) = hvaltemp(nv,iPO4D)
           fPO4A(nv) = hvaltemp(nv,iPO4A)
           fPO4I(nv) = hvaltemp(nv,iPO4I)
           fPO4C(nv) = hvaltemp(nv,iPO4C)
           fRORP(nv) = hvaltemp(nv,iRORP)
           fLORP(nv) = hvaltemp(nv,iLORP)
           fSAND(nv) = hvaltemp(nv,iSAND)
           fSILT(nv) = hvaltemp(nv,iSILT)
           fCLAY(nv) = hvaltemp(nv,iCLAY)
           fTOTN(nv) = fNO3D(nv)+fNH3D(nv)+fNH3A(nv)+fNH3I(nv)+fNH3C(nv)
     .                +fRORN(nv)+fLORN(nv)
           fTOTP(nv) = fPO4D(nv)+fPO4A(nv)+fPO4I(nv)+fPO4C(nv)+fRORP(nv)
     .                +fLORP(nv)
           fSEDM(nv) = fSAND(nv)+fSILT(nv)+fCLAY(nv)
         end do
        end if

        year = reqSRTy
        month = reqSRTm
        day = reqSRTd
        nv = 0
        do jday = 1,jdaytot
         do hour = 1,24
          nv = nv + 1
          if(year.ge.sYRbb .and. year.le.eYRbb) then
           rPARAload(1)    = rPARAload(1) + fTOTN(nv)
           rPARAload(2)    = rPARAload(2) + fTOTP(nv)
           rPARAload(3)    = rPARAload(3) + fSEDM(nv)
          end if
         end do
         call tomorrow(year,month,day)
        end do
        write(*,'( A, E14.6, E14.6, E14.6)'),
     .   'RIV: ',rPARAload(1),rPARAload(2),rPARAload(3)

        do icstr = 1,3
           print*,''
           facPARAbb(icstr) = 0.0
           facPARAfl(icstr) = 0.0
           if ( rPARAload(icstr) .ne. 0 ) then
            facPARAbb(icstr) = rbedbankload(icstr)    / rPARAload(icstr)
            facPARAfl(icstr) = rfloodplainload(icstr) / rPARAload(icstr)
           end if

           write(*,*),
     ,          '---------------------------------------------------'
           write(*,*),nps(icstr),
     .      '    STREAM         RIVER           FACTOR'
           write(*,*),
     .          '---------------------------------------------------'
           write(*,'( A, E14.6, E14.6, F14.6)')
     .          'BED ',rbedbankload(icstr),rPARAload(icstr),
     .            facPARAbb(icstr)
           write(*,'( A, E14.6, E14.6, F14.6)')
     .          'FLD ',rfloodplainload(icstr),rPARAload(icstr),
     .            facPARAfl(icstr)
           write(*,'( A, F14.6)'),
     .           '... fac before ',(1+facPARAbb(icstr)+facPARAfl(icstr))
           if ( facPARAbb(icstr) + facPARAfl(icstr) .lt. -1 ) then
              pfac = 1
              nfac = 0
              if( facPARAbb(icstr) .gt. 0 ) then
                 pfac = pfac + facPARAbb(icstr)
              else
                 nfac = nfac + facPARAbb(icstr)
              end if
              if( facPARAfl(icstr) .gt. 0 ) then
                 pfac = pfac + facPARAfl(icstr)
              else
                 nfac = nfac + facPARAfl(icstr)
              end if
              adjfac = - pfac / nfac

              print*,'... ... adjfac = ',adjfac

              if( facPARAbb(icstr) .lt. 0 )
     .           facPARAbb(icstr) = facPARAbb(icstr) * adjfac + 0.01
              if( facPARAfl(icstr) .lt. 0 )
     .           facPARAfl(icstr) = facPARAfl(icstr) * adjfac + 0.01
           end if
           write(*,'( A, F14.6)'),
     .       '... fac after  ',(1+facPARAbb(icstr)+facPARAfl(icstr))
        end do

        if ( .not. (iNO3D.eq.-9 .or. iNH3D.eq.-9 .or. iPO4D.eq.-9 .or.
     .              iRORP.eq.-9 .or. iSAND.eq.-9 ) ) then
         do nv = 1,nvals
           hvaltemp(nv,iNO3D) = hvaltemp(nv,iNO3D) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iNH3D) = hvaltemp(nv,iNH3D) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iNH3A) = hvaltemp(nv,iNH3A) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iNH3I) = hvaltemp(nv,iNH3I) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iNH3C) = hvaltemp(nv,iNH3C) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iRORN) = hvaltemp(nv,iRORN) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )
           hvaltemp(nv,iLORN) = hvaltemp(nv,iLORN) *
     .                  (1 + facPARAbb(1) + facPARAfl(1) )

           hvaltemp(nv,iPO4D) = hvaltemp(nv,iPO4D) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )
           hvaltemp(nv,iPO4A) = hvaltemp(nv,iPO4A) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )
           hvaltemp(nv,iPO4I) = hvaltemp(nv,iPO4I) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )
           hvaltemp(nv,iPO4C) = hvaltemp(nv,iPO4C) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )
           hvaltemp(nv,iRORP) = hvaltemp(nv,iRORP) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )
           hvaltemp(nv,iLORP) = hvaltemp(nv,iLORP) *
     .                  (1 + facPARAbb(2) + facPARAfl(2) )

           hvaltemp(nv,iSAND) = hvaltemp(nv,iSAND) *
     .                  (1 + facPARAbb(3) + facPARAfl(3) )
           hvaltemp(nv,iSILT) = hvaltemp(nv,iSILT) *
     .                  (1 + facPARAbb(3) + facPARAfl(3) )
           hvaltemp(nv,iCLAY) = hvaltemp(nv,iCLAY) *
     .                  (1 + facPARAbb(3) + facPARAfl(3) )
         end do
        end if

************* HANDLING BED/BANK & FLOODPLAIN: STOP ***********





************* HANDLING NO3D and TOTN Balance: START ***********
        iNO3D = -9
        iNH3D = -9
        iNH3A = -9
        iNH3I = -9
        iNH3C = -9
        iRORN = -9
        iLORN = -9
        do Rvar = 1,nRvar
           if ( Rname(Rvar) .eq. 'NO3D' ) iNO3D = Rvar
           if ( Rname(Rvar) .eq. 'NH3D' ) iNH3D = Rvar
           if ( Rname(Rvar) .eq. 'NH3A' ) iNH3A = Rvar
           if ( Rname(Rvar) .eq. 'NH3I' ) iNH3I = Rvar
           if ( Rname(Rvar) .eq. 'NH3C' ) iNH3C = Rvar
           if ( Rname(Rvar) .eq. 'RORN' ) iRORN = Rvar
           if ( Rname(Rvar) .eq. 'LORN' ) iLORN = Rvar
        end do
        if ( .not. (iRORN.eq.-9 .or. iRORP.eq.-9 .or. iBODA.eq.-9 .or.
     .              iLORN.eq.-9 .or. iLORP.eq.-9 ) ) then
        !{
         do nv = 1,nvals
          fNO3D(nv) = hvaltemp(nv,iNO3D)
          fNH3D(nv) = hvaltemp(nv,iNH3D)
          fNH3A(nv) = hvaltemp(nv,iNH3A)
          fNH3I(nv) = hvaltemp(nv,iNH3I)
          fNH3C(nv) = hvaltemp(nv,iNH3C)
          fRORN(nv) = hvaltemp(nv,iRORN)
          fLORN(nv) = hvaltemp(nv,iLORN)
          fTOTN(nv) = fNO3D(nv)+fNH3D(nv)+fNH3A(nv)+fNH3I(nv)+
     .                 fNH3C(nv)+fRORN(nv)+fLORN(nv)
         end do

         year = reqSRTy
         month = reqSRTm
         day = reqSRTd
         nv = 0
         do year = reqSRTy,reqENDy
         !{
          nv1 = nv + 1
          AVG_NO3D = 0.0
          AVG_NH3D = 0.0
          AVG_NH3A = 0.0
          AVG_NH3I = 0.0
          AVG_NH3C = 0.0
          AVG_RORN = 0.0
          AVG_LORN = 0.0
          AVG_TOTN = 0.0
          do month=1,12
           do day = 1,ndaysinmonth(year,month)
            do hour = 1,24
             nv = nv + 1
             AVG_NO3D = AVG_NO3D + fNO3D(nv)
             AVG_NH3D = AVG_NH3D + fNH3D(nv)
             AVG_NH3A = AVG_NH3A + fNH3A(nv)
             AVG_NH3I = AVG_NH3I + fNH3I(nv)
             AVG_NH3C = AVG_NH3C + fNH3C(nv)
             AVG_RORN = AVG_RORN + fRORN(nv)
             AVG_LORN = AVG_LORN + fLORN(nv)
             AVG_TOTN = AVG_TOTN + fTOTN(nv)
            end do
           end do
          end do
          print*,'YEAR      = ',year
          print*,'AVG_NO3D  = ',AVG_NO3D
          print*,'AVG_NH3D  = ',AVG_NH3D
          print*,'AVG_NH3A  = ',AVG_NH3A
          print*,'AVG_NH3I  = ',AVG_NH3I
          print*,'AVG_NH3C  = ',AVG_NH3C
          print*,'AVG_RORN  = ',AVG_RORN
          print*,'AVG_LORN  = ',AVG_LORN
          print*,'AVG_TOTN  = ',AVG_TOTN
          print*,rseg(:lenrseg),' old NOX fraction = ',
     .            AVG_NO3D/AVG_TOTN
          call getnox(
     I           rscen,rseg,AVG_TOTN,
     O           NOXfrac)
          print*,rseg(:lenrseg),' reg NOX fraction = ',NOXfrac
          print*,AVG_TOTN,NOXfrac,AVG_NO3D
          NO3Dfac = AVG_TOTN * NOXfrac / AVG_NO3D
          NH3Xfac = AVG_TOTN * (1-NOXfrac) / (AVG_TOTN-AVG_NO3D)
          ORGNfac = AVG_TOTN * (1-NOXfrac) / (AVG_TOTN-AVG_NO3D)
          print*,'fac ',NO3Dfac,NH3Xfac,ORGNfac
          nv2 = nv
          do inv = nv1,nv2
          !{
           if ( AVG_NO3D .gt. 0 ) then
            hvaltemp(inv,iNO3D) = fNO3D(inv) * NO3Dfac
           else
            hvaltemp(inv,iNO3D) = fTOTN(inv) * NOXfrac
           end if
           hvaltemp(inv,iNH3D) = fNH3D(inv) * NH3Xfac
           hvaltemp(inv,iNH3A) = fNH3A(inv) * NH3Xfac
           hvaltemp(inv,iNH3I) = fNH3I(inv) * NH3Xfac
           hvaltemp(inv,iNH3C) = fNH3C(inv) * NH3Xfac
           hvaltemp(inv,iRORN) = fRORN(inv) * ORGNfac
           hvaltemp(inv,iLORN) = fLORN(inv) * ORGNfac
          !}
          end do
         !}
         end do
        !}
        end if
************* HANDLING NO3D and TOTN Balance: STOP  ***********





************* HANDLING BODA: START ***********
! iRORN, iRORP, iBODA, iLORN, iLORP
        ! fRORN, fRORP, fBODA, fLORN, fLORP
        CVP2N   = 7.23
        CVN2BOD = 22.9
        iRORN = -9
        iRORP = -9
        iBODA = -9
        iLORN = -9
        iLORP = -9
        do Rvar = 1,nRvar
           if ( Rname(Rvar) .eq. 'RORN' ) iRORN = Rvar
           if ( Rname(Rvar) .eq. 'RORP' ) iRORP = Rvar
           if ( Rname(Rvar) .eq. 'BODA' ) iBODA = Rvar
           if ( Rname(Rvar) .eq. 'LORN' ) iLORN = Rvar
           if ( Rname(Rvar) .eq. 'LORP' ) iLORP = Rvar
        end do
        if ( .not. (iRORN.eq.-9 .or. iRORP.eq.-9 .or. iBODA.eq.-9 .or. 
     .              iLORN.eq.-9 .or. iLORP.eq.-9 ) ) then
         do nv = 1,nvals
           fRORN(nv) = hvaltemp(nv,iRORN)
           fRORP(nv) = hvaltemp(nv,iRORP)
           fLORN(nv) = hvaltemp(nv,iLORN)
           fLORP(nv) = hvaltemp(nv,iLORP)
           fBODA(nv) = hvaltemp(nv,iBODA)

           if ( fLORN(nv) + fLORP(nv) .gt. 0 ) then
             if ( fLORN(nv) .lt. CVP2N * fLORP(nv) ) then
               fBODA(nv) = CVN2BOD * fLORN(nv)
               fRORP(nv) = fRORP(nv) + ( fLORP(nv) - fLORN(nv) / CVP2N )
             else
               fBODA(nv) = CVN2BOD * fLORP(nv) * CVP2N
               fRORN(nv) = fRORN(nv) + ( fLORN(nv) - fLORP(nv) * CVP2N )
             end if
           end if

           hvaltemp(nv,iRORN) = fRORN(nv)
           hvaltemp(nv,iRORP) = fRORP(nv)
           hvaltemp(nv,iLORN) = 0.0
           hvaltemp(nv,iLORP) = 0.0
           hvaltemp(nv,iBODA) = fBODA(nv)
         end do
        end if
************* HANDLING BODA: FINISH ***********




*********** WDM OUTPUT
        do Rvar = 1,nRvar      ! store in wdm
          do nv = 1,nvals
            hval(nv) = hvaltemp(nv,Rvar)
          end do
          call puthourdsn(wdmrch,sdate,edate,Rdsn(Rvar),nvals,hval)
        end do

      end if  ! if etm is to be run

      print*,'before close wdmrch'
      call wdflc1(wdmrch,err)
      if (err.ne.0) go to 993

      print*,' '

      return

123   format(a4,4x,a5,4x,a3,4x,20(a4,11x))
1234  format(i4,4x,i5,4x,i3,4x,20(1x,e14.7))

************************* ERROR SPACE **********************************

800   report(1) = 'EOR load can not be zero'
      report(2) = luname(l)//' '//rseg(:lenrseg)//' '//l2r(ns)(:lenlseg)
     .             //' '//BMPconname(nb)
      write(report(3),*),annEORbcon(year),AnnualPounds(year,nb,l,ns)
      go to 999

988   report(1) = 'land use must be specified as '//char(39)//'all'//
     .            char(39)
      report(2) = 'if the ETM is running'
      report(3) = ''
      go to 999

989   report(1) = 'major problem with transfer logic. Make sure that'
      report(2) = 'files in ./config/catalog/iovars agree with '
      report(3) = 'each other for bmp consituents'
      go to 999

990   report(1) = 'problem in wdm, difference between '
      report(2) = 'expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',jdaytot
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
      report(3) = ' '
      go to 999

993   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

994   report(1) = 'problem with etm file: end date too early'
      write(report(2),*)'file end: ',etmENDy,' ',etmENDm,' ',etmENDd
      write(report(3),*)'req end date: ',reqENDy,' ',reqENDm,' ',reqENDd
      go to 999

995   report(1) = 'problem with etm file: start date too late'
      write(report(1),*) 'file start: ',etmSRTy,' ',etmSRTm,' ',etmSRTd
      write(report(3),*) 'req date: ',reqSRTy,' ',reqSRTm,' ',reqSRTd
      go to 999

996   report(1) = 'problem with etm file: variables not correctly'
      report(2) = 'aligned in binary file'
      report(3) = 'check etm and postprocessor code'
      go to 999

997   report(1) = 'Error initializing land postprocessor'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and run_postproc.csh'
      go to 999

998   report(1) = 'Error initializing land postprocessor'
      report(2) = 'not enough input data in run_postproc.csh'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

      subroutine getstrlus ( ioscen,lenioscen,
     M                       isastrlu )
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      logical isastrlu(nlu)
      integer i,j
      integer nstrlus
      character*3 C3_tlu

      do i=1,nlu
         isastrlu(i) = .false.
      end do

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/str_loads'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990

      read(dfile,'(a100)') line ! header line

      read(dfile,'(a100)') line
c      print*,line

      call d2x(line,last)
c      call spaceshift(line,last)
      read(line,*)nstrlus
      print*, ' '
      print*, 'Stream Load Landuse'
      print*, 'Number of str load landuse = ',nstrlus

      do i = 1,nstrlus
         call spaceshift(line,last)
         read(line,*)C3_tlu
         do j = 1,nlu
            if ( luname(j) .eq. C3_tlu ) then
               isastrlu(j) = .true.
               print*, '   -> Stream LU #', i, j, ' ', luname(j)
            end if
         end do
      end do
      print*,' '

      close (dfile)
      return

990   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine getstrlus'
      go to 999

999   call stopreport(report)

      end


      subroutine get_lus_annload ( ioscen,lenioscen,
     M                       lus_annload )
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      logical lus_annload(nlu)
      integer i,j
      integer nstrlus
      character*3 C3_tlu

      do i=1,nlu
         lus_annload(i) = .false.
      end do

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lus_annload'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990

      read(dfile,'(a100)') line ! header line

      read(dfile,'(a100)') line
c      print*,line

      call d2x(line,last)
c      call spaceshift(line,last)
      read(line,*)nstrlus
      print*, ' '
      print*, 'Annual Load Landuse'
      print*, 'Number of Ann load landuse = ',nstrlus

      do i = 1,nstrlus
         call spaceshift(line,last)
         read(line,*)C3_tlu
         do j = 1,nlu
            if ( luname(j) .eq. C3_tlu ) then
               lus_annload(j) = .true.
               print*, '   -> Load LU #', i, j, ' ', luname(j)
            end if
         end do
      end do
      print*,' '

      close (dfile)
      return

990   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine getstrlus'
      go to 999

999   call stopreport(report)

      end



      subroutine get_lus_avgload ( ioscen,lenioscen,
     M                       lus_avgload )
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      logical lus_avgload(nlu)
      integer i,j
      integer nstrlus
      character*3 C3_tlu

      do i=1,nlu
         lus_avgload(i) = .false.
      end do

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lus_avgload'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990
                  
      read(dfile,'(a100)') line ! header line
     
      read(dfile,'(a100)') line
c      print*,line
     
      call d2x(line,last)         
c      call spaceshift(line,last)
      read(line,*)nstrlus
      print*, ' '
      print*, 'Average Load Landuse'
      print*, 'Number of Avg load landuse = ',nstrlus

      do i = 1,nstrlus
         call spaceshift(line,last)
         read(line,*)C3_tlu
         do j = 1,nlu
            if ( luname(j) .eq. C3_tlu ) then
               lus_avgload(j) = .true.
               print*, '   -> Load LU #', i, j, ' ', luname(j)
            end if
         end do
      end do
      print*,' '

      close (dfile)
      return

990   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine getstrlus'
      go to 999

999   call stopreport(report)

      end



      subroutine get_lus_xlbsbmp ( ioscen,lenioscen,
     M                       lus_xlbsbmp )
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      logical lus_xlbsbmp(nlu)
      integer i,j
      integer nstrlus
      character*3 C3_tlu

      do i=1,nlu
         lus_xlbsbmp(i) = .false.
      end do

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lus_xlbsbmp'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990

      read(dfile,'(a100)') line ! header line

      read(dfile,'(a100)') line
c      print*,line

      call d2x(line,last)
c      call spaceshift(line,last)
      read(line,*)nstrlus
      print*, ' '
      print*, 'No lbsBMP cap Landuse'
      print*, 'Number of xlbsbmp landuse = ',nstrlus

      do i = 1,nstrlus
         call spaceshift(line,last)
         read(line,*)C3_tlu
         do j = 1,nlu
            if ( luname(j) .eq. C3_tlu ) then
               lus_xlbsbmp(j) = .true.
               print*, '   -> Load LU #', i, j, ' ', luname(j)
            end if
         end do
      end do
      print*,' '

      close (dfile)
      return

990   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine getstrlus'
      go to 999

999   call stopreport(report)

      end



      subroutine get_lus_nonphys ( ioscen,lenioscen,
     M                       lus_nonphys )
      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      logical lus_nonphys(nlu)
      integer i,j
      integer nstrlus
      character*3 C3_tlu

      do i=1,nlu
         lus_nonphys(i) = .false.
      end do

      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/lus_nonphys'

      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 990

      read(dfile,'(a100)') line ! header line

      read(dfile,'(a100)') line
c      print*,line

      call d2x(line,last)
c      call spaceshift(line,last)
      read(line,*)nstrlus
      print*, ' '
      print*, 'Non-physical Load Landuse'
      print*, 'Number of non-physical landuse = ',nstrlus

      do i = 1,nstrlus
         call spaceshift(line,last)
         read(line,*)C3_tlu
         do j = 1,nlu
            if ( luname(j) .eq. C3_tlu ) then
               lus_nonphys(j) = .true.
               print*, '   -> Load LU #', i, j, ' ', luname(j)
            end if
         end do
      end do
      print*,' '

      close (dfile)
      return

990   report(1) = 'Problem: unable to open file'
      report(2) = fnam
      report(3) = 'from subroutine getstrlus'
      go to 999

999   call stopreport(report)

      end

