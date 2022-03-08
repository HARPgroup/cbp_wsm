************************************************************************
** this program gets all the non-land associated loads                **
**  point source, septic, and atmospheric deposition.                 **
**  They are associated with a specific land seg / water seg pair     **
************************************************************************
      implicit none

      include 'data.inc'

***************  daily load
      real dWWTPrvar(ndaymax,maxRvar)     ! daily by river var
      real dCSOrvar(ndaymax,maxRvar) 
      real dINDrvar(ndaymax,maxRvar) 
      real dATDEPrvar(ndaymax,maxRvar) 
      real dSEPrvar(ndaymax,maxRvar) 
      real dRIBrvar(ndaymax,maxRvar)
      real dRPArvar(ndaymax,maxRvar)

      real dWWTPload(ndaymax,nloadmax)    ! daily by load var
      real dCSOload(ndaymax,nloadmax)     
      real dINDload(ndaymax,nloadmax)   
      real dATDEPload(ndaymax,nloadmax)   
      real dSEPload(ndaymax,nloadmax)     
      real dRIBload(ndaymax,nloadmax)
      real dRPAload(ndaymax,nloadmax)

      integer outfil,datwdm,wdmrch
      parameter (outfil=dfile+1)    ! file number for output
      parameter (datwdm=dfile+2)     ! file number for point source
      parameter (wdmrch=dfile+5)    ! file number for fake wdm

      real wateracres(ndaymax)   ! acre to multiply atdep

      integer nlrsegs,numsegs,nlr          ! number of segments 

      character*11 ps         ! atdep, pointsource, or septic
      data ps /'pointsource'/

      character*50 pradscen, psscen, sepscen, ribscen, rpascen
      integer lenpradscen, lenpsscen, lensepscen, lenribscen, lenrpascen

************* output flags
      integer idaily,imonthly,iannual,iaveann
      logical daily,monthly,annual,aveann

      integer year1,year2,hour,jday,ndays
      integer nv,nvar,Rvar,nc,ns,nl,nd

      integer julian
      external julian

      logical doatdep,dops,dosep,dorib,dorpa
      logical found

***************** END DECLARATIONS *************************************

      read(*,*,err=995,end=996) rscen,rseg,
     .                          idaily,imonthly,iannual,iaveann,
     .                          year1,year2

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)

      daily = .true.
      if (idaily.eq.0) daily = .false.
      monthly = .true.
      if (imonthly.eq.0) monthly = .false.
      annual = .true.
      if (iannual.eq.0) annual = .false.
      aveann = .true.
      if (iaveann.eq.0) aveann = .false.

      print*,'creating data output for ',rseg,' ',rscen(:lenrscen)

********  open FAKE WDM file, used ONLY to solve the problem of
*******       opening and closing WDM file, never close this file
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmrch,wdmfnam,0,err)
      if (err .ne. 0) go to 998

********** READ THE CONTROL FILE FOR START AND STOP TIME
      call readcontrol_time(rscen,lenrscen,
     O                      sdate,edate)
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))

********** READ CONTROL FILE FOR SCENARIOS
      call readcontrol_wdm(rscen,lenrscen,
     O                     pradscen,psscen,sepscen,ribscen,rpascen,
     O                     doatdep,dops,dosep,dorib,dorpa)  ! names of wdms
      call lencl(psscen,lenpsscen)
      call lencl(sepscen,lensepscen)
      call lencl(ribscen,lenribscen)
      call lencl(rpascen,lenrpascen)
      call lencl(pradscen,lenpradscen)

********** POPULATE VARIABLES
      call readcontrol_modules(   ! get active modules
     I                         rscen,lenrscen,
     O                         modules,nmod)
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call lencl(ioscen,lenioscen)

      call getRvars(              ! get active Rvars
     I              ioscen,lenioscen,
     I              modules,nmod,
     O              nRvar,Rname)
  
      call loadinfo(            ! get loads for active Rvars
     I              ioscen,lenioscen,
     I              nRvar,Rname,
     O              nloads,loadname,unit,ncons,con,confactor)
   
********** GET DSN, VARIABLE NAMES AND FACTORS
      if (dops) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,ps,
     O               nPSvar,PSdsn,PSname,PSfac)
      end if
    
      if (dosep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,sep,
     O               nSEPvar,SEPdsn,SEPname,SEPfac)
      end if

      if (dorib) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,rib,
     O               nRIBvar,RIBdsn,RIBname,RIBfac)
      end if


      if (dorpa) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,rpa,
     O               nRPAvar,RPAdsn,RPAname,RPAfac)
      end if
     
      if (doatdep) then
        call getvars(
     I               ioscen,lenioscen,
     I               nRvar,Rname,atdep,
     O               nATDEPvar,ATDEPdsn,ATDEPname,ATDEPfac)
      end if
     
********** GET THE SEGMENTS IN THIS RIVER
      call getl2rlist(                                 ! get the exhaustive list of all land-river segments
     I                rseg,rscen,lenrscen,
     O                numsegs,l2r)
 
********** START TO GREP DATA, LOOP OVER LAND SEGMENTS 
      call lencl(wwtp,lenwwtp)
      call lencl(indus,lenindus)
      call lencl(cso,lencso)
      call lencl(sep,lensep)
      call lencl(rib,lenrib)
      call lencl(rpa,lenrpa)
      call lencl(atdep,lenatdep)

      do ns = 1,numsegs

        call lencl(l2r(ns),lenlseg)

        call ttyput('  land segment ')
        call ttyput(l2r(ns)(:lenlseg))
        call ttyput('  ')

        do Rvar = 1,nRvar    ! initialize
          do nd = 1,ndaymax
            dWWTPrvar(nd,Rvar)  = 0.0
            dCSOrvar(nd,Rvar)   = 0.0
            dINDrvar(nd,Rvar)   = 0.0
            dATDEPrvar(nd,Rvar) = 0.0
            dSEPrvar(nd,Rvar)   = 0.0
            dRIBrvar(nd,Rvar)   = 0.0
            dRPArvar(nd,Rvar)   = 0.0
          end do
        end do
       
        do nd = 1,ndaymax
           wateracres(nd)  = 0.0
        end do
      
**************** POINT SOURCE SECTION *********************************
        if (dops) then                 ! get three PS data sets: WWTP,CSO,IND 
                                         ! they use the same dsns, variable names and factors 
************ WWTP DATA
          foundwdm = .false.         
          call ttyput('WWTP, ')
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/'//
     .              wwtp(:lenwwtp)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist
               
          if (foundwdm) then                       ! if WDM file exist for the segment, read in data 
            call wdbopnlong(datwdm,wdmfnam,0,err)  ! otherwise, the loads=0.0 as initilized  
            if (err .ne. 0) go to 998     ! open ps wdm

            do Rvar = 1,nRvar
              if (nPSvar(Rvar).le.0) cycle
              do nvar = 1,nPSvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,PSdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do nv = 1,ndays
                  dWWTPrvar(nv,Rvar) = dWWTPrvar(nv,Rvar) 
     .                               + dval(nv) * PSfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997

          end if               ! end if file exist
          
************ INDUSTRIAL DATA
          foundwdm =.false.
          call ttyput('Indust Point Source, ')
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/'//
     .              indus(:lenindus)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist
       
          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998               ! open ps wdm

            do Rvar = 1,nRvar
              if (nPSvar(Rvar).le.0) cycle
              do nvar = 1,nPSvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,PSdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do nv = 1,ndays
                  dINDrvar(nv,Rvar) = dINDrvar(nv,Rvar)
     .                              + dval(nv) * PSfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if                    ! end if foundwdm = true

************ CSO DATA
          foundwdm =.false.
          call ttyput('CSO, ')
          wdmfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//'/'//
     .              cso(:lencso)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)   ! check if wdm file already exist

          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998     ! open ps wdm

            do Rvar = 1,nRvar
              if (nPSvar(Rvar).le.0) cycle
              do nvar = 1,nPSvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,PSdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do nv = 1,ndays
                  dCSOrvar(nv,Rvar) = dCSOrvar(nv,Rvar)
     .                              + dval(nv) * PSfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if         ! end if file exist

        end if                      ! end if dops

**************** SEPTIC SECTION ***************************************
        if (dosep) then
          call ttyput('Septic, ')
          wdmfnam = ScenDatDir//'river/septic/'//sepscen(:lensepscen)//
     .              '/'//sep(:lensep)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist

          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998               !open septic wdm        

            do Rvar = 1,nRvar
              if (nSEPvar(Rvar).le.0) cycle
              do nvar = 1,nSEPvar(Rvar)
                print*,SEPdsn(Rvar,nvar)
                call getdailydsn(datwdm,sdate,edate,SEPdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990
  
                do nv = 1,ndays
                  dSEPrvar(nv,Rvar) = dSEPrvar(nv,Rvar)
     .                              + dval(nv) * SEPfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if                    ! end if foundwdm 

        end if                      ! end if dosep

**************** RIB SECTION ***************************************
        if (dorib) then
          call ttyput('RIB, ')
          wdmfnam = ScenDatDir//'river/rib/'//ribscen(:lenribscen)//
     .              '/'//rib(:lenrib)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist

          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998               !open rib wdm

            do Rvar = 1,nRvar
              if (nRIBvar(Rvar).le.0) cycle
              do nvar = 1,nRIBvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,RIBdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do nv = 1,ndays
                  dRIBrvar(nv,Rvar) = dRIBrvar(nv,Rvar)
     .                              + dval(nv) * RIBfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if                    ! end if foundwdm

        end if                      ! end if dorib



**************** RPA SECTION ***************************************
        if (dorpa) then
          call ttyput('RPA, ')
          wdmfnam = ScenDatDir//'river/rpaload/'//rpascen(:lenrpascen)//
     .              '/'//rpa(:lenrpa)//'_'//l2r(ns)(:lenlseg)//'_to_'//
     .              rseg(:lenrseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist

          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998               !open rpa wdm        

            do Rvar = 1,nRvar
              if (nRPAvar(Rvar).le.0) cycle
              do nvar = 1,nRPAvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,RPAdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do nv = 1,ndays
                  dRPArvar(nv,Rvar) = dRPArvar(nv,Rvar)
     .                              + dval(nv) * RPAfac(Rvar,nvar)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if                    ! end if foundwdm 

        end if                      ! end if dorpa


**************** ATMOSPHERIC DEPOSITION SECTION ***********************
        if (doatdep) then
          call ttyput('Atmospheric Deposition')
          wdmfnam = ScenDatDir//'climate/prad/'//pradscen(:lenpradscen)
     .              //'/prad_'//l2r(ns)(:lenlseg)//'.wdm'
          print*,wdmfnam
          inquire (file=wdmfnam,exist=foundwdm)       ! check if wdm file already exist

          if (foundwdm) then                        ! if WDM file exist for the segment, open and read data
            call wdbopnlong(datwdm,wdmfnam,0,err)   ! otherwise, the loads=0.0 as initilized
            if (err .ne. 0) go to 998               ! open atdep wdm

            call getwateracres(
     I                         rscen,lenrscen,rseg,l2r(ns),
     I                         sdate,edate,
     O                         wateracres)

            do Rvar = 1,nRvar
              if (nATDEPvar(Rvar).le.0) cycle
              do nvar = 1,nATDEPvar(Rvar)
                call getdailydsn(datwdm,sdate,edate,ATDEPdsn(Rvar,nvar),
     O                           nvals,dval)
                if (nvals.ne.ndays) go to 990

                do jday = 1,ndays
                  dATDEPrvar(jday,Rvar) = dATDEPrvar(jday,Rvar)
     .             + dval(jday) * ATDEPfac(Rvar,nvar) * wateracres(jday)
                end do
              end do
            end do

            call wdflcl(datwdm,err)
            if (err.ne.0) go to 997
          end if                    ! end if foundwdm

        end if                      ! end if dosep

********** CONVERT TO DAILY LOADS
        do nl = 1,nloads

          do nv = 1,ndays  ! initialize
            dWWTPload(nv,nl)  = 0.0
            dCSOload(nv,nl)   = 0.0
            dINDload(nv,nl) = 0.0
            dATDEPload(nv,nl) = 0.0
            dSEPload(nv,nl)   = 0.0
            dRIBload(nv,nl)   = 0.0
            dRPAload(nv,nl)   = 0.0
          end do

          do nc = 1,ncons(nl)
            do Rvar = 1,nRvar        ! get the right dsn
              if (con(nl,nc).eq.Rname(Rvar)) then
                do nv = 1,ndays
                  dWWTPload(nv,nl) = dWWTPload(nv,nl)
     .                         + dWWTPrvar(nv,Rvar) * confactor(nl,nc)
                  dCSOload(nv,nl) = dCSOload(nv,nl)
     .                         + dCSOrvar(nv,Rvar) * confactor(nl,nc)
                  dINDload(nv,nl) = dINDload(nv,nl)
     .                         + dINDrvar(nv,Rvar) * confactor(nl,nc)
                  dSEPload(nv,nl) = dSEPload(nv,nl)
     .                         + dSEPrvar(nv,Rvar) * confactor(nl,nc)
                  dRIBload(nv,nl) = dRIBload(nv,nl)
     .                         + dRIBrvar(nv,Rvar) * confactor(nl,nc)
                  dRPAload(nv,nl) = dRPAload(nv,nl)
     .                         + dRPArvar(nv,Rvar) * confactor(nl,nc)
                  dATDEPload(nv,nl) = dATDEPload(nv,nl)
     .                         + dATDEPrvar(nv,Rvar) * confactor(nl,nc)
                end do
              end if
            end do
          end do
        end do

********** OUTPUT SECTION 
        if (daily) then
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dops,wateracres,wwtp,
     .                       ndays,nloads,loadname,dWWTPload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dops,wateracres,cso,
     .                       ndays,nloads,loadname,dCSOload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dops,wateracres,indus,
     .                       ndays,nloads,loadname,dINDload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dosep,wateracres,sep,
     .                       ndays,nloads,loadname,dSEPload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dorib,wateracres,rib,
     .                       ndays,nloads,loadname,dRIBload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       dorpa,wateracres,rpa,
     .                       ndays,nloads,loadname,dRPAload)
          call writeDATdaily(rscen,l2r(ns),rseg,outfil,sdate,
     .                       doatdep,wateracres,atdep,
     .                       ndays,nloads,loadname,dATDEPload)
        end if

        if (monthly) then

          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,wwtp,
     .                         ndays,nloads,loadname,dWWTPload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,cso,
     .                         ndays,nloads,loadname,dCSOload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,indus,
     .                         ndays,nloads,loadname,dINDload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dosep,wateracres,sep,
     .                         ndays,nloads,loadname,dSEPload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dorib,wateracres,rib,
     .                         ndays,nloads,loadname,dRIBload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dorpa,wateracres,rpa,
     .                         ndays,nloads,loadname,dRPAload)
          call writeDATmonthlyEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                         doatdep,wateracres,atdep,
     .                         ndays,nloads,loadname,dATDEPload)

          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,wwtp,
     .                         ndays,nloads,loadname,dWWTPload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,cso,
     .                         ndays,nloads,loadname,dCSOload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dops,wateracres,indus,
     .                         ndays,nloads,loadname,dINDload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dosep,wateracres,sep,
     .                         ndays,nloads,loadname,dSEPload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dorib,wateracres,rib,
     .                         ndays,nloads,loadname,dRIBload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         dorpa,wateracres,rpa,
     .                         ndays,nloads,loadname,dRPAload)
          call writeDATmonthlyEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                         doatdep,wateracres,atdep,
     .                         ndays,nloads,loadname,dATDEPload)
        end if

        if (annual) then
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,wwtp,
     .                        ndays,nloads,loadname,dWWTPload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,cso,
     .                        ndays,nloads,loadname,dCSOload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,indus,
     .                        ndays,nloads,loadname,dINDload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dosep,wateracres,sep,
     .                        ndays,nloads,loadname,dSEPload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorib,wateracres,rib,
     .                        ndays,nloads,loadname,dRIBload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorpa,wateracres,rpa,
     .                        ndays,nloads,loadname,dRPAload)
          call writeDATannualEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        doatdep,wateracres,atdep,
     .                        ndays,nloads,loadname,dATDEPload)

          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,wwtp,
     .                        ndays,nloads,loadname,dWWTPload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,cso,
     .                        ndays,nloads,loadname,dCSOload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,indus,
     .                        ndays,nloads,loadname,dINDload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dosep,wateracres,sep,
     .                        ndays,nloads,loadname,dSEPload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorib,wateracres,rib,
     .                        ndays,nloads,loadname,dRIBload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorpa,wateracres,rpa,
     .                        ndays,nloads,loadname,dRPAload)
          call writeDATannualEOR(rscen,l2r(ns),rseg,outfil,sdate,
     .                        doatdep,wateracres,atdep,
     .                        ndays,nloads,loadname,dATDEPload)
        end if

        if (aveann) then
          
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,wwtp,
     .                        ndays,nloads,loadname,dWWTPload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,cso,
     .                        ndays,nloads,loadname,dCSOload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dops,wateracres,indus,
     .                        ndays,nloads,loadname,dINDload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dosep,wateracres,sep,
     .                        ndays,nloads,loadname,dSEPload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorib,wateracres,rib,
     .                        ndays,nloads,loadname,dRIBload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        dorpa,wateracres,rpa,
     .                        ndays,nloads,loadname,dRPAload,
     .                        year1,year2)
          call writeDATaveannEOS(rscen,l2r(ns),rseg,outfil,sdate,
     .                        doatdep,wateracres,atdep,
     .                        ndays,nloads,loadname,dATDEPload,
     .                        year1,year2)

          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dops,wateracres,wwtp,
     .                        ndays,nloads,loadname,dWWTPload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dops,wateracres,cso,
     .                        ndays,nloads,loadname,dCSOload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dops,wateracres,indus,
     .                        ndays,nloads,loadname,dINDload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dosep,wateracres,sep,
     .                        ndays,nloads,loadname,dSEPload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dorib,wateracres,rib,
     .                        ndays,nloads,loadname,dRIBload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        dorpa,wateracres,rpa,
     .                        ndays,nloads,loadname,dRPAload,
     .                        year1,year2)
          call writeDATaveannEOR(rscen,l2r(ns),rseg,outfil+6,sdate,
     .                        doatdep,wateracres,atdep,
     .                        ndays,nloads,loadname,dATDEPload,
     .                        year1,year2)
        end if

        print*,' '
      end do          ! end loop over land segs in river segs


      return

************************ ERROR SPACE *************************
990   report(1) = 'problem in wdm '//wdmfnam
      report(2) = 'difference between expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',nvals
      go to 999

994   write(report(1),'(a23,a13,a9)')
     .     'Could not find segment ',rseg
      report(2) = 'in list'
      report(3) = ' '
      go to 999

995   report(1) = 'Error initializing data postprocessor'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and run_postproc.csh'
      go to 999

996   report(1) = 'Error initializing data postprocessor'
      report(2) = 'not enough input data in run_postproc.csh'
      report(3) = ' '
      go to 999

997   report(1) = 'Error:  closing wdm'
      report(2) = wdmfnam
      report(3) = 'Error = '
      write(report(3)(9:11),'(i3)') err
      go to 999

998   report(1) = 'Error: opening wdm= '
      report(2) = wdmfnam
      if (err.lt.0) then
        report(3) = 'Error = '
        write(report(3)(9:11),'(i3)') err
      else
        report(3) = 'Not a wdm file'
      end if
      go to 999

999   call stopreport(report)

      end


