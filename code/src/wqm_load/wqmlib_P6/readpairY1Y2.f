************************************************************************
** Program to populate eos wdms with river input variables            **
**  wdms are stored in ./wdm/river/$scen/eos/$seg.wdm                 **
**    general strategy:                                               **
**      Run the program once for each river or bfl subseg             **
**      For each land segment in the river                            **
**         for each land use in the segment                           **
**           determine the time series of river input by:             **
**             extracting relevant dsns                               **
**             multiply by the etm binary files                       **
**           add to the appropriate river variable                    **
**      sum over all segments                                         **
**      store in river wdm                                            **
**      write appropriate output files (eos, wqm)                     **
************************************************************************
      subroutine readpairY1Y2(rscen,Tlseg,Trseg,
     I                    ioscen,lenioscen,
     I                    reqSRTy,reqSRTm,reqSRTd,
     I                    reqENDy,reqENDm,reqENDd,
     I                    nBvar,Bname,nRv2Bv,Rname2Bv,Rfactor,
     I                    nRvar,Rname,RvarBMP,
     I                    nLvar,Ldsn,Lname,Lfactor,LandScen,
     O                    pairwq)  ! get the data
      implicit none

      include '../wqmlib/transfer_wdm.inc'
      include '../wqmlib_P6/time_period.inc'
Ctest      double precision acc(maxRvar)
      integer year,month,day

      integer reqSRTy,reqSRTm,reqSRTd,reqENDy,reqENDm,reqENDd
      character*13 Trseg
      character*6 Tlseg
c      real pairwq(366,1990:2010,maxBvar)
c      real pairwq(366,1984:2001,maxBvar)
c      real pairwq(366,1995:2014,maxBvar)
      real pairwq(366,ISYEAR:IEYEAR,maxBvar)
      integer ny              ! year of infomation to get

      real tlufac
      real lufac(ndaymax,nlu)           ! variables to hold the etm file
      real bmpfac(ndaymax,maxBMPcon,nlu)
      real tranfac_ltw(maxBMPcon,nlu) 
      real tranfac_str(maxBMPcon,nlu)
      real dailypounds(ndaymax,maxBMPcon,nlu)
      real dailypoundsEOT(ndaymax,maxBMPcon,nlu)

      real dvaltemp(ndaymax,maxRvar)   ! daily values to river

      character*50 LStest(nlu)             ! test the etm file

      integer jday,jdaytot,j1,j2    ! julian day determined from hours

      character*3 c3varid
      real fac1,fac2

      integer i, l, Rvar, Lvar, nv, nlconi, nb, nbi, hour, nR2B !indices
      integer wdmlnd,etmfil
       
      parameter (etmfil=dfile+1)
      parameter (wdmlnd=etmfil+11)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      logical dothislu(maxL2R,nlu)       ! decide to use this land use
      integer numsegs,nl2r   ! index for L2R

      integer ndaysinyear,ndays,nd
      external ndaysinyear

      integer julian
      external julian

      logical found

      logical isastrlu(nlu)
      logical lus_annload(nlu)
      logical lus_avgload(nlu)
      logical lus_xlbsbmp(nlu)
      logical lus_nonphys(nlu)

      integer etmSRTy,etmSRTm,etmSRTd  ! start time of etm file
      integer etmENDy,etmENDm,etmENDd  ! stop time of etm file
      integer day1 ! first day of requested data relative to etm
      integer hour1  ! first hour of requested data relative to etm

************** variables to calcuate pound BMPs
      double precision annEORbcon(EarliestYear:LatestYear)
      real AnnualPoundFactor(EarliestYear:LatestYear)
      real AnnualPoundFactorEOT(EarliestYear:LatestYear)
      double precision AnnualPounds(EarliestYear:LatestYear,
     .                              MaxBMPcon,nlu)
      double precision AnnualPoundsEOT(EarliestYear:LatestYear,
     .                              MaxBMPcon,nlu)



      real CVP2N, CVN2BOD
      integer iNO3D, iNH3D, iNH3A, iNH3I, iNH3C, iRORN,
     .        iPO4D, iPO4A, iPO4I, iPO4C, iRORP, iBODA, iLORN, iLORP
      real fNO3D(ndaymax), fNH3D(ndaymax), fNH3A(ndaymax),
     .     fNH3I(ndaymax), fNH3C(ndaymax), fRORN(ndaymax),
     .     fPO4D(ndaymax), fPO4A(ndaymax), fPO4I(ndaymax),
     .     fPO4C(ndaymax), fRORP(ndaymax), fBODA(ndaymax),
     .     fLORN(ndaymax), fLORP(ndaymax)

      real AVG_NO3D,AVG_NH3D,AVG_NH3A,AVG_NH3I,AVG_NH3C
      real AVG_RORN,AVG_LORN
      real AVG_TOTN
      real NOXfrac,NO3Dfac,NH3Xfac,ORGNfac

      real fTOTN(ndaymax), fTOTP(ndaymax), fSEDM(ndaymax)

************ END DECLARATIONS ******************************************
      call getBMPcons(
     I                ioscen,lenioscen,
     O                BMPconname,nbcon)


********* GET START AND END DATE IN WDM FORMAT
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

      do nv = 1,ndays     ! initialize temp variables
        do Rvar = 1,maxRvar
          dvaltemp(nv,Rvar) = 0.0
        end do
      end do

******* GET THE LIST OF STREAM LOAD LAND-USES
      call getstrlus(ioscen,lenioscen,isastrlu)
      call get_lus_annload(ioscen,lenioscen,lus_annload)
      call get_lus_avgload(ioscen,lenioscen,lus_avgload)
      call get_lus_xlbsbmp(ioscen,lenioscen,lus_xlbsbmp)
      call get_lus_nonphys(ioscen,lenioscen,lus_nonphys)

*********  OPEN AND READ ETM FILE
      call lencl(rscen,lenrscen)
      call lencl(Tlseg,lenlseg)
      call lencl(Trseg,lenrseg)
      fnam = tree//'output/etm/'//rscen(:lenrscen)//'/'//
     .       Tlseg(:lenlseg)//'to'//Trseg(:lenrseg)//'.etm'
      open(etmfil,file=fnam,status='old',form='unformatted',iostat=err)
      if (err.ne.0) go to 991
      print*, 'BHATT 1'
      read(etmfil)
     .            etmSRTy,etmSRTm,etmSRTd,
     .            etmENDy,etmENDm,etmENDd,
     .            lufac,bmpfac,tranfac_ltw,tranfac_str,
     .            dailypounds,dailypoundsEOT,dothislu,LStest(1)
      print*, 'BHATT 2'
      close(etmfil)

************* test data
      print*,'LStest-',LStest,'-',LandScen(1),'-'
      print*,'LStest-',LStest(1),'-',LandScen(1),'-'
      if (LStest(1).ne.LandScen(1)) go to 997
      jday = julian(etmENDy,etmENDm,etmENDd,reqENDy,reqENDm,reqENDd)
      if (jday.gt.1) go to 9951
      jday = julian(etmSRTy,etmSRTm,etmSRTd,reqSRTy,reqSRTm,reqSRTd)
      if (jday.lt.1) go to 9952

      day1 = jday

************** klugy section - dothislu should be indexed to nlu, not 
************** lr2 and nlu.  need to figure out which index of dothis lu
************** to use.  Not available in main program
      call lencl(rscen,lenrscen)
      call getl2r(
     I            Trseg,rscen,lenrscen,
     O            numsegs,l2r)
      found = .false.
      do nl2r = 1,numsegs
        if (l2r(nl2r).eq.Tlseg) then
          found = .true.
          exit 
        end if
      end do
      if (.not.found) go to 993
*********** end klugy section

      do l = 1,nlu   ! loop over land uses
         
        if (luname(l).eq.'wat') dothislu(nl2r,l) = .false.
        if ( isastrlu(l) ) dothislu(nl2r,l) = .true.
        if ( lus_nonphys(l) ) dothislu(nl2r,l) = .true.

        if (luname(l).eq.'sho') dothislu(nl2r,l) = .false.

        if (.not.dothislu(nl2r,l)) cycle

c        call ttyput(' ')
c        call ttyput(luname(l))

************ READY TO OPEN WDM FILE
        call lencl(LandScen(l),lenls)
        if ( lus_annload(l) .or. lus_avgload(l) ) then
           wdmfnam = outwdmdir//'land/'//luname(l)//'/'//
     .                LandScen(l)(:lenls)//'/'//luname(l)//'_'//
     .                Tlseg(:lenlseg)//'_'//Trseg(:lenrseg)//'.wdm'
        else
           wdmfnam = outwdmdir//'/land/'//luname(l)//'/'//
     .            LandScen(l)(:lenls)//'/'//luname(l)//
     .            Tlseg(:lenlseg)//'.wdm'
        end if
        call wdbopnlong(wdmlnd+l,wdmfnam,1,err)     ! open land read only
        if (err .ne. 0) go to 998

        do Rvar = 1,nRvar  ! loop over river variables

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

          do Lvar = 1,nLvar(Rvar,l)  ! loop over land variables

************************************************************************
!              for each land variable that goes to a river variable
!              1.  get the hourly variable amount
!              2.  find out what type of variable it is (e.g. tn tp)
!              3.  multiply by land use
!              4.  multiply by appropriate bmp factor
!              5.  multiply by the land to water factor
!              6.  add to river variable temporary variable
************************************************************************

            call gethourdsn(wdmlnd+l,sdate,edate,
     I                       Ldsn(Rvar,l,Lvar),
     O                       nvals,hval)                     !(1)

************ check if land factor is a flag for segment-specific factor
*************** if so, get the factor
            fac1 = Lfactor(Rvar,l,Lvar)                    !(3)
            if (fac1.lt.0.0) then
              write(c3varid,'(I3)') int(abs(Lfactor(Rvar,l,Lvar)))
              call getvarfac2(
     I                       ioscen,lenioscen,c3varid,
     I                       Lname(Rvar,l,Lvar),Rname(Rvar),Tlseg,
     O                       fac1)
            end if

            fac1 = fac1 * tranfac_ltw(nb,l) * tranfac_str(nb,l)    !(4)
 
            jdaytot = nvals/24
            if (jdaytot.ne.ndays) go to 992
            nv = 0

            do jday = 1,jdaytot
              tlufac = lufac(jday+day1-1,l)
              if(lus_nonphys(l)) tlufac = 1.0
              fac2 = fac1 * tlufac *
     .                      bmpfac(jday+day1-1,nb,l)
              do hour = 1,24
                nv = nv + 1
                dvaltemp(jday,Rvar)=fac2*hval(nv)+dvaltemp(jday,Rvar)
              end do
            end do          ! end loop over days

          end do           ! end loop over land variables

        end do            ! end loop over river variables in land use

        call wdflcl (wdmlnd+l,err)
C        if (err.ne.0) call wdflc1(wdmlnd+l,err)  ! causes problems
        if (err.ne.0) go to 996

*************** take the lb reduction BMPs out of the
*********** daily EOS rvar loads and the hvaltemp loads
********** dealing with a particular land use
************** start loop over BMP constituents
        do nb = 1,nbcon

************ calculate annual pound reduction for the BMPcon
          do year = reqSRTy,reqENDy
            AnnualPounds(year,nb,l) = 0.0
            AnnualPoundsEOT(year,nb,l) = 0.0

          end do
          year = reqSRTy
          month = reqSRTm
          day = reqSRTd
          do jday = 1,jdaytot
            AnnualPounds(year,nb,l) = AnnualPounds(year,nb,l)
     .                              + dailypounds(jday+day1-1,nb,l)
     .                                  * tranfac_str(nb,l)
            AnnualPoundsEOT(year,nb,l) =
     .                         AnnualPoundsEOT(year,nb,l)
     .                       + dailypoundsEOT(jday+day1-1,nb,l)
            call tomorrow(year,month,day)
          end do

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
              annEORbcon(year) = annEORbcon(year) + dvaltemp(jday,Rvar)
              call tomorrow(year,month,day)
            end do
          end do

********** QUICK FIX kludge.  The sediment is in tons rather than pounds
********** the annual load needs to be converted before the factor is
************ applied.  A better fix would be to put a conversion
************ factor to pounds in the rchres_in file
c          if (BMPconname(nb).eq.'sed') then
c            do year = reqSRTy,reqENDy
c              annEOSbcon(year) = annEOSbcon(year) * 2000.0
c            end do
c          end if
****************** end QUICK FIX

************* annual factor
          do year = reqSRTy,reqENDy
                  AnnualPoundFactor(year) =
     .                   (annEORbcon(year) - AnnualPounds(year,nb,l))
c                  print*,AnnualPoundFactor(year),annEORbcon(year),
c     .               AnnualPounds(year,nb,l,ns)

                  if (AnnualPoundFactor(year) .le. 0.0) then
                    if (lus_xlbsbmp(l) .eqv. .false.) then
                      ! this improves the accuracy but will result in
                      ! inconsistency in hourly river loads if anyEOS is set to false
                     AnnualPounds(year,nb,l) = annEORbcon(year) ! BHATT added
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


                  if ( AnnualPoundsEOT(year,nb,l) .eq. 0 ) then
                     AnnualPoundFactorEOT(year) = 1.0
                  else
                     AnnualPoundFactorEOT(year) = annEORbcon(year)
     .                   - AnnualPounds(year,nb,l)
     .                   - AnnualPoundsEOT(year,nb,l)
c                  print*,nb,year,AnnualPoundsEOT(year,nb,l,ns),
c     .               annEORbcon(year)-AnnualPounds(year,nb,l,ns)
                     if ( AnnualPoundFactorEOT(year) .le. 0 ) then
                        if(annEORbcon(year)-AnnualPounds(year,nb,l)
     .                        .ne. 0 ) then
                           AnnualPoundFactorEOT(year) =
     .                       AnnualPoundFactorEOT(year) /
     .                       (annEORbcon(year)-AnnualPounds(year,nb,l))
                        else
                         AnnualPoundFactorEOT(year) = 1.0
                        end if
                     else
                        AnnualPoundFactorEOT(year) =
     .                     AnnualPoundFactorEOT(year) /
     .                     (annEORbcon(year)-AnnualPounds(year,nb,l))
                     end if
                  end if
          end do

****************** apply factor to all EOS
          do Rvar = 1,nRvar
            if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle
            year = reqSRTy
            month = reqSRTm
            day = reqSRTd
            do jday = 1,jdaytot
              dvaltemp(jday,Rvar) = dvaltemp(jday,Rvar)
     .                            * AnnualPoundFactor(year)
     .                            * AnnualPoundFactorEOT(year)
              call tomorrow(year,month,day)
            end do
          end do


        end do  ! end loop over BMP constituents
*************** END ADD POUND BMPs

      end do            ! end loop over land uses in segment

Ctest*********** check code
Ctest      do Rvar = 1,nRvar
Ctest        acc(Rvar) = 0.0
Ctest        do jday = 1,jdaytot
Ctest          acc(Rvar) = acc(Rvar)+ dvaltemp(jday,Rvar)
Ctest        end do
Ctest      end do
Ctest      print*,(Rname(Rvar),',',Rvar=1,nRvar)
Ctest      print*,(acc(Rvar),',',Rvar=1,nRvar)
Ctest************** end check






************* HANDLING NO3D and TOTN Balance: START ***********
        AVG_NO3D = 0.0
        AVG_NH3D = 0.0
        AVG_NH3A = 0.0
        AVG_NH3I = 0.0
        AVG_NH3C = 0.0
        AVG_RORN = 0.0
        AVG_LORN = 0.0
        AVG_TOTN = 0.0

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
        if ( .not. (iNO3D.eq.-9 .or. iNH3D.eq.-9 .or. iNH3A.eq.-9 .or.
     .              iNH3I.eq.-9 .or. iNH3C.eq.-9 .or. iRORN.eq.-9 .or.
     .              iLORN.eq.-9 ) ) then
        !{
          print*,'... Applying the NO23|TOTN Calculations'
          do nv = 1,jdaytot
            fNO3D(nv) = dvaltemp(nv,iNO3D)
            fNH3D(nv) = dvaltemp(nv,iNH3D)
            fNH3A(nv) = dvaltemp(nv,iNH3A)
            fNH3I(nv) = dvaltemp(nv,iNH3I)
            fNH3C(nv) = dvaltemp(nv,iNH3C)
            fRORN(nv) = dvaltemp(nv,iRORN)
            fLORN(nv) = dvaltemp(nv,iLORN)
            fTOTN(nv) = fNO3D(nv)+fNH3D(nv)+fNH3A(nv)+fNH3I(nv)+
     .                 fNH3C(nv)+fRORN(nv)+fLORN(nv)
            AVG_NO3D = AVG_NO3D + fNO3D(nv)
            AVG_NH3D = AVG_NH3D + fNH3D(nv)
            AVG_NH3A = AVG_NH3A + fNH3A(nv)
            AVG_NH3I = AVG_NH3I + fNH3I(nv)
            AVG_NH3C = AVG_NH3C + fNH3C(nv)
            AVG_RORN = AVG_RORN + fRORN(nv)
            AVG_LORN = AVG_LORN + fLORN(nv)
            AVG_TOTN = AVG_TOTN + fTOTN(nv)
          end do
          AVG_NO3D = AVG_NO3D / ( reqENDy - reqSRTy + 1 )
          AVG_NH3D = AVG_NH3D / ( reqENDy - reqSRTy + 1 )
          AVG_NH3A = AVG_NH3A / ( reqENDy - reqSRTy + 1 )
          AVG_NH3I = AVG_NH3I / ( reqENDy - reqSRTy + 1 )
          AVG_NH3C = AVG_NH3C / ( reqENDy - reqSRTy + 1 )
          AVG_RORN = AVG_RORN / ( reqENDy - reqSRTy + 1 )
          AVG_LORN = AVG_LORN / ( reqENDy - reqSRTy + 1 )
          AVG_TOTN = AVG_TOTN / ( reqENDy - reqSRTy + 1 )

          print*,'AVG_NO3D  = ',AVG_NO3D
          print*,'AVG_NH3D  = ',AVG_NH3D
          print*,'AVG_NH3A  = ',AVG_NH3A
          print*,'AVG_NH3I  = ',AVG_NH3I
          print*,'AVG_NH3C  = ',AVG_NH3C
          print*,'AVG_RORN  = ',AVG_RORN
          print*,'AVG_LORN  = ',AVG_LORN
          print*,'AVG_TOTN  = ',AVG_TOTN

          print*,Trseg(:lenrseg),Tlseg(:lenlseg),' old NOX fraction = ',
     .             AVG_NO3D/AVG_TOTN
          call getnox(
     I           rscen,Trseg,Tlseg,AVG_TOTN,
     O           NOXfrac)
          print*,Trseg(:lenrseg),Tlseg(:lenlseg),' reg NOX fraction = ',
     .             NOXfrac
          print*,AVG_TOTN,NOXfrac,AVG_NO3D
          NO3Dfac = AVG_TOTN * NOXfrac / AVG_NO3D
          NH3Xfac = AVG_TOTN * (1-NOXfrac) / (AVG_TOTN-AVG_NO3D)
          ORGNfac = AVG_TOTN * (1-NOXfrac) / (AVG_TOTN-AVG_NO3D)
          print*,'fac ',NO3Dfac,NH3Xfac,ORGNfac

          do nv = 1,jdaytot
            if ( AVG_NO3D .gt. 0 ) then
              dvaltemp(nv,iNO3D) = fNO3D(nv) * NO3Dfac
            else
              dvaltemp(nv,iNO3D) = fTOTN(nv) * NOXfrac
            end if
            dvaltemp(nv,iNH3D) = fNH3D(nv) * NH3Xfac
            dvaltemp(nv,iNH3A) = fNH3A(nv) * NH3Xfac
            dvaltemp(nv,iNH3I) = fNH3I(nv) * NH3Xfac
            dvaltemp(nv,iNH3C) = fNH3C(nv) * NH3Xfac
            dvaltemp(nv,iRORN) = fRORN(nv) * ORGNfac
            dvaltemp(nv,iLORN) = fLORN(nv) * ORGNfac
          end do
        else
          print*,'... *NOT* Applying the NO23|TOTN Calculations'
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
         print*,'... Applying the BODA Calculations'
         do nv = 1,jdaytot
           fRORN(nv) = dvaltemp(nv,iRORN)
           fRORP(nv) = dvaltemp(nv,iRORP)
           fLORN(nv) = dvaltemp(nv,iLORN)
           fLORP(nv) = dvaltemp(nv,iLORP)
           fBODA(nv) = dvaltemp(nv,iBODA)
           if(fBODA(nv).gt.0) goto 801

           if ( fLORN(nv) + fLORP(nv) .gt. 0 ) then
             if ( fLORN(nv) .lt. CVP2N * fLORP(nv) ) then
               fBODA(nv) = CVN2BOD * fLORN(nv)
               fRORP(nv) = fRORP(nv) + ( fLORP(nv) - fLORN(nv) / CVP2N )
             else
               fBODA(nv) = CVN2BOD * fLORP(nv) * CVP2N
               fRORN(nv) = fRORN(nv) + ( fLORN(nv) - fLORP(nv) * CVP2N )
             end if
           end if

           dvaltemp(nv,iRORN) = fRORN(nv)
           dvaltemp(nv,iRORP) = fRORP(nv)
           dvaltemp(nv,iLORN) = 0.0
           dvaltemp(nv,iLORP) = 0.0
           dvaltemp(nv,iBODA) = fBODA(nv)
         end do
        else
           print*,'... *NOT* Applying the BODA Calculations'
        end if
************* HANDLING BODA: FINISH ***********







********** done with wdm file arrange data in pairwq
      do nB = 1,nBvar  ! initialize
        do ny = reqSRTy,reqENDy
          do i = 1,366
            pairwq(i,ny,nB) = 0.0
          end do
        end do
      end do

      do nB = 1,nBvar  ! loop over Bvars
        do nR2B = 1,nRv2Bv(nB)  ! loop over Rvars in this Bvar
          do Rvar = 1,nRvar  ! look for matches
            if (Rname2Bv(nB,nR2B).eq.Rname(Rvar)) then ! Match!!

              nd = 0
              do ny = reqSRTy,reqENDy
                j1 = 1
                j2 = ndaysinyear(ny)
                if (ny.eq.reqSRTy) j1 =julian(ny,1,1,ny,reqSRTm,reqSRTd)
                if (ny.eq.reqENDy) j2 =julian(ny,1,1,ny,reqENDm,reqENDd)
                do i = j1,j2
                  nd = nd + 1
                  pairwq(i,ny,nB) = pairwq(i,ny,nB) 
     .                            + dvaltemp(nd,Rvar) * Rfactor(nB,nR2B)
                end do
              end do

            end if
          end do
        end do
      end do

      return


********************************* ERROR SPACE **************************

800   report(1) = 'EOR load can not be zero'
      report(2) = luname(l)//' '//Trseg(:lenrseg)//' '//Tlseg(:lenlseg)
     .             //' '//BMPconname(nb)
      write(report(3),*),annEORbcon(year),AnnualPounds(year,nb,l)
      go to 999

801   report(1) = 'BOD is greater than zero'
      report(2) = Trseg(:lenrseg)//' '//Tlseg(:lenlseg)
      write(report(3),*) nv,fBODA(nv)

989   report(1) = 'major problem with transfer logic. Make sure that'
      report(2) = 'files in ./pp/catalog/iovars agree with '
      report(3) = 'each other for bmp consituents'
      go to 999

991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem in wdm, difference between '
      report(2) = 'expected days and read days'
      write(report(3),*)'exp ',ndays,' read ',jdaytot
      go to 999

993   report(1) = 'LRseg '//Tlseg//' '//Trseg
      report(2) = 'found in link file is not a real LRseg'
      report(3) = ' '
      go to 999

994   report(1) = 'major problem with transfer logic. Make sure that'
      report(2) = 'files in ./pp/catalog/iovars agree with files in'
      report(3) = './pp/src/lib/inc/ for lu types in bmp consituents'
      go to 999

9951  report(1) = 'problem with etm file: end date too early'
      write(report(2),*) 'file end: ',etmENDy,' ',etmENDm,' ',etmENDd
      write(report(3),*) 'req end date: ',reqENDy,' 1, 1'
      go to 999

9952  report(1) = 'problem with etm file: start date too late'
      write(report(1),*) 'file start: ',etmSRTy,' ',etmSRTm,' ',etmSRTd
      write(report(3),*) 'req date: ',reqSRTy,' 12, 31'
      go to 999

996   report(1) = 'Error: closing wdm = '
      write(report(1)(22:24),'(i3)')err
      report(2) = ' '
      report(3) = ' '
      go to 999

997   report(1) = '  etm file not correct format '//
     .     Tlseg(:lenlseg)//'to'//Trseg(:lenrseg)
      report(2) = ' expected string '//
     .         LandScen(1)//' after bmpfac variable'
      report(3) = ' got instead the string '//LStest(1)//' readpairY1Y2'
      go to 999

998   if (err.lt.0) then
        report(1) = 'Error: opening wdm= '
        write(report(1)(22:24),'(i3)')err
        report(2) = wdmfnam
      else
        report(1) = wdmfnam
        report(2) = ' is not a wdm file'
      end if
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
