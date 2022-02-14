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
Ctest      double precision acc(maxRvar)
      integer year,month,day

      integer reqSRTy,reqSRTm,reqSRTd,reqENDy,reqENDm,reqENDd
      character*13 Trseg
      character*6 Tlseg
      real pairwq(366,1991:2011,maxBvar)
      integer ny              ! year of infomation to get

      real lufac(ndaymax,nlu)           ! variables to hold the etm file
      real bmpfac(ndaymax,maxBMPcon,nlu)
      real tranfac(maxBMPcon,nlu) 
      real dailypounds(ndaymax,maxBMPcon,nlu)

      real dvaltemp(ndaymax,maxRvar)   ! daily values to river

      character*25 LStest             ! test the etm file

      integer jday,jdaytot,j1,j2    ! julian day determined from hours

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

      integer etmSRTy,etmSRTm,etmSRTd  ! start time of etm file
      integer etmENDy,etmENDm,etmENDd  ! stop time of etm file
      integer day1 ! first day of requested data relative to etm
      integer hour1  ! first hour of requested data relative to etm

************** variables to calcuate pound BMPs
      double precision annEOSbcon(EarliestYear:LatestYear)
      real AnnualPoundFactor(EarliestYear:LatestYear)
      double precision AnnualPounds(EarliestYear:LatestYear,
     .                              MaxBMPcon,nlu)

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

*********  OPEN AND READ ETM FILE
      call lencl(rscen,lenrscen)
      call lencl(Tlseg,lenlseg)
      call lencl(Trseg,lenrseg)
      fnam = tree//'output/etm/'//rscen(:lenrscen)//'/'//
     .       Tlseg(:lenlseg)//'to'//Trseg(:lenrseg)//'.etm'
      open(etmfil,file=fnam,status='old',form='unformatted',iostat=err)
      if (err.ne.0) go to 991
      read(etmfil)
     .            etmSRTy,etmSRTm,etmSRTd,
     .            etmENDy,etmENDm,etmENDd,
     .            lufac,bmpfac,tranfac,dailypounds,dothislu,LStest
      close(etmfil)

************* test data
      if (LStest.ne.LandScen(1)) go to 997
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

        if (.not.dothislu(nl2r,l)) cycle

        call ttyput(' ')
        call ttyput(luname(l))

************ READY TO OPEN WDM FILE
        call lencl(LandScen(l),lenls)
        wdmfnam = outwdmdir//'/land/'//luname(l)//'/'//
     .            LandScen(l)(:lenls)//'/'//luname(l)//
     .            Tlseg(:lenlseg)//'.wdm'
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
            if (abs(fac1+9.0).lt.0.01) then
              call getvarfac(
     I                       ioscen,lenioscen,
     I                       Lname(Rvar,l,Lvar),Rname(Rvar),Tlseg,
     O                       fac1)
            end if

            fac1 = fac1 * tranfac(nb,l)           !(4)
 
            jdaytot = nvals/24
            if (jdaytot.ne.ndays) go to 992
            nv = 0

            do jday = 1,jdaytot
              fac2 = fac1 * lufac(jday+day1-1,l) *
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
          end do
          year = reqSRTy
          month = reqSRTm
          day = reqSRTd
          do jday = 1,jdaytot
            AnnualPounds(year,nb,l) = AnnualPounds(year,nb,l)
     .                              + dailypounds(jday+day1-1,nb,l)
            call tomorrow(year,month,day)
          end do

************ calculate annual loads by BMP constituent
          do year = reqSRTy,reqENDy
            annEOSbcon(year) = 0.0
          end do
          do Rvar = 1,nRvar
            if (RvarBMP(Rvar).ne.BMPconname(nb)) cycle
            year = reqSRTy
            month = reqSRTm
            day = reqSRTd
            do jday = 1,jdaytot
              annEOSbcon(year) = annEOSbcon(year) + dvaltemp(jday,Rvar)
              call tomorrow(year,month,day)
            end do
          end do

********** QUICK FIX kludge.  The sediment is in tons rather than pounds
********** the annual load needs to be converted before the factor is
************ applied.  A better fix would be to put a conversion
************ factor to pounds in the rchres_in file
          if (BMPconname(nb).eq.'sed') then
            do year = reqSRTy,reqENDy
              annEOSbcon(year) = annEOSbcon(year) * 2000.0
            end do
          end if
****************** end QUICK FIX

************* annual factor
          do year = reqSRTy,reqENDy
            AnnualPoundFactor(year) =
     .                 (annEOSbcon(year) - AnnualPounds(year,nb,l))
            if (AnnualPoundFactor(year) .le. 0.0) then
              AnnualPoundFactor(year) = 0.0
            else
              AnnualPoundFactor(year) = AnnualPoundFactor(year)
     .                                /  annEOSbcon(year)
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

997   report(1) = '  etm file not correct format'
      report(2) = ' expected the string '//
     .         LandScen(1)//' after bmpfac variable'
      report(3) = '  got instead the string '//LStest
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

