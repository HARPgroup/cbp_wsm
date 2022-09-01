************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine maketransfile(
     I                         rseg,numsegs,l2r,
     I                         rscen,lenrscen,LandScen,
     I                         StartY,StartM,StartD,
     I                         EndY,EndM,EndD,
     I                         trandataL2W,trandataS2R,
     I                         LBJday,nLB,LBdata,
     I                         BMPconname,nBmpCon,
     I                       C_BMPconnameEOT,I_BMPconnameEOT,nBmpConEOT,
C     I                         BmpName,nBmpTypes,
C     I                         BmpEff,AdditiveBmp,BmpDefined,
C     I                         BmpHydEffType,BmpHydEffParm,
C     I                         BmpEffDistType,BmpEffDistParm,seed,
     I                         BBJday,nBB,
     I                         PBJday,nPB,PoundsReduced,
     I                         PBEOTJday,nPBEOT,PoundsReducedEOT,
C     I                         indHGMR,
C     I                         BmpAcres)
     I                         BmpPassThru)

      implicit none
      include 'mbtc.f'
*************** output time series for etm file
      real lufac(ndaymax,nlu)
      real bmpfac(ndaymax,maxBMPcon,nlu)
      real tranfacL2W(maxBMPcon,nlu)
      real tranfacS2R(maxBMPcon,nlu)
      real dailypounds(ndaymax,maxBMPcon,nlu)
      real dailypoundsEOTx(ndaymax,maxBMPcon,nlu) ! as EOT cons
      real dailypoundsEOT(ndaymax,maxBMPcon,nlu)  ! as BMP cons

      real annPoundsEOT(StartY:EndY,maxBMPcon)
      integer year, month, day

      integer  ndaysinyear
      external ndaysinyear

********* indices
      logical lastindx
      integer i,l,j,indx1,indx2,nBmp1,nBmp2,nb
      integer nLB,nBB,nPB,nPBEOT,numsegs,ndays

      integer npssegs
      character*6 psl2r(maxL2R)

********* calculation variables
      real denom,lfactor(nlu),bfactor(MaxBmpTypes)
      real pfactor(MaxBMPcon,nlu)
      real maxfactor(MaxBmpTypes)  ! maximum acreage implementation

      real retfreq(ndaymax) ! return frequency of the storm flow
                            ! for days between start and end date
      real retflow(6)

      integer julian
      external julian

      integer seed  ! random number seed

******** temporary storage variables to calcuate daily bmps efficiencies
C      real DailyBmpAcres(ndaymax,MaxBmpTypes)
      real DailyBmpPassThru(ndaymax,maxBMPcon)
      real sumDailyBmpAcres   ! temporary summation variable

      real DailyIndEff(ndaymax,nBmpCon,MaxBmpTypes) ! individual bmp 
      real TempEff
      logical tempAdditiveBmp(maxBmpTypes,maxBmpTypes)  ! temp

      logical NoBmpPassThru(MaxBmpCon) ! does this BMP exist in lu & seg

      real LUredux  ! reduction for overstating additive bmps
      integer BmpIndx(MaxBmpTypes)  ! index for bmps in additive routine
      integer nAddBmps  ! number of additive bmps for any given bmp

******* variables to open the dummy wdm file that never closes
      integer dummywdm  ! dummy wdm file
      parameter (dummywdm=dfile+2)
c      character*64 wdmfnam

      logical dothislu(maxL2R,nlu)

************* variables to write out the actual applied bmps
      real RealDailyBmpAcres(ndaymax,nlu,MaxBmpTypes)
      double precision AveAcres,AveBmpAcres(MaxBmpTypes)
      real redux

      integer CSV

      CSV = 0

********** END OF DECLARATION ******************************************

      do year=StartY,EndY
        do nb = 1,nBmpConEOT
          annPoundsEOT(year,nb) = 0.0
        end do
      end do

******* open the dummy wdm, never read, never close
      wdmfnam = dummyWDMname
      call wdbopnlong(dummywdm,wdmfnam,1,err) ! open read only
      if (err .ne. 0) go to 991

      ndays = julian(StartY,StartM,StartD,EndY,EndM,EndD)

******* if only one land use time period is specified
*********  create second point for interpolation
      if (nLB.eq.1) then 
        nLB = 2
        LBJday(2) = ndays
        if (ndays-LBJday(1).le.300) LBJday(2) = LBJday(1) + 1000
        do l = 1,nlu
          do i = 1,numsegs
            LBdata(2,i,l) = LBdata(1,i,l)
          end do
        end do
      end if

********** test to see if all breaks are zero and set dothislu
      do i = 1,maxL2R
        do l = 1,nlu
          dothislu(i,l) = .false.
        end do
      end do
      do i = 1,numsegs
        do l = 1,nlu
          do nb = 1,nLB
            if (LBdata(nb,i,l).gt.0.00001) dothislu(i,l) = .true.
          end do
        end do
      end do

******* if only one BMP acres time period is specified
*********  create second point for interpolation
      if (nBB.eq.1) then
        nBB = 2
        BBJday(2) = ndays
        if (ndays-BBJday(1).le.300) BBJday(2) = BBJday(1) + 1000
        do l = 1,nlu
          do nBmp = 1,nBmpCon
            do i = 1,numsegs
              BmpPassThru(nBmp,1,i,l) = max(BmpPassThru(nBmp,1,i,l),0.0)
              BmpPassThru(nBmp,2,i,l) = BmpPassThru(nBmp,1,i,l)
            end do
          end do
        end do
      end if

******* if only one BMP pounds time period is specified
*********  create second point for interpolation
      if (nPB.eq.1) then
        nPB = 2
        PBJday(2) = ndays
        if (ndays-PBJday(1).le.300) PBJday(2) = PBJday(1) + 1000
        do nb = 1,nBmpCon
          do i = 1,numsegs
            do l = 1,nlu
              PoundsReduced(2,nb,i,l) = PoundsReduced(1,nb,i,l)
            end do
          end do
        end do
      end if

******* if only one BMP pounds EOT time period is specified
*********  create second point for interpolation
      if (nPBEOT.eq.1) then
        nPBEOT = 2
        PBEOTJday(2) = ndays
        if (ndays-PBEOTJday(1).le.300) PBEOTJday(2) = PBEOTJday(1) + 1000
        do nb = 1,nBmpConEOT
          do i = 1,numsegs
            do l = 1,nlu
              PoundsReducedEOT(2,nb,i,l) = PoundsReducedEOT(1,nb,i,l)
            end do
          end do
        end do
      end if

******* loop over land segments in this rseg and write a file for each
********* 1. generate the single segment transport factor
********* 2. temporally interpolate the land use
********* 3. temporally interpolate the BMP pounds
********* 4. perform the BMP calculation as follows:
*********      for each land use
*********        temporally interpolate acres to a daily value
*********        for each day
*********          for each Bmp
*********            calculate the efficiency modified by
*********                   land use, randomness, and hydrology
*********          add additive bmps
*********          multiply multiplicative bmps

********** formula for interpolation is:
*******  break1 + (break2-break1)/(day2-day1) * (currentday - day1)
***********   this holds true up to day2
      do i = 1,numsegs
      !{

        call lencl(l2r(i),lenlseg)
        call ttyput(l2r(i)(:lenlseg))
        call ttyput(' ')
************** re-arrange transport factor 
        do l = 1,nlu
          do nb = 1,nBmpCon
            tranfacL2W(nb,l) = trandataL2W(i,nb,l) 
            tranfacS2R(nb,l) = trandataS2R(i,nb,l)
          end do
        end do

C       print*,'interpolate land use'
********* interpolate land use
        j = 1                        
        indx1 = 1
        indx2 = 2
        lastindx = .false.
        
        do while (j.le.ndays)
        !{
          denom = real(LBJday(indx2) - LBJday(indx1))
          do l = 1,nlu
            lfactor(l) = (LBdata(indx2,i,l)-LBdata(indx1,i,l))/denom
          end do

          do while ((j.le.LBJday(indx2).or.lastindx).and.j.le.ndays)
            do l = 1,nlu
              lufac(j,l) = LBdata(indx1,i,l) + 
     .                     lfactor(l)*real(j-LBJday(indx1))
            end do
            j = j + 1
          end do

          if (indx2.eq.nLB) then
            lastindx = .true.
          else
            indx2 = indx2+1
            indx1 = indx1+1
          end if
        !}
        end do

********** check for negatives
        call fixneglu(
     I                ndaymax,nlu,ndays,
     M                lufac)

********* interpolate BMP pounds
************ disallow negatives only if both indices are non-negative
        j = 1                        
        indx1 = 1
        indx2 = 2
        lastindx = .false.
        
        do while (j.le.ndays)
        !{
          denom = real(PBJday(indx2) - PBJday(indx1))
          do nb = 1,nBmpCon
            do l = 1,nlu
              pfactor(nb,l) = (PoundsReduced(indx2,nb,i,l)
     .                        -PoundsReduced(indx1,nb,i,l))/denom
            end do
          end do

          do while ((j.le.PBJday(indx2).or.lastindx).and.j.le.ndays)
            do nb = 1,nBmpCon
              do l = 1,nlu
                dailypounds(j,nb,l) = PoundsReduced(indx1,nb,i,l) + 
     .                              pfactor(nb,l)*real(j-PBJday(indx1))
                if (dailypounds(j,nb,l).lt.0.0) then
                  if (PoundsReduced(indx1,nb,i,l).ge.0.0 .and.
     .                PoundsReduced(indx2,nb,i,l).ge.0.0 ) then
                     dailypounds(j,nb,l) = 0.0
                  end if
                end if
              end do
            end do
            j = j + 1
          end do

          if (indx2.eq.nPB) then
            lastindx = .true.
          else
            indx2 = indx2+1
            indx1 = indx1+1
          end if
        !}
        end do

********** convert from annual to daily
        year  = StartY
        month = StartM
        day   = StartD
        do j = 1,ndays
          do l = 1,nlu
            do nb = 1,nBmpCon
              dailypounds(j,nb,l) = dailypounds(j,nb,l) 
     .                               / ndaysinyear(year)
            end do
          end do
          call tomorrow(year,month,day)
        end do



********* interpolate BMP pounds EOT
************ disallow negatives only if both indices are non-negative
        j = 1
        indx1 = 1
        indx2 = 2
        lastindx = .false.

        do while (j.le.ndays)
        !{
          denom = real(PBEOTJday(indx2) - PBEOTJday(indx1))
          do nb = 1,nBmpConEOT
            do l = 1,nlu
              pfactor(nb,l) = (PoundsReducedEOT(indx2,nb,i,l)
     .                        -PoundsReducedEOT(indx1,nb,i,l))/denom
            end do
          end do

          do while ((j.le.PBEOTJday(indx2).or.lastindx).and.j.le.ndays)
            do nb = 1,nBmpConEOT
              do l = 1,nlu
                dailypoundsEOTx(j,nb,l) =PoundsReducedEOT(indx1,nb,i,l)+
     .                            pfactor(nb,l)*real(j-PBEOTJday(indx1))
                if (dailypoundsEOTx(j,nb,l).lt.0.0) then
                  if (PoundsReducedEOT(indx1,nb,i,l).ge.0.0 .and.
     .                PoundsReducedEOT(indx2,nb,i,l).ge.0.0 ) then
                     dailypoundsEOTx(j,nb,l) = 0.0
                  end if
                end if
              end do
            end do
            j = j + 1
          end do

          if (indx2.eq.nPBEOT) then
            lastindx = .true.
          else
            indx2 = indx2+1
            indx1 = indx1+1
          end if
        !}
        end do

********** convert from annual to daily
        year  = StartY
        month = StartM
        day   = StartD
        do j = 1,ndays
          do l = 1,nlu
            do nb = 1,nBmpConEOT
              dailypoundsEOTx(j,nb,l) = dailypoundsEOTx(j,nb,l) 
     .                                  / ndaysinyear(year)
            end do
          end do
          call tomorrow(year,month,day)
        end do

********** convert from NO23->tnx NH3X->tnx
        do l = 1,nlu
          do nb = 1,nBmpCon
            do j = 1,ndays
              dailypoundsEOT(j,nb,l) = 0.0
            end do
          end do
        end do


        do l = 1,nlu
          do nb = 1,nBmpConEOT
            year  = StartY
            month = StartM
            day   = StartD
c            print*,luname(l),C_BMPconnameEOT(nb),
c     .             Bmpconname(I_BMPconnameEOT(nb))
            do j = 1,ndays
              dailypoundsEOT(j,I_BMPconnameEOT(nb),l) = 
     .           dailypoundsEOT(j,I_BMPconnameEOT(nb),l)
     .           + dailypoundsEOTx(j,nb,l)
              annPoundsEOT(year,nb) = 
     .                 annPoundsEOT(year,nb) + dailypoundsEOTx(j,nb,l)
              call tomorrow(year,month,day)
            end do
          end do
        end do



C       print*,'calculate BMPs'
***************** calculate BMPS by land use
        do l = 1,nlu  ! for each land use
        !{

          do nb = 1,nBmpCon
             do j = 1,ndays
                bmpfac(j,nb,l) = 1.0
             end do
          end do

          if (luname(l).eq.'wat') cycle


************ if zero land use or otherwise not doing this land use
*************** set all bmpfacs to 1 and skip the BMP calculations
          if (.not.dothislu(i,l)) then
c            do nb = 1,nBmpCon
c              do j = 1,ndays
c                bmpfac(j,nb,l) = 1.0
c              end do
c            end do
            call ttyput('x')
            call ttyput(luname(l))
            call ttyput(' ')
            cycle  ! next land use in this lseg
          end if

          call ttyput(luname(l))
          call ttyput(' ')

************ determine the return frequency of storm flow
CBHATT          call returnfq(
CBHATT     I                  LandScen(l),l2r(i),l,
CBHATT     I                  StartY,StartM,StartD,EndY,EndM,EndD,
CBHATT     O                  retfreq, retflow)

********** assign to zero acres if missing for any particular index
CBHATT          do nBmp = 1,nBmpCon   ! does this exist
CBHATT            NoBmpPassThru(nBmp) = .true.
CBHATT            do indx1 = 1,nBB
CBHATT              if (BmpPassThru(nBmp,indx1,i,l).lt.1) then
CBHATT                NoBmpPassThru(nBmp) = .false.
CBHATT              end if
CBHATT            end do
CBHATT          end do



****************** randomness effect
CBHATT          call randomeffect(
CBHATT     I                      nBmpTypes,NoBmpAcres,BmpName,
CBHATT     I                      indHGMR(i),l,ndays,
CBHATT     I                      BmpEffDistType,BmpEffDistParm,seed,
CBHATT     I                      BMPconname,nBmpCon,
CBHATT     M                      DailyIndEff)
c          do nBmp = 1,nBmpTypes  !GY
c            do nb = 1,nBmpCon  !GY
c              do j = 1,ndays  !GY
c                if (DailyIndEff(j,nb,nBmp) > 0) then  !GY
c                print *, 'maketransfile DailyIndEff 2= ',
c     .                    DailyIndEff(j,nb,nBmp)  !GY
c                end if !GY
c              end do !GY
c            end do  !GY
c          end do  !GY

C       print*,'hydrologic effect'
****************** hydrologic effect
CBHATT          call hydroeffect(
CBHATT     I                     nBmpTypes,NoBmpAcres,BmpName,
CBHATT     I                     indHGMR(i),l,ndays,
CBHATT     I                     BmpHydEffType,BmpHydEffParm,retfreq,
CBHATT     I                     BMPconname,nBmpCon,
CBHATT     M                     DailyIndEff)


***************** interpolate bmp acres for this land use
***********  bmp acreage is also corrected for going below zero
*********** in an extrapolation or going above the maximum allowable
          j = 1                        
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (j.le.ndays)
          !{
            denom = real(BBJday(indx2) - BBJday(indx1))
            do nBmp = 1,nBmpCon
CBHATT              if (NoBmpAcres(nBmp)) cycle
              bfactor(nBmp) = 
     .         (BmpPassThru(nBmp,indx2,i,l)-BmpPassThru(nBmp,indx1,i,l))
     .                                    /denom
            end do
            do while ((j.le.BBJday(indx2).or.lastindx).and.j.le.ndays)
            !{
              do nBmp = 1,nBmpCon
              !{
CBHATT                if (NoBmpAcres(nBmp)) cycle
                DailyBmpPassThru(j,nBmp) = BmpPassThru(nBmp,indx1,i,l) +
     .                                  bfactor(nBmp) * 
     .                                  real(j-BBJday(indx1))
                DailyBmpPassThru(j,nBmp) = 
     .                    max(DailyBmpPassThru(j,nBmp),0.0)
                DailyBmpPassThru(j,nBmp) = 
     .                        min(DailyBmpPassThru(j,nBmp),
     .                            1.0)
                bmpfac(j,nBmp,l) = 
     .                        DailyBmpPassThru(j,nBmp)
              !}
              end do
              j = j + 1
            !}
            end do
            if (indx2.eq.nBB) then
              lastindx = .true.
            else
              indx2 = indx2+1
              indx1 = indx1+1
            end if
          !}
          end do

        !}
        end do  ! end loop over land use for BMPs

************ write out etm file
        call lencl(rseg,lenrseg)

        fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .         l2r(i)(:lenlseg)//'to'//rseg(:lenrseg)//'.etm'
        open (dfile,file=fnam,status='unknown',form='unformatted',
     .        iostat=err)
        if (err.ne.0) go to 994
        write(dfile,err=951) 
     .            StartY,StartM,StartD,
     .            EndY,EndM,EndD,
     .            lufac,bmpfac,tranfacL2W,tranfacS2R,
     .            dailypounds,dailypoundsEOT,
     .            dothislu,LandScen,rscen,lenrscen,
     .            rseg,numsegs,l2r,
     .            ndays,checkend
        close (dfile)

        if ( CSV .eq. 1) then
           fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .            l2r(i)(:lenlseg)//'to'//rseg(:lenrseg)//'.csv'
           open (dfile,file=fnam,status='unknown',iostat=err)
           if (err.ne.0) go to 994
           write(dfile,*)
     .                StartY,StartM,StartD,
     .                EndY,EndM,EndD

           write(dfile,*)'NODE lufac'
           do j=1,ndays
               write(dfile,*) (lufac(j,l),',',l=1,nlu)
           end do

           write(dfile,*)'NODE bmpfac'
           do j=1,ndays
               do nBMP=1,nBmpCon
                  write(dfile,*) BmpConName(nBMP),',',
     .                 (bmpfac(j,nBMP,l),',',l=1,nlu)
               end do
           end do

           write(dfile,*)'NODE tranfac'
           do nBMP=1,nBMPCon
              write(dfile,*) (tranfacL2W(nBMP,l),',',l=1,nlu)
              write(dfile,*) (tranfacS2R(nBMP,l),',',l=1,nlu)
           end do

           write(dfile,*)'NODE dailypounds'
           do l = 1,nlu
              do j = 1,ndays
             write(dfile,*) j,',',(dailypounds(j,nb,l),',',nb=1,nBmpCon)
              end do
           end do

           close (dfile)

        end if

CC code to write out factors by day
C        fnam = '/model/temp/'//rseg//l2r(i)(:6)//'.csv'
C        open(19,file=fnam,status='unknown')
C        write(19,*) 'tranfac'
C        do nb = 1,nBmpCon
C          write(19,'(i5,25(a1,f10.6))') nb, (',',tranfac(nb,l),l=1,nlu)
C        end do
C        write(19,*) 'lufac'
C        do j = 1,ndays
C          write(19,'(i5,25(a1,f10.0))') j, (',',lufac(j,l),l=1,nlu)
C        end do
C        do nb = 1,nBmpCon
C          write(19,*) 'bmpfac ',nb
C          do j = 1,ndays
C            write(19,'(i5,a1,i5,25(a1,f10.6))') j,',',nb, 
C     .                    (',',bmpfac(j,nb,l),l=1,nlu)
C          end do
C        end do
C        close (19)

        print*,' '
      !}
      end do     ! end loop of all land segments to a river


      fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .            rseg(:lenrseg)//'.csv'
      open (dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 994
      write(dfile,*)'Year',(',',C_BMPconnameEOT(nb),nb=1,nBmpConEOT)
      do year=StartY,EndY
         write(dfile,1236),
     .     year,(',',annPoundsEOT(year,nb),nb=1,nBmpConEOT)
      end do

*********** Find Point Source only segments, and write dummy factors 
      call getpsonlyl2r(
     I                  rseg,rscen,lenrscen,numsegs,l2r,
     O                  npssegs,psl2r)
      
      do i = 1, npssegs                   ! creat a.etm file for each PS only segment
        call lencl(psl2r(i),lenlseg)
        fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .         psl2r(i)(:lenlseg)//'to'//rseg(:lenrseg)//'.etm'
        open (dfile,file=fnam,status='unknown',form='unformatted',
     .        iostat=err)
        if (err.ne.0) go to 994

********** assign dummy factors  
        do l = 1,nlu
          dothislu(i,l) = .false.
        end do

        do j = 1,ndays                            
          do l = 1,nlu
            lufac(j,l) = 0.0
          end do
        end do

        do l = 1,nlu
          do nb = 1,nBmpCon
            do j = 1,ndays
              bmpfac(j,nb,l) = 1.0
            end do
          end do
        end do

        do l = 1,nlu
          do nb = 1,nBmpCon
            tranfacL2W(nb,l) = 1.0
            tranfacS2R(nb,l) = 1.0
          end do
        end do

        do l = 1,nlu
          do nb = 1,nBmpCon
            do j = 1,ndays
              dailypounds(j,nb,l) = 0.0
            end do
          end do
        end do

        do l = 1,nlu
          do nb = 1,nBmpCon
            do j = 1,ndays
              dailypoundsEOT(j,nb,l) = 0.0
            end do
          end do
        end do

*********** create a dummy binary file for PS only segments
        write(dfile,err=951)
     .            StartY,StartM,StartD,
     .            EndY,EndM,EndD,
     .            lufac,bmpfac,tranfacL2W,tranfacS2R,
     .            dailypounds,dailypoundsEOT,
     .            dothislu,LandScen,rscen,lenrscen,
     .            rseg,npssegs,psl2r,
     .            ndays,checkend

        close (dfile)
     
      end do    ! end loop of all PS only segments        

      return

1234  format(a13,50(',',A20))
1235  format(a3,',',e14.7,50(',',e14.7))
1236  format(I4,10(A,E13.6))

**************** ERROR SPACE *****************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Error: problem opening wdm= '
      write(report(1)(22:24),'(i3)')err
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

992   report(1) = 'Error in bmp specifications or code logic'
      write(report(2),*) 'efficiency for land use ',luname(l),' day ',j
      write(report(3),*) 'constituent ',BMPconname(nb),' is ',
     .                    bmpfac(j,nb,l)
      go to 999

993   report(1) = 'something wrong with additive logic'
      report(2) = 'reprogramming required'
      report(3) = ''
      go to 999

994   report(1) = 'Error: problem opening file= '
      report(2) = fnam
      report(3) = 'error ='
      write(report(3)(9:11),'(i3)')err
      go to 999

996   report(1) = 'error in bmp efficincy defination'
      write(report(2),*) 'UNDEFINED ',rseg,',',l2r(i),',',luname(l),',',
     .                        BmpName(nBmp),',',BmpAcres(nBmp,1,i,l)
      report(3) = 'check bmp efficieny in ./input/scenario/riiver/
     .             bmptyes/additive_bmps.csv'
      go to 999

999   call stopreport(report)

      end