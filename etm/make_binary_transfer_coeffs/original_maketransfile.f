************************************************************************
** program to make the binary file that contains the time-varying     **
**  transfer coefficients for each land use going to a river segment  **
************************************************************************
      subroutine maketransfile(
     I                         rseg,numsegs,l2r,
     I                         rscen,lenrscen,LandScen,
     I                         StartY,StartM,StartD,
     I                         EndY,EndM,EndD,
     I                         trandata,
     I                         LBJday,nLB,LBdata,
     I                         BMPconname,nbcon,
     I                         BmpName,nBmpTypes,
     I                         BmpEff,MaxImplement,AdditiveBmp,
     I                         BmpHydEffType,BmpHydEffParm,
     I                         BmpEffDistType,BmpEffDistParm,seed,
     I                         BBJday,nBB,
     I                         indHGMR,
     I                         BmpAcres,constrained)
      implicit none

      include 'mbtc.f'

      real lufac(ndaymax,nlu)
      real bmpfac(ndaymax,maxBMPcon,nlu)
      real tranfac(maxBMPcon,nlu)
      logical lastindx
      integer i,l,j,indx1,indx2,nBmp1,nBmp2,nb
      integer nLB,nBB,numsegs,ndays
      real denom,lfactor(nlu),bfactor(MaxBmpTypes)
      real maxfactor(MaxBmpTypes)  ! maximum acreage implementation

      real retfreq(ndaymax) ! return frequency of the storm flow
                            ! for days between start and end date
      real retflow(6)

      integer julian
      external julian

      integer seed  ! random number seed

******** temporary storage variables to calcuate daily bmps efficiencies
      real DailyBmpAcres(ndaymax,MaxBmpTypes)
      real DailyIndEff(ndaymax,nbcon,MaxBmpTypes) ! individual bmp 
      real TempEff
      logical tempAdditiveBmp(maxBmpTypes,maxBmpTypes)  ! temp

      logical NoBmpAcres(MaxBmpTypes) ! does this BMP exist in lu & seg

      real LUredux  ! reduction for overstating additive bmps
      integer BmpIndx(MaxBmpTypes)  ! index for bmps in additive routine
      integer nAddBmps  ! number of additive bmps for any given bmp

******* variables to open the dummy wdm file that never closes
      integer dummywdm  ! dummy wdm file
      parameter (dummywdm=dfile+2)
      character*64 wdmfnam

      logical dothislu(maxL2R,nlu)

********** END OF DECLARATION ******************************************

******* open the dummy wdm, never read, never close
      wdmfnam = dummyWDMname
      call wdbopnlong(dummywdm,wdmfnam,1,err) ! open read only
      if (err .ne. 0) go to 991
C       print*,'dummy wdm file open'

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
      do i = 1,numsegs
        do l = 1,nlu
          dothislu(i,l) = .false.
          do nb = 1,nLB
            if (LBdata(nb,i,l).gt.0.00001) dothislu(i,l) = .true.
          end do
        end do
      end do

********** do the same for bmp acres and constrained variable
      if (nBB.eq.1) then
        nBB = 2
        BBJday(2) = ndays
        if (ndays-BBJday(1).le.300) BBJday(2) = BBJday(1) + 1000
        do l = 1,nlu
          do nBmp = 1,nBmpTypes
            do i = 1,numsegs
              BmpAcres(nBmp,2,i,l) = BmpAcres(nBmp,1,i,l)
              constrained(nBmp,2,i,l) = constrained(nBmp,1,i,l)
            end do
          end do
        end do
      end if

******* loop over land segments in this rseg and write a file for each
********* 1. generate the single segment transport factor
********* 2. temporally interpolate the land use
********* 3. perform the BMP calculation as follows:
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

        call lencl(l2r(i),lenlseg)
        call ttyput(l2r(i)(:lenlseg))
        call ttyput(' ')
************** re-arrange transport factor 
        do l = 1,nlu
          do nb = 1,nbcon
            tranfac(nb,l) = trandata(i,nb,l) 
          end do
        end do

C       print*,'interpolate land use'
********* interpolate land use
        j = 1                        
        indx1 = 1
        indx2 = 2
        lastindx = .false.
        
        do while (j.le.ndays)
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

        end do

********** check for negatives
        call fixneglu(
     I                ndaymax,nlu,ndays,
     M                lufac)

C       print*,'calculate BMPs'
***************** calculate BMPS by land use
        do l = 1,nlu  ! for each land use

          if (luname(l).eq.'wat') cycle


************ if zero land use or otherwise not doing this land use
*************** set all bmpfacs to 1 and skip the BMP calculations
          if (.not.dothislu(i,l)) then
            do nb = 1,nbcon
              do j = 1,ndays
                bmpfac(j,nb,l) = 0.0
              end do
            end do
            cycle  ! next land use in this lseg
          end if

          call ttyput(luname(l))
          call ttyput(' ')

************ determine the return frequency of storm flow
          call returnfq(
     I                  LandScen(l),l2r(i),l,
     I                  StartY,StartM,StartD,EndY,EndM,EndD,
     O                  retfreq, retflow)

********** determine if acres for a bmp exist for a land use.
************ BMP acres are initialized to -999, so to interpolate,
********** assign to zero acres if missing for any particular index
          do nBmp = 1,nBmpTypes   ! does this exist
            NoBmpAcres(nBmp) = .true.
            do indx1 = 1,nBB
              if (BmpAcres(nBmp,indx1,i,l).gt.-900) then
                NoBmpAcres(nBmp) = .false.
              else
                BmpAcres(nBmp,indx1,i,l) = 0.0
              end if
            end do
          end do

************ set missing values of efficiency to zero and get into
************* daily variable
          do nBmp = 1,nBmpTypes
            if (NoBmpAcres(nBmp)) cycle
            do nb = 1,nbcon
              TempEff = max(BmpEff(nBmp,nb,indHGMR(i),l),0.0)
              do j = 1,ndays
                DailyIndEff(j,nb,nBmp) = TempEff
              end do
            end do
          end do

****************** randomness effect
          call randomeffect(
     I                      nBmpTypes,NoBmpAcres,BmpName,
     I                      indHGMR(i),l,ndays,
     I                      BmpEffDistType,BmpEffDistParm,seed,
     I                      BMPconname,nbcon,
     M                      DailyIndEff)

C       print*,'hydrologic effect'
****************** hydrologic effect
          call hydroeffect(
     I                     nBmpTypes,NoBmpAcres,BmpName,
     I                     indHGMR(i),l,ndays,
     I                     BmpHydEffType,BmpHydEffParm,retfreq,
     I                     BMPconname,nbcon,
     M                     DailyIndEff)

************* set maximum factor
          do nBmp = 1,nBmpTypes  ! set maximum implementation factor
            maxfactor(nBmp) = MaxImplement(nBmp,indHGMR(i),l)
            do indx1 = 1,nBB
              if (.not.constrained(nBmp,indx1,i,l)) then
                maxfactor(nBmp) = 1.0
                exit
              end if
            end do
          end do

***************** interpolate bmp acres for this land use
***********  bmp acreage is also corrected for going below zero
*********** in an extrapolation or going above the maximum allowable
          j = 1                        
          indx1 = 1
          indx2 = 2
          lastindx = .false.
          do while (j.le.ndays)
            denom = real(BBJday(indx2) - BBJday(indx1))
            do nBmp = 1,nBmpTypes
              if (NoBmpAcres(nBmp)) cycle
              bfactor(nBmp) = 
     .               (BmpAcres(nBmp,indx2,i,l)-BmpAcres(nBmp,indx1,i,l))
     .                                    /denom
            end do
            do while ((j.le.BBJday(indx2).or.lastindx).and.j.le.ndays)
              do nBmp = 1,nBmpTypes
                if (NoBmpAcres(nBmp)) cycle
                DailyBmpAcres(j,nBmp) = BmpAcres(nBmp,indx1,i,l) +
     .                                  bfactor(nBmp) * 
     .                                  real(j-BBJday(indx1))
                DailyBmpAcres(j,nBmp) = max(DailyBmpAcres(j,nBmp),0.0)
                DailyBmpAcres(j,nBmp) = 
     .                        min(DailyBmpAcres(j,nBmp),
     .                            maxfactor(nBmp)*lufac(j,l))
              end do
              j = j + 1
            end do
            if (indx2.eq.nBB) then
              lastindx = .true.
            else
              indx2 = indx2+1
              indx1 = indx1+1
            end if
          end do

C       print*,'daily weighted efficiency'
************* loop over BMPs, days, and constituents and find daily 
************* weighted efficiency modified by land use
          do nBmp = 1,nBmpTypes
            if (NoBmpAcres(nBmp)) cycle
            do nb = 1,nbcon
              do j = 1,ndays
                if (lufac(j,l).gt.0.00001) then
                  DailyIndEff(j,nb,nBmp) = DailyIndEff(j,nb,nBmp) * 
     .                            DailyBmpAcres(j,nBmp) / lufac(j,l)
                else
                  DailyIndEff(j,nb,nBmp) = 0.0
                end if
              end do 
            end do
          end do

C       print*,'additive bmps'
*********          add additive bmps
******* copy the additive matrix to a temporary variable
******* loop over bmps and when an additive match is found, add the
******* acres and the efficiencies from bmp2 to bmp1, then set the
********  acres and efficiency of bmp2 to zero, making sure that there
*******    are enough acres between the two.  
          do nBmp1 = 1,nBmpTypes
            do nBmp2 = 1,nBmpTypes
              tempAdditiveBmp(nBmp1,nBmp2) = AdditiveBmp(nBmp1,nBmp2)
            end do
            tempAdditiveBmp(nBmp1,nBmp1) = .false.  ! will trip up below
          end do

          do nBmp1 = 1,nBmpTypes-1
            if (NoBmpAcres(nBmp1)) then
              do nBmp2 = nBmp1+1,nBmpTypes
                tempAdditiveBmp(nBmp1,nBmp2) = .false. ! if no acres
                tempAdditiveBmp(nBmp2,nBmp1) = .false. ! does not exist
              end do
            else
              nAddBmps = 1
              BmpIndx(nAddBmps) = nBmp1
              do nBmp2 = nBmp1+1,nBmpTypes
                if (tempAdditiveBmp(nBmp1,nBmp2)) then
                  tempAdditiveBmp(nBmp1,nBmp2)=.false. ! delete additive
                  tempAdditiveBmp(nBmp2,nBmp1)=.false. ! triang matrix
                  if (NoBmpAcres(nBmp2)) cycle  ! only if bmp exists
                  nAddBmps = nAddBmps + 1
                  BmpIndx(nAddBmps) = nBmp2
                  NoBmpAcres(nBmp2) = .true.
                end if
              end do

***************** additive bmps found, process into single bmp
              if (nAddBmps.gt.1) then

                do nBmp = 2,nAddBmps ! loop over other bmps

                  nBmp2 = BmpIndx(nBmp)
              
                  do j = 1,ndays ! add acres to bmp1
                    DailyBmpAcres(j,nBmp1) = DailyBmpAcres(j,nBmp1)
     .                                     + DailyBmpAcres(j,nBmp2)
                    DailyBmpAcres(j,nBmp2) = 0.0
                  end do

                  do nb = 1,nbcon  ! add efficiency to bmp1
                    do j = 1,ndays
                      DailyIndEff(j,nb,nBmp1) = DailyIndEff(j,nb,nBmp1)
     .                                        + DailyIndEff(j,nb,nBmp2)
                      DailyIndEff(j,nb,nBmp2) = 0.0
                    end do
                  end do

                  NoBmpAcres(nBmp2) = .true.

                end do  ! loop over other bmps

******************** check that not too many acres
                do j = 1,ndays
                  if (DailyBmpAcres(j,nBmp1) .gt. lufac(j,l)) then
                    LUredux = lufac(j,l)/DailyBmpAcres(j,nBmp1)
                    DailyBmpAcres(j,nBmp1) = lufac(j,l)
                    do nb = 1,nbcon
                      DailyIndEff(j,nb,nBmp1) = DailyIndEff(j,nb,nBmp1)
     .                                        * LUredux
                    end do
                  end if

                end do  ! days
              end if ! if more than one additive

            end if

          end do  ! loop over bmps

          do nBmp1 = 1,nBmpTypes  ! check logic
            do nBmp2 = 1,nBmpTypes
              if (tempAdditiveBmp(nBmp1,nBmp2)) go to 993
            end do
          end do

*********          multiply multiplicative bmps
C       print*,'multiplicative bmps'
          do nb = 1,nbcon
            do j = 1,ndays
              bmpfac(j,nb,l) = 1.0
            end do
          end do

          do nBmp = 1,nBmpTypes
            if (NoBmpAcres(nBmp)) cycle
            do nb = 1,nbcon
              do j = 1,ndays
                bmpfac(j,nb,l) = bmpfac(j,nb,l) 
     .                         * (1.0-DailyIndEff(j,nb,nBmp))
                if (bmpfac(j,nb,l).lt.-0.0001) go to 992
              end do
            end do
          end do


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
     .            lufac,bmpfac,tranfac,dothislu,LandScen,rscen,lenrscen,
     .            rseg,numsegs,l2r,
     .            ndays,checkend

        close (dfile)


CC code to write out factors by day
C        fnam = '/model/temp/'//rseg//l2r(i)(:6)//'.csv'
C        open(19,file=fnam,status='unknown')
C        write(19,*) 'tranfac'
C        do nb = 1,nbcon
C          write(19,'(i5,25(a1,f10.6))') nb, (',',tranfac(nb,l),l=1,nlu)
C        end do
C        write(19,*) 'lufac'
C        do j = 1,ndays
C          write(19,'(i5,25(a1,f10.0))') j, (',',lufac(j,l),l=1,nlu)
C        end do
C        do nb = 1,nbcon
C          write(19,*) 'bmpfac ',nb
C          do j = 1,ndays
C            write(19,'(i5,a1,i5,25(a1,f10.6))') j,',',nb, 
C     .                    (',',bmpfac(j,nb,l),l=1,nlu)
C          end do
C        end do
C        close (19)


        print*,' '
      end do

      return

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
      write(report(1)(22:24),'(i3)')err
      report(2) = fnam
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end
