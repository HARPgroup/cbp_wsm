************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This program reads the sensitivity matrix, calculates summarized   **
**   results and writes the output to csv files for further analysis  **
************************************************************************
      implicit none
      include 'sens.inc'
      include 'sensmatrix.inc'
      include 'startmatrix.inc'
      include 'resultsmatrices.inc'

      integer nr

      real dconc,dpar

      integer testnrsegs,testnconcs
 
      logical missing  ! check for missing values

C      integer nvM1      ! nval - 1 is a popular number
C      parameter (nvM1 = nval - 1)

      real slpnv(nval)  ! temp slope variable
      real slpnr(maxrsegs)

      real STvector(maxrsegs)

      integer denom

      real rsize(maxrsegs)  ! river size

      real median,slope       ! utility functions
      integer imode
      external median,imode,slope

      real findST        ! function in this file
      external findST

      integer npmax,lake

************* END DECLARATIONS *****************************************
      print*,'enter river scenario'
      read*,rscen
      call lencl(rscen,lenrscen)
      print*,'collecting information'

*********** get rsegs and lakeflags
      call getrsegs(rscen,lenrscen,
     O              rsegs,uniqid,dsid,uniqindex,nrsegs) 

********** get paramscen
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getlakeflags(
     I                  paramscen,uniqindex,
     O                  lakeflags)

********** get possible concs
      call readcontrol_Rioscen(
     I                         rscen,lenrscen,
     O                         ioscen)
      call concinfo(               ! POPULATE concentration variables
     I              ioscen,
     O              nconcs,concname)

*********** read and store parameter list
      call readparams(
     I                paramscen,
     O                parLKflag,parModule,parTable,parName,parAorM,
     O                parstart,parmin,parmax,parval,header,npar)

********** read startmatrix
      fnam = outdir//'river/cfd/'//rscen(:lenrscen)//
     .       '/startmatrix.bin'
      open(dfile,file=fnam,form='unformatted',status='old',
     .     iostat=err)
      if (err.ne.0) go to 996
      read(dfile) startmatrix,startksKstat,testnrsegs,testnconcs
      if (nrsegs.ne.testnrsegs.or.nconcs.ne.testnconcs) go to 998
      close(dfile)

********** read sensmatrix 
      fnam = outdir//'river/cfd/'//rscen(:lenrscen)//
     .       '/sensmatrix.bin'
      open(dfile,file=fnam,form='unformatted',status='unknown',
     .     iostat=err)
      if (err.ne.0) go to 996
      read(dfile,err=997) sensmatrix,sensksKstat,testnrsegs,testnconcs
      if (nrsegs.ne.testnrsegs.or.nconcs.ne.testnconcs) go to 998
      close(dfile)


      print*,'Summarizing sensitivity info'
************* summarize information over all segs, bins, concs, and pars
      do nr = 1,nrsegs
        do nb = 1,nbinmax
          do nc = 1,nconcs
            do np = 1,npar

********************* calculate vector of slopes
              missing = .false.
              do nv = 1,nval
                if (abs(sensmatrix(np,nv,nc,nb,nr)+999.0).lt.0.1. or.
     .              abs(startmatrix(nc,nb,nr)+999.0).lt.0.1) then
                  missing = .true.
                  exit
                end if
                dconc = sensmatrix(np,nv,nc,nb,nr) 
     .                  - startmatrix(nc,nb,nr)
                if (parAorM(np).eq.'M') then
                  dpar = log10(parval(np,nv))-log10(parstart(np))
                else
                  dpar = parval(np,nv) - parstart(np)
                end if
                slpnv(nv) = dconc/dpar
              end do

******************* calculate median, type, and monotonicity
              if (missing) then  ! set some defaults if no data
                MedSlope(np,nc,nb,nr) = -999.0
                SlopeType(np,nc,nb,nr) = 0
                Monotonic(np,nc,nb,nr) = .false.
              else

******************* find median
                MedSlope(np,nc,nb,nr) = median(slpnv,nval,nval,err)
                if (err.ne.0) MedSlope(np,nc,nb,nr) = -999.0

****************** find SlopeType
C                SlopeType(np,nc,nb,nr) = findST(slpnv,nvM1)

***************** find monotonicity
                Monotonic(np,nc,nb,nr) = .true.
                if (slpnv(1)*slpnv(2).lt.0.0) then
                  Monotonic(np,nc,nb,nr) = .false.
                end if
                  
              end if
            end do
          end do
        end do
      end do
***************** populated MedSlope, SlopeType, and Monotonic
      print*,' summarizing summaries'
************* summarize into more useful variables
      do np = 1,nparmax
        do nc = 1,nconcs
          do nb = 1,nbinmax

            denom = 0
            FracMono(np,nc,nb) = 0.0
            FracSameSign(np,nc,nb) = 0.0
            do nr = 1,nrsegs
              if (abs(MedSlope(np,nc,nb,nr)+999.0).lt.0.1) cycle
              if (parLKflag(np).ne.2) then
                if (parLKflag(np).ne.lakeflags(nr)) cycle
              end if

              denom = denom + 1  ! increment denominator
C             print*,denom,np,nc,'   finding nonmissing Medslopes'

              slpnr(denom) = MedSlope(np,nc,nb,nr)  ! store slope

              STvector(denom) = SlopeType(np,nc,nb,nr)  

              if (Monotonic(np,nc,nb,nr)) then
                FracMono(np,nc,nb) = FracMono(np,nc,nb) + 1.0 
              end if

              if (MedSlope(np,nc,nb,nr).gt.0.0) then
                FracSameSign(np,nc,nb) = FracSameSign(np,nc,nb) + 1.0
              end if

              read(rsegs(nr)(3:3),'(f1.0)') rsize(denom)
            end do

            if (denom.gt.0) then   ! some exist

              MedMedSlope(np,nc,nb) = median(slpnr,denom,maxrsegs,err)

              ModalSlopeType(np,nc,nb) = 
     .              imode(STvector,denom,maxrsegs,err)

              FracMono(np,nc,nb) = FracMono(np,nc,nb) / real(denom)

              FracSameSign(np,nc,nb) = 
     .            FracSameSign(np,nc,nb) / real(denom)
              if (FracSameSign(np,nc,nb).lt.0.5) then
                FracSameSign(np,nc,nb) = 1.0 - FracSameSign(np,nc,nb)
              end if

              SizeDependence(np,nc,nb) = 
     .               slope(slpnr,rsize,denom,maxrsegs,err)

            else

              MedMedSlope(np,nc,nb) = -999.0
              ModalSlopeType(np,nc,nb) = -999
              FracMono(np,nc,nb) = -999.0
              FracSameSign(np,nc,nb) = -999.0
              SizeDependence(np,nc,nb) = -999.0

            end if

          end do
        end do
      end do

*************** write out a file for each conc
      print*,'writing out concentration file for: '
      do nc = 1,nconcs
        fnam = outdir//'river/cfd/'//rscen(:lenrscen)//'/sens_'//
     .         concname(nc)//'.csv'
        open(dfile,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 996
        print*,fnam

        write(dfile,'(a,a,a,a,a,a)',err=951)
     .    'Module,Table,Variable,Type,LakeFlag,',
     .    'slope1,slopetype1,fracMono1,FracSameSign1,SizeDepend1,',
     .    'slope2,slopetype2,fracMono2,FracSameSign2,SizeDepend2,',
     .    'slope3,slopetype3,fracMono3,FracSameSign3,SizeDepend3,',
     .    'slope4,slopetype4,fracMono4,FracSameSign4,SizeDepend4,',
     .    'slope5,slopetype5,fracMono5,FracSameSign5,SizeDepend5,'
        do np = 1,npar
          write(dfile,1234,err=951) parModule(np),parTable(np),
     .                              parName(np),parAorM(np),
     .                              parLKflag(np),
     .                      (MedMedSlope(np,nc,nb),
     .                      ModalSlopeType(np,nc,nb),FracMono(np,nc,nb),
     .                      FracSameSign(np,nc,nb),
     .                      SizeDependence(np,nc,nb),nb=1,nbinmax)
        end do
        close(dfile)

      end do

************ find the most sensitive parameter for each 
******** concentration, bin, and lakeflag
      print*,'finding most sensitive parameters'
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'/river/'//paramscen(:lenparamscen)//
     .       '/most_sensitive_parameters.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 996
      print*,fnam

      write(dfile,'(a,a)',err=951)
     .    'Concentration,Bin,Lakeflag,Module,Table,Variable,Type,',
     .    'Slope,Slopetype,FracMono,FracSameSign,SizeDepend'
      do nc = 1,nconcs
        do nb = 1,nbinmax
          do lake = 0,2
            npmax = 1
            do np = 2,npar
              if (lake.eq.parLKflag(np)) then
                if (abs(MedMedSlope(np,nc,nb)).gt.
     .            abs(MedMedSlope(npmax,nc,nb))) then
                  if (FracSameSign(np,nc,nb).gt. 0.8) then 
                    if (abs(MedMedSlope(np,nc,nb)+999.0).gt. 0.1) then
                      npmax = np
                    end if
                  end if
                end if
              end if
            end do
            write(dfile,1233,err=951) 
     .                        concname(nc),nb,lake,parModule(npmax),
     .                        parTable(npmax),parName(npmax),
     .                        parAorM(npmax),MedMedSlope(npmax,nc,nb),
     .                        ModalSlopeType(npmax,nc,nb),
     .                        FracMono(npmax,nc,nb),
     .                        FracSameSign(npmax,nc,nb),
     .                        SizeDependence(npmax,nc,nb)
          end do
        end do
      end do
      close(dfile)
          

      print*,'******* done **********'
      return
1233  format(a4,',',i1,',',i1,',',a6,',',a14,',',a14,',',a1,',',
     .       e10.4,',',i1,',',f10.4,',',f10.4,',',e10.4)
1234  format(a6,',',a14,',',a14,',',a1,',',i1,5(',',e10.4,',',i1,
     .       ',',f10.4,',',f10.4,',',e10.4))

********************** ERROR SPACE *************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

996   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

997   report(1) = 'Problem reading file'
      report(2) = fnam
      write(report(3),*) 'Error = ',err
      go to 999

998   report(1) = 'Problem reading file, nrsegs and/or nconcs were not'
     .            //' read correctly'
      report(2) = fnam
      write(report(3),*) nrsegs,' ',testnrsegs,' ',nconcs,' ',testnconcs
      go to 999

999   print*,'****************** ERROR IN RUN *************'
      print*,'   CHECK ',char(39),'PROBLEM',char(39),' FILE'
      call stopreport(report)

      end


************************************************************************
** function to find the slope type by running logical tests ************
**           1 = concave up                                           **
**           2 = concave down                                         **
**           3 = middle is more sensitive                             **
**           4 = ends are more sensitive                              **
**           0 = other                                                **
************************************************************************
      function findST(slope,n)
      implicit none
      integer findST,n,nv
      real slope(n)
      logical conditiontrue

      findST = 0

      conditiontrue = .true.    ! test for concave up
      do nv = 1,n-1
        if (slope(nv+1).lt.slope(nv)) conditiontrue = .false.
      end do
      if (conditiontrue) then
        findST = 1
        return
      end if 

      conditiontrue = .true.    ! test for concave down
      do nv = 1,n-1
        if (slope(nv+1).gt.slope(nv)) conditiontrue = .false.
      end do
      if (conditiontrue) then
        findST = 2
        return
      end if 

      conditiontrue = .true.    ! test for ends least sensitive
      do nv = 2,n-1
        if (slope(nv).lt.slope(1)) conditiontrue = .false.
        if (slope(nv).lt.slope(n)) conditiontrue = .false.
      end do
      if (conditiontrue) then
        findST = 3
        return
      end if 

      conditiontrue = .true.    ! test for ends most sensitive
      do nv = 2,n-1
        if (slope(nv).gt.slope(1)) conditiontrue = .false.
        if (slope(nv).gt.slope(n)) conditiontrue = .false.
      end do
      if (conditiontrue) then
        findST = 4
        return
      end if 

      end
