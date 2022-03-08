************************************************************************
**  reformat ps input files                                           **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      integer y1,y2  ! start and end year
      parameter (y1=1991,y2=2000)

************ data scenario and raw data location
      character*35 psscen            ! ps data scenario for wdm location
      character*60 dataversion,nonCSOfnam,CSOfnam ! location of raw data
      integer lenpsscen,lendv,lennonCSOfnam,lenCSOfnam
      character*60 CSOfacfnam
      integer lenCSOfacfnam

************** conversion factors
      real mg2acft    ! output is daily time series ac-ft/day
                    ! divided in UCI to ac-ft/hour
      parameter (mg2acft=1000000./7.479/43560.)

      real acftC2heat  ! multiply acft*temp (c) to get heat
      parameter (acftC2heat = 4.895e6)

      real bod2lorn, bod2lorp ! labile fraction of orn and orp
                          ! which is interpreted in HSPF as refractory
      real rorn2rorc  ! ratio of refractory ORC to refractory ORN
      parameter (bod2lorn=0.0436,bod2lorp=0.00603,rorn2rorc=5.678)

      real bod2tss  ! bod is particulate and counted in VSS of TSS
      parameter (bod2tss=0.505)

      real tempC(12) ! assumed temperature in degrees C
      data tempC /10.,10.,14.,16.,20.,23.,25.,25.,23.,20.,16.,14./

*************** facility information
      character*10 Tfac
      integer TdisPoint
      integer Tfips
      character*6 cell

**************** variables related to WWTP
      integer maxkeys,nkeys,nkey,nk     ! combined key: type+rseg+lseg+facility 
      parameter (maxkeys = 6000)
      character*100 combkey(maxkeys),longkey
     
             ! indexed monthly
      real lrflow(maxkeys,y1:y2,12),Tflow
      real lrheat(maxkeys,y1:y2,12)
      real lrbodx(maxkeys,y1:y2,12),Tbod
      real lrtssx(maxkeys,y1:y2,12),Ttss
      real lrnh3x(maxkeys,y1:y2,12),Tnh3
      real lrno3x(maxkeys,y1:y2,12),Tno3
      real lrornx(maxkeys,y1:y2,12),Torn
      real lrpo4x(maxkeys,y1:y2,12),Tpo4
      real lrorpx(maxkeys,y1:y2,12),Torp
      real lrdoxx(maxkeys,y1:y2,12),Tdox
      real lrorcx(maxkeys,y1:y2,12)

      real Ndum,Pdum  ! dummies for TN and TP
      character*10, type1, type2

*************** variables for each data type
      real WWTPflow(maxkeys)
      real WWTPheat(maxkeys)
      real WWTPbodx(maxkeys)
      real WWTPtssx(maxkeys)
      real WWTPnh3x(maxkeys)
      real WWTPno3x(maxkeys)
      real WWTPornx(maxkeys)
      real WWTPpo4x(maxkeys)
      real WWTPorpx(maxkeys)
      real WWTPdoxx(maxkeys)
      real WWTPorcx(maxkeys)

*************** variables for CSOs
      integer maxCSOkeys,nCSOkeys,nCSOkey  ! combined CSO keys
      parameter (maxCSOkeys = 200)
      character*100 CSOkey(maxkeys)

      real annCSOflow(maxCSOkeys,y1:y2)
      real annCSOheat(maxCSOkeys,y1:y2)
      real annCSObodx(maxCSOkeys,y1:y2)
      real annCSOtssx(maxCSOkeys,y1:y2)
      real annCSOnh3x(maxCSOkeys,y1:y2)
      real annCSOno3x(maxCSOkeys,y1:y2)
      real annCSOornx(maxCSOkeys,y1:y2)
      real annCSOpo4x(maxCSOkeys,y1:y2)
      real annCSOorpx(maxCSOkeys,y1:y2)
      real annCSOdoxx(maxCSOkeys,y1:y2)
      real annCSOorcx(maxCSOkeys,y1:y2)

      real CSOflow(maxCSOkeys)
      real CSOheat(maxCSOkeys)
      real CSObodx(maxCSOkeys)
      real CSOtssx(maxCSOkeys)
      real CSOnh3x(maxCSOkeys)
      real CSOno3x(maxCSOkeys)
      real CSOornx(maxCSOkeys)
      real CSOpo4x(maxCSOkeys)
      real CSOorpx(maxCSOkeys)
      real CSOdoxx(maxCSOkeys)
      real CSOorcx(maxCSOkeys)

********** NPDES number, CSO BMP fatcors
      integer maxfacs,nfacs,nf
      parameter (maxfacs = 100)
      character*10 facility(maxfacs)
      real TNfac(maxfacs),TPfac(maxfacs),TSSfac(maxfacs)

************** utility variables
      character*400 longline,longfnam,command
      character*2 state 

      integer ny,nm,nd,ndays

      integer ndaysinmonth
      external ndaysinmonth

      integer tnerr,tperr
      integer CSOtnerr,CSOtperr

      logical found,scompcase 

      integer lentype1,lentype2,lenTfac,lenkey

**************** END DECLARATIONS **************************************

******** get input data
      read*, psscen,dataversion,nonCSOfnam,CSOfnam,CSOfacfnam
      call lencl(psscen,lenpsscen)
      call lencl(dataversion,lendv)
      call lencl(nonCSOfnam,lennonCSOfnam)
      call lencl(CSOfnam,lenCSOfnam)

**************  intitialize all variables
      do nk = 1,maxkeys  
        do ny = y1,y2
          do nm = 1,12
            lrflow(nk,ny,nm) = 0.0
            lrheat(nk,ny,nm) = 0.0
            lrbodx(nk,ny,nm) = 0.0
            lrtssx(nk,ny,nm) = 0.0
            lrnh3x(nk,ny,nm) = 0.0
            lrno3x(nk,ny,nm) = 0.0
            lrornx(nk,ny,nm) = 0.0
            lrpo4x(nk,ny,nm) = 0.0
            lrorpx(nk,ny,nm) = 0.0
            lrdoxx(nk,ny,nm) = 0.0
            lrorcx(nk,ny,nm) = 0.0
          end do
        end do
        WWTPflow(nk) = 0.0
        WWTPheat(nk) = 0.0
        WWTPbodx(nk) = 0.0
        WWTPtssx(nk) = 0.0
        WWTPnh3x(nk) = 0.0
        WWTPno3x(nk) = 0.0
        WWTPornx(nk) = 0.0
        WWTPpo4x(nk) = 0.0
        WWTPorpx(nk) = 0.0
        WWTPdoxx(nk) = 0.0
        WWTPorcx(nk) = 0.0
      end do

      do nk = 1,maxCSOkeys
        do ny = y1,y2
          annCSOflow(nk,ny) = 0.0
          annCSOheat(nk,ny) = 0.0
          annCSObodx(nk,ny) = 0.0
          annCSOtssx(nk,ny) = 0.0
          annCSOnh3x(nk,ny) = 0.0
          annCSOno3x(nk,ny) = 0.0
          annCSOornx(nk,ny) = 0.0
          annCSOpo4x(nk,ny) = 0.0
          annCSOorpx(nk,ny) = 0.0
          annCSOdoxx(nk,ny) = 0.0
          annCSOorcx(nk,ny) = 0.0
        end do
        CSOflow(nk) = 0.0
        CSOheat(nk) = 0.0
        CSObodx(nk) = 0.0
        CSOtssx(nk) = 0.0
        CSOnh3x(nk) = 0.0
        CSOno3x(nk) = 0.0
        CSOornx(nk) = 0.0
        CSOpo4x(nk) = 0.0
        CSOorpx(nk) = 0.0
        CSOdoxx(nk) = 0.0
        CSOorcx(nk) = 0.0
      end do

************ open the data file - nonCSO
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//nonCSOfnam(:lennonCSOfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading point source data from'
      print*,longfnam
      
      read(dfile,*)longline  ! ditch header row

      tnerr=0
      tperr=0

************ loop over all lines and populate variables
      nkeys = 0
      do
        read(dfile,'(a400)',end=111,err=992) longline
        call d2x(longline,last)
        read(longline,*,end=993,err=993)
     .                                  type1,type2,
     .                                  rseg,lseg,cell,
     .                                  Tfac,TdisPoint,Tfips,
     .                                  ny,nm,nd,
     .                                  Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                                  Ndum,Tpo4,Torp,Pdum,Ttss

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) tperr=tperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) tnerr=tnerr+1

**************** get total number of combined key represented
        call lencl(type1,lentype1)
        call lencl(type2,lentype2)
        call lencl(rseg,lenrseg)
        call lencl(lseg,lenlseg)
        call lencl(Tfac,lenTfac)

        longkey = type1(:lentype1)//','//type2(:lentype2)//','//
     .            rseg(:lenrseg)//','//lseg(:lenlseg)//','//
     .            Tfac(:lenTfac)
        found = .false.
        do nkey = 1,nkeys
          if (scompcase(combkey(nkey),longkey)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nkeys = nkeys + 1
          if (mod(nkeys,100).eq.0) print*,'read ',nkeys,' nkeys'
          if (nkeys.gt.maxkeys) go to 994
          nkey = nkeys
          combkey(nkey) = longkey
        end if
       
************** add this line to lrseg variables
        do ny = y1,y2  ! start loop that copies to all years
          lrflow(nkey,ny,nm) = lrflow(nkey,ny,nm) + Tflow*mg2acft
          lrheat(nkey,ny,nm) = lrheat(nkey,ny,nm) 
     .                       + Tflow*mg2acft*tempC(nm)*acftC2heat
          lrdoxx(nkey,ny,nm) = lrdoxx(nkey,ny,nm) + Tdox
          lrnh3x(nkey,ny,nm) = lrnh3x(nkey,ny,nm) + Tnh3
          lrno3x(nkey,ny,nm) = lrno3x(nkey,ny,nm) + Tno3
          lrpo4x(nkey,ny,nm) = lrpo4x(nkey,ny,nm) + Tpo4

          Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
          lrornx(nkey,ny,nm) = lrornx(nkey,ny,nm) + Torn-Tbod*bod2lorn
          lrorpx(nkey,ny,nm) = lrorpx(nkey,ny,nm) + Torp-Tbod*bod2lorp
          lrorcx(nkey,ny,nm) = lrorcx(nkey,ny,nm) 
     .                       + (Torn-Tbod*bod2lorn)*rorn2rorc
          lrbodx(nkey,ny,nm) = lrbodx(nkey,ny,nm) + Tbod
          lrtssx(nkey,ny,nm) = lrtssx(nkey,ny,nm) + Ttss
        end do  ! end loop that copies to all years
      end do
111   close(dfile)
       
*********** summarize annual loads
      do nkey = 1,nkeys   
        do ny = y1,y2
          do nm = 1,12
            ndays = ndaysinmonth(ny,nm)
            if(lrornx(nkey,ny,nm) .lt. 0.0) lrornx(nkey,ny,nm)=0.0       ! elimate potential very small negative value
            if(lrorpx(nkey,ny,nm) .lt. 0.0) lrorpx(nkey,ny,nm)=0.0
            if(lrorcx(nkey,ny,nm) .lt. 0.0) lrorcx(nkey,ny,nm)=0.0
            WWTPflow(nkey) = WWTPflow(nkey)+lrflow(nkey,ny,nm)*ndays
            WWTPheat(nkey) = WWTPheat(nkey)+lrheat(nkey,ny,nm)*ndays
            WWTPbodx(nkey) = WWTPbodx(nkey)+lrbodx(nkey,ny,nm)*ndays
            WWTPtssx(nkey) = WWTPtssx(nkey)+lrtssx(nkey,ny,nm)*ndays
            WWTPnh3x(nkey) = WWTPnh3x(nkey)+lrnh3x(nkey,ny,nm)*ndays
            WWTPno3x(nkey) = WWTPno3x(nkey)+lrno3x(nkey,ny,nm)*ndays
            WWTPornx(nkey) = WWTPornx(nkey)+lrornx(nkey,ny,nm)*ndays
            WWTPpo4x(nkey) = WWTPpo4x(nkey)+lrpo4x(nkey,ny,nm)*ndays
            WWTPorpx(nkey) = WWTPorpx(nkey)+lrorpx(nkey,ny,nm)*ndays
            WWTPdoxx(nkey) = WWTPdoxx(nkey)+lrdoxx(nkey,ny,nm)*ndays
            WWTPorcx(nkey) = WWTPorcx(nkey)+lrorcx(nkey,ny,nm)*ndays
          end do
        end do
        WWTPflow(nkey) = WWTPflow(nkey)/real(y2-y1+1)        ! average annual over time period
        WWTPheat(nkey) = WWTPheat(nkey)/real(y2-y1+1)
        WWTPbodx(nkey) = WWTPbodx(nkey)/real(y2-y1+1)
        WWTPtssx(nkey) = WWTPtssx(nkey)/real(y2-y1+1)
        WWTPnh3x(nkey) = WWTPnh3x(nkey)/real(y2-y1+1)
        WWTPno3x(nkey) = WWTPno3x(nkey)/real(y2-y1+1)
        WWTPornx(nkey) = WWTPornx(nkey)/real(y2-y1+1)
        WWTPpo4x(nkey) = WWTPpo4x(nkey)/real(y2-y1+1)
        WWTPorpx(nkey) = WWTPorpx(nkey)/real(y2-y1+1)
        WWTPorcx(nkey) = WWTPorcx(nkey)/real(y2-y1+1)
      end do
 
*********** done with all data
      print*,'got all non CSO data into memory'
      print*, '  '

***************CSO data ****************************************
************* read in BMP factors for CSO
      call getCSOfacs(
     I                maxfacs,dataversion,lendv,CSOfacfnam,
     O                facility,TNfac,TPfac,TSSfac,nfacs)

************ open the CSO data file
      longfnam = tree//'input/unformatted/point_source/'//
     .           dataversion(:lendv)//'/'//CSOfnam(:lenCSOfnam)
      open(dfile,file=longfnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      print*,'reading CSO data from'
      print*,longfnam

      read(dfile,*)longline  ! ditch header row

      CSOtnerr=0
      CSOtperr=0

************ loop over all lines and populate variables
      nCSOkeys = 0
      do
        read(dfile,'(a400)',end = 222,err=992)longline
        call d2x(longline,last)
        read(longline,*,end=993,err=993)
     .                                  rseg,lseg,cell,
     .                                  Tfac,TdisPoint,Tfips,
     .                                  ny,nm,nd,
     .                                  Tflow,Tbod,Tdox,Tnh3,Tno3,Torn,
     .                                  Ndum,Tpo4,Torp,Pdum,Ttss

        if (ny.lt.y1.or.ny.gt.y2) cycle  ! only process within years

************* check for TN and TP errors
        if (abs(Tpo4+Torp-Pdum).gt.0.1.and.
     .      abs((Tpo4+Torp-Pdum)/Pdum).gt.0.05) CSOtperr=CSOtperr+1
        if (abs(Tnh3+Tno3+Torn-Ndum).gt.0.2.and.
     .      abs((Tnh3+Tno3+Torn-Ndum)/Ndum).gt.0.05) CSOtnerr=CSOtnerr+1

************* check for all zeros
        if (Tflow+Tbod+Tdox+Tnh3+Tno3+Torn+Tpo4+Torp+Ttss.lt.0.01) cycle
        
************* get BMP factors for CSO
         found = .false.
         do nf = 1,nfacs
           if (Tfac.eq.facility(nf)) then
             found = .true.
************* Multiply all consitutens by BMP factors
             Tflow = Tflow*(1.0-TNfac(nf))
             Tbod  = Tbod*(1.0-TNfac(nf))
             Tdox  = Tdox*(1.0-TNfac(nf))
             Tnh3  = Tnh3*(1.0-TNfac(nf))
             Tno3  = Tno3*(1.0-TNfac(nf))
             Torn  = Torn*(1.0-TNfac(nf))
             Tpo4  = Tpo4*(1.0-TPfac(nf))
             Torp  = Torp*(1.0-TPfac(nf))
             Ttss  = Ttss*(1.0-TSSfac(nf))
             exit
           end if
        end do
        if (.not.found) go to 995

*************** find lrseg index if already exists, otherwise create new
        call lencl(rseg,lenrseg)
        call lencl(lseg,lenlseg)
        call lencl(Tfac,lenTfac)

        longkey = 'NA,CSO,'//rseg(:lenrseg)//','//lseg(:lenlseg)//','//
     .            Tfac(:lenTfac)
        found = .false.
        do nCSOkey = 1,nCSOkeys
          if (scompcase(CSOkey(nCSOkey),longkey)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then
          nCSOkeys = nCSOkeys + 1
          if (mod(nCSOkeys,10).eq.0) then
            print*,'read ',nCSOkeys,' CSO combined keys'
          end if
          if (nCSOkeys.gt.maxCSOkeys) go to 996
          nCSOkey = nCSOkeys
          CSOkey(nCSOkey) = longkey
        end if

************** add this line to lrseg variables
       annCSOflow(nCSOkey,ny) = annCSOflow(nCSOkey,ny)
     .                          + Tflow*mg2acft
       annCSOheat(nCSOkey,ny) = annCSOheat(nCSOkey,ny)
     .                          + Tflow*mg2acft*tempC(nm)*acftC2heat
       annCSOdoxx(nCSOkey,ny) = annCSOdoxx(nCSOkey,ny) + Tdox
       annCSOnh3x(nCSOkey,ny) = annCSOnh3x(nCSOkey,ny) + Tnh3
       annCSOno3x(nCSOkey,ny) = annCSOno3x(nCSOkey,ny) + Tno3
       annCSOpo4x(nCSOkey,ny) = annCSOpo4x(nCSOkey,ny) + Tpo4

       Tbod = min(Torn/bod2lorn,Torp/bod2lorp,Tbod)
       annCSOornx(nCSOkey,ny) = annCSOornx(nCSOkey,ny)
     .                          + Torn-Tbod*bod2lorn
       annCSOorpx(nCSOkey,ny) = annCSOorpx(nCSOkey,ny)
     .                          + Torp-Tbod*bod2lorp
       annCSOorcx(nCSOkey,ny) = annCSOorcx(nCSOkey,ny)
     .                          + (Torn-Tbod*bod2lorn)*rorn2rorc
       annCSObodx(nCSOkey,ny) = annCSObodx(nCSOkey,ny)+Tbod
       annCSOtssx(nCSOkey,ny) = annCSOtssx(nCSOkey,ny)+Ttss

      end do
222   close(dfile)
      print*,'got all CSO data into memory'

********** calculate average annual CSO loads
      do nCSOkey = 1, nCSOkeys
        do ny = y1, y2
          CSOflow(nCSOkey) = CSOflow(nCSOkey) + annCSOflow(nCSOkey,ny)
          CSOheat(nCSOkey) = CSOheat(nCSOkey) + annCSOheat(nCSOkey,ny)
          CSOdoxx(nCSOkey) = CSOdoxx(nCSOkey) + annCSOdoxx(nCSOkey,ny) 
          CSOnh3x(nCSOkey) = CSOnh3x(nCSOkey) + annCSOnh3x(nCSOkey,ny) 
          CSOno3x(nCSOkey) = CSOno3x(nCSOkey) + annCSOno3x(nCSOkey,ny) 
          CSOpo4x(nCSOkey) = CSOpo4x(nCSOkey) + annCSOpo4x(nCSOkey,ny) 
          CSOornx(nCSOkey) = CSOornx(nCSOkey) + annCSOornx(nCSOkey,ny)
          CSOorpx(nCSOkey) = CSOorpx(nCSOkey) + annCSOorpx(nCSOkey,ny)
          CSOorcx(nCSOkey) = CSOorcx(nCSOkey) + annCSOorcx(nCSOkey,ny)
          CSObodx(nCSOkey) = CSObodx(nCSOkey) + annCSObodx(nCSOkey,ny) 
          CSOtssx(nCSOkey) = CSOtssx(nCSOkey) + annCSOtssx(nCSOkey,ny)
        end do
        CSOflow(nCSOkey) = CSOflow(nCSOkey)/real(y2-y1+1) 
        CSOheat(nCSOkey) = CSOheat(nCSOkey)/real(y2-y1+1) 
        CSOdoxx(nCSOkey) = CSOdoxx(nCSOkey)/real(y2-y1+1) 
        CSOnh3x(nCSOkey) = CSOnh3x(nCSOkey)/real(y2-y1+1) 
        CSOno3x(nCSOkey) = CSOno3x(nCSOkey)/real(y2-y1+1) 
        CSOpo4x(nCSOkey) = CSOpo4x(nCSOkey)/real(y2-y1+1) 
        CSOornx(nCSOkey) = CSOornx(nCSOkey)/real(y2-y1+1) 
        CSOorpx(nCSOkey) = CSOorpx(nCSOkey)/real(y2-y1+1) 
        CSOorcx(nCSOkey) = CSOorcx(nCSOkey)/real(y2-y1+1) 
        CSObodx(nCSOkey) = CSObodx(nCSOkey)/real(y2-y1+1) 
        CSOtssx(nCSOkey) = CSOtssx(nCSOkey)/real(y2-y1+1) 
      end do

********** write out annual point source loads
      longfnam = ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .           '/full_TSS_summary_ps_loads.csv'
      open(dfile,file=longfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,'(a,a,a)',err=951) 'significance,source,rseg,lseg,',
     .                             'NPDES,year1,year2,',
     .                   'flow,heat,bod,do,nh3,no3,po4,tss,orn,orp,orc'

**********write waste water loads
      do nk = 1,nkeys
        call lencl(combkey(nk),lenkey)
        write(dfile,1234,err=951) combkey(nk)(:lenkey),',',
     .                        y1,',',y2,',',
     .                        WWTPflow(nk),',',
     .                        WWTPheat(nk),',',
     .                        WWTPbodx(nk),',',
     .                        WWTPdoxx(nk),',',
     .                        WWTPnh3x(nk),',',
     .                        WWTPno3x(nk),',',
     .                        WWTPpo4x(nk),',',
     .                        WWTPtssx(nk),',',
     .                        WWTPornx(nk),',',
     .                        WWTPorpx(nk),',',
     .                        WWTPorcx(nk)
      end do

********** write CSO loads
      do nk = 1,nCSOkeys
        call lencl(CSOkey(nk),lenkey)
        write(dfile,1234,err=951) CSOkey(nk)(:lenkey),',',
     .                        y1,',',y2,',',
     .                        CSOflow(nk),',',
     .                        CSOheat(nk),',',
     .                        CSObodx(nk),',',
     .                        CSOdoxx(nk),',',
     .                        CSOnh3x(nk),',',
     .                        CSOno3x(nk),',',
     .                        CSOpo4x(nk),',',
     .                        CSOtssx(nk),',',
     .                        CSOornx(nk),',',
     .                        CSOorpx(nk),',',
     .                        CSOorcx(nk)
      end do

      close(dfile)
1234  format(a,a1,i4,a1,i4,11(a1,e10.3))

      stop

************************* ERROR SPACE **********************************
951   report(1) = 'error writing to file'
      report(2) = longfnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'could not open file'
      report(2) = longfnam
      report(3) = ' '
      print*,longfnam
      go to 999

992   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = longline
      go to 999

993   report(1) = 'problem reading file:  near line: could not parse'
      report(2) = longfnam
      report(3) = longline 
      print*, Tfac,nm,nd,ny,Tflow,Tbod,Tdox,Tnh3,
     .                    Tno3,Torn,Ndum,Tpo4,Torp,Pdum,'tss',Ttss
      go to 999

994   report(1) = 'more combined key than anticipated in WWTP file'
      report(2) = 'fix maxkeys in ./code/src/'
      report(3) = 'data_import/point_source/'
      go to 999

995   report(1) = 'facility not found in reduction file:'
      report(2) = Tfac
      report(3) = 'check reduction file'
      go to 999

996   report(1) = 'more comined key than anticipated in CSO file'
      report(2) = 'fix maxCSOkeys in ./code/src/'
      report(3) = 'data_import/point_source/'
      go to 999

999   call stopreport(report)
  
      end
