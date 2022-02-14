************************************************************************
**** Get information about BMPs.                                      **
************************************************************************
      subroutine getBmpSpecs(
     I                       BmpTypeScen,BMPconname,nbcon,
     O                       nCats,CatName,CatLU,nLUinCat,
     O                       allHGMRs,nHGMR,
     O                       LongBmpName,BmpName,BmpEff,nBmpTypes,
     O                       AdditiveBmp,BmpDefined,
C     O                       MaxImplement,AdditiveBmp,
     O                       BmpHydEffType,BmpHydEffParm,
     O                       BmpEffDistType,BmpEffDistParm)
      implicit none
      include 'mbtc.f'

      integer lenscen

      logical found
      logical comment
      external comment

      character*2700 longline ! BHATT changed 2000 to 2300 on Oct 28 2013
                              ! GY changed 2300 to 2700 on May 8 2014

      character*3 templu(nlu)  ! temporary reading variable for land use
      integer nl, nl2, nb, nb2  ! indices
      integer nBmpH, nBmpV, nBmp2 ! horizontal and vertical indices
      integer H1,H2,B1,B2,C1,C2  ! indices

*********** header order for reading columns
      integer order(maxBmpTypes)

*********** variables for reading bmp description header
      character*3 tBMPcon
      logical firstline
      integer nbcons  ! number of bmp constituents

      real Teff(maxBMPcon)  ! temporary efficiency variable
C      real TmaxI        ! temporary max implementation variable
      integer Tadd(maxBmpTypes) ! temporary additive variable
      real Ttype,Tparm(nparms)  ! temporary distribution variables

******** temp printing variable
      real realvector(MaxBmpTypes)
      logical writeout
      data writeout /.false./ ! all input verified 6/27/2008

*********** END DECLARATIONS *******************************************

      call lencl(BmpTypeScen,lenscen)

      if (writeout) then
        fnam = '/model/temp/testout.csv'
        open(99,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991
      end if
      
*********** get HGMR information
      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)
     .       //'/HGMRs.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nHGMR = 1
      do 
        read(dfile,'(a1000)',err=992,end=100) longline
        call d2x(longline,last)
        if (last.ge.1000-1) go to 990  ! check for overrun
        if (comment(longline)) cycle
        read(longline,'(a4)',err=993,end=994) allHGMRs(nHGMR)
        call lowercase(allHGMRs(nHGMR))
        nHGMR = nHGMR + 1
      end do
100   nHGMR = nHGMR - 1
      close(dfile)
      if (nHGMR.gt.maxHGMR) go to 985

      if (writeout) then
        write(99,*) 'HGMR ',nHGMR
        do nb = 1,nHGMR
         write(99,*) nb,', ',allHGMRs(nb)
        end do
      end if

********** get land use categories
      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)//'/'
     .       //'landuse_categories.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nCats = 1
      do 
        read(dfile,'(a1000)',err=992,end=200) longline
        call d2x(longline,last)
        if (last.ge.1000-1) go to 990  ! check for overrun
        if (comment(longline)) cycle
        read(longline,*,err=993,end=994) CatName(nCats),nLUinCat(nCats),
     .                                 (templu(nl),nl=1,nLUinCat(nCats))

        call lowercase(CatName(nCats))

        do nl = 1,nLUinCat(nCats)  ! assign land use number
          found = .false.
          do nl2 = 1,nlu
            if (templu(nl).eq.luname(nl2)) then
              found = .true.
              CatLU(nCats,nl) = nl2
              exit
            end if
          end do
          if (.not.found) go to 995
        end do

        nCats = nCats + 1
        if (nCats.ge.maxCats) go to 984

      end do
200   nCats = nCats - 1
      close(dfile)

      nCats = nCats + 1  ! add the 'all' category
      CatName(nCats) = 'all'
      nLUinCat(nCats) = nlu
      do nl = 1,nlu
        CatLU(nCats,nl) = nl
      end do

      if (nCats.ge.maxCats) go to 984

      if (writeout) then
        write(99,*) 'CATS,',nCATS
        do nb = 1,nCATS
         write(99,*) nb,',',CatName(nb)
         do nl = 1,nLUinCat(nb)
           write(99,*) ' ,',nl,',',luname(CatLU(nb,nl))
         end do
        end do
      end if

********** get BMP Names, efficiencies, associations with 
************* land use and HGMRs, and maximum implementation levels
******** initial all to -999, then fill in values as they are read
*********  assign values to all HGMRs and land uses in categories
******** if a BMP acreage is assigned to a bmp where the efficiencies
********* for all constituents is -999, it will cause an error
      do nl = 1,nlu    ! initialize BMP efficiencies
        do nHG = 1, maxHGMR
          do nb = 1,nbcon
            do nBmp = 1,maxBmpTypes
              BmpEff(nBmp,nb,nHG,nl) = -999.0
            end do
          end do
        end do
      end do
      do nl = 1,nlu    ! initialize BMP efficiencies
        do nHG = 1, maxHGMR
          do nBmp = 1,maxBmpTypes
              BmpDefined(nBmp,nHG,nl) = .false.
          end do
        end do
      end do

      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)//'/'
     .       //'BMP_descriptions.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nBmpTypes = 0
      firstline = .true.
      do
        read(dfile,'(a1000)',err=992,end=300) longline
        call d2x(longline,last)
        if (last.ge.1000-1) go to 990  ! check for overrun
        if (comment(longline)) cycle
        if (firstline) then  ! parse first line and find order
          call shift(longline) ! bmp
          call shift(longline) ! shortname
          call shift(longline) ! land use
          call shift(longline) ! hgmr
C          call shift(longline) ! maximplement
          nbcons = 1
          do   ! define order
            read(longline,*,err=993,end=301) tBMPcon  ! read header
            call lowercase(tBMPcon)
            found = .false.
            do nb = 1,nbcon
              if (tBMPcon.eq.BMPconname(nb)) then
                order(nbcons) = nb
                found = .true.
                exit
              end if
            end do
            if (.not.found) go to 997
            call shift(longline)
            nbcons = nbcons + 1
          end do
301       nbcons = nbcons - 1
          firstline = .false.
          cycle
        end if  ! end firstline special code

        call findcomma(longline,last)   ! parse line
        read(longline(:last-1),'(a60)',err=993,end=994) 
     .                                 TLongBmpName
        call shift(longline)
        read(longline,*,err=993,end=994) TBmpName,TCat,THGMR,
     .                                   (Teff(nb),nb=1,nbcons)
C     .                                   TmaxI,(Teff(nb),nb=1,nbcons)
        call lowercase(TLongBmpName)
        call lowercase(TBmpName)
        call lowercase(TCat)
        call lowercase(THGMR)

        found = .false.  ! find Bmp index
        do nBmp = 1,nBmpTypes
          if (TlongBmpName.eq.LongBmpName(nBmp) .and.
     .            TBmpName.eq.BmpName(nBmp)) then
            found = .true.
            exit  ! nBmp has been assigned
          end if
        end do
        if (.not.found) then  ! new BMP type
          nBmpTypes = nBmpTypes + 1
          if (nBmpTypes.gt.MaxBmpTypes) go to 983
          nBmp = nBmpTypes
          LongBmpName(nBmp) = TLongBmpName
          BmpName(nBmp) = TBmpName
        end if
          

********** handle land use first.  If category is a land use, 
******** define a new category with just that land use.  if cat not
********* found, cause error
        found = .false. 
        do nl = 1,nlu
          if (TCat.eq.luname(nl)) then
            nc = nCats + 1
            nLUinCat(nc) = 1
            CatLU(nc,1) = nl
            CatName(nc) = luname(nl)
            found = .true.
            exit
          end if
        end do
        if (.not.found) then  ! look for category
          do nc = 1,nCats
            if (TCat.eq.CatName(nc)) then
              found = .true.
              exit
            end if
          end do
        end if  ! nc should be defined now
        if (.not.found) go to 996

*********** assign efficiencies
************** depending on the HGMR specifier
        if (THGMR(:3).eq.'all') then
          do nHG = 1,nHGMR
            do nl = 1,nLUinCat(nc)
              do nb = 1,nbcons
                BmpEff(nBmp,order(nb),nHG,CatLU(nc,nl)) = Teff(nb)
c                print *,'getbmpspecs BmpEff =', !GY
c     .            BmpEff(nBmp,order(nb),nHG,CatLU(nc,nl)) !GY
              end do
              BmpDefined(nBmp,nHG,CatLU(nc,nl)) = .true.
C              MaxImplement(nBmp,nHG,CatLU(nc,nl)) = TmaxI
            end do
          end do
        else
          found = .false.
          do nHG = 1,nHGMR
            if (THGMR.eq.allHGMRs(nHG)) then
              found = .true.
              do nl = 1,nLUinCat(nc)
                do nb = 1,nbcons
                  BmpEff(nBmp,order(nb),nHG,CatLU(nc,nl)) = Teff(nb)
                end do
                BmpDefined(nBmp,nHG,CatLU(nc,nl)) = .true.
C               MaxImplement(nBmp,nHG,CatLU(nc,nl)) = TmaxI
              end do
              exit
            end if
          end do
          if (.not.found) go to 998
        end if

      end do
300   close(dfile)


      if (writeout) then
        write(99,*) 'BMP efficiencies'
        write(99,332) 'con,lu,hgmr',(',',BmpName(nBmp),nBmp=1,nBmpTypes)
        do nb = 1,nbcon
          do nl = 1,nlu
            do nHG = 1,nHGMR
              write(99,333) BMPconname(nb),',',luname(nl),',',
     .                      allHGMRs(nHG),
     .                     (',',BmpEff(nBmp,nb,nHG,nl),nBmp=1,nBmpTypes)
            end do
          end do
        end do
332     format(a13,100(a1,a20))
333     format(a3,a1,a3,a1,a4,200(a1,f8.3))
      end if

************* get additive matrix
********* first assign all as multiplicative, then get additive from 
******* the matrix.  Check for triangluarity.
      do nBmpV = 1,maxBmpTypes  ! initialize as multiplicative
        do nBmpH = 1,maxBmpTypes
          AdditiveBmp(nBmpH,nBmpV) = .false.
        end do
      end do

******** open file
      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)//'/'
     .       //'additive_bmps.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      firstline = .true.
      do
        read(dfile,'(a2700)',err=992,end=400) longline  ! BHATT changed 2000 to 2300 on Oct 28 2013
        call d2x(longline,last)
        if (last.ge.2700-1) go to 990  ! check for overrun ! BHATT changed 2000 to 2300 on Oct 28 2013
        if (comment(longline)) cycle
        if (firstline) then  ! parse first line and find order
          call shift(longline) ! bmp
          call shift(longline) ! shortname
          do nBmpH = 1,nBmpTypes  ! define order
            read(longline,*,err=993,end=989) TBmpName
            call lowercase(TBmpName)
            found = .false.
            do nBmp = 1,nBmpTypes
              if (TBmpName.eq.BmpName(nBmp)) then
                if (found) go to 987  ! double not allowed
                found = .true.
                order(nBmpH) = nBmp
              end if
            end do
            if (.not.found) go to 988  ! no bmp defined for this column
            call shift(longline)
          end do
          firstline = .false.
          cycle
        end if  ! end firstline special code

        call findcomma(longline,last)   ! parse line
        read(longline(:last-1),'(a60)',err=993,end=994)
     .                                 TLongBmpName
        call shift(longline)
        read(longline,*,err=993,end=994) TBmpName,
     .                                   (Tadd(nBmp),nBmp=1,nBmpTypes)

        call lowercase(TLongBmpName)
        call lowercase(TBmpName)

        found = .false.  ! find Bmp index
        do nBmpV = 1,nBmpTypes
          if (TlongBmpName.eq.LongBmpName(nBmpV) .and.
     .            TBmpName.eq.BmpName(nBmpV)) then
            found = .true.
            exit  ! nBmpV has been assigned
          end if
        end do
        if (.not.found)  go to 986

*********** assign additive bmps
        do nBmpH = 1,nBmpTypes
          if (Tadd(nBmpH).eq.1) AdditiveBmp(nBmpV,order(nBmpH))=.true.
        end do

      end do
400   close(dfile)

********** check for symmetric matrix
      do nBmpV = 1,nBmpTypes
        do nBmpH = 1,nBmpTypes
          if (
     .      (additiveBmp(nBmpV,nBmpH).and..not.additiveBmp(nBmpH,nBmpV))
     .           .or.
     .      (.not.additiveBmp(nBmpV,nBmpH).and.additiveBmp(nBmpH,nBmpV))
     .          ) go to 982
        end do
      end do


      if (writeout) then
        write(99,*) 'additivity'
        write(99,442) 'BMPname',(',',BmpName(nBmp),nBmp=1,nBmpTypes)
        do nb = 1,nBmpTypes
          write(99,443) BmpName(nb),
     .                    (',',AdditiveBmp(nb,nBmp),nBmp=1,nBmpTypes)
        end do
442     format(a13,100(a1,a20))
443     format(a14,100(a1,i1))
      end if

********** get BMP hydrologic effect type and association with 
************* land use and HGMRs
******** initial all to -999, then fill in values as they are read
*********  assign values to all HGMRs and land uses in categories
******** if a BMP acreage is assigned to a bmp where all constituents
********* have a parameter of -999, it will cause an error
      do nl = 1,nlu    ! initialize hydrologic effect values
        do nHG = 1, maxHGMR
          do nb = 1,nbcon
            do nBmp = 1,maxBmpTypes
              BmpHydEffType(nBmp,nb,nHG,nl) = -999
              do np = 1,nparms
                BmpHydEffParm(nBmp,nb,nHG,nl,np) = -999
              end do
            end do
          end do
        end do
      end do

      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)//'/'
     .       //'hydrologic_parameters.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do
        read(dfile,'(a1000)',err=992,end=500) longline
        call d2x(longline,last)
        if (last.ge.1000-1) go to 990  ! check for overrun
        if (comment(longline)) cycle

        call findcomma(longline,last)   ! parse line
        read(longline(:last-1),'(a60)',err=993,end=994) 
     .                                 TLongBmpName
        call shift(longline)
        read(longline,*,err=993,end=994) TBmpName,TCat,THGMR,tBMPcon,
     .                                   Ttype,(Tparm(np),np=1,nparms)
        call lowercase(TLongBmpName)
        call lowercase(TBmpName)
        call lowercase(TCat)
        call lowercase(THGMR)
        call lowercase(tBMPcon)

********* find bmp index
        if (TlongBmpName.eq.'all' .and. 
     .      TBmpName.eq.'all') then
          B1 = 1 
          B2 = nBmpTypes
        else
          found = .false.  ! find Bmp index
          do nBmp = 1,nBmpTypes
            if (TlongBmpName.eq.LongBmpName(nBmp) .and.
     .              TBmpName.eq.BmpName(nBmp)) then
              found = .true.
              B1 = nBmp
              B2 = nBmp
            end if
          end do
          if (.not.found) go to 988
        end if

********** handle land use first.  If category is a land use, 
******** define a new category with just that land use.  if cat not
********* found, cause error
        found = .false. 
        do nl = 1,nlu
          if (TCat.eq.luname(nl)) then
            nc = nCats + 1
            nLUinCat(nc) = 1
            CatLU(nc,1) = nl
            CatName(nc) = luname(nl)
            found = .true.
            exit
          end if
        end do
        if (.not.found) then  ! look for category
          do nc = 1,nCats
            if (TCat.eq.CatName(nc)) then
              found = .true.
              exit
            end if
          end do
        end if  ! nc should be defined now
        if (.not.found) go to 996

************** process HGMR specifier
        if (THGMR(:3).eq.'all') then
          H1 = 1      ! limits for all
          H2 = nHGMR
        else
          found = .false.
          do nHG = 1,nHGMR
            if (THGMR.eq.allHGMRs(nHG)) then
              found = .true.
              H1 = nHG
              H2 = nHG
            end if
          end do
          if (.not.found) go to 998
        end if

********** process constituent next
        if (tBMPcon.eq.'all') then
          C1 = 1      ! limits for all
          C2 = nbcon
        else
          found = .false.
          do nb = 1,nbcon
            if (tBMPcon.eq.BMPconname(nb)) then
              found = .true.
              C1 = nb
              C2 = nb
            end if
          end do
          if (.not.found) go to 997
        end if

        do nl = 1,nLUinCat(nc)
          do nHG = H1,H2
            do nBmp = B1,B2
              do nb = C1,C2
                BmpHydEffType(nBmp,nb,nHG,CatLU(nc,nl)) = Ttype
                do np = 1,nparms
                  BmpHydEffParm(nBmp,nb,nHG,CatLU(nc,nl),np) = Tparm(np)
                end do
              end do
            end do
          end do
        end do


      end do
500   close(dfile)

      if (writeout) then
        write(99,*) 'BMP hydro parameters'
        write(99,332) 'con,lu,hgmr',(',',BmpName(nBmp),nBmp=1,nBmpTypes)
        do nb = 1,nbcon
          do nl = 1,nlu
            do nHG = 1,nHGMR
              do nBmp = 1,nBmpTypes
                realvector(nBmp) = BmpHydEffType(nBmp,nb,nHG,nl)
              end do
              write(99,333) BMPconname(nb),',',luname(nl),',',
     .                      allHGMRs(nHG),
     .           (',',realvector(nBmp),nBmp=1,nBmpTypes),
     .           (',',BmpHydEffParm(nBmp,nb,nHG,nl,1),nBmp=1,nBmpTypes),
     .           (',',BmpHydEffParm(nBmp,nb,nHG,nl,2),nBmp=1,nBmpTypes),
     .           (',',BmpHydEffParm(nBmp,nb,nHG,nl,3),nBmp=1,nBmpTypes)
            end do
          end do
        end do
      end if

 
********** get BMP random distribution effect type and association with 
************* land use and HGMRs
******** initial all to -999, then fill in values as they are read
*********  assign values to all HGMRs and land uses in categories
******** if a BMP acreage is assigned to a bmp where all constituents
********* have a value of -999, it will cause an error
      do nl = 1,nlu    ! initialize hydrologic effect values
        do nHG = 1, maxHGMR
          do nb = 1,nbcon
            do nBmp = 1,maxBmpTypes
              BmpEffDistType(nBmp,nb,nHG,nl) = -999
              do np = 1,nparms
                BmpEffDistParm(nBmp,nb,nHG,nl,np) = -999
              end do
            end do
          end do
        end do
      end do

      fnam = ScenDatDir//'river/bmptypes/'//BmpTypeScen(:lenscen)//'/'
     .       //'random_distribution_parameters.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do
        read(dfile,'(a1000)',err=992,end=600) longline
        call d2x(longline,last)
        if (last.ge.1000-1) go to 990  ! check for overrun
        if (comment(longline)) cycle

        call findcomma(longline,last)   ! parse line
        read(longline(:last-1),'(a60)',err=993,end=994) 
     .                                 TLongBmpName
        call shift(longline)
        read(longline,*,err=993,end=994) TBmpName,TCat,THGMR,tBMPcon,
     .                                   Ttype,(Tparm(np),np=1,nparms)
        call lowercase(TLongBmpName)
        call lowercase(TBmpName)
        call lowercase(TCat)
        call lowercase(THGMR)
        call lowercase(tBMPcon)

********* find bmp index
        if (TlongBmpName.eq.'all' .and. 
     .      TBmpName.eq.'all') then
          B1 = 1  ! limits for all Bmps
          B2 = nBmpTypes
        else
          found = .false.  ! find Bmp index
          do nBmp = 1,nBmpTypes
            if (TlongBmpName.eq.LongBmpName(nBmp) .and.
     .              TBmpName.eq.BmpName(nBmp)) then
              found = .true.
              B1 = nBmp
              B2 = nBmp
            end if
          end do
          if (.not.found) go to 988
        end if

********** handle land use first.  If category is a land use, 
******** define a new category with just that land use.  if cat not
********* found, cause error
        found = .false. 
        do nl = 1,nlu
          if (TCat.eq.luname(nl)) then
            nc = nCats + 1
            nLUinCat(nc) = 1
            CatLU(nc,1) = nl
            CatName(nc) = luname(nl)
            found = .true.
            exit
          end if
        end do
        if (.not.found) then  ! look for category
          do nc = 1,nCats
            if (TCat.eq.CatName(nc)) then
              found = .true.
              exit
            end if
          end do
        end if  ! nc should be defined now
        if (.not.found) go to 996

************** process HGMR specifier
        if (THGMR(:3).eq.'all') then
          H1 = 1      ! limits for all
          H2 = nHGMR
        else
          found = .false.
          do nHG = 1,nHGMR
            if (THGMR.eq.allHGMRs(nHG)) then
              found = .true.
              H1 = nHG
              H2 = nHG
            end if
          end do
          if (.not.found) go to 998
        end if

********** process constituent next
        if (tBMPcon.eq.'all') then
          C1 = 1      ! limits for all
          C2 = nbcon
        else
          found = .false.
          do nb = 1,nbcon
            if (tBMPcon.eq.BMPconname(nb)) then
              found = .true.
              C1 = nb
              C2 = nb
            end if
          end do
          if (.not.found) go to 997
        end if

        do nl = 1,nLUinCat(nc)
          do nHG = H1,H2
            do nBmp = B1,B2
              do nb = C1,C2
                BmpEffDistType(nBmp,nb,nHG,CatLU(nc,nl)) = Ttype

c                if (BmpEffDistType(nBmp,nb,nHG,CatLU(nc,nl))>0)then !GY
c                  print*,'getbmpspecs BmpEffDistType = ', !GY
c     .            BmpEffDistType(nBmp,nb,nHG,CatLU(nc,nl)) !GY
c                end if !GY

                do np = 1,nparms
                  BmpEffDistParm(nBmp,nb,nHG,CatLU(nc,nl),np)=Tparm(np)

c                if (BmpEffDistParm(nBmp,nb,nHG,CatLU(nc,nl),np)>0)then !GY
c                  print*,'getbmpspecs BmpEffDistParm = ', !GY
c     .            BmpEffDistParm(nBmp,nb,nHG,CatLU(nc,nl),np) !GY
c                end if !GY

                end do
              end do
            end do
          end do
        end do


      end do
600   close(dfile)

      if (writeout) then
        write(99,*) 'BMP random parameters'
        write(99,332) 'con,lu,hgmr',(',',BmpName(nBmp),nBmp=1,nBmpTypes)
        do nb = 1,nbcon
          do nl = 1,nlu
            do nHG = 1,nHGMR
              do nBmp = 1,nBmpTypes
                realvector(nBmp) = BmpEffDistType(nBmp,nb,nHG,nl)
              end do
              write(99,333) BMPconname(nb),',',luname(nl),',',
     .                      allHGMRs(nHG),
     .          (',',realvector(nBmp),nBmp=1,nBmpTypes),
     .          (',',BmpEffDistParm(nBmp,nb,nHG,nl,1),nBmp=1,nBmpTypes),
     .          (',',BmpEffDistParm(nBmp,nb,nHG,nl,2),nBmp=1,nBmpTypes),
     .          (',',BmpEffDistParm(nBmp,nb,nHG,nl,3),nBmp=1,nBmpTypes)
            end do
          end do
        end do
        close(99)
      end if

      return
*********** ERROR SPACE ************************************************
982   report(1) = 'additive bmp matrix is not symmetric'
      report(2) = fnam
      report(3) = 'revise additive bmp matrix and re-run'
      go to 999

983   report(1) = 'max number of allowable BMP types exceeded in file'
      report(2) = fnam
      report(3) = 'revise code and recompile with higher limits'
      go to 999

984   report(1) = 'maximum number of allowable land use categories '//
     .            'exceeded in file'
      report(2) = fnam
      report(3) = 'revise code and recompile with higher limits'
      go to 999

985   report(1) = 'maximum number of allowable HGMRs exceeded in file'
      report(2) = fnam
      report(3) = 'revise code and recompile with higher limits'
      go to 999

986   report(1) = 'problem with file: long or shortname in row invalid'
      report(2) = fnam
      report(3) = 'shortname in row = '//TBmpName
      go to 999

987   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'column '//TBmpName//' listed twice'
      go to 999

988   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'no bmp defined for column '//TbmpName
      go to 999

989   report(1) = 'problem with file: '
      report(2) = fnam
      report(3) = 'There must be a column for each unique Bmp short'//
     .            ' name in Bmp_descriptions.csv'
      go to 999

990   report(1) = 'line too long in file: '
      report(2) = fnam
      report(3) = 'increase longline variable and recompile'
      go to 999

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'error reading file: near line:'
      report(2) = fnam
      report(3) = longline
      go to 999

993   report(1) = 'error parsing line: from file:'
      report(2) = longline
      report(3) = fnam
      go to 999

994   report(1) = 'line ended unexpectedly. file: line:'
      report(2) = fnam
      report(3) = longline
      go to 999

995   report(1) = 'did not recognize land use '//templu(nl)//
     .            ' in file: line:'
      report(2) = fnam
      report(3) = longline
      go to 999

996   report(1) = 'did not recognize land use category '//TCat//
     .            ' in file: line:'
      report(2) = fnam
      report(3) = longline
      go to 999

997   report(1)='did not recognize bmp variable '//tBMPcon//' in file'
      report(2) = fnam
      write(report(3),*)'legal names:',(' ',BMPconname(nb),nb=1,nbcon)
      go to 999

998   report(1) = 'did not recognize HGMR '//THGMR//' in file: line:'
      report(2) = fnam
      report(3) = longline
      go to 999

999   call stopreport(report)

      end


