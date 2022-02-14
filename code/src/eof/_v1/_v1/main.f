
      implicit none

      include 'eof.inc'

      integer year1, year2

      logical comment
      external comment

      character*3   t3lus(nlu)
      character*3   t3CharFlags(nquals)
      character*10  t10CharFlags(nlu)
      character*4   t4Char
      character*300 longline
      integer       tInt
      real          tRealSpecies(nappspecies)
      integer       tAppTypesFlag(napptypes)
      integer       tquals(nquals)
      integer       tlus(nlu)

      character*25  lcalib
      integer       lenlcalib

      character*6   tlseg
      integer       lentlseg

      real          tsensitivity(nquals)
      character*1   tsmethod(nquals)

      integer       lenfnam


      read*,lseg,lscen,year1,year2
      print*,''
      print*,'Number of Land Uses: ',nlu

      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

      print*,"Land Segment: "//lseg//" Scenario: "//lscen(:lenlscen)
c      print*,"Land Use: #",nlu


      !>> initialize WDM DSNs using perlnd file
      ! OutWDMs [ nquals ][ nstates ]
      do i = 1,nquals
      !{
         do j = 1,nstates
         !{
            OutWDMs(i,j) = 999
         !}
         end do
      !}
      end do
      call readcontrol_Lgeoscen(
     I                          lscen,lenlscen,
     O                          geoscen)
      call lencl(geoscen,lengeoscen)
      fnam = catdir//'iovars/'//geoscen(:lengeoscen)//'/perlnd'

      do i = 1,nquals
      !{
         do j = 1,nstates
         !{
            t4char = states(j)//quals(i)
c            print*,'>>',t4char
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            do
            !{
               read(dfile,'(a100)',err=992,end=112) line
               call d2x(line,last)
               if (comment(line)) cycle
               if (line(:20).eq.'                    ') cycle
c               print*,line(26:29),'<>',t4char
               if (line(26:29).eq.t4char) then
               !{
                  read(line(:3),*) OutWDMs(i,j)
               !}
               end if
            !}
            end do
112         close (dfile)
         !}
         end do
      !}
      end do
      print*,''
      print*,'PQUAL Land WDM Strcture (nquals,nstates) from iovar/'
     .        //geoscen(:lengeoscen)//'/perlnd'
      write(*,'(A4,4(A4$))') ' ',(states(i),i=1,nstates)
      write(*,*) ''
      do i=1,nquals
         print*,quals(i),(OutWDMs(i,j),j=1,nstates)
      end do


      !>> get PQUAL to Input Mapping
      fnam = 'PQUAL_vs_Input.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      do i=1,nquals
      !{
         do j=1,nstates
         !{
100         read(dfile,'(a100)',err=992,end=114) line
            call d2x(line,last)
            if (comment(line)) goto 100
            if (line(:10).eq.'          ') goto 100
            read(line,*,end=993,err=993)
     .           (pquals_vs_inputs(i,j,k),k=1,nappspecies)
         !}
         end do
      !}
      end do
114   close(dfile)

      print*,(pquals_vs_inputs(4,1,k),k=1,nappspecies)




      !>> Get what app(lication) type inputs (flags) are available
      ! Using spec_flags.csv file at the moment that allows basin specific flags
      ! Alternatively "landuse_apptypes.csv" can be used as global descriptor
      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)

      call lencl(specscen,lenspecscen)

      fnam = ScenDatDir//'land/spec_flags/'//specscen(:lenspecscen)//
     .       '/spec_flags.csv'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)',err=992,end=115)longline
      call d2x(longline,last)
      read (longline,*,end=993,err=993) tlseg,(t3lus(i),i=1,nlu-1)
      do i = 1,nlu-1
      !{
         j = 1
201      if ( t3lus(i).ne.luname(j) ) then
            j = j + 1
            go to 201
         end if
         tlus(i) = j
      !}
      end do
      print*,'spec_flags lus: ',(tlus(i),i=1,nlu)
      

      do
      !{
         read(dfile,'(a300)',err=992,end=115)longline
         call d2x(longline,last)
         read(longline,*,end=994,err=994)
     .               tlseg,(t10CharFlags(i),i=1,nlu-1)
         if ( tlseg.eq.lseg(:lenlseg) ) then
         !{
c            print*,t10CharFlags(3),len(t10CharFlags(3))
            do i = 1,nlu
            !{
               do k = 1,napptypes
               !{
                  apptypesflag(i,k) = 0
               !}
               end do

               call lencl(t10CharFlags(i), tInt)
               do j = 1,tInt
               !{
                 if(t10CharFlags(i)(j:j).eq.'Q')t10CharFlags(i)(j:j)='A'
c                  print*,t10CharFlags(i)(j:j)
                  do k = 1,napptypes
                  !}
c                     print*,'>',t10CharFlags(i)(j:j),'#',apptypes(k),'<'
                     if ( t10CharFlags(i)(j:j).eq.apptypes(k) ) then
                     !{
                        apptypesflag(tlus(i),k) = 1
                     !}
                     end if
                  !}
                  end do
               !}
               end do
            !}
            end do
            exit
         !}
         end if
      !}
      end do
      do i = 1,nlu
         print*,'nlu= ',i,' ',(apptypesflag(i,k),k=1,napptypes)
      end do
      do k = 1,napptypes
         if ( forceignore(k).eq.0 ) then
            do i = 1,nlu
               apptypesflag(i,k) = 0
            end do
         end if
      end do
      print*,'Forcing A, V=0, P=0, F, M, L, T, U=0, R=0'
      do i = 1,nlu               
         print*,'nlu= ',i,' ',(apptypesflag(i,k),k=1,napptypes)
      end do

115   close(dfile)





      !>> FOR each land-use { use .inc to get lu char key }
      do l = 1,nlu 
      !{
         print*,''
         print*,"*** Land Use: "//luname(l)

         if ( luname(l).eq.'wat' ) cycle
         if ( luname(l).ne.'for' .and. luname(l).ne.'hom' ) cycle ! TODO DELETE


         !>> for calibration (1991-2000)
         ! compute inputs from each source and for each species
         ! IN_CALIB[ SOURCE ][ SPECIES ]
         ! IN_CALIB_SRC[ SOURCE ]

         call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          lcalib)
         call lencl(lcalib,lenlcalib)
         print*,'CALIBRATION: ',lcalib(:lenlcalib)
        
         do i = 1,napptypes
            do j = 1,nappspecies
               inp_calib_app(i,j) = 0
            end do
         end do

         do i = 1,napptypes
         !{
            if (apptypesflag(l,i) == 0) cycle

            call lencl(apptypeslong(i),tInt)
cc            print*,">>",t6Char(:tInt),"<<"

            fnam = outdir//'input/'//lcalib(:lenlcalib)//'/annual_'//
     .         apptypeslong(i)(:tInt)//'_'//lseg//'_'//luname(l)//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a100)',err=992,end=113) line ! header line

            ! *** assuming a fixed column structure -- no mapping required ***
            ! i.e. column sequence in the annual_* files in output/input
            do
            !{
               read(dfile,'(a100)',err=992,end=113) line
               call d2x(line,last)
               read(line,*,end=993,err=993)
     .                  tInt,(tRealSpecies(j),j=1,nappspecies)
               if (tInt.ge.year1.and.tInt.le.year2) then
               !{
                  do j=1,nappspecies
                  !{
                     inp_calib_app(i,j) = inp_calib_app(i,j)
     .                                    + tRealSpecies(j)
                  !}
                  end do
               !}
               end if
            !}
            end do
113         close (dfile)
         !}
         end do
         write(*,'(A6,5(A12))') 'apptypes',
     .                   (appspecies(j),j=1,nappspecies)
         do i = 1,napptypes
           write(*,'(A6,5(e12.5))')
     .          apptypeslong(i),(inp_calib_app(i,j),j=1,nappspecies)
         end do
         



         !>> for scenario (1991-2000)
         ! compute inputs from each source and for each species
         ! IN_SCEN[ SOURCE ][ SPECIES ]
         ! IN_SCEN_SRC[ SOURCE ]

         print*,'SCENARIO: ',lscen(:lenlscen)

         do i = 1,napptypes
            do j = 1,nappspecies
               inp_scen_app(i,j) = 0
            end do
         end do

         do i = 1,napptypes
         !{
            if (apptypesflag(l,i) == 0) cycle

            call lencl(apptypeslong(i),tInt)
cc            print*,">>",t6Char(:tInt),"<<"

            fnam = outdir//'input/'//lscen(:lenlscen)//'/annual_'//
     .         apptypeslong(i)(:tInt)//'_'//lseg//'_'//luname(l)//'.csv'
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a100)',err=992,end=117) line ! header line

            ! *** assuming a fixed column structure -- no mapping required ***
            ! i.e. column sequence in the annual_* files in output/input
            do
            !{
               read(dfile,'(a100)',err=992,end=117) line
               call d2x(line,last)
               read(line,*,end=993,err=993)
     .                  tInt,(tRealSpecies(j),j=1,nappspecies)
               if (tInt.ge.year1.and.tInt.le.year2) then
               !{
                  do j=1,nappspecies
                  !{
                     inp_scen_app(i,j) = inp_scen_app(i,j)
     .                                    + tRealSpecies(j)
                  !}
                  end do
               !}
               end if
            !}
            end do
117         close (dfile)
         !}
         end do
         write(*,'(A6,5(A12))') 'apptypes',
     .                   (appspecies(j),j=1,nappspecies)
         do i = 1,napptypes
           write(*,'(A6,5(e12.5))')
     .          apptypeslong(i),(inp_scen_app(i,j),j=1,nappspecies)
         end do


      !>* OPEN land wdm file { a copy of calibration land wdm that is }
      ! * FOR each input source (src) { use .inc to get input source key }
      ! * * OPEN sensitivity file e.g. <lu>_<src>_sensitivity.csv
      ! * * READ sensitivity matrix while parsing constituents and method
      ! . . for the given land segment { use input constituent key from .inc }
      ! . . S[ 
      ! * * FOR each constituent { use input constituent key from .inc }
      ! * * * * SWITCH based on prescribed 'method'
      ! * * * * * compute cummulative change in output { DEL_OUT += S * (IN_SCE - IN_CALIB) }
      ! * OPEN LAND WDM FILE { A COPY OF CALIBRATION WDM }
      ! *

         !************* Dummy WDM
         wdmfnam = dummyWDMname
         call wdbopnlong(wdmfil+1,wdmfnam,0,err)
         if (err.ne.0) go to 801

         wdmfnam = luname(l)//lseg(:lenlseg)//'.wdm'
         print*,'Land WDM File: ',wdmfnam
         call wdbopnlong(wdmfil,wdmfnam,0,err)
         if (err.ne.0) go to 801

         sdate(1) = year1
         sdate(2) = 1
         sdate(3) = 1
         sdate(4) = 0
         sdate(5) = 0
         sdate(6) = 0

         edate(1) = year2
         edate(2) = 12
         edate(3) = 31
         edate(4) = 24
         edate(5) = 0
         edate(6) = 0


         do j = 1,nquals
         !{
            do k = 1,nstates
            !{
               delta_out(j,k) = 0.0
            !}
            end do
         !}
         end do

         do i = 1,napptypes
         !{
            if (apptypesflag(l,i) == 0) cycle

            call lencl(apptypeslong(i),tInt)
            fnam = luname(l)//'_'//apptypeslong(i)(:tInt)
     .                 //'_sensitivity.csv'
            call lencl(fnam,lenfnam)
            write(*,'(3(A$))'),' Reading ',fnam(:lenfnam),' ...'
            print*,''
            open(dfile,file=fnam,status='old',iostat=err)
            if (err.ne.0) go to 991
            read(dfile,'(a300)',err=992,end=118) longline
c            print*,'$',longline
            call d2x(longline,last)
            read (longline,*,end=993,err=993) 
     .             tlseg,(t3CharFlags(j),j=1,nquals)
c            print*,(t3CharFlags(j),j=1,nquals)
            
            do j = 1,nquals
            !{
               k = 1
202            if ( t3CharFlags(j).ne.quals(k) ) then
                  k = k + 1
                  go to 202
               end if
               tquals(j) = k
            !}
            end do
            print*,(quals(j),j=1,nquals),' :: ',(tquals(j),j=1,nquals)

            do
            !{
               read (dfile,'(a300)',end=992,err=118) longline
               call d2x(longline,last)
               read(longline,*,end=994,err=994),tlseg,
     .                 (tsensitivity(j),j=1,nquals),
     .                 (tsmethod(j),j=1,nquals)
c               print*,tlseg
c               print*,(tsensitivity(j),j=1,nquals)

               call lencl(tlseg,lentlseg)
               if( tlseg(:lentlseg).eq.lseg(:lenlseg) ) then
               !{
                  do j = 1,nquals
                  !{
                     sensitivity( tquals(j) ) = tsensitivity(j)
                     smethod ( tquals(j) ) = tsmethod(j)
                  !}
                  end do
                  exit
               !}
               end if
            !}
            end do

            print*,'Sensitivites: ',lseg(:lenlseg)
            write(*,'(5(A4$))') (quals(j),j=1,nquals)
            print*,''
            print*,(sensitivity(j),j=1,nquals)

118         close(dfile)



            do j = 1,nquals
            !{
               do k = 1,nstates
               !{
                  !> calculate delta_out
                  print*,'delta_out(nqual=1,nstate=1) = ',delta_out(1,1)
                  do m = 1,nappspecies
                     if (smethod(j).eq.'A') then
                        delta_out(j,k) = delta_out(j,k) +
     .                     pquals_vs_inputs(j,k,m) * sensitivity(j) *
     .                     ( inp_scen_app(i,m) - inp_calib_app(i,m) )
                     else
                        go to 995
                     end if
                  end do
               !}
               end do
            !}
            end do
         !}
         end do

         do j = 1,nquals
         !{
            do k = 1,nstates
            !{
               dsn = OutWDMs(j,k)
               if ( dsn.eq.999 ) cycle
               call gethourdsn(wdmfil,sdate,edate,dsn,nvals,hvals)
               out_calib = 0
               do m = 1,nvals
               !{
                  out_calib = out_calib + hvals(m)
               !}
               end do
               pcent_delta = delta_out(j,k) / out_calib
               print*,' qual= ',j,' state= ',k,' delta_out= ',
     .                delta_out(j,k), ' pcent_delta= ',pcent_delta
               do m = 1,nvals
               !{
                  hvals(m) = (1 + pcent_delta) * hvals(m)
               !}
               end do

               call puthourdsn(wdmfil,sdate,edate,dsn,nvals,hvals)
            !}
            end do
            print*,' '
         !}
         end do

         call wdflcl(wdmfil,err)
         if (err.ne.0) go to 803
      !}
      end do

      ! 

      do i=1,nquals

         ! 

         ! GET TOTAL OUTPUT

         ! BASED ON 
         do j=1,nstates

         end do
      end do



      return

111   close(dfile)


991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = 'Problem parsing line: in file:'
      report(2) = line
      report(3) = fnam
      go to 999

994   report(1) = 'Problem parsing line: in file:'
      report(2) = longline
      report(3) = fnam
      go to 999

995   report(1) = 'Problem using sensitivity function:'
      report(2) = smethod(j)
      report(3) = 'Oops! this method not yet impelemented?'
      go to 999

801   report(1) = 'WDM file must previously exist'
      report(2) = 'create wdm:'
      report(3) = wdmfnam
      go to 999

803   report(1) = 'error closing wdm file: '
      report(2) = wdmfnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end
