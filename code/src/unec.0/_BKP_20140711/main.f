
      implicit none

      include 'eofc.inc'


      character*6   tlseg
      integer       lentlseg
      integer       tlus(nlu)
      character*3   t3lus(nlu)
      character*10  t10CharFlags(nlu)
      integer       tInt
      integer       tYear, tMonth
      real          tRealSpecies(nappspecies)
      

      real input(maxMonths)
c      data input /maxMonths*0/

      integer nMonths

      integer sParamLeadEdge, sParamPeakConc, sParamTrailEdge
      real    sParamExponent1, sParamExponent2
      integer sParamNormalize

      integer dParamLeadEdge, dParamPeakConc, dParamTrailEdge
      real    dParamExponent1, dParamExponent2
      integer dParamNormalize

      integer iParamLeadEdge, iParamPeakConc, iParamTrailEdge
      real    iParamExponent1, iParamExponent2
      integer iParamNormalize

      integer aParamLeadEdge, aParamPeakConc, aParamTrailEdge
      real    aParamExponent1, aParamExponent2
      integer aParamNormalize

      real    fracS, fracD, fracI, fracA ! Partition of Input between Surface, Detached, Interflow & Groundwater


      read*,lseg,lscen,year1,year2 

      !>> ASSIGN partition of input loads to transport mechanism
      !   fracS + fracD + fracI + fracA = 1
      !   NOTE: This can be time variable as well based on flow regimes
      fracS    = 0.25
      fracD    = 0.25
      fracI    = 0.25
      fracA    = 0.25

      !>> ASSIGN 5 Breakthrough Curve Parameters
      !   Read from CSV File btc_<lu>.csv
      !   A10001,S,sp1,sp2,sp3,sp4,sp5
      !   A10001,D,dp1,dp2,dp3,dp4,dp5
      sParamLeadEdge    = 1
      sParamPeakConc    = 1
      sParamTrailEdge   = 12
      sParamExponent1   = 0.1
      sParamExponent2   = 0.3
      sParamNormalize   = 1

      dParamLeadEdge    = 1
      dParamPeakConc    = 1
      dParamTrailEdge   = 12
      dParamExponent1   = 0.1
      dParamExponent2   = 0.2
      dParamNormalize   = 1

      iParamLeadEdge    = 2
      iParamPeakConc    = 2
      iParamTrailEdge   = 25
      iParamExponent1   = 0.1
      iParamExponent2   = 0.1
      iParamNormalize   = 1

      aParamLeadEdge    = 3
      aParamPeakConc    = 7
      aParamTrailEdge   = 36
      aParamExponent1   = 0.1
      aParamExponent2   = 0.08
      aParamNormalize   = 1

      !>> COMPUTE "characteristic" Breakthrouh Curve factors
      call BreakThroughFactors(
     .        sParamLeadEdge,sParamPeakConc,sParamTrailEdge,
     .        sParamExponent1,sParamExponent2,sParamNormalize,s1Factors)
      call BreakThroughFactors(
     .        dParamLeadEdge,dParamPeakConc,dParamTrailEdge,
     .        dParamExponent1,dParamExponent2,dParamNormalize,d1Factors)
      call BreakThroughFactors(
     .        iParamLeadEdge,iParamPeakConc,iParamTrailEdge,
     .        iParamExponent1,iParamExponent2,iParamNormalize,i1Factors)
      call BreakThroughFactors(
     .        aParamLeadEdge,aParamPeakConc,aParamTrailEdge,
     .        aParamExponent1,aParamExponent2,aParamNormalize,a1Factors)



      !>> READ "monthly" inputs from output/input/
      data input /maxMonths*0/
c      input(1) = 0
c      input(2) = 0
c      input(3) = 0
c      input(4) = 0
c      input(5) = 0
      input(6) = 1
      input(7) = 0
      input(8) = 0

      !>> Get what app(lication) type inputs (flags) are available
      ! Using spec_flags.csv file at the moment that allows basin specific flags
      ! Alternatively "landuse_apptypes.csv" can be used as global descriptor
      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)

      call readcontrol_Lspecscen(
     I                           lscen,lenlscen,
     O                           specscen)

      print*,specscen            
      call lencl(specscen,lenspecscen)
      print*,specscen(:lenspecscen)

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
      !{
         if ( forceignore(k).eq.0 ) then
            do i = 1,nlu
               apptypesflag(i,k) = 0
            end do
         end if
      !}
      end do
      print*,'Forcing A, V=0, P=0, F, M, L, T, U=0, R=0'
      do i = 1,nlu
         print*,'nlu= ',i,' ',(apptypesflag(i,k),k=1,napptypes)
      end do
115   close(dfile)



      call readcontrol_Lcalscen(
     I                          lscen,lenlscen,
     O                          lcalib)
      call lencl(lcalib,lenlcalib)
      print*,'CALIBRATION: ',lcalib(:lenlcalib)


      !>> FOR each land-use { use .inc to get lu char key }
      do l = 1,nlu
      !{
         print*,''
         print*,"*** Land Use: "//luname(l)

         if ( luname(l).eq.'wat' ) cycle
         if ( luname(l).ne.'hom' ) cycle ! TODO DELETE

         !>> for calibration (1991-2000)
         ! compute inputs from each source and for each species
         ! IN_CALIB[ SOURCE ][ SPECIES ]
         ! IN_CALIB_SRC[ SOURCE ]

         do i = 1,napptypes
         !{
            do j = 1,nappspecies
            !{
               do tYear = year1,year2
                  do tMonth = 1,12
                     inp_calib_app(i,j,tYear,tMonth) = 0
                  end do
               end do
            !}
            end do
         !}
         end do

         do i = 1,napptypes
         !{
            if (apptypesflag(l,i) == 0) cycle

            call lencl(apptypeslong(i),tInt)
cc            print*,">>",t6Char(:tInt),"<<"

            fnam = outdir//'input/'//lcalib(:lenlcalib)//'/monthly_'//
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
     .                  tYear,tMonth,(tRealSpecies(j),j=1,nappspecies)
               if (tYear.ge.year1.and.tYear.le.year2) then
               !{
                  do j=1,nappspecies
                  !{
                     inp_calib_app(i,j,tYear,tMonth) = 
     .                         inp_calib_app(i,j,tYear,tMonth)
     .                         + tRealSpecies(j)
                  !}
                  end do
               !}
               end if
            !}
            end do
113         close (dfile)
         !}
         end do
         do i = 1,napptypes
         !{
            write(*,'(A6,5(A12))') 'apptypes',
     .                   (appspecies(j),j=1,nappspecies)
            do yy = year1,year2
             do mm = 1,12
                  write(*,'(A6,5(e12.5))')
     .        apptypeslong(i),(inp_calib_app(i,j,yy,mm),j=1,nappspecies)
             end do
            end do
         !}
         end do



         nMonths = (year2 - year1 + 1) * 12

         do k = 1,1 ! TODO FIX THIS --> k=1,nquals
         !{
c            write(*,'(A)') 'Break Through Curve'
c            do i = 1,nMonths
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c            end do
            do yy = year1,year2
            !{
               do mm = 1,12
               !{
                  input((yy-year1)*12+mm) = 0.0
                  do i = 1,napptypes
                  !{
                     do j = 1,3 !TODO FIX THIS
                     !{
                        if (apptypeslong(i) .ne. 'uptake') then
                           input((yy-year1)*12+mm) = 
     .                         input((yy-year1)*12+mm)
     .                         + inp_calib_app(i,j,yy,mm)
                        else
                           input((yy-year1)*12+mm) =
     .                         input((yy-year1)*12+mm)
     .                         - inp_calib_app(i,j,yy,mm)
                        end if
                     !}
                     end do
                  !}
                  end do
               !}
               end do
            !}
            end do

            !>> FIX NEGATIVE INPUT TODO
            do i = 1,nMonths
c??               if ( input(i) .lt. 0 ) input(i) = 0
               print*,i,input(i)
            end do
            
            do i = 1,nMonths
            !{
               do j = i,nMonths
               !{
                  s2Factors(j) = s2Factors(j) 
     .                     + fracS * input(i) * s1Factors(j-i+1)
                  d2Factors(j) = d2Factors(j)
     .                     + fracD * input(i) * d1Factors(j-i+1)
                  i2Factors(j) = i2Factors(j)
     .                     + fracI * input(i) * i1Factors(j-i+1)
                  a2Factors(j) = a2Factors(j)
     .                     + fracA * input(i) * a1Factors(j-i+1)
               !}
               end do
            !}
            end do
            write(*,'(A)') 'Break Through Curve of Analyte'
            do i = 1,nMonths
            !{
c               write(*,'(I2, E12.5, E12.5)'), i,s1Factors(i-1),s2Factors(i)
c               write(*,'(I2,E12.5,E12.5,E12,5,E12.5)'),i,
c     .            s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
               print*,i,
     .              s2Factors(i),d2Factors(i),i2Factors(i),a2Factors(i)
            !}
            end do

            !>> OPEN WDM

            !>> READ SURO, WSSD, IFWO, AGWO

            !>> COMPUTE YIELDS (Surface,S & Baseflow,B) = FLOW * CONC

            !>> READ TARGETS (S & B)

            !>> Compute Scalaing Factor TARGET / YIELD

            !>> ADJUST S/D/I/A-2 factors to get EOF Yields

            !>> WRITE WDM

            !>> CLOSE WDM
         !}
         end do
      !}
      end do

      return

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

999   call stopreport(report)
      end







***** Follow subroutine BreakThroughFactors computes the Breakthrough curve
***** based on follow four parameters.
***** paramLeadEdge  = elapsed time to the LEADing EDGE
***** paramPeakConc  = elapsed time to the arrival of PEAK CONCentration
***** paramTrailEdge = elapsed time to the TRAILing EDGE of the Response Curve

      subroutine BreakThroughFactors(
     I          paramLeadEdge,
     I          paramPeakConc,
     I          paramTrailEdge,
     I          paramExponent1,
     I          paramExponent2,
     I          paramNormalize,
     O          tFactors)
      !{

      implicit none
         include 'eofc.inc'

         integer paramLeadEdge
         integer paramPeakConc
         integer paramTrailEdge
         real    paramExponent1, paramExponent2
         integer paramNormalize

         integer tempI1
         real    tempR1

c         integer maxMonths
c         parameter ( maxMonths = 360 ) ! 30 Years x 12
         integer nMonths

c         real    tFactors(maxMonths)
c         data    tFactors /maxMonths*0/

         real    total
         data    total /1*0.0/


         ! PARAMETERS ASSIGN
c         paramLeadEdge    = 6
c         paramPeakConc    = 12
c         paramTrailEdge   = 24
c         paramExponent1   = 0.1
c         paramExponent2   = 0.2
c         paramNormalize   = 0


         write(*,'(A I4 I4 I4$)')
     .        'Param: ',paramLeadEdge,paramPeakConc,paramTrailEdge
         write(*,'(E12.5, E12.5$)') paramExponent1,paramExponent2
         write(*,'(I2)') paramNormalize

         do i = 1,maxMonths
            tFactors(i) = 0.0
         end do

c         nMonths = 60 ! TODO

c         if (paramTrailEdge .le. 0) paramTrailEdge = nMonths
         do i = 1,paramTrailEdge
         !{
            if      ( i .lt. paramLeadEdge ) then
                 tFactors(i) = 0
            else if ( i .lt. paramPeakConc ) then
                 tFactors(i) = EXP( paramExponent1 * (i-paramPeakConc) )
            else
                 tFactors(i) = EXP(-paramExponent2 * (i-paramPeakConc) )
            end if
            write(*,'(I2, E12.5)'), i,tFactors(i)
         !}
         end do

         if ( paramNormalize .eq. 1 ) then
         !{
            total = 0.0
            do i = 1,paramTrailEdge
            !{
               total = total + tFactors(i)
            !}
            end do
            write(*,'(A E12.5)') 'Total = ',total
         !}
         else
            total = 1
         end if

         tempR1 = 0.0
         tempI1 = 0
         do i = 1,paramTrailEdge
         !{
            tFactors(i) = tFactors(i)/total
            tempR1 = tempR1 + tFactors(i)
            if ( paramNormalize.eq.1
c     .          .and. tempR1.gt.0
     .          .and. tempR1.lt.0.5 ) then
               tempI1 = tempI1 + 1
            end if
            write(*,'(I2, E12.5)'), i,tFactors(i)
         !}
         end do
         tempI1 = tempI1 + 1
         if ( paramNormalize.eq.1 ) then
           ! http://www.epa.gov/ncea/pdfs/qtracer/qtracerch03.pdf Section 3.2.1
           write(*,'(A I2 A)'),'Mean Residence Time = ',tempI1,' months'
         end if

         return

      end
      !}

