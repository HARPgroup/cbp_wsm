**********************************************************************
**  + + + PURPOSE + + +                                             **
**  Find flow frequency                                             **  
**  Fit data into a log Pearson Type III distribution               **
**  return frequency for any particular event                       **
**                                                                  **
** program originally written approximately 2007 by CBPO staff      **
**                                                                  **
**  gshenk 8/2012 - problem found where variables for flow and days **
**   were never populated.  Return frequencies were based on random **
**   flows.  Additionally, flows were converted to natural log, but **
**   untransformed using base 10, the K tables was only read in on  **
**   the first pass and the distribution of annual max flow was set **
**   to 1 if below 1. fixed all four issues 8/2012                  **
**********************************************************************
      subroutine returnfq
     I                   (lscen,landseg,nl,
     I                    StartY,StartM,StartD,EndY,EndM,EndD,
     O                    retfreq, retflow)
      implicit none
      include 'mbtc.f'
      include '../../lib/inc/wdm.inc'      

      integer wdmlnd                  ! file numbers
      parameter (wdmlnd=dfile+1)

      integer sdate(ndate), edate(ndate)  ! start and end dates in wdm format
      integer asdate(ndate)               ! start dates 

      character*6 landseg             ! land segment name
      integer lenlandseg
      integer nl  ! land use index

      integer ndays                   ! days within a year
      integer ndaytot                 ! total days in simulation
      integer i,hour,n,j,indx1        ! indices
      integer oldyear
      real dflow(ndaymax),dfl         ! daily flow
      real Tmax                ! max daily flow for a given year
      real hsuro(ndaymax*24),hifwo(ndaymax*24)

      integer maxevents, nv  ! number of values in the flow or rain vector
      integer timestep                ! number of timesteps per year (or other descriptive variable)
      real flow(ndaymax)              ! read-in flow values
      real p(ndaymax)                 ! probability of every value in 'flow' 
      real retfreq(ndaymax)           ! return frequency of every value in 'flow'
      real retflow(6)                 ! flow at 1,2,5,10,50,and 100 year return frequency

      real fac  ! temporary factor

      integer  nmax
      real annmax(nyearmax)
      real mean,var,std,skew
      real sum1,sum2,sum3,sum4,dev,svar

      real Tskew(70)            ! skew values in K table
      real Tk(27,70)            ! K values in table
      real k(27)                ! K values of computed skew for each probabilities in Ktable
      real q(27)                ! flow of computed skew for each probabilities in Ktable

      real Tp (27)       ! the probabilities in K table
      data Tp /0.9999, 0.9995, 0.999, 0.995, 0.99, 0.98, 0.975, 0.96,
     .          0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05,
     .          0.04, 0.025, 0.02, 0.01, 0.005, 0.001, 0.0005, 0.0001/

      integer pn(6)  ! position of flow at 1,2,5,10,50,and 100 year in ktp vector
      data   pn /5, 14, 17, 18, 22, 23/

      logical found
      real nonZeroMin
 
*********** END SPECIFICATIONS *****************************************

      call lencl(lscen,lenlscen)
      call lencl(landseg,lenlandseg)

******* GET START AND END DATE IN WDM FORMAT
      sdate(1) = StartY
      sdate(2) = StartM
      sdate(3) = StartD
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = EndY
      edate(2) = EndM
      edate(3) = EndD
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

*********** open WDM file to read in simulated flow
      wdmfnam = outwdmdir//'land/'//luname(nl)//'/'//lscen(:lenlscen)//
     .          '/'//luname(nl)//landseg(:lenlandseg)//'.wdm'
      print*,'BHATT ',wdmfnam
      call wdbopnlong(wdmlnd,wdmfnam,1,err)     ! open land read only
      if (err .ne. 0) go to 991

      dsn = 111  ! SURO, hardcode dsn number
                 ! (see config/catalog/iovars/land_to_river)     
      print *,'BHATT$ ',sdate,edate,nvals
      call gethourdsn(
     I                wdmlnd,sdate,edate,dsn,
     O                nvals,hsuro)

      dsn = 211                       ! IFWO, hourly interflow
      call gethourdsn(
     I                wdmlnd,sdate,edate,dsn,
     O                nvals,hifwo)
 
************** get annual max daily flow 
      do i = 1,6
        asdate(i) = sdate(i)
      end do

      hour = 0
      dfl = 0.
      ndays = 0
      nmax = 0
      ndaytot = 0
      oldyear = sdate(1)

****** get daily flow for each year
      do i = 1,nvals
        hour = hour + 1
        dfl = dfl + hsuro(i)+hifwo(i)  ! add SURO and IFWO to get runoff
        if (hour.eq.24) then
          ndays = ndays + 1
          ndaytot = ndaytot + 1
          dflow(ndays) = dfl          
          flow(ndaytot) = dfl
          hour = 0
          dfl = 0.
          call tomorrow(asdate(1),asdate(2),asdate(3))
        end if

****** find max daily flow of each year
        if (asdate(1).ne.oldyear) then
          nmax = nmax + 1
          Tmax = dflow(1)
          do n = 2, ndays
            if (dflow(n) .gt. Tmax) then
              Tmax = dflow(n)
            end if
          end do
          annmax(nmax) = Tmax
          oldyear = asdate(1)
          ndays = 0
        end if

      end do        ! end loop for time intervals in WDM

******* if any annual max values are zero, set to half of lowest
****** non-zero value
      nonZeroMin = 1.0E10
      found = .false.
      do n = 1,nmax
        if (annmax(n).gt.1.0E-9) then
          nonZeroMin = min(nonZeroMin,annmax(n))
          found = .true.
        end if
      end do
      if (.not.found) go to 996
      do n = 1,nmax
        if (annmax(n).lt.1.0E-9) then
          annmax(n) = nonZeroMin / 2.0
        end if
      end do

      
**********  compute general statistics (mean, standard deviation, skew)
      sum1   = 0.
      do n = 1, nmax
        annmax(n) = log(annmax(n))     ! log transform data
        sum1 = sum1 + annmax(n)
      end do
      mean = sum1/real(nmax)  ! mean

**********  accumulate sum of squares and cube of deviations from mean
      sum2 = 0.
      sum3 = 0.
      do n = 1, nmax
        dev = annmax(n) - mean
        if (abs(dev) .lt. 1.0e12) then
          sum2 = sum2  +  dev * dev
          sum3 = sum3  +  dev * dev * dev
        end if
      end  do

**********  compute the statistics
      var = sum2/real(nmax-1)
      if (var .gt. 0.1e-7) then
        std = sqrt(var)   ! compute the standard deviation
        skew = (nmax*sum3)/(real(nmax-1)*real(nmax-2)*std*std*std)   
      end if

**********  fit curve and compute conditional probablility

**** skew values in available K table for Log Pearson III distribution
      Tskew(1) = 3.4                   ! Range = [-3.4, 3.4]
      do i = 2, 69     
        Tskew(i) = Tskew(i-1) - 0.1
      end do

****  read in K value from table
      call Ktable (Tk)           ! read K value 

CBHATT??      if (abs(skew) .GT. 3.30) go to 992     !  skew out of range
CBHATT??      if (abs(skew) .GT. 3.399) go to 992     !  skew out of range
      if ( skew .GT.  3.399) skew =  3.399
      if ( skew .LT. -3.399) skew = -3.399
****  skew in valid range, interpolate
      do i = 1,69
        if (skew .le. Tskew(i) .and. skew.ge.Tskew(i+1)) then
          fac = (skew-Tskew(i))/(Tskew(i+1)-Tskew(i))
          do j = 1,27  
            k(j) = Tk(j,i) + fac*(Tk(j,i+1)-Tk(j,i))
          end do
          exit
        end if
      end do  

****  calculate flow from computed mean,std and K value
      do j = 1,27
        q(j) = mean + k(j)*std
        q(j) = exp(q(j))   ! transform from log value back to real flow
      end do

**** for any given flow event, interporate to find return probabilities 
      do n = 1, ndaytot
        if (flow(n).le.q(2)) then
          indx1 = 1
        elseif (flow(n).ge.q(26)) then
          indx1 = 26
        else
          do j = 2, 25
            if (flow(n).ge.q(j) .and. flow(n).le.q(j+1)) then
              indx1 = j
              exit
            end if
          end do
        end if
        fac = (flow(n) - q(indx1))/(q(indx1+1)-q(indx1))
        p(n)= Tp(indx1) + fac*(Tp(indx1+1)-Tp(indx1))
        retfreq(n) = 1.0/p(n)
C        print*,indx1,',',flow(n),',',p(n),',',retfreq(n) ! verified
      end do

**********  Compute flow for selected recurrence intervals
      do i = 1, 6
        retflow(i) = q(pn(i))
      end do

      call wdflcl(wdmlnd,err)     ! close land wdm
      if (err.ne.0) go to 995

      return

*********** ERROR SPACE ************************************************
991   write(report(1),*)'Error: problem opening wdm = ',err
      report(2) = wdmfnam
      report(3) = 'opening to calculate return frequency'
      go to 999

992   report(1) = 'computed skew value'
      report(2) = 'outside the range in K table [-3.3, 3.3] '
c      report(3) = 'skew =   '
c      write(report(3)(8:13),'(i5)') skew
      write(report(3),*) 'skew= ',skew
      go to 999

993   report(1) = 'first column must be the River segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

994   report(1) = 'second column must be the land segment and '
      report(2) = '   must have a title line in file'
      report(3) = fnam
      go to 999

995   write(report(1),*) 'Error: problem closing wdm = ',err
      report(2) = wdmfnam
      write(report(3),*) wdmlnd 
      go to 999

996   write(report(1),*) 'Error: no non-zero values found for annual'
      write(report(2),*) 'max quickflow in returnfq subroutine'
      report(3) = wdmfnam
      go to 999

999   call stopreport(report)

      end


************************************************************************
**  + + + PURPOSE + + +                                               **
**                                                                    **
** Table of Frequency Factors K for Gamma and log Pearson Type III    **
** Distributions                                                      **
************************************************************************

CBHATT ftp://ftp-fc.sc.egov.usda.gov/NHQ/pub/outgoing/jbernard/CED-Directives/technical-releases/tr38.pdf

      subroutine Ktable
     O                 (Tk)

      real    Tk(27,70)
      integer i, j
      real    K(27,70)

********** K value for  positive skew: -3.4 =< skew <= 3.4 
      data  (K(1,j), j = 1, 69)  ! staring from skew=3.4 down to -3.4
     .      /-0.58824,-0.60606,-0.62500,-0.64516,-0.66667,-0.68966,
     .       -0.71429,-0.74074,-0.76923,-0.80000,-0.83333,-0.86956,
     .       -0.90908,-0.95234,-0.99990,-1.05239,-1.11054,-1.17520,
     .       -1.24728,-1.32774,-1.41753,-1.51752,-1.62838,-1.75053,
     .       -1.88410,-2.02891,-2.18448,-2.35015,-2.52507,-2.70836,
     .       -2.89907,-3.09631,-3.29921,-3.50703,-3.71902,-3.93453,
     .       -4.15301,-4.37394,-4.59687,-4.82141,-5.04718,-5.27389,
     .       -5.50124,-5.72899,-5.95691,-6.18480,-6.41249,-6.63980,
     .       -6.86661,-7.09277,-7.31818,-7.54272,-7.76632,-7.98888,
     .       -8.21034,-8.43064,-8.64971,-8.86753,-9.08403,-9.29920,
     .       -9.51301,-9.72543,-9.93643,-10.14602,-10.35418,-10.56090, 
     .       -10.76618,-10.97001,-11.17239/

      data  (K(2,j), j = 1, 69)
     .      /-0.58824,-0.60606,-0.62500,-0.64516,-0.66667,-0.68966,
     .       -0.71429,-0.74074,-0.76923,-0.79999,-0.83331,-0.86952,
     .       -0.90899,-0.95215,-0.99950,-1.05159,-1.10901,-1.17240,
     .       -1.24235,-1.31944,-1.40413,-1.49673,-1.59738,-1.70603,
     .       -1.82241,-1.94611,-2.07661,-2.21328,-2.35549,-2.50257,
     .       -2.65390,-2.80889,-2.96698,-3.12767,-3.29053,-3.45513,
     .       -3.62113,-3.78820,-3.95605,-4.12443,-4.29311,-4.46189,
     .       -4.63057,-4.79899,-4.96701,-5.13449,-5.30130,-5.46735,
     .       -5.63252,-5.79673,-5.95990,-6.12196,-6.28285,-6.44251,
     .       -6.60090,-6.75798,-6.91370,-7.06804,-7.22098,-7.37250,
     .       -7.52258,-7.67121,-7.81839,-7.96411,-8.10836,-8.25115,
     .       -8.39248,-8.53236,-8.67079/

      data  (K(3,j), j = 1, 69)
     .      /-0.58824,-0.60606,-0.62500,-0.64516,-0.66667,-0.68965,
     .       -0.71428,-0.74074,-0.76922,-0.79998,-0.83328,-0.86945,
     .       -0.90885,-0.95188,-0.99900,-1.05068,-1.10743,-1.16974,
     .       -1.23805,-1.31275,-1.39408,-1.48216,-1.57695,-1.67825,
     .       -1.78572,-1.89894,-2.01739,-2.14053,-2.26780,-2.39867,
     .       -2.53261,-2.66915,-2.80786,-2.94834,-3.09023,-3.23322,
     .       -3.37703,-3.52139,-3.66608,-3.81090,-3.95567,-4.10022,
     .       -4.24439,-4.38807,-4.53112,-4.67344,-4.81492,-4.95549,
     .       -5.09505,-5.23353,-5.37087,-5.50701,-5.64190,-5.77549,
     .       -5.90776,-6.03865,-6.16816,-6.29626,-6.42292,-6.54814,
     .       -6.67191,-6.79421,-6.91505,-7.03443,-7.15235,-7.26881,
     .       -7.38382,-7.49739,-7.60953/

      data  (K(4,j), j = 1, 69)
     .      /-0.58824,-0.60606,-0.62500,-0.64516,-0.66667,-0.68964,
     .       -0.71425,-0.74067,-0.76909,-0.79973,-0.83283,-0.86863,
     .       -0.90742,-0.94945,-0.99499,-1.04427,-1.09749,-1.15477,
     .       -1.21618,-1.28167,-1.35114,-1.42439,-1.50114,-1.58110,
     .       -1.66390,-1.74919,-1.83660,-1.92580,-2.01644,-2.10825,
     .       -2.20092,-2.29423,-2.38795,-2.48187,-2.57583,-2.66965,
     .       -2.76321,-2.85636,-2.94900,-3.04102,-3.13232,-3.22281,
     .       -3.31243,-3.40109,-3.48874,-3.57530,-3.66073,-3.74497,
     .       -3.82798,-3.90973,-3.99016,-4.06926,-4.14700,-4.22336,
     .       -4.29832,-4.37186,-4.44398,-4.51467,-4.58393,-4.65176,
     .       -4.71815,-4.78313,-4.84669,-4.90884,-4.96959,-5.02897,
     .       -5.08697,-5.14362,-5.19892/
 
      data  (K(5,j), j = 1, 69)
     .      /-0.58823,-0.60606,-0.62499,-0.64514,-0.66663,-0.68959,
     .       -0.71415,-0.74049,-0.76878,-0.79921,-0.83196,-0.86723,
     .       -0.90521,-0.94607,-0.98995,-1.03695,-1.08711,-1.14042,
     .       -1.19680,-1.25611,-1.31815,-1.38267,-1.44942,-1.51808,
     .       -1.58838,-1.66001,-1.73271,-1.80621,-1.88029,-1.95472,
     .       -2.02933,-2.10394,-2.17840,-2.25258,-2.32635,-2.39961,
     .       -2.47226,-2.54421,-2.61539,-2.68572,-2.75514,-2.82359,
     .       -2.89101,-2.95735,-3.02256,-3.08660,-3.14944,-3.21103,
     .       -3.27134,-3.33035,-3.38804,-3.44438,-3.49935,-3.55295,
     .       -3.60517,-3.65600,-3.70543,-3.75347,-3.80013,-3.84540,
     .       -3.88930,-3.93183,-3.97301,-4.01286,-4.05138,-4.08859,
     .       -4.12452,-4.15917,-4.19257/
    
      data  (K(6,j), j = 1, 69)
     .      /-0.58822,-0.60603,-0.62495,-0.64507,-0.66649,-0.68935,
     .       -0.71377,-0.73987,-0.76779,-0.79765,-0.82959,-0.86371,
     .       -0.90009,-0.93878,-0.97980,-1.02311,-1.11628,-1.11628,
     .       -1.16584,-1.21716,-1.26999,-1.32412,-1.37929,-1.43529,
     .       -1.49188,-1.54886,-1.60604,-1.66325,-1.72033,-1.77716,
     .       -1.83361,-1.88959,-1.94499,-1.99973,-2.05375,-2.10697,
     .       -2.15935,-2.21081,-2.26133,-2.31084,-2.35931,-2.40670,
     .       -2.45298,-2.49811,-2.54206,-2.58480,-2.62631,-2.66657,
     .       -2.70556,-2.74325,-2.77964,-2.81472,-2.84848,-2.88091,
     .       -2.91202,-2.94181,-2.97028,-2.99744,-3.02330,-3.04787,
     .       -3.07116,-3.09320,-3.11399,-3.13356,-3.15193,-3.16911,
     .       -3.18512,-3.20000,-3.21375/
 
      data  (K(7,j), j = 1, 69)
     .      /-0.58821,-0.60601,-0.62491,-0.64500,-0.66638,-0.68917,
     .       -0.71348,-0.73943,-0.76712,-0.79667,-0.82817,-0.86169,
     .       -0.89728,-0.93495,-0.97468,-1.01640,-1.06001,-1.10537,
     .       -1.15229,-1.20059,-1.25004,-1.30042,-1.35153,-1.40314,
     .       -1.45507,-1.50712,-1.55914,-1.61099,-1.66253,-1.71366,
     .       -1.76427,-1.81427,-1.86360,-1.91219,-1.95996,-2.00688,
     .       -2.05290,-2.09795,-2.14202,-2.18505,-2.22702,-2.26790,
     .       -2.30764,-2.34623,-2.38364,-2.41984,-2.45482,-2.48855,
     .       -2.52102,-2.55222,-2.58214,-2.61076,-2.63810,-2.66413,
     .       -2.68888,-2.71234,-2.73451,-2.75541,-2.77506,-2.79345,
     .       -2.81062,-2.82658,-2.84134,-2.85492,-2.86735,-2.87865,
     .       -2.88884,-2.89795,-2.90599/
   
      data  (K(8,j), j = 1, 69)
     .      /-0.58812,-0.60587,-0.62469,-0.64465,-0.66585,-0.68836,
     .       -0.71227,-0.73765,-0.76456,-0.79306,-0.82315,-0.85486,
     .       -0.88814,-0.92295,-0.95918,-0.99672,-1.03543,-1.07513,
     .       -1.11566,-1.15682,-1.19842,-1.24028,-1.28225,-1.32414,
     .       -1.36584,-1.40720,-1.44813,-1.48852,-1.52830,-1.56740,
     .       -1.60574,-1.64329,-1.67999,-1.71580,-1.75069,-1.78462,
     .       -1.81756,-1.84949,-1.88039,-1.91022,-1.93896,-1.96660,
     .       -1.99311,-2.01848,-2.04269,-2.06573,-2.08758,-2.10823,
     .       -2.12768,-2.14591,-2.16293,-2.17873,-2.19332,-2.20670,
     .       -2.21888,-2.22986,-2.23967,-2.24831,-2.25581,-2.26217,
     .       -2.26743,-2.27160,-2.27470,-2.27676,-2.27780,-2.27785,
     .       -2.27693,-2.27506,-2.27229/
     
      data  (K(9,j), j = 1, 69)
     .      /-0.58802,-0.60572,-0.62445,-0.64429,-0.66532,-0.68759,
     .       -0.71116,-0.73610,-0.76242,-0.79015,-0.81927,-0.84976,
     .       -0.88156,-0.91458,-0.94871,-0.98381,-1.01973,-1.05631,
     .       -1.09338,-1.13075,-1.16827,-1.20578,-1.24313,-1.28019,
     .       -1.31684,-1.35299,-1.38855,-1.42345,-1.45762,-1.49101,
     .       -1.52357,-1.55527,-1.58607,-1.61594,-1.64485,-1.67279,
     .       -1.69971,-1.72562,-1.75048,-1.77428,-1.79701,-1.81864,
     .       -1.83916,-1.85856,-1.87683,-1.89395,-1.90992,-1.92472,
     .       -1.93836,-1.95083,-1.96213,-1.97227,-1.98124,-1.98906,
     .       -1.99573,-2.00128,-2.00570,-2.00903,-2.01128,-2.01247,
     .       -2.01263,-2.01177,-2.00992,-2.00710,-2.00335,-1.99869,
     .       -1.99314,-1.98674,-1.97951/

      data  (K(10,j), j = 1, 69)
     .      /-0.58666,-0.60379,-0.62175,-0.64056,-0.66023,-0.68075,
     .       -0.70209,-0.72422,-0.74709,-0.77062,-0.79472,-0.81929,
     .       -0.84422,-0.86938,-0.89464,-0.91988,-0.94496,-0.96977,
     .       -0.99418,-1.01810,-1.04144,-1.06413,-1.08608,-1.10726,
     .       -1.12762,-1.14712,-1.16574,-1.18347,-1.20028,-1.21618,
     .       -1.23114,-1.24516,-1.25824,-1.27037,-1.28155,-1.29178,
     .       -1.30105,-1.30936,-1.31671,-1.32309,-1.32850,-1.33294,
     .       -1.33640,-1.33889,-1.34039,-1.34092,-1.34047,-1.33904,
     .       -1.33665,-1.33330,-1.32900,-1.32376,-1.31760,-1.31054,
     .       -1.30259,-1.29377,-1.28412,-1.27365,-1.26240,-1.25039,
     .       -1.23766,-1.22422,-1.21013,-1.19539,-1.18006,-1.16416,
     .       -1.14772,-1.13078,-1.11337/

      data  (K(11,j), j = 1, 69)
     .      /-0.57652,-0.59096,-0.60567,-0.62060,-0.63569,-0.65086,
     .       -0.66603,-0.68111,-0.69602,-0.71067,-0.72495,-0.73880,
     .       -0.75211,-0.76482,-0.77686,-0.78816,-0.79868,-0.80837,
     .       -0.81720,-0.82516,-0.83223,-0.83841,-0.84369,-0.84809,
     .       -0.85161,-0.85426,-0.85607,-0.85703,-0.85718,-0.85653,
     .       -0.85508,-0.85285,-0.84986,-0.84611,-0.84162,-0.83639,
     .       -0.83044,-0.82377,-0.81638,-0.80829,-0.79950,-0.79002,
     .       -0.77986,-0.76902,-0.75752,-0.74537,-0.73257,-0.71915,
     .       -0.70512,-0.69050,-0.67532,-0.65959,-0.64335,-0.62662,
     .       -0.60944,-0.59183,-0.57383,-0.55549,-0.53683,-0.51789,
     .       -0.49872,-0.47934,-0.45980,-0.44015,-0.42040,-0.40061,
     .       -0.38081,-0.36104,-0.34133/

      data  (K(12,j), j = 1, 69)
     .      /-0.55000,-0.55989,-0.56953,-0.57887,-0.58783,-0.59634,
     .       -0.60434,-0.61176,-0.61854,-0.62463,-0.62999,-0.63456,
     .       -0.63833,-0.64125,-0.64333,-0.64453,-0.64488,-0.64436,
     .       -0.64300,-0.64080,-0.63779,-0.63400,-0.62944,-0.62415,
     .       -0.61815,-0.61146,-0.60412,-0.59615,-0.58757,-0.57840,
     .       -0.56867,-0.55839,-0.54757,-0.53624,-0.52440,-0.51207,
     .       -0.49927,-0.48600,-0.47228,-0.45812,-0.44352,-0.42851,
     .       -0.41309,-0.39729,-0.38111,-0.36458,-0.34772,-0.33054,
     .       -0.31307,-0.29535,-0.27740,-0.25925,-0.24094,-0.22250,
     .       -0.20397,-0.18540,-0.16682,-0.14827,-0.12979,-0.11143,
     .       -0.09323,-0.07523,-0.05746,-0.03997,-0.02279,-0.00596,
     .        0.01050,0.02654,0.04215/

      data  (K(13,j), j = 1, 69)
     .      /-0.49844,-0.50244,-0.50585,-0.50863,-0.51073,-0.51212,
     .       -0.51276,-0.51263,-0.51171,-0.50999,-0.50744,-0.50409,
     .       -0.49991,-0.49494,-0.48917,-0.48265,-0.47538,-0.46739,
     .       -0.45873,-0.44942,-0.43949,-0.42899,-0.41794,-0.40638,
     .       -0.39434,-0.38186,-0.36889,-0.35565,-0.34198,-0.32796,
     .       -0.31362,-0.29897,-0.28403,-0.26882,-0.25335,-0.23763,
     .       -0.22168,-0.20552,-0.18916,-0.17261,-0.15589,-0.13901,
     .       -0.12199,-0.10486,-0.08763,-0.07032,-0.05297,-0.03560,
     .       -0.01824,-0.00092,0.01631,0.03340,0.05040,0.06718,0.08371,
     .        0.09997,0.11590,0.13148,0.14665,0.16138,0.17564,0.18939,
     .        0.20259,0.21523,0.22726,0.23868,0.24946,0.25958,0.26904/

      data  (K(14,j), j = 1, 69)
     .      /-0.41058,-0.40792,-0.40454,-0.40041,-0.39554,-0.38991,
     .       -0.38353,-0.37640,-0.36852,-0.35992,-0.35062,-0.34063,
     .       -0.32999,-0.31872,-0.30685,-0.29443,-0.28150,-0.26808,
     .       -0.25422,-0.23996,-0.22535,-0.21040,-0.19517,-0.17968,
     .       -0.16397,-0.14807,-0.13199,-0.11578,-0.09945,-0.08302,
     .       -0.06651,-0.04993,-0.03325,-0.01662,0.0,0.01662,0.03325,
     .        0.04993,0.06651,0.08302,0.09945,0.11578,0.13199,0.14807,
     .        0.16397,0.17968,0.19517,0.21040,0.22535,0.23996,0.25422,
     .        0.26808,0.28150,0.29443,0.30685,0.31872,0.32999,0.34063,
     .        0.35062,0.35992,0.36852,0.37640,0.38353,0.38991,0.39554,
     .        0.40041,0.40454,0.40792,0.41058/

      data  (K(15,j), j = 1, 69)
     .      /-0.26904,-0.25958,-0.24946,-0.23868,-0.22726,-0.21523,
     .       -0.20259,-0.18939,-0.17564,-0.16138,-0.14665,-0.13148,
     .       -0.11590,-0.09997,-0.08371,-0.06718,-0.05040,-0.03344,
     .       -0.01631,0.00092,0.01824,0.03560,0.05297,0.07032,0.08763,
     .        0.10486,0.12199,0.13901,0.15589,0.17261,0.18916,0.20552,
     .        0.22168,0.23763,0.25335,0.26882,0.28403,0.29897,0.31362,
     .        0.32796,0.34198,0.35565,0.36889,0.38186,0.39434,0.40638,
     .        0.41794,0.42899,0.43949,0.44942,0.45873,0.46739,0.47538,
     .        0.48265,0.48917,0.49494,0.49991,0.50409,0.50744,0.50999,
     .        0.51171,0.51263,0.51276,0.51212,0.51073,0.50863,0.50585,
     .        0.50244,0.49844/
  
      data  (K(16,j), j = 1, 69)
     .       /-0.04215,-0.02654,-0.01050,0.00596,0.02279,0.03997,
     .         0.05746,0.07523,0.09323,0.11143,0.12979,0.14827,0.16682,
     .         0.18540,0.20397,0.22250,0.24094,0.25925,0.27740,0.29535,
     .         0.31307,0.33054,0.34772,0.36458,0.38111,0.39729,0.41309,
     .         0.42851,0.44352,0.45812,0.47228,0.48600,0.49927,0.51207,
     .         0.52440,0.53624,0.54757,0.55839,0.56867,0.57840,0.58757,
     .         0.59615,0.60412,0.61146,0.61815,0.62415,0.62944,0.63400,
     .         0.63779,0.64080,0.64300,0.64436,0.64488,0.64453,0.64333,
     .         0.64125,0.63833,0.63456,0.62999,0.62463,0.61854,0.61176,
     .         0.60434,0.59634,0.58783,0.57887,0.56953,0.55989,0.55000/

      data  (K(17,j), j = 1, 69)
     .      /0.34133,0.36104,0.38081,0.40061,0.42040,0.44015,0.45980,
     .       0.47934,0.49872,0.51789,0.53683,0.55549,0.57383,0.59183,
     .       0.60944,0.62662,0.64335,0.65959,0.67532,0.69050,0.70512,
     .       0.71915,0.73257,0.74537,0.75752,0.76902,0.77986,0.79002,
     .       0.79950,0.80829,0.81638,0.82377,0.83044,0.83639,0.84162,
     .       0.84611,0.84986,0.85285,0.85508,0.85653,0.85718,0.85703,
     .       0.85607,0.85426,0.85161,0.84809,0.84369,0.83841,0.83223,
     .       0.82516,0.81720,0.80837,0.79868,0.78816,0.77686,0.76482,
     .       0.75211,0.73880,0.72495,0.71067,0.69602,0.68111,0.66603,
     .       0.65086,0.63569,0.62060,0.60567,0.59096,0.57652/

      data  (K(18,j), j = 1, 69)
     .      /1.11337,1.13078,1.14772,1.16416,1.18006,1.19539,1.21013,
     .       1.22422,1.23766,1.25039,1.26240,1.27365,1.28412,1.29377,
     .       1.30259,1.31054,1.31760,1.32376,1.32900,1.33330,1.33665,
     .       1.33904,1.34047,1.34092,1.34039,1.33889,1.33640,1.33294,
     .       1.32850,1.32309,1.31671,1.30936,1.30105,1.29178,1.28155,
     .       1.27037,1.25824,1.24516,1.23114,1.21618,1.20028,1.18347,
     .       1.16574,1.14712,1.12762,1.10726,1.08608,1.06413,1.04144,
     .       1.01810,0.99418,0.96977,0.94496,0.91988,0.89464,0.86938,
     .       0.84422,0.81929,0.79472,0.77062,0.74709,0.72422,0.70209,
     .       0.68075,0.66023,0.64056,0.62175,0.60379,0.58666/

      data  (K(19,j), j = 1, 69)
     .      /1.97951,1.98674,1.99314,1.99869,2.00335,2.00710,2.00992,
     .       2.01177,2.01263,2.01247,2.01128,2.00903,2.00570,2.00128,
     .       1.99573,1.98906,1.98124,1.97227,1.96213,1.95083,1.93836,
     .       1.92472,1.90992,1.89395,1.87683,1.85856,1.83916,1.81864,
     .       1.79701,1.77428,1.75048,1.72562,1.69971,1.67279,1.64485,
     .       1.61594,1.58607,1.55527,1.52357,1.49101,1.45762,1.42345,
     .       1.38855,1.35299,1.31684,1.28019,1.24313,1.20578,1.16827,
     .       1.13075,1.09338,1.05631,1.01973,0.98381,0.94871,0.91458,
     .       0.88156,0.84976,0.81927,0.79015,0.76242,0.73610,0.71116,
     .       0.68759,0.66532,0.64429,0.62445,0.60572,0.58802/

      data  (K(20,j), j = 1, 69)
     .      /2.27229,2.27506,2.27693,2.27785,2.27780,2.27676,2.27470,
     .       2.27160,2.26743,2.26217,2.25581,2.24831,2.23967,2.22986,
     .       2.21888,2.20670,2.19332,2.17873,2.16293,2.14591,2.12768,
     .       2.10823,2.08758,2.06573,2.04269,2.01848,1.99311,1.96660,
     .       1.93896,1.91022,1.88039,1.84949,1.81756,1.78462,1.75069,
     .       1.71580,1.67999,1.64329,1.60574,1.56740,1.52830,1.48852,
     .       1.44813,1.40720,1.36584,1.32414,1.28225,1.24028,1.19842,
     .       1.15682,1.11566,1.07513,1.03543,0.99672,0.95918,0.92295,
     .       0.88814,0.85486,0.82315,0.79306,0.76456,0.73765,0.71227,
     .       0.68336,0.66585,0.64465,0.62469,0.60587,0.58812/
 
      data  (K(21,j), j = 1, 69)
     .      /2.90599,2.89795,2.88884,2.87865,2.86735,2.85492,2.84134,
     .       2.82658,2.81062,2.79345,2.77506,2.75541,2.73451,2.71234,
     .       2.68888,2.66413,2.63810,2.61076,2.58214,2.55222,2.52102,
     .       2.48855,2.45482,2.41984,2.38364,2.34623,2.30764,2.26790,
     .       2.22702,2.18505,2.14202,2.09795,2.05290,2.00688,1.95996,
     .       1.91219,1.86360,1.81427,1.76427,1.71366,1.66253,1.61099,
     .       1.55914,1.50712,1.45507,1.40314,1.35153,1.30042,1.25004,
     .       1.20059,1.15229,1.10537,1.06001,1.01640,0.97468,0.93495,
     .       0.89728,0.86169,0.82817,0.79667,0.76712,0.73943,0.71348,
     .       0.68917,0.66638,0.64500,0.62491,0.60601,0.58821/

      data  (K(22,j), j = 1, 69)
     .      /3.21375,3.20000,3.18512,3.16911,3.15193,3.13356,3.11399,
     .       3.09320,3.07116,3.04787,3.02330,2.99744,2.97028,2.94181,
     .       2.91202,2.88091,2.84848,2.81472,2.77964,2.74325,2.70556,
     .       2.66657,2.62631,2.58480,2.54206,2.49811,2.45298,2.40670,
     .       2.35931,2.31084,2.26133,2.21081,2.15935,2.10697,2.05375,
     .       1.99973,1.94499,1.88959,1.83361,1.77716,1.72033,1.66325,
     .       1.60604,1.54886,1.49188,1.43529,1.37929,1.32412,1.26999,
     .       1.21716,1.16584,1.11628,1.06864,1.02311,0.97980,0.93878,
     .       0.90009,0.86371,0.82959,0.79765,0.76779,0.73987,0.71377,
     .       0.68935,0.66649,0.64507,0.62495,0.60603,0.58822/

      data  (K(23,j), j = 1, 69)
     .      /4.19257,4.15917,4.12452,4.08859,4.05138,4.01286,3.97301,
     .       3.93183,3.88930,3.84540,3.80013,3.75347,3.70543,3.65600,
     .       3.60517,3.55295,3.49935,3.44438,3.38804,3.33035,3.27134,
     .       3.21103,3.14944,3.08660,3.02256,2.95735,2.89101,2.82359,
     .       2.75514,2.68572,2.61539,2.54421,2.47226,2.39961,2.32635,
     .       2.25258,2.17840,2.10394,2.02933,1.95472,1.88029,1.80621,
     .       1.73271,1.66001,1.58838,1.51808,1.44942,1.38267,1.31815,
     .       1.25611,1.19680,1.14042,1.08711,1.03695,0.98995,0.94607,
     .       0.90521,0.86723,0.83196,0.79921,0.76878,0.74049,0.71415,
     .       0.68959,0.66663,0.64514,0.62499,0.60606,0.58823/

      data  (K(24,j), j = 1, 69)
     .      /5.19892,5.14362,5.08697,5.02897,4.96959,4.90884,4.84669,
     .       4.78313,4.71815,4.65176,4.58393,4.51467,4.44398,4.37186,
     .       4.29832,4.22336,4.14700,4.06926,3.99016,3.90973,3.82798,
     .       3.74497,3.66073,3.57530,3.48874,3.40109,3.31243,3.22281,
     .       3.13232,3.04102,2.94900,2.85636,2.76321,2.66965,2.57583,
     .       2.48187,2.38795,2.29423,2.20092,2.10825,2.01644,1.92580,
     .       1.83660,1.74919,1.66390,1.58110,1.50114,1.42439,1.35114,
     .       1.28167,1.21618,1.15477,1.09749,1.04427,0.99499,0.94945,
     .       0.90742,0.86863,0.83283,0.79973,0.76909,0.74067,0.71425,
     .       0.68964,0.66666,0.64516,0.62500,0.60606,0.58824/
 
      data  (K(25,j), j = 1, 69)
     .      /7.60953,7.49739,7.38382,7.26881,7.15235,7.03443,6.91505,
     .       6.79421,6.67191,6.54814,6.42292,6.29626,6.16816,6.03865,
     .       5.90776,5.77549,5.64190,5.50701,5.37087,5.23353,5.09505,
     .       4.95549,4.81492,4.67344,4.53112,4.38807,4.24439,4.10022,
     .       3.95567,3.81090,3.66608,3.52139,3.37703,3.23322,3.09023,
     .       2.94834,2.80786,2.66915,2.53261,2.39867,2.26780,2.14053,
     .       2.01739,1.89894,1.78572,1.67825,1.57695,1.48216,1.39408,
     .       1.31275,1.23805,1.16974,1.10743,1.05068,0.99900,0.95188,
     .       0.90885,0.86945,0.83328,0.79998,0.76922,0.74074,0.71428,
     .       0.68965,0.66667,0.64516,0.62500,0.60606,0.58824/

      data  (K(26,j), j = 1, 69)
     .      /8.67076,8.53236,8.39248,8.25115,8.10836,7.96411,7.81839,
     .       7.67121,7.52258,7.37250,7.22098,7.06804,6.91370,6.75798,
     .       6.60090,6.44251,6.28285,6.12196,5.95990,5.79673,5.63252,
     .       5.46735,5.30130,5.13449,4.96701,4.79899,4.63057,4.46189,
     .       4.29311,4.12443,3.95605,3.78820,3.62113,3.45513,3.29053,
     .       3.12767,2.96698,2.80889,2.65390,2.50257,2.35549,2.21328,
     .       2.07661,1.94611,1.82241,1.70603,1.59738,1.49673,1.40413,
     .       1.31944,1.24235,1.17240,1.10901,1.05159,0.99950,0.95215,
     .       0.90899,0.86952,0.83331,0.79999,0.76923,0.74074,0.71429,
     .       0.68966,0.66667,0.64516,0.62500,0.60606,0.58824/

      data  (K(27,j), j = 1, 69)
     .      /11.17239,10.97001,10.76618,10.56090,10.35418,10.14602,
     .       9.93643,9.72543,9.51301,9.29920,9.08403,8.86753,8.64971,
     .       8.43064,8.21034,7.98888,7.76632,7.54272,7.31818,7.09277,
     .       6.86661,6.63980,6.41249,6.18480,5.95691,5.72899,5.50124,
     .       5.27389,5.04718,4.82141,4.59687,4.37394,4.15301,3.93453,
     .       3.71902,3.50703,3.29921,3.09631,2.89907,2.70836,2.52507,
     .       2.35015,2.18448,2.02891,1.88410,1.75053,1.62838,1.51752,
     .       1.41753,1.32774,1.24728,1.17520,1.11054,1.05239,0.99990,
     .       0.95234,0.90908,0.86956,0.83333,0.80000,0.76923,0.74074,
     .       0.71429,0.68966,0.66667,0.64516,0.62500,0.60606,0.58824/
    
*********** END SPECIFICATIONS **********************************************

********** assign to K
      do i = 1, 27
        do j = 1, 69
          Tk(i,j) = K(i,j)
        end do 
      end do 

      return

      end



