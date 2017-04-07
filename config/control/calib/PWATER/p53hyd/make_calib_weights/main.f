************************************************************************
** Program to generate weights for each land segments at each         **
**   calibration site.  The sum of all calibration-site weights for   **
**   each segment must equal 1                                        **
** NOTE, nrsegs in this program means the number of segments that are **
**   in the calibration, not the total in the model as in most        **
**   programs.  other routines are slightly different as well.        **
************************************************************************
      include 'flow_weight.inc'

      character*13 basin
      integer lenbasin

      integer revallup(maxcsegs,maxrsegs) ! index of all upstream segments
      integer nrevallup(maxrsegs),nrev         ! number of all upstream segments

      character*13 revnm(maxrservoir)
      real         revsize(maxrservoir)
      integer i,nl,nr1,nr2,ncsegs2,nr,ns
      integer npoints,nonLOD  ! number of data points found
      logical foundall,anyobs(maxrsegs)

      integer year1,year2,ny,totdays
      integer nm,nd,nh,nmin   ! variables to read observed file
      real value,test
      character*1 qflag

      logical gtlimit,orphan
      real minweight, limit

      real maxpercent(maxlsegs) ! max importance of a lseg to any rseg

************ END DECLARATIONS ******************************************
      print*,' determining the weights for use in hydro calibration'

************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen,obscen,basin,year1,year2,limit

      call lencl(calscen,lencalscen)
      call lencl(rscen,lenrscen)
      call lencl(basin,lenbasin)

      module = 'PWATER'
      call uppercase(module)
      call lencl(module,lenmod)
      call lencl(calscen,lencalscen)
      call lencl(obscen,lenobscen)

************* HOUSEKEEPING
      call readLandSeglist(basin,
     O                     lsegs,nlsegs)
      
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)

      do nr = 1,nrsegs
        csegs(nr) = rsegs(nr)
      end do
      ncsegs = nrsegs

*********** populate uniqid, dsid, uniqindex
      do ns = 1,supermax
        uniqindex(ns) = 0
      end do
      do nr = 1,nrsegs
        read(rsegs(nr)(5:8),'(i4)') uniqid(nr)
        read(rsegs(nr)(10:13),'(i4)') dsid(nr)
        uniqindex(uniqid(nr)) = nr
      end do

*********** read in possible doubles as well
************* add to the end of csegs
      call adddoubles(
     I                rscen,lenrscen,
     M                csegs,ncsegs)

********** read in upstream land segs and acres
      call getlandtoriver(nrsegs,rsegs,nlsegs,lsegs,
     I                    rscen,lenrscen,
     O                    nallland,allland,acres)
     
********** read in all reservoirs and calculate their basin size
      call get_reservoir_info(
     I                    rscen,lenrscen,
     I                    nrsegs,rsegs,nlsegs,lsegs,
     I                    nallland,allland,acres,
     I                    uniqindex,uniqid,dsid,
     O                    nrev,revnm,revsize,nrevallup,revallup)

******** loop over simulated segments  
********* keep those segments that have data
      print*,'finding observed values for '
      totdays =(year2-year1+1)*365     ! total days within time period

      do ns = 1,ncsegs
        npoints = 0
        nonLOD = 0
        fnam = tree//'input/calib/observed/'//obscen(:lenobscen)//
     .           '/FLOW/'//csegs(ns)//'.OFLOW'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.eq.0) then

          do
            read(dfile,*,end=333) ny,nm,nd,nh,nmin,value,qflag
       
            if (ny.ge.year1 .and. ny.le.year2) then
              npoints = npoints + 1
              if (qflag.ne.'<') nonLOD = nonLOD + 1
            end if
          end do
333       close(dfile)           ! close observed load file
        end if

        obs(ns) = npoints
        goodobs(ns) = nonLOD

        anyobs(ns) = .false.
        if (obs(ns).ge.0.5*totdays .or. obs(ns).ge.1000) 
     .           anyobs(ns)=.true.  
           ! at least over 1/2 the time period or 1000 observations
 
      end do          ! end loop of simulated segments
      
*********** remove segs from ncsegs that do not have data
      ncsegs2 = ncsegs
      do ns = 1,ncsegs2     ! get rid of missing values
        if (ns.gt.ncsegs) exit
        if (.not.anyobs(ns)) then
          do while (.not.anyobs(ncsegs))
            ncsegs = ncsegs - 1
          end do
          if (ncsegs.lt.ns) exit
          csegs(ns) = csegs(ncsegs)
          anyobs(ns) = anyobs(ncsegs)
          obs(ns) = obs(ncsegs)
          goodobs(ns) = goodobs(ncsegs)
          csegs(ncsegs) = ' '
          ncsegs = ncsegs - 1
        end if
      end do

      do ns = 1,ncsegs
        write(*,'(A13," ",$)') csegs(ns)
      end do
      write(*,*)
   
******** find list of all upstream segments for each station
      do ns = 1,ncsegs  
        call findupstream(
     I                    csegs(ns),
     I                    nrsegs,uniqindex,uniqid,dsid,     ! use nrsegs here INSTEAD OF ncsegs
     O                    upstream,nup)
        nallup(ns) = nup
        do nr = 1,nup
          allup(ns,nr) = upstream(nr)
        end do
        if (csegs(ns)(10:13).eq.'0003') then
          nallup(ns) = nup-1
        end if
       end do  
     
************ get basin size of each calibration site
      do ns = 1,ncsegs
        call getlseglist(
     I                   ns,nallup,allup,nallland,allland,acres,
     O                   clseglist,clarea,nclseglist)

        do nl = 1, nclseglist
          basinsize(ns) = basinsize(ns) + clarea(nl)
        end do
C        print*,csegs(ns),' ',basinsize(ns)
   
      end do          ! loop over all calibration stations
    
*********** remove calib stations that have more than 50% of watershed passing through a reservoir
      call remove_reservoir_stations(
     I                            calscen,nrsegs,rsegs,basinsize,
     I                            nrev,revnm,revsize,nrevallup,revallup,
     M                            ncsegs,csegs,nallup,allup)
   
********* get area percent for each lseg/rseg
      call get_area_percents( 
     I                       ncsegs,csegs,nlsegs,lsegs,
     I                       nallup,allup,nallland,allland,acres,
     O                       lsegpect)
      
************* check sum lsegpect = 1 for each calibration station
       do ns = 1,ncsegs
         test = 0.0
         do nl = 1,nlsegs
          test = test + lsegpect(ns,nl)
         end do
C        print*,csegs(ns),' ',test
        if (abs(test-1.0).gt.0.01) go to 992
       end do
    
************* NORMALIZATION; SUM=1 FOR EACH LSEG
       fnam = tree//'config/control/calib/PWATER/'//calscen(:lencalscen)
     .          //'/'//calscen(:lencalscen)//'_orphans.txt'
       open (dfile+1,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 991

        do nl = 1,nlsegs
         sumweight = 0.0
         maxpercent(nl) = 0.0
         do ns = 1,ncsegs
          sumweight = sumweight + lsegpect(ns,nl)
          maxpercent(nl) = max(maxpercent(nl),lsegpect(ns,nl))
         end do
 
         if (sumweight.gt.0.0001) then
          do ns = 1,ncsegs
           weight(nl,ns) = lsegpect(ns,nl)/sumweight
          end do
         else
          do ns = 1,ncsegs
           weight(nl,ns) = 0.0
          end do
         end if

************ SIGNAL OUT ORPHANS
        orphan = .true.
        do ns = 1,ncsegs
         if (weight(nl,ns).ne.0.0) orphan = .false. 
        end do      

        if (orphan) then
        write(dfile+1,123) lsegs(nl), ' is not associated 
     . with any calibration site in this basin' 
        end if

       end do           ! loop over all land segments

      close(dfile+1)
************ for a lseg, find the lowest, non-zero weight
       do nl = 1,nlsegs
        gtlimit = .false.

        do while (.not.gtlimit)
         minweight = 1.0
         do ns = 1,ncsegs                 
          if(weight(nl,ns).gt.0.000) then           ! Don't compare zero weights
           if (weight(nl,ns).lt. minweight) then
            minweight = weight(nl,ns)
            nmin = ns
           end if
          end if 
         end do

************ recalculte weight when lowest weight < limit (user defined)
         if(minweight.lt.limit) then
          weight(nl,nmin) = 0.0               ! if less than limit,remove the lowest weight
          sumweight = 0.0
          gtlimit = .false.

          do ns = 1,ncsegs                    ! recalculate the weights for this lseg
           sumweight = sumweight + weight(nl,ns)
          end do
          if (sumweight.gt.0.0001) then
           do ns = 1,ncsegs
            weight(nl,ns) = weight(nl,ns)/sumweight
           end do
          else
           do ns = 1,ncsegs
            weight(nl,ns) = 0.0
           end do
          end if
         else
          gtlimit = .true.
         end if
        end do                 ! loop over all weights lower than limit
       end do                  ! loop over all lsegs

************ WRITE OUTPUT INTO WEIGHT FILE   

      fnam = tree//'config/control/calib/'//module(:lenmod)//'/'//
     .       calscen(:lencalscen)//'/'//calscen(:lencalscen)//
     .       '_river_land.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

       write(dfile,1001) 'river,land,weight,maxLsegImportance***'
       do ns = 1,ncsegs
        do nl = 1,nlsegs
         if(weight(nl,ns) .ne. 0.0) 
     .      write(dfile,1002) csegs(ns),',',lsegs(nl),',',weight(nl,ns),
     .                        ',',maxpercent(nl)
        end do
       end do
       write(dfile,1003) 'end'

      close(dfile)

123   format(a6,a50)
1001  format(a40)
1002  format(a13,a1,a6,a1,f6.3,a1,f6.3)
1003  format(a3)

      stop

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'weights do not add to one '
      report(2) = csegs(ns)
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end

