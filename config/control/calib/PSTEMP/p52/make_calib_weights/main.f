************************************************************************
** Program to create generate weights for each land segments at each  **
**   calibration sites.  The sum of all calibration-site weights for  **
**   each segment must equal 1                                        **
** NOTE, nrsegs in this program means the number of segments that are **
**   in the calibration, not the total in the model as in most        **
**   programs.  other routines are slightly different as well.        **
************************************************************************
      include 'wtmp_weight.inc'

      character*25 calscen,basin 
      integer lencalscen,lenbasin,lenlsegs

      character*25 obscen
      integer lenobscen

      integer i,nl,nr1,nr2,ncsegs2,nr,ns,nt
      integer npoints,nonLOD  ! number of data points found
      logical foundall,anyobs(maxrsegs)

      integer year1,year2,ny
      integer nm,nd,nh,nmin   ! variables to read observed file
      real value,test
      character*1 qflag

      logical gtlimit,orphan
      real minweight, limit
************ END DECLARATIONS ******************************************
      print*,' determining the weights for use in calibration'

************ SPECIFICATION ENTRY SECTION
      read*,calscen,rscen,obscen,basin,year1,year2,limit

      call lencl(calscen,lencalscen)
      call lencl(basin,lenbasin)

      module = 'PSTEMP'
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

*********** populate uniqid, dsid, uniqindex
      do ns = 1,supermax
        uniqindex(ns) = 0
      end do
      do nr = 1,nrsegs
        read(rsegs(nr)(5:8),'(i4)') uniqid(nr)
        read(rsegs(nr)(10:13),'(i4)') dsid(nr)
        uniqindex(uniqid(nr)) = nr
      end do

********** read in upstream land segs and acres
      call getlandtoriver(rscen,nrsegs,rsegs,nlsegs,lsegs,
     O                    nallland,allland,acres)

******** loop over simulated segments  
********* keep those segments that have data
      print*,'finding observed values for '
      ncsegs = 0
******** loop over simulated segments
********* keep those segments that have data
      do nr = 1,nrsegs

********* loop over types, regular and double
******** do double first as it is upstream if both exist
        do nt = 1,2
          tseg = rsegs(nr)
          if (nt.eq.1) tseg(10:13) = '0003'
          npoints = 0
          nonLOD = 0
          fnam = tree//'input/calib/observed/'//obscen(:lenobscen)//
     .           '/WTMP/'//tseg//'.OWTMP'
          open (dfile,file=fnam,status='old',iostat=err)

          if (err.ne.0) cycle

          do
            read(dfile,*,err=992,end=333) ny,nm,nd,nh,nmin,value,qflag
            if (ny.ge.year1 .and. ny.le.year2) then
              npoints = npoints + 1
              if (qflag.ne.'<') nonLOD = nonLOD + 1
            end if
          end do
333       close(dfile)           ! close observed load file

          if (npoints.ge.24) then
            ncsegs = ncsegs + 1
            csegs(ncsegs) = tseg
            goodobs(ncsegs) = nonLOD
          end if

        end do  ! end loop over 2 types
      end do          ! end loop of simulated segments

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
      
********* get area for each lseg/rseg
       call get_area_percents( 
     I                        ncsegs,csegs,nlsegs,lsegs,
     I                        nallup,allup,nallland,allland,acres,
     O                        lsegpect)
       
************* test sum lsegpect = 1 for each river segment
       do ns = 1,ncsegs
         test = 0.0
         do nl = 1,nlsegs
          test = test + lsegpect(ns,nl)
         end do
        if (abs(test-1.0).gt.0.01) go to 992
       end do

************** determine weight for each lseg/rseg pair
       do nl = 1,nlsegs
        do ns = 1,ncsegs
        weight(nl,ns)= lsegpect(ns,nl)*log(real(goodobs(ns)))
       end do
      end do

************* NORMALIZATION; SUM=1 FOR EACH LSEG
        fnam = tree//'config/control/calib/PSTEMP/'//
     .         calscen(:lencalscen)//
     .          '/'//calscen(:lencalscen)//'_orphans.txt'
        open (dfile+1,file=fnam,status='unknown',iostat=err)
        if (err.ne.0) go to 991

       do nl = 1,nlsegs
        sumweight = 0.0
        do ns = 1,ncsegs
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

************ SIGNAL OUT ORPHANS
        orphan = .true.
        do ns = 1,ncsegs
         if (weight(nl,ns).ne.0.0) orphan = .false. 
        end do      

        if (orphan) then
         write(dfile+1,123) lsegs(nl), ' is not associated 
     .  with any calibration site in this basin' 
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

       write(dfile,1001) 'river,land,weight***'
       do ns = 1,ncsegs
        do nl = 1,nlsegs
         if(weight(nl,ns) .ne. 0.0) 
     .      write(dfile,1002) csegs(ns),',',lsegs(nl),',',weight(nl,ns)
        end do
       end do
       write(dfile,1003) 'end'

      close(dfile)

123   format(a6,a50)
1001  format(a20)
1002  format(a13,a1,a6,a1,f6.3)
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

