************************************************************************
** Program to remove calibration stations that have more than 50% of  **
** their watershed passing through a reservior                        **
************************************************************************
      subroutine remove_reservoir_stations(
     I                            calscen,nrsegs,rsegs,basinsize,
     I                            nrev,revnm,revsize,nrevallup,revallup,
     M                            ncsegs,csegs,nallup,allup)

      include 'flow_weight.inc'
      integer ns,nr,n2,n3,nm,n    ! indices
      integer nsup,nu             ! index to upstream segment
      integer ncsegs2

      integer revallup(maxcsegs,maxrsegs) ! index of all upstream segments
      integer nrevallup(maxrsegs),matchid(maxrservoir)      

      integer nrev,nuprsev,matchrsev(maxrservoir)
      character*13 revnm(maxrservoir)

      real revsize(maxrservoir),areapect(maxrservoir)
      real totpect
      logical findremv(maxrsegs)
************** END DECLARATION *****************************************

      call lencl(calscen,lencalscen)

********** find if there are reservoirs in upstream for each calibration station 
      do ns = 1,ncsegs

        nuprsev = 0
        do nu = 1,nallup(ns)                  ! loop over all upstream segments
         nsup = allup(ns,nu) 
         do nr = 1, nrev    
          if (revnm(nr) .eq. rsegs(nsup)) then          ! found reservoir in upstream
           nuprsev = nuprsev + 1
           areapect(nuprsev) = revsize(nr)/basinsize(ns)
           matchid(nuprsev) = nr
           exit
          end if
         end do
        end do
   
*********** find if one reservoir is in upstream of another
       if (nuprsev .gt. 1) then                   !if more than one reservoir
         do n2 = 1, nuprsev 
           nm = matchid(n2)                     ! find the index of the reservoir
           do nu = 1,nrevallup(nm)-1               ! loop over all upstream segments
             nsup = revallup(nm,nu)
             do n3 = 1, nuprsev
              if (rsegs(nsup).eq.revnm(matchid(n3))) then
               areapect(n3) = 0.0
               exit
              end if
             end do
           end do
         end do
       end if

       totpect = 0.0
       do n = 1, nuprsev
         totpect =totpect + areapect(n)
       end do

       if (nuprsev .eq. 1) totpect = areapect(1)  ! if only one reservoir
       findremv(ns) = .false.
       if(totpect .gt. 0.5) findremv(ns) = .true. 
      end do          ! loop over all calibration stations

********** write reservoir stations into a file
       fnam = tree//'config/control/calib/PWATER/'//calscen(:lencalscen)
     .           //'/reservoir_stations'
       open(dfile,file=fnam,status='unknown',iostat=err)
       if (err.ne.0) go to 991

       do ns = 1,ncsegs
        if(findremv(ns)) write(dfile, '(a13)') csegs(ns)
       end do  
       close(dfile)

*********** remove segs from ncsegs that have revervoir stations
      ncsegs2 = ncsegs
      do ns = 1,ncsegs2     
        if (ns.gt.ncsegs) exit
        if (findremv(ns)) then
          do while (findremv(ncsegs))
            ncsegs = ncsegs - 1
          end do
          if (ncsegs.lt.ns) exit
          csegs(ns) = csegs(ncsegs)
          findremv(ns) = findremv(ncsegs)
          nallup(ns) = nallup(ncsegs)
          nup = nallup(ns)
          do nr = 1,nup
           allup(ns,nr) = allup(ncsegs,nr)
          end do
          csegs(ncsegs) = ' '
          ncsegs = ncsegs - 1
        end if
      end do
      
      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

