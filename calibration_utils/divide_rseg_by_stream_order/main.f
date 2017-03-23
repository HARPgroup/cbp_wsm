************************************************************************
** reads in a .riv seglist and separates it into stream order seglists**
** Useful for domain decomposition                                    **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

      character*100 seglist,splitseglist    ! file names
      integer lens

      integer nr,nr2,no  ! indices

      integer maxorder    ! highest order of any stream
      integer maxuporder  ! highest order of directly upstream seg

      integer orderseg(maxrsegs)
      data orderseg /maxrsegs*-9/

      logical FoundUpstream  ! are there any upstream rivers
      logical UpstreamUndefined ! are there any that have no order yet

      logical alldone  ! end the loop determining order

      character*13 splitsegs(maxrsegs)
      integer nsplit

*********************** END DECLARATIONS ******************************
      read*,rscen,seglist
      call lencl(rscen,lenrscen)

*********** read in calibration stations from the supplied seglist
      call readRiverSeglist(
     I                      seglist,
     O                      rsegs,nrsegs)

************ loop until everything has been assigned an order
      maxorder = 0
      alldone = .false.
      do while (.not.alldone)

        alldone = .true.
        do nr = 1,nrsegs     ! loop over all segments look 4 upstream

          if (orderseg(nr).ne.-9) cycle  ! do not need to process

          alldone = .false.     ! set to cycle back

          FoundUpstream = .false.
          UpstreamUndefined = .false.
          maxuporder = 0
          do nr2 = 1,nrsegs
            if (rsegs(nr)(5:8).eq.rsegs(nr2)(10:13)) then ! nr2 upstream
              FoundUpstream = .true.
              if (orderseg(nr2).eq.-9) then
                UpstreamUndefined = .true.
                exit
              end if
              maxuporder = max(maxuporder,orderseg(nr2))
            end if
          end do
          if (.not.FoundUpstream) then
            orderseg(nr) = 1
          else
            if (.not.UpstreamUndefined) then
              orderseg(nr) = maxuporder + 1
              maxorder = max(maxorder,orderseg(nr))
            end if
          end if 

        end do  ! loop over rsegs
      end do  ! loop while not all done

************ got order set, open files and write
      do no = 1,maxorder

        call lencl(seglist,lens)  ! get filenema
        splitseglist = seglist(:lens)//'_order_'
        write(splitseglist(lens+8:lens+9),'(i2)') no
        if (no.lt.10) splitseglist(lens+8:lens+8) = '0'

        nsplit = 0  ! determine segs with this order
        do nr = 1,nrsegs
          if (orderseg(nr).eq.no) then
            nsplit = nsplit + 1
            splitsegs(nsplit) = rsegs(nr)
          end if
        end do

        call WriteRiverSeglist(         ! write results
     I                         splitseglist,
     O                         splitsegs,nsplit)

      end do 
      stop
999   call stopreport(report)

      end

