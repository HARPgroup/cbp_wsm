      subroutine getweight(
     I                     maxrsegs,maxcsegs,nconcmax,
     I                     rsegs,nrsegs,csegs,ncsegs,nconcs,
     I                     allup,nallup,
     I                     obs,goodobs,rsegarea,
     O                     weight)
      implicit none
      integer maxrsegs,maxcsegs,nconcmax
      integer nc,ns,nr2,nr,ns2,nr3,nr4
      integer nrsegs,ncsegs,nconcs
      integer nallup(maxcsegs)
      integer allup(maxcsegs,maxrsegs)
      real rsegarea(maxrsegs),stationarea
      real PctOfStation(maxcsegs,maxrsegs)
      integer obs(maxcsegs,nconcmax) ! number of obs for cseg/conc pair
      integer goodobs(maxcsegs,nconcmax) ! number of obs above LOD
      real weight(maxrsegs,maxcsegs,nconcmax),sumweight
      character*13 rsegs(maxrsegs),csegs(maxcsegs)

      logical found
      real discount  ! multiplyer for intervening stations
      data discount /0.10/ ! produces a 10:1 importance ratio

************* METHOD
********** assign the number of good observations as the weight
********** then discount this weight for each calibration station
******** that you hit on the way upstream


      do nc = 1,nconcs  ! assign weight = # of good observations
        do ns = 1,ncsegs   ! at downstream end
          do nr2 = 1,nallup(ns)
            nr = allup(ns,nr2)
            weight(nr,ns,nc) = goodobs(ns,nc)
          end do
        end do
      end do

C*********** print out TOTN weights
C      nc = 7
C      print*,'TOTN weights'
C      print*,'river',(',',csegs(ns),ns=1,ncsegs)
C      do nr = 1,nrsegs
C        print*,rsegs(nr),(',',weight(nr,ns,nc),ns=1,ncsegs)
C      end do

********* now loop over calibration stations.  If other stations
********* are found upstream of that station, discount those segments
******* using this method, the discount will be squared if two 
******  stations intervene, etc.
      do nc = 1,nconcs
        do ns = 1,ncsegs
          do nr2 = 1,nallup(ns)
            nr = allup(ns,nr2)
            do ns2 = 1,ncsegs
              if (ns.eq.ns2) cycle ! don't discout for this station
              if (csegs(ns2)(4:7).eq.rsegs(nr)(4:7)) then ! found
                do nr3 = 1,nallup(ns2)
                  nr4 = allup(ns2,nr3)
                  weight(nr4,ns,nc) = weight(nr4,ns,nc) * discount
                end do
              end if
            end do
          end do
        end do
      end do

C*********** print out TOTN weights
C      nc = 7
C      print*,'TOTN weights'
C      print*,'river',(',',csegs(ns),ns=1,ncsegs)
C      do nr = 1,nrsegs
C        print*,rsegs(nr),(',',weight(nr,ns,nc),ns=1,ncsegs)
C      end do

************* NORMALIZATION; SUM=1 FOR EACH RSEG
      do nr = 1,nrsegs  
        do nc = 1,nconcs
          sumweight = 0.0
          do ns = 1,ncsegs
            sumweight = sumweight + weight(nr,ns,nc)
          end do
          if (sumweight.gt.0.0001) then
            do ns = 1,ncsegs
              weight(nr,ns,nc) = weight(nr,ns,nc) / sumweight
            end do
          else
            do ns = 1,ncsegs
              weight(nr,ns,nc) = 0.0
            end do
          end if
        end do
      end do

C*********** print out TOTN weights
C      nc = 7
C      print*,'TOTN weights'
C      print*,'river',(',',csegs(ns),ns=1,ncsegs)
C      do nr = 1,nrsegs
C        print*,rsegs(nr),(',',weight(nr,ns,nc),ns=1,ncsegs)
C      end do

      return
      end
