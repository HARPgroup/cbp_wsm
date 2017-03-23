************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This include file dimensions the matrices that aggregate the       **
**   results of the sensitivity test                                  **
** The senstivity of multiplicative parameters is the senstivity of   **
**   the logs                                                         **
************************************************************************

************** the following parameters apply to the range of values
      real MedSlope(nparmax,nconcmax,nbinmax,maxrsegs)
            !   median slope d(conc) / d(param) 
            !   slope uses the log10 of the parameter if multiplicative

      integer SlopeType(nparmax,nconcmax,nbinmax,maxrsegs)
            ! 1 = concave up
            ! 2 = concave down
            ! 3 = ends are least sensitive
            ! 4 = ends are most sensitive
            ! 0 = other

      logical Monotonic(nparmax,nconcmax,nbinmax,maxrsegs)
            ! is slope monotonic?

************ the following parameters apply over the range of values 
****************  and all segments
      real MedMedSlope(nparmax,nconcmax,nbinmax)
            ! median over segs of median over values

      integer ModalSlopeType(nparmax,nconcmax,nbinmax)
            ! most frequent slope type

      real FracMono(nparmax,nconcmax,nbinmax)
            ! fraction of rsegs that are monotonic

      real FracSameSign(nparmax,nconcmax,nbinmax)
            ! fraction of rsegs with same slope sign

      real SizeDependence(nparmax,nconcmax,nbinmax)
            ! d(med slope) / d(watershed size)

