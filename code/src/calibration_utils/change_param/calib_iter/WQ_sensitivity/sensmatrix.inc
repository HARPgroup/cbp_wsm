************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This include file dimensions the sensitivity values matrices       **
************************************************************************

************ sensitivity matrix
      real sensmatrix(nparmax,nval,nconcmax,nbinmax,maxrsegs)

************* Kolmogorov-Smirnov stat
      real sensksKstat(nparmax,nval,nconcmax,maxrsegs)


