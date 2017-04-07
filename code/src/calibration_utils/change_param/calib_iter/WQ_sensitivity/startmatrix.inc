************************************************************************
** The programs in this directory combine to conduct a sensitivity    **
**  analysis of river parameters on the simulated-observed CFDs       **
** The CFDs are divided into 5 bins, with region 1 covering the       **
**   zeroth to 20th percentiles, etc.                                 **
**                                                                    **
** This include file dimensions the starting values matrices          **
************************************************************************

*********** starting value matrix
      real startmatrix(nconcmax,nbinmax,maxrsegs)

************* Kolmogorov-Smirnov stat
      real startksKstat(nconcmax,maxrsegs)

