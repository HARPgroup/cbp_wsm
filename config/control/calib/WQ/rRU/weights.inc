************************************************************************
** include file for basingen fortran version of basingen program      **
************************************************************************
*********** river variables
      integer maxcsegs  ! maximum calibration stations
      parameter (maxcsegs = 300)

      integer ncsegs,ns
      character*13 csegs(maxcsegs)  ! calibration segments
                               ! can include 0003 segs

      integer nconcmax,nconcs,nc     ! number of concentrations
      parameter (nconcmax = 20)
      character*4 concname(nconcmax) ! name of each conc (e.g. 'totn')

*********** output variables
      real weight(maxrsegs,maxcsegs,nconcmax),sumweight,stationarea
      

