************************************************************************
** Program to populate eos wdms with river input variables            **
**  wdms are stored in ./wdm/river/$scen/eos/$seg.wdm                 **
**    general strategy:                                               **
**      Run the program once for each river or bfl subseg             **
**      For each land segment in the river                            **
**         for each land use in the segment                           **
**           determine the time series of river input by:             **
**             extracting relevant dsns                               **
**             multiply by the etm binary files                       **
**           add to the appropriate river variable                    **
**      sum over all segments                                         **
**      store in river wdm                                            **
**      write appropriate output files (eos, wqm)                     **
************************************************************************
      call readdat(
     I             year1,month1,day1,year2,month2,day2,
     I             wdmfnam,maxDat2Rv,nRvar,Rname,Rdsn,
     I             nDvar,Ddsn,Dname,Dfac,
     O             pairq,pairH )
      implicit none

      include '../lib/inc/standard.inc'
      include '../lib/inc/locations.inc'
      include '../lib/inc/masslinks.inc'
      include '../lib/inc/wdm.inc'

********* input variables
      integer maxDat2Rv

************ input variables specifiers
      integer nDvar(maxRvar)           ! number of PSvars for each Rvar
      integer Ddsn(maxRvar,maxDat2Rv)  ! dsns of PSvars
      character*4 Dname(maxRvar,maxDat2Rv) ! name of PSvars
      real Dfac(maxRvar,maxDat2Rv)      ! conversion factor


      integer year1,month1,day1,year2,month2,day2
      integer ny              ! year of infomation to get

      real pairq(-5:366,year1:year2)
      real pairH(-5:366,year1:year2)

      real hvaltemp(ndaymax*24,maxRvar)   ! hourly values to river

      character*25 LStest             ! test the etm file

      integer jday,jdaytot,j1,j2   ! julian day determined from hours
      integer jdcorrect  ! correction between start time of etm
                         ! file and time series from wdm

      real fac1,fac2

      integer i, l, Rvar, Lvar, nv, nlconi, nb, nbi, hour  ! indices
      integer wdmlnd
       
      parameter (wdmlnd=dfile+1)

      integer sdate(ndate),edate(ndate)  ! start end dates wdm format

      logical dothislu                ! decide to use this land use

      integer ndaysinyear,ndays,nd
      external ndaysinyear

      integer julian
      external julian

      logical found

      integer temp1y,temp1m,temp1d  ! start time of etm file
      integer temp2y,temp2m,temp2d  ! stop time of etm file


************ END DECLARATION ******************************************


      print*,'x.f start'
********* GET START AND END DATE IN WDM FORMAT
      sdate(1) = year1
      sdate(2) = month1
      sdate(3) = day1
      sdate(4) = 0
      sdate(5) = 0
      sdate(6) = 0

      edate(1) = year2
      edate(2) = month2
      edate(3) = day2
      edate(4) = 24
      edate(5) = 0
      edate(6) = 0

      ndays = julian(sdate(1),sdate(2),sdate(3),
     .               edate(1),edate(2),edate(3))
      print*,'x.f end'
      end 
