************************************************************************
**  parameters for use in wdm file manipulations                      **
**    necessary for communicating with wdm library functions          **
**    the date parameters are year, mon, day, hour, min,sec           **
**    the wdm file name must always be 64 characters                  **
**                                                                    **
**  ndate = number of constituents in a date variable                 **
**  dsn   = data set number                                           **
**  tcode = time code (1=sec,2=min,3=hour,4=day,5=mon,6=year)         **
**  dtran = transformation code 0=ave/same 1=sum/div 2=max 3=min      **
**  nvals = number of values to put or retrieve                       **
************************************************************************

      integer ndate
      parameter (ndate = 6)

      real tsfill
      parameter (tsfill = -9.9)

      integer dsn,tcode,dtran,nvals

      character*200 wdmfnam

      real hval(ndaymax*24)
      real dval(ndaymax)


