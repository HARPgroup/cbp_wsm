************************************************************************
**  reads from a file and writes a new wdm with similar name          **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      integer year,month,day,hour

      integer nh,nd

      character*100 datasource,version ! location of data
      integer lendatasource,lenversion

      character*200 dfnam

      double precision AccTempAve(12,31)
      integer TempAveCount(12,31)
C      data AccTempAve /(12*31)*0.0/ 
      data AccTempAve /372*0.0/
C      data TempAveCount / (12*31)*0 /
      data TempAveCount / 372*0 /

      real tempC,TempMin,TempMax

************* open datafile and new wdm
      read(*,*) lseg, datasource, version
      call lencl(datasource,lendatasource)
      call lencl(version,lenversion)
      print*,lseg

**************** TEMPERATURE FIRST
************* open datafile 
      dfnam = tree//'input/unformatted/precip_some_met/'//
     .       datasource(:lendatasource)//'/'//version(:lenversion)//
     .       '/OUTPUT/FIPSAB/'//lseg//'.TMP'
      open(dfile,file=dfnam,status='old',iostat=err)
      if (err.ne.0) go to 992

******************* read in the data to local variables
      do
        read(dfile,'(a100)',err=993,end=222)line
        call d2x(line,last)
        read(line,*,err=993,end=993) year,month,day,hour,TempC
        TempMin = TempC
        TempMax = TempC
        do nh = 2,24
          read(dfile,'(a100)',end=222)line
          call d2x(line,last)
          read(line,*) year,month,day,hour,TempC
          if (nh.ne.hour) go to 994
          TempMin = min(TempMin,TempC)
          TempMax = max(TempMax,TempC)
        end do
        AccTempAve(month,day) = AccTempAve(month,day)
     .           + ( (TempMax+TempMin) / 2.0 )
        TempAveCount(month,day) = TempAveCount(month,day) + 1
      end do
222   close (dfile)

********* write file
      dfnam=tree//'input/unformatted/AveTemp_for_uptake/'//lseg//'.csv'
      open(dfile,file=dfnam,status='unknown',iostat=err)
      if (err.ne.0) go to 992
      year = 1984  ! choose leap year
      month = 1 
      day = 1
      do nd = 1,366
        write(dfile,*)lseg(2:6),',',month,',',day,',',
     .          AccTempAve(month,day)/real(TempAveCount(month,day))
     .          * 9.0/5.0 + 32.0
        call tomorrow(year,month,day)
      end do

      close(dfile)

      return

********************************* ERROR SPACE **************************
992   print*,dfnam
      report(1) = 'could not open file'
      report(2) = dfnam
      report(3) = ' '
      go to 999

993   report(1) = 'Error reading file near line '
      report(2) = dfnam
      report(3) = line
      go to 999

994   report(1) = 'error in file near line, data not in groups of 24'
      report(2) = dfnam
      report(3) = line
      go to 999

999   call stopreport(report)


      end

