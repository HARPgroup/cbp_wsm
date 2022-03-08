************************************************************************
**  subroutine to find the acres of water for an lseg-seg pair        **
************************************************************************
      subroutine getwateracres(
     I                         rscen,lenrscen,rseg,lseg,
     I                         sdate,edate,
     O                         wateracres)

      implicit none

      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/land_use.inc'

      real lufac(ndaymax,nlu)
      real wateracres(ndaymax)
      integer i        ! index 
      integer etmfil
      integer temp1y,temp1m,temp1d  ! start time of etm file
      integer temp2y,temp2m,temp2d  ! stop time of etm file
      integer sdate(6),edate(6)

      integer jday,jdcorrect,julian
      external julian

      call lencl(rseg,lenrseg)
      call lencl(lseg,lenlseg)
      call findopen(etmfil)
      fnam = outdir//'etm/'//rscen(:lenrscen)//'/'//
     .       lseg(:lenlseg)//'to'//rseg(:lenrseg)//'.etm'
      open (etmfil,file=fnam,status='unknown',form='unformatted',
     .      iostat=err)
      if (err.ne.0) go to 991
      read(etmfil) temp1y,temp1m,temp1d,
     .             temp2y,temp2m,temp2d,
     .             lufac

      jday = julian(temp2y,temp2m,temp2d,edate(1),edate(2),edate(3))
      if (jday.gt.1) go to 992
      jday = julian(temp1y,temp1m,temp1d,sdate(1),sdate(2),sdate(3))
      if (jday.lt.1) go to 993
      jdcorrect = jday - 1
 
      jday=julian(sdate(1),sdate(2),sdate(3),edate(1),edate(2),edate(3))

      do i = 1,jday
        wateracres(i) = lufac(i+jdcorrect,lwat) 
      end do

      close (etmfil)

      return
******************************* ERROR SPACE ****************************
991   report(1) = 'Problem opening following file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'problem with etm file: end date too early'
      write(report(2),*) 'file end date: ',temp2y,' ',temp2m,' ',temp2d
      write(report(3),*) 'req end date: ',edate(1),' ',edate(2),' ',
     .                    edate(3)
      go to 999

993   report(1) = 'problem with etm file: start date too late'
      write(report(1),*)'file start date: ',temp1y,' ',temp1m,' ',temp1d
      write(report(3),*)'req start date: ',sdate(1),' ',sdate(2),' ',
     .                    sdate(3)
      go to 999

999   call stopreport(report)

      end
