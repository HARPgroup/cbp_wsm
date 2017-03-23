************************************************************************
** subroutine to echo applications to a text file                     **
**   if the file does not exist, create it.  if the file exists then  **
**   add or replace as appropriate                                    **
** verified code from uci  - gshenk 9/07                              **
************************************************************************
      subroutine writeapp_monthly(
     I                    lscen,lenlscen,lseg,clu,species,nspecies,
     I                    year1,year2,apptype,annapp,monapp)!GY
      implicit none
      include 'lug.inc'
      include 'acts.inc'
C      include '../lib/inc/lsegs.inc'
C      include '../lib/inc/land_use.inc'

      integer year1,year2,year,month
      double precision totapp(nspecies) ! total application per year
      double precision annapp(year1:year2,nspecies) !ind annual
      double precision monapp(year1:year2,12,nspecies) !GY
      character*(*) apptype
      character*4 cy1,cy2

**************** END DECLARATIONS **************************************

********* calc total application
      do sp = 1,nspecies
        totapp(sp) = 0.0
        do year = year1,year2
          totapp(sp) = totapp(sp) + annapp(year,sp)
        end do
        totapp(sp) = totapp(sp) / real(year2-year1+1)
      end do
         
********** get character year
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

********** write annual file
      fnam = outdir//'input/'//lscen(:lenlscen)//'/annual_'//apptype//
     .       '_'//lseg//'_'//clu//'.csv'

      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,*,err=951) 'year,',species(1),
     .                    (',',species(sp),sp=2,nspecies)
      do year = year1,year2
        write(11,1233,err=951) year,(annapp(year,sp),sp=1,nspecies)
      end do

      close(11)

********** write average annual file
      fnam = outdir//'input/'//lscen(:lenlscen)//'/'//apptype//
     .       '_'//lseg//'_'//clu//'_'//cy1//'_'//cy2//'.csv'

      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,*,err=951) species(1),(',',species(sp),sp=2,nspecies)
      write(11,1234,err=951) (totapp(sp),sp=1,nspecies)

      close(11)

********** write monthly file !GY
      fnam = outdir//'input/'//lscen(:lenlscen)//'/monthly_'//apptype//
     .       '_'//lseg//'_'//clu//'.csv'

      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,*,err=951) 'year,month,',species(1),
     .                    (',',species(sp),sp=2,nspecies)
      do year = year1,year2
        do month =1,12
          write(11,1235,err=951) year,month,(monapp(year,month,sp),sp=1,
     .     nspecies)
        end do
      end do

      close(11)

      return

1233  format(i4,5(',',e12.5))
1234  format(e12.5,4(',',e12.5))
1235  format(i4,',',i3,5(',',e12.5))

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end
