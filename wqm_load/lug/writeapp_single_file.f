************************************************************************
** subroutine to echo applications to a text file                     **
**   if the file does not exist, create it.  if the file exists then  **
**   add or replace as appropriate                                    **
** verified code from uci  - gshenk 9/07                              **
** THIS CODE IS OUT OF DATE = DOES NOT WRITE THE ANNUAL VALUE         **
************************************************************************
      subroutine writeapp(
     I                    lscen,lenlscen,lseg,clu,species,nspecies,
     I                    year1,year2,apptype,totapp)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
      include '../lib/inc/lsegs.inc'
      include '../lib/inc/land_use.inc'

      double precision totapp(nspecies) ! total application per year
      character*(*) apptype
      character*4 cy1,cy2
      integer year1,year2

      integer nlines,nl  ! number of lines existing in the file

      character*6 tlseg
      character*3 tlu

      logical exist  ! does the file exist

      character*100 header

      character*100 dataline(maxlsegs*nlu)

**************** END DECLARATIONS **************************************

      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      fnam = outdir//'input/'//lscen(:lenlscen)//'/'//apptype//
     .       '_'//cy1//'_'//cy2//'.csv'

      inquire (file=fnam,exist=exist)

      nlines = 0
      if (exist) then  ! read whole file into memory
        open(11,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(11,*) header ! skip header line

        do
          nlines = nlines + 1 
          read(11,'(a100)',end=111,err=992) dataline(nlines)

************ if match, then forget this line
          read(dataline(nlines),*),tlseg,tlu
          if (tlseg.eq.lseg.and.tlu.eq.clu) nlines = nlines - 1

        end do

111     nlines = nlines - 1
        close(11)

      end if

************ write the new file
      write(header,*) 'lseg,landuse',(',',species(sp),sp=1,nspecies)
      open(11,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(11,'(a)',err=951) header
      do nl = 1,nlines
        write(11,'(a)',err=951) dataline(nl)
      end do
      write(11,1234,err=951) lseg,clu,(totapp(sp),sp=1,nspecies)

      return

1234  format(a6,',',a3,5(',',e12.5))

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

992   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = dataline(nlines)
      go to 999

999   call stopreport(report)

      end
