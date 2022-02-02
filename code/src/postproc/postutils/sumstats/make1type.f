C     STATS.F
C     READS .CSV FILES AND CREATES A COMMA-DELIMITED
C     FILE WITH ALL STATISTICS FOR A GIVEN SCENARIO.
C
      subroutine make1type(rscen,type,
     O                     segment,values,ns)
      implicit none
      include 'sumstats.inc'

      character*(*) type

************ END DECLARATIONS ******************8    

      call lencl(rscen,lenrscen)    ! open file for writing
C      fnam=outdir//'river/summary/'//rscen(:lenrscen)//'_'//type//
C     .     '_stats.csv'
C      open(91,file=fnam,status='unknown',iostat=err)
C      if (err.ne.0) go to 991

      do np = 1,nparms
**      Look into output dir opening each csv, 1 per param
**      to verify that all files are there else bail
**      stash a file pointer for each file in the fnum() array
**      Note: fnum starts at 11, which is the first parm,
**            which is "bias"
        fnam=outdir//'river/summary/'//rscen(:lenrscen)//'/'//
     .       type//'_'//parname(np)//'.csv' 
        print*,"DEBUG: opening file ",fnum(np)," ",fnam
        open(unit=fnum(np),file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991
      end do

      ns=0
      do 
***     This file "11" must be opened somewhere else? looking for 
***     list of river segments? must find file 11!
        read(11,'(a200)',end=222) longline
        ns = ns + 1
***     Check here
        do i=1,len(longline)
***       now we go through and extract each river seg, which are 
***       delimited by slashes '/', stash in segments
          if (longline(i:i).eq.'/') segment(ns)=longline(i+1:i+13)
        end do
        call shift(longline)
        read(longline,*) values(1,ns)
        do np = 2,nparms
          read(fnum(np),'(a200)',end=222) longline
          call shift(longline)
          read(longline,*) values(np,ns)
        end do
      end do

222   do np = 1,nparms
        close(fnum(np))
      end do

C     write(91,'(a13,7(a1,a15))')'SEGMENT',(',',parname(np),np=1,nparms)
C      do i=1,ns
C        write(91,111) segment(i),(values(np,i),np=1,nparms)
C      end do
C      close(91)

111   format(a13,7(', ',f14.7))

      return

991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*)' Error = ',err
      go to 999

999   call stopreport(report)

      end

