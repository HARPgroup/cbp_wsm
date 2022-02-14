************************************************************************
** subroutine to get pltgen file for each output variable of interest **
**  this code was verified by spreadsheed 3/17/2005                   **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'

      integer:: file1          ! file number

      integer:: year,month,day,hour,zero
      integer:: year1            ! the first and last simulation year

      integer:: maxvals, nval, n, i, np
      parameter (maxvals = 900000)
      
      real:: value,annvalue,allval(maxvals)
      real:: aveann, aveload

      real::   pctle100,pctle99,pctle98,pctle97,pctle96,pctle95,pctle93,
     .         pctle90,pctle70,pctle50,pctle30,pctle10,pctle7,pctle5,
     .         pctle4,pctle3,pctle2,pctle1,pctle0

      integer npcnt
      parameter (npcnt = 43)
      real   pcntle(npcnt),taupctle(npcnt)
      data pcntle /100,99,98,97,96,95,94,93,92,91,90,88,86,84,82,80,75,
     .              70,65,60,55,50,45,40,35,30,25,20,18,16,14,12,10,9,
     .               8,7,6,5,4,3,2,1,0/

      integer:: ord(maxvals)

********** END DECLARATIONS  *******************************************

      read*,rseg,rscen
      call riverstop(rseg) ! stop the program if rseg is not simulated

      call lencl(rseg,lenrseg)
      call lencl(rscen,lenrscen)
      
      call findopen(file1)                            ! open pltgen
      fnam = outdir//'pltgen/river/'//rscen(:lenrscen)//'/'//
     .              rseg(:lenrseg)//'.tau'

      open(file1,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(file1,'(A26)') line                         

      if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! original pltgen from HSPF
        do i = 1,25
          read(file1,'(a)')line
        end do
      end if

      nval = 0
      do
        read(file1,*,err=994,end=111) year,month,day,hour,zero,value
        nval = nval + 1
        if (nval.gt.maxvals) go to 992
        allval(nval) = value
      end do

111   close(file1)

      call qsortr(ord,nval,allval)

      do np = 1,npcnt-1
        taupctle(np) = allval(ord(int(real(nval)*pcntle(np)/100.)))
      end do
      taupctle(npcnt) = allval(ord(1))

      print 11, 'pcnt',(pcntle(np),np=1,npcnt)
      print 12, rseg,(taupctle(np),np=1,npcnt)
11    format (A4,50(',',F10.3))
12    format (A13,50(',',e11.5))

      return

************* ERROR SPACE **********************************************
991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

992   report(1) = 'too many values in file'
      report(2) = 'increase maxvals variable in '
      report(3) = 
     .     './pp/src/calibration_utils/get_pltgen_percentiles/main.f'
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,zero,value
      go to 999

999   call stopreport(report)
      end

