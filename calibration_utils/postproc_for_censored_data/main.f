************************************************************************
**  This program is to develope sediment rating curve for each        **
**    river segment and put out relevent statistics                   **
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'    
 
      integer:: pltfil
      parameter (pltfil=40)                             ! file number for loadting
      
      character(100):: pfname
      integer:: year1,year2             ! first and last year to average

      logical:: founddouble

      character(13):: conseg
      integer:: lenconseg

******************* END DECLARATIONS ***********************************

      read*,rscen,rseg,year1,year2
                         ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)  !look for rseg in doubles.csv file   

      if (founddouble) then
        call lencl(conseg,lenconseg)
        pfname = outdir//'river/rating/'//rscen(:lenrscen)//'/'//
     .         conseg(:lenconseg)//'.curve'
        open (pltfil,file = pfname, status = 'unknown',iostat = err)
  
        call rating_SMcensor(rscen,conseg,year1,year2,pltfil)
        call rating_OBcensor(rscen,conseg,year1,year2,pltfil)
        close(pltfil)
      end if

      call riverstop(rseg)     ! stop if not a simulated river

      call lencl(rseg,lenrseg) 
      pfname = outdir//'river/rating/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'.curve'

      open (pltfil,file = pfname, status = 'unknown',iostat = err)
      call rating_SMcensor(rscen,rseg,year1,year2,pltfil)
      call rating_OBcensor(rscen,rseg,year1,year2,pltfil)
   
      close (pltfil)
      
      end

