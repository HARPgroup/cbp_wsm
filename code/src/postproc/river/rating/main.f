************************************************************************
**  This program is to develope sediment rating curve for each        **
**    river segment and put out relevent statistics                   **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'    
 
      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      integer pltfil
      parameter (pltfil=40)                             ! file number for loadting
      
      character*100 pfname
      integer year1,year2             ! first and last year to average

      logical founddouble

      character*13 conseg
      integer lenconseg

      character*1 resflag
      integer Nexits,idummy,timestep

******************* END DECLARATIONS ***********************************

      read*,rscen,rseg,year1,year2
                         ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)  !look for rseg in doubles.csv file   

      if (founddouble) then
        Nexits = 3
        call lencl(conseg,lenconseg)
        pfname = outdir//'river/rating/'//rscen(:lenrscen)//'/'//
     .         conseg(:lenconseg)//'.curve'

        open (pltfil,file = pfname, status = 'unknown',iostat = err)
         
        call smrating(rscen,conseg,year1,year2,wdmfil+1,Nexits,pltfil)
        call obrating(rscen,conseg,year1,year2,Nexits,pltfil)
        call wdflc1(wdmfil+1,err)    ! close wdm file
        close(pltfil)
      end if

      call riverstop(rseg)     ! stop if not a simulated river

      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)
      
      call lencl(rseg,lenrseg) 
      pfname = outdir//'river/rating/'//rscen(:lenrscen)//'/'//
     .         rseg(:lenrseg)//'.curve'

      open (pltfil,file = pfname, status = 'unknown',iostat = err)
      
      call smrating(rscen,rseg,year1,year2,wdmfil,Nexits,pltfil)
      call obrating(rscen,rseg,year1,year2,Nexits,pltfil)

      if (founddouble) then
        call wdflcl(wdmfil,err)    ! close wdm file
      else
        call wdflc1(wdmfil,err)    ! close wdm file
      end if
      
      close (pltfil)
      
      end

