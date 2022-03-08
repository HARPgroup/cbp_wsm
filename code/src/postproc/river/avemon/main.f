************************************************************************
**  The purpose of this program is to summarize the loads from a      **
**    river segment and put out relevent statistics                   **
************************************************************************
      implicit none
      include 'Ravemon.inc'

      integer wdmfil
      parameter (wdmfil=12)           ! file number for wdm

      integer year1,year2             ! first and last year to average
      logical founddouble

      character*1 resflag
      integer Nexits,idummy,timestep

******************* END DECLARATIONS ***********************************
      
      read(*,*) rscen,rseg,year1,year2     ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)
     
      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)  !look for rseg in doubles.csv file
      
      if (founddouble) then
        Nexits = 3
        call avemon(rscen,conseg,year1,year2,wdmfil+1,Nexits)
        call wdflcl(wdmfil+1,err)    ! close wdm file
      end if
      
      call riverstop(rseg)     ! stop if not a simulated river
 
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)

      call getrflags(
     I               paramscen,lenparamscen,rseg,
     O               Nexits,idummy,resflag,timestep)
 
      call avemon(rscen,rseg,year1,year2,wdmfil,Nexits)
  
      call wdflcl(wdmfil,err)    ! close wdm file

      end

