************************************************************************
**  The purpose of this program is to summarize the FLOW from a       **
**    river segment and put out relevent statistics                   **

**  Adapted by Gopal Bhatt (gopal.bhatt@psu.edu)                      **
**  to generate Water Year / Seasonal Estimates of Sim Riv FLOW       **
************************************************************************
      implicit none
      include 'Rstats.inc'

      integer wdmfil
      parameter (wdmfil=12)         ! file number for wdm

      integer year1,year2             ! first and last year to average
      logical founddouble

      character*1 resflag
      integer Nexits,idummy,timestep

******* variables for investigation of peaks
      integer npeaks          ! peaks to investigate

      character*10 postfix
      integer lenpostfix

******************* END DECLARATIONS ***********************************

      read*,rscen,rseg,year1,year2,postfix
                         ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)
      call lencl(postfix, lenpostfix)

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)   
                  !look for rseg in doubles.csv file

      if (founddouble) then
        Nexits = 3
        call FlowStats(rscen,conseg,year1,year2,wdmfil+1,
     I              Nexits,postfix(:lenpostfix))
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

      call FlowStats(rscen,rseg,year1,year2,wdmfil,
     I            Nexits, postfix(:lenpostfix))

      if (founddouble) then
        call wdflcl(wdmfil,err)    ! close wdm file
      else
        call wdflcl(wdmfil,err)    ! close wdm file
      end if

      end

