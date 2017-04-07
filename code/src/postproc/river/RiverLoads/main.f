************************************************************************
**  The purpose of this program is to summarize the loads from a      **
**    river segment and put out relevent statistics                   **
************************************************************************
      implicit none
      include 'RiverLoads.inc'

      integer wdmfil
      parameter (wdmfil=12)           ! file number for wdm

      logical founddouble
      character*1 resflag
      integer Nexits,idummy,timestep

******************* END DECLARATIONS ***********************************
      print*,''
      print*,'Input (1) Scenario Name (2) River Segment '//
     . '(3) Time Interval [D,M,Y] (4) Input-Output Flags [I,O]'
      print*,''
      read(*,*) rscen,rseg,TimeInterval,OutInFlag
                         ! variables supplied by pp/run/run_postproc.com
      call lencl(rscen,lenrscen)

      wdmfnam = dummyWDMname
      call wdbopnlong(wdmfil+5,wdmfnam,0,err)

      call getconseg(rseg,rscen,lenrscen,
     O               conseg,founddouble)   
                  !look for rseg in doubles.csv file

      if (founddouble) then
        Nexits = 3
        call RiverLoads(rscen,conseg,wdmfil+1,Nexits,OutInFlag,
     .               TimeInterval)
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

      call RiverLoads(rscen,rseg,wdmfil,Nexits,OutInFlag,TimeInterval)

      call wdflcl(wdmfil,err)    ! close wdm file

      end

