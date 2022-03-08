************************************************************************
**  main calling program for the calculation of statistics            **
************************************************************************
      subroutine single(obscen,rseg,year1,year2,rscen,lenrscen)
    
      implicit none
      include 'Rstats.inc'
      
      integer year1,year2             ! first and last year to average
      integer obflodays

******************* END DECLARATIONS ***********************************
      call lencl(rseg,lenrseg)

********************Calculate statistics for flow
      call doflows(
     I             obscen,rseg,year1,year2,rscen,lenrscen,
     O             obfl,bofl,qofl,FloObsExist,
     O             obflow,obflodays)

      if (.not.FloObsExist) return

******* Calculate stats for concs and instantaneous loads
************   in rchres_out_to_conc vs observed
      call doconcs(
     I             obscen,rseg,year1,year2,
     I             obfl,bofl,qofl,
     I             obflow,obflodays)

      return
  
************************ error reporting

990   report(1) = 'Problem with opening wdm for river segment '//rseg
      report(2) = '  Error =    '
      write(report(2)(11:13),'(i3)') err
      report(3) = wdmfnam
      go to 999

991   report(1) = 'Problem getting dates from open wdm '
      report(2) = wdmfnam
      report(3) = '  Error = '
      write(report(3)(11:13),'(i3)') err
      go to 999

992   report(1) = 'too many variables names WATR in catalog file'
      report(2) = 'increase maxWATR variable in'
      report(3) = './pp/postproc/src/river/*/main.f'
      go to 999

999   call stopreport(report)

      end

