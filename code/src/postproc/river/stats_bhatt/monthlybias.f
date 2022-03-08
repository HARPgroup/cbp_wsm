************************************************************************
**  program to calculate statistics for 'windowed' concentration data **
**   and output the windowed data for plotting purposes in cal5       **
**  'Windowing' means two things:                                     **
**     Any simulated or observed value below LOD is set to LOD        **
**     The simulated value is set to the closest value to the         **
**        observed that the simulation obtained within the specified  **
**        time window                                                 **
************************************************************************
      subroutine monthlybias(rscen,rseg,year1,year2,loadnm,
     I                       obconc,obyear,obmonth,obday,obhour,ndays,
     I                       simconc,bofl,bsfl,qofl,qsfl)
      implicit none
      include 'Rstats.inc'

      character*100 pfnam,obldfnam,winfnam
      character*4 loadnm
      character*4 cy1,cy2

      real simconc(ndaymax) ! daily concentration

      integer i,ifl
      integer ny,nm,nd,nh,nh2,nday,ndays   ! indices
      integer year1,year2                   ! first and last year to average
       
******** flow and load variables
      real obconc(ndaymax)

********* monthly bias variablesa
      real basebias(12),quickbias(12)
      integer baseobs(12),quickobs(12)

      logical findflow
******************* END DECLARATIONS ***********************************
      do nm = 1,12
        basebias(nm) = 0.0
        quickbias(nm) = 0.0
        baseobs(nm) = 0
        quickobs(nm) = 0
      end do

      findflow = .false.  
      do nday = 1,ndays                  ! loop over to find if there is observed flow
       if (qofl(obyear(nday),obmonth(nday),obday(nday)).ne. 0.0 .or.
     .      bofl(obyear(nday),obmonth(nday),obday(nday)) .ne.0.0) then
         findflow = .true. 
       end if 
      end do

      do nday = 1,ndays
        nm = obmonth(nday)
        if (qsfl(obyear(nday),obmonth(nday),obday(nday)).gt.
     .      bsfl(obyear(nday),obmonth(nday),obday(nday))) then
          quickbias(nm) = quickbias(nm) + simconc(nday)-obconc(nday)
          quickobs(nm) = quickobs(nm) + 1
        else
          basebias(nm) = basebias(nm) + simconc(nday)-obconc(nday)
          baseobs(nm) = baseobs(nm) + 1
        end if
      end do

      do nm = 1,12
       if (quickobs(nm).eq.0) then
         quickbias(nm) = -99.0
       else
         quickbias(nm) = quickbias(nm)/real(quickobs(nm))
       end if
       if (baseobs(nm).eq.0) then
         basebias(nm) = -99.0
       else
         basebias(nm) = basebias(nm)/real(baseobs(nm))
       end if
      end do 

      if (.not.findflow) then             ! if flow data non-exist
        do nm = 1,12
          quickbias(nm) = basebias(nm)     
        end do
      end if
        
****************** write out monthly biases
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      call lencl(rscen,lenrscen)
      call lencl(rseg,lenrseg)
      pfnam = outdir//'river/stats/'//rscen(:lenrscen)//'/'
     .      //rseg(:lenrseg)//'_'//cy1//'_'//cy2//'_monthbias.'//loadnm
      open (pltfil,file = pfnam, status = 'unknown',iostat = err)
      if (err.ne.0) goto 994

      write(pltfil,1234,err=951) 'base ',(basebias(nm),nm=1,12)
      write(pltfil,1234,err=951) 'quick ',(quickbias(nm),nm=1,12)
      close(pltfil)

      return

1234  format(a6,12(',',f10.3))
  
************************ ERROR REPORTING *******************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

994   report(1) = 'Problem opening output load files for segment '//rseg
      report(2) = pfnam
      report(3) = '  Error =    '
      write(report(3)(11:13),'(i3)') err
      go to 999

999   call stopreport(report)

      end

