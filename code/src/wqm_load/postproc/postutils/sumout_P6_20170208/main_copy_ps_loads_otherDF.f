************************************************************************
** program to summarize delivery factor in the ./output/del/dfs/      **
**  and put them into ./sumout/ diretory                              **
************************************************************************
      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/rsegs.inc'

      character*20 psscen
      integer lenpsscen

      character*300 command

      logical comment
      external comment

      character*25 DFscen
      integer lenDFscen

      character*60 CombScen
      integer lenCombScen

************ END DECLARATIONS ******************************************

      read(*,*,err=997,end=998) rscen,DFscen
      call lencl(rscen,lenrscen) 
      call lencl(DFscen,lenDFscen)

      CombScen = rscen(:lenrscen)//'_DF_'//DFscen(:lenDFscen)
      lenCombscen = lenrscen + lenDFscen + 4

      print*,'copying ps loads for scenario ', rscen(:lenrscen)

      fnam = controldir//'river/'//rscen(:lenrscen)//'.con'
      open (dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=992)line
        if (.not.comment(line)) then
          if (line(:12).eq.'POINT SOURCE') then
            read(dfile,'(a)') psscen
            read(dfile,'(a100)',err=992)line
            if (line(:16).ne.'END POINT SOURCE') go to 993
          end if
        end if
      end do
     
      close (dfile)
      
*********** COPY PS LOADS INTO SUMOUT DIRECTORY
      call lencl(psscen,lenpsscen)
      command = 'cp -v '//ScenDatDir//'river/ps/'//psscen(:lenpsscen)//
     .          '/summary_ps_loads_'//psscen(:lenpsscen)//'.csv '//tree
     .          //'/sumout/aveann/'//combscen(:lenCombscen)//
     .          '/summary_ps_loads_'//psscen(:lenpsscen)//'_'//
     .          combscen(:lenCombscen)//'.csv' 
      if (command(300-4:300).ne.'     ') go to 994
      print*, command
      call system(command)

      stop

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file:  near line:'
      report(2) = fnam
      report(3) = line
      go to 999

993   report(1) = ' ERROR in control file '
      report(2) = fnam
      report(3) = ' only allowed one row in table POINT SOURCE'
      go to 999

994   report(1) = 'variable '//char(39)//'command'//char(39)//' in code'
      report(2) = 'postproc/postutils/sumout/main_copy_ps_loads.f'
      report(3) = 'is too small, increase the size and recompile'
      go to 999

997   report(1) = 'Error initializing point source copy program'
      report(2) = 'mismatch of data types between program expectation'
      report(3) = ' and calling script'
      go to 999

998   report(1) = 'Error initializing point source copy program'
      report(2) = 'not enough input data in calling script'
      report(3) = ' '
      go to 999

999   call stopreport(report)

      end


