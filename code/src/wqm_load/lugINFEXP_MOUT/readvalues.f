************************************************************************
** subroutines for getting data into appropriate variables            **
************************************************************************
** V = variable cover in sediment                                     **
** P = plowing                                                        **
** A = atmospheric deposition                                         **
** F = fertilizer                                                     **
** M = manure                                                         **
** L = fixation for legumes                                           **
** T = variable total annual uptake target                            **
** U = variable montly fraction of uptake target                      **
************************************************************************

************************************************************************
** GET DETS FROM PLOWING -                                   **
** programming strategy -:  for each breakpoint (n in nB), open the **
**      file and read the header line to find the column order of     **
**      land uses. Look down the file to find the land segment, then  **
**      use the 'order' variable to correctly populate the variable   **
**      'dets'                                                      **
************************************************************************
      subroutine readDets(lseg,lenlseg,clu,nB,Pfile,dets)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
 
      character*300 dline
      character*10 ctemp
      character*6 Tlseg
      character*3 Tlu    ! land use

      logical found
      logical scompcase           ! string compare subroutine
      character*30 Pfile(maxTimeBreaks)

      integer n,nB,month
      real dets(maxTimeBreaks,12)

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks
         do month = 1, 12    ! initialize
            dets(n,month) = -999.0
          end do
      end do

      do n = 1,nB

        call trims(Pfile(n),last)

********** open DETS file
        fnam = ScenDatDir//'land/dets/dets_'//Pfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000,end=111) dline          ! read header line
       
********** read down to find the land seg
        found = .false.
        do            ! read down until all land segs found
          read(dfile,'(a300)',err=1001,end=111) dline
          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 992

          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then
            found = .true.
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu .eq. clu) then                    ! get land use
              do month = 1,12                       ! read across for each month
                call shift(dline)                        
                call fread(dline,dets(n,month))
              end do
            end if
          end if
        end do
      
111     close (dfile)

      end do

      return

*********** ERROR SPACE ***********************************************

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./src/lug/readvalues.f'
      go to 999

993   report(1) = 'did not find segment '//lseg//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

1000   report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001   report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end



************************************************************************
** GET THE LAND USE PERCENT COVER -                                   **
** programming strategy -:  for each breakpoint (n in nLCV), open the **
**      file and read the header line to find the column order of     **
**      land uses. Look down the file to find the land segment, then  **
**      use the 'order' variable to correctly populate the variable   **
**      'petcov'                                                      **
************************************************************************
      subroutine readlucv(lseg,lenlseg,clu,nLCV,LCVfile,pctcov)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*6 Tlseg
      character*3 Tlu
      character*30 LCVfile(maxTimeBreaks)

      integer n,nLCV,nm
      real pctcov(maxTimeBreaks,12)

      logical comment
      external comment

******** END DECLARATION **********************************************
      do n = 1, maxTimeBreaks  
        do nm = 1, 12               ! initialize percent cover data 
          pctcov(n,nm) = -9.0
        end do
      end do 
      
      do n = 1,nLCV
       
        call trims(LCVfile(n),last) 
            
********** open land percent cover file
        fnam = ScenDatDir//'land/cover/cover_'
     .          //LCVfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        do 
          read(dfile,'(a300)',err=992,end=111) dline 
          call d2x(dline,last)
          if (last.ge.300-2) go to 995
          if (comment(dline)) cycle
    
          read(dline,*,err=994,end=994) Tlseg,Tlu

          if (Tlseg.eq.lseg .or. Tlseg.eq.'defaul') then
            if (Tlu.eq.clu) then  ! match
              read(dline,*,err=994,end=994) 
     .                     Tlseg,Tlu,(pctcov(n,nm),nm=1,12)
              exit
            end if
          end if
     
        end do

111     close (dfile)

      end do   
     
      return

*********** ERROR SPACE ***********************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

992   report(1) = 'problem reading file: near line:'
      report(2) = fnam
      report(3) = dline
      go to 999

993   report(1) = 'did not find land seg / land use '//lseg//' '//clu
      report(2) = 'in file '
      report(3) = fnam
      go to 999

994   report(1) = 'Problem parsing line: in file:'
      report(2) = dline
      report(3) = fnam
      go to 999

995   report(1) = 'Problem reading file:'
      report(2) = fnam
      report(3) = ' line is too long, resize variable and recompile'
      go to 999

999   call stopreport(report)

      end

************************************************************************
*** GET THE LEGUME FIXATION -                                         **
** programming strategy -:  for each breakpoint (n in nB), open the   **
**      file and read the header line to find the column order of     **
**      land uses. Look down the file to find the land segment, then  **
**      use the 'order' variable to correctly populate the variable   **
**      'petcov'                                                      **
************************************************************************
      subroutine readlegfix(lseg,lenlseg,clu,nB,Lfile,legfix)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg
      character*3 Tlu
      character*30 Lfile(maxTimeBreaks)

      logical found
      logical scompcase           ! string compare subroutine

      integer n,nB,month
      real legfix(maxTimeBreaks,12)

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks  
        do month = 1, 12               ! initialize 
          legfix(n,month) = -9.0
        end do
      end do 
      
      do n = 1,nB
       
        call trims(Lfile(n),last) 
       
********** open land percent cover file
       
        fnam =ScenDatDir//'land/legume/legume_'//Lfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        found = .false.

        do while ( .not. found)                                     ! read down until all land segs found
          read(dfile,'(a300)',err=1001,end=111) dline
          call d2x(dline,last)

          if (dline(:3).eq.'end') go to 993
          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 995

          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu.eq.clu) then
              call shift(dline)
              found = .true.
              do month = 1,12     ! we have land seg, read across
                call shift(dline)    
                call fread(dline,legfix(n,month))
              end do
            end if
          end if
      
        end do

111     close (dfile)

      end do   
     
      return

*********** ERROR SPACE ***********************************************

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'reached the end of file: '
      report(2) = fnam
      report(3) = 'without finding seg '//lseg//'  and land use '//clu
      go to 999

995   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

1000   report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001   report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end



************************************************************************
** GET MANURE APPLICATION -                                           **
** programming strategy   -  for each breakpoint (n in nB), open the  **
**   file and read lines to find the land segment. If the segment and **
**   land use match read-ins, then populate the variable 'manure' for **
**   each species                                                     **
**  if no data found in this file, leave at 'missing' value           **
************************************************************************
      subroutine readmanure(lseg,lenlseg,clu,nB,Mfile,manure,
     I                      species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg  ! land segment
      character*3 Tlu    ! land use
      character*4 Tsp    ! nutrient species
      character*30 Mfile(maxTimeBreaks)

      logical found,foundsp(nspecies)
      logical scompcase           ! string compare subroutine

      integer n,nB,month
      real manure(maxTimeBreaks,nspecies,12)

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks  
        do sp = 1,nspecies
          do month = 1, 12    ! initialize 
            manure(n,sp,month) = -999.0
          end do
        end do
      end do 
      
      do n = 1,nB
       
        call trims(Mfile(n),last) 
       
********** open land percent cover file
        fnam =ScenDatDir//'land/manure/manure_'//Mfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        found = .false.
        do sp = 1,nspecies
          foundsp(sp) = .false.
        end do
        
        do  
          read(dfile,'(a300)',err=1001,end=992) dline
          call d2x(dline,last)
         
          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993

          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then            ! get land segment
            found =.true.
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu.eq.clu) then            ! get land use
              call shift(dline)
              call findcomma(dline,last)
              Tsp = dline(:last-1)           ! find nutrient species
              call lowercase(Tsp)
              do sp = 1,nspecies+1
                if (sp.eq.nspecies+1) go to 994
                if (Tsp.eq.species(sp)) exit
              end do
              foundsp(sp) = .true.
              do month = 1,12     ! we have land seg, read across
                call shift(dline)    
                call fread(dline,manure(n,sp,month))
              end do
            end if
          end if
      
        end do

992     close (dfile)

      end do   
     
      return

*********** ERROR SPACE ***********************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

994   report(1) = ' do not recognize nutrient species '//Tsp
      report(2) = ' found in file:  must match ./pp/src/lug/acts.inc'
      report(3) = fnam
      go to 999

995   report(1) = 'did not find segment '//lseg//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

1000  report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end


************************************************************************
** GET THE FERTILIZER APPLICATION -                                   **
** programming strategy   -  for each breakpoint (n in nB), open the  **
**   file and read lines to find the land segment. If the segment and **
**   land use match read-ins, then populate the variable 'fert' for   **
**   each species                                                     **
************************************************************************
      subroutine readfert(lseg,lenlseg,clu,nB,Ffile,fert,
     I                      species,nspecies)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg  ! land segment
      character*3 Tlu    ! land use
      character*4 Tsp    ! nutrient species
      character*30 Ffile(maxTimeBreaks)

      logical found,foundsp(nspecies)
      logical scompcase           ! string compare subroutine

      integer n,nB,month
      real fert(maxTimeBreaks,nspecies,12)

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks  
        do sp = 1,nspecies
          do month = 1, 12    ! initialize 
            fert(n,sp,month) = -999.0
          end do
        end do
      end do 
      
      do n = 1,nB
       
        call trims(Ffile(n),last) 
       
********** open land percent cover file
        fnam = ScenDatDir//'land/fertilizer/fertilizer_'
     .         //Ffile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        found =.false.
        do sp = 1,nspecies
          foundsp(sp) = .false.
        end do

        do 
          read(dfile,'(a300)',err=1001,end=992) dline
          call d2x(dline,last)

          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993
      
          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then            ! get land segment
            found =.true. 
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu.eq.clu) then            ! get land use
              call shift(dline)
              call findcomma(dline,last)
              Tsp = dline(:last-1)           ! find nutrient species
              call lowercase(Tsp)
              do sp = 1,nspecies+1
                if (sp.eq.nspecies+1) go to 994
                if (Tsp.eq.species(sp)) exit
              end do
              foundsp(sp) = .true.
              do month = 1,12     ! we have land seg, read across
                call shift(dline)    
                call fread(dline,fert(n,sp,month))
              end do
            end if
          end if
      
        end do
       
992     close (dfile)

      end do   
     
      return

************** ERROR SPACE *****************************************
991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

994   report(1) = ' do not recognize nutrient species '//Tsp
      report(2) = ' found in file:  must match ./pp/src/lug/acts.inc'
      report(3) = fnam
      go to 999

995   report(1) = 'did not find segment '//lseg//' in file'
      report(2) = fnam
      report(3) = dline
      go to 999

1000  report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end


************************************************************************
** GET ANNUAL MAX PLANT UPTAKE  -                                     **
** programming strategy   -  for each breakpoint (n in nB), open the  **
**   file and read lines to find the land segment. If the segment and **
**   land use match read-ins, then populate the variable 'nupt' for   **
**   each species                                                     **
************************************************************************
      subroutine readuptake(lseg,lenlseg,clu,nB,Tfile,nupt)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg  ! land segment
      character*3 Tlu    ! land use
      character*4 Tsp    ! nutrient species
      character*30 Tfile(maxTimeBreaks)

      logical found(2)  ! N is 1, P is 2
      logical scompcase           ! string compare subroutine

      integer n,nB,month
      real nupt(maxTimeBreaks,2)  ! N is 1, P is 2

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks  
        nupt(n,1) = -999.0
        nupt(n,2) = -999.0
      end do 
      
      do n = 1,nB
       
        call trims(Tfile(n),last) 
       
********** open land percent cover file
       
        fnam = ScenDatDir//'land/annual_uptake/max_uptake_'//
     .         Tfile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        found(1) = .false.
        found(2) = .false.

        do while ( .not. (found(1).and.found(2))) ! uptake cons
          read(dfile,'(a300)',err=1001,end=992) dline
          call d2x(dline,last)

          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993

          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then            ! get land segment
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu.eq.clu) then            ! get land use
              call shift(dline)
              call findcomma(dline,last)
              Tsp = dline(:last-1)           ! find nutrient species
              call lowercase(tsp)
              if (Tsp.eq.'nitr') then
                found(1) = .true.
                call shift(dline)
                read(dline,*,err=994,end=994) nupt(n,1)
              else if (Tsp.eq.'phos') then
                found(2) = .true.
                call shift(dline)
                read(dline,*,err=994,end=994) nupt(n,2)
              else 
                go to 995
              end if
            end if
          end if
      
        end do

992     close (dfile)

      end do   
     
      return

*********** ERROR SPACE ***********************************************

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

994   report(1) = 'Error reading data file'
      report(2) = fnam
      report(3) = ' For land segment '//lseg//' land use '//clu
      go to 999

995   report(1) = ' do not recognize nutrient species '//Tsp
      report(2) = ' found in file:  must match ./pp/src/lug/acts.inc'
      report(3) = fnam
      go to 999

1000  report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end


************************************************************************
** GET MONTHLY UPTAKE FRACTION -                                      **
** programming strategy   -  for each breakpoint (n in nB), open the  **
**   file and read lines to find the land segment. If the segment and **
**   land use match read-ins, then populate the variable 'pctnup' for **
**   each species                                                     **
************************************************************************
      subroutine readpctnup(lseg,lenlseg,clu,nB,Ufile,pctnup)
      implicit none
      include 'lug.inc'
      include 'acts.inc'
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg  ! land segment
      character*3 Tlu    ! land use
      character*4 Tsp    ! nutrient species
      character*30 Ufile(maxTimeBreaks)

      logical found(2)  ! N is 1, P is 2
      logical scompcase           ! string compare subroutine

      integer n,nB,month
      real pctnup(maxTimeBreaks,2,12)  ! N is 1, P is 2

******** END DECLARATION **********************************************

      do n = 1, maxTimeBreaks  
        do month = 1,12
          pctnup(n,1,month) = -999.0
          pctnup(n,2,month) = -999.0
        end do
      end do 
      
      do n = 1,nB
       
        call trims(Ufile(n),last) 
       
********** open land percent cover file
       
        fnam = ScenDatDir//'land/monthly_fraction_uptake/uptakecurve_'//
     .         Ufile(n)(:last)//'.csv'
        open (dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read(dfile,'(a300)',err=1000)dline          ! read header line

********** read down to find the land seg
        found(1) = .false.
        found(2) = .false.

        do while ( .not. (found(1).and.found(2))) ! monthly cons
          read(dfile,'(a300)',err=1001,end=992) dline
          call d2x(dline,last)

          if (dline(len(dline)-3:len(dline)).ne.'    ') go to 993

          call findcomma(dline,last)
          Tlseg = dline(:last-1)
          if (Tlseg .eq. lseg) then            ! get land segment
            call shift(dline)
            call findcomma(dline,last)
            Tlu = dline(:last-1)
            if (Tlu.eq.clu) then            ! get land use
              call shift(dline)
              call findcomma(dline,last)
              Tsp = dline(:last-1)           ! find nutrient species
              call lowercase(Tsp)
              if (Tsp.eq.'nitr') then
                found(1) = .true.
                do month = 1,12
                  call shift(dline)
                  call fread(dline,pctnup(n,1,month))
                end do
              else if (Tsp.eq.'phos') then
                found(2) = .true.
                do month = 1,12
                  call shift(dline)
                  call fread(dline,pctnup(n,2,month))
                end do
              else 
                go to 994
              end if
            end if
          end if
      
        end do

992     close (dfile)

      end do   
     
      return

*********** ERROR SPACE ***********************************************

991   report(1) = 'problem opening file'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

993   report(1) = 'data line too long, modify '
      report(2) = fnam
      report(3) = ' or adjust dline in ./pp/src/lug/readvalues.f'
      go to 999

994   report(1) = ' do not recognize nutrient species '//Tsp
      report(2) = ' found in file:  must match ./pp/src/lug/acts.inc'
      report(3) = fnam
      go to 999

1000  report(1) = 'Could not read first line in file '
      report(2) = fnam
      report(3) = ' '
      go to 999

1001  report(1) = 'Could not read file: near line: '
      report(2) = fnam
      report(3) = dline
      go to 999

999   call stopreport(report)

      end
