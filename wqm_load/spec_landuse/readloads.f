************************************************************************
** GET MANURE APPLICATION -                                           **
** programming strategy   -  for each breakpoint (n in nB), open the  **
**   file and read lines to find the land segment. If the segment and **
**   land use match read-ins, then populate the variable 'manure' for **
**   each species                                                     **
**  if no data found in this file, leave at 'missing' value           **
************************************************************************
      subroutine readloads(lseg,lenlseg,clu,nB,Mfile,
     I                     Byear,year1,year2,species,nspecies,
     O                     manure)
      implicit none
      include 'loadsim.inc'     
    
      character*300 dline
      character*13 ctemp
      character*6 Tlseg  ! land segment
      character*3 clu,Tlu    ! land use
      character*4 Tsp    ! nutrient species

      logical found,foundsp(nspecies)
      logical scompcase           ! string compare subroutine

      real manure(maxTimeBreaks,nspecies,12),man

      integer year1,year2,oldyear,indx1,indx2
      integer year,month,hour
      integer ny,nsp,i,nB,n

      logical lastindx

******** END DECLARATION **********************************************
      do n = 1, maxTimeBreaks  
        do nsp = 1, nspecies
          do month = 1, 12    ! initialize 
            manure(n,nsp,month) = -999.0
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

