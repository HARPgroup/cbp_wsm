************************************************************************
******* Routine to populate nRvar, Rdsn, Rname, nLvar, Ldsn, Lname    **
********** using the files in the pp/lib/catalogs/ directory          **
************************************************************************
**  if recoding for more land uses is necessary, you should only have **
**     to change the length of the variable 'lin3', the format of any **
**     statement that reads 'lin3', and the test line going to 992    **
************************************************************************
      subroutine readspeclu(clu,ioscen,lenioscen,
     O                      dothislu,nspecies,species)

      implicit none
      include 'loadsim.inc'
  
      integer nl,nlus     ! indices

      logical comment,dothislu
      external comment

      character*3 clu,Tlu(nlu)                        ! change to lenf

************** END DECLARATION *****************************************
      fnam = catdir//'iovars/'//ioscen(:lenioscen)//'/load_simulation'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
      
      line = 'GO HOOS'
      do while (line(:3).ne.'end')
        read(dfile,'(a100)',err=992) line
        if (comment(line)) cycle

        if (line(:8).eq.'LAND USE') then
          nlus = 0
          read(dfile,'(a100)',err=992) line
          call d2x(line,last)
          do while (line(:12).ne.'END LAND USE')
            nlus = nlus +1
            Tlu(nlus) = line(:3)
            read(dfile,'(a100)',err=992) line
            call d2x(line,last)
          end do
        end if

        if (line(:7).eq.'SPECIES') then
          nspecies = 0
          read(dfile,'(a100)',err=992) line
          call d2x(line,last)
          do while (line(:11).ne.'END SPECIES')
            nspecies = nspecies +1
            read(line,*,end=993,err=993) species(nspecies)
            read(dfile,'(a100)',err=992) line
            call d2x(line,last)
          end do
        end if

      end do
      
      do nl = 1, nlus
        dothislu = .false.
        if(clu .eq. Tlu(nl)) then
          dothislu = .true.
          exit
        end if 
      end do 
  
      close (dfile)

      return

*********** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = 'Error reading line in file:  line:'
      report(2) = fnam
      report(3) = line(:64)
      go to 999

993   report(1) = 'Error reading value line:'
      report(2) = line
      report(3) = line(:64)
      go to 999

999   call stopreport(report)

      end 
