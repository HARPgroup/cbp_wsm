************************************************************************
** subroutine to modify HSPF pltgen files for readiblilty             **
**  and summary PLTGEN results                                        **
************************************************************************
      program main 

      implicit none
      include '../../../lib/inc/standard.inc'
      include '../../../lib/inc/locations.inc'
      include '../../../lib/inc/pltgen.inc'      
 
      character*100 dfnam
      character*3 clu                                 ! character land use
      integer np                                      ! indices

      integer year,month,day,hour,zero,oldyear,year1
      real value,annvalue,allvalues

********** END DECLARATIONS  *******************************************

      read*, lseg,clu,lscen
      
      call lencl(lseg,lenlseg)
      call lencl(lscen,lenlscen)
 
      call readcontrol_vartypes(clu,lscen,maxplts,pltname,numplts)

      fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//'/'//clu//
     .         '_'//lseg(:lenlseg)//'.sums'
      open(dfile,file=fnam,status='unknown',iostat=err) ! open new file to write into summary results
      if (err.ne.0) go to 991

      write(dfile,100,err=951)'scen',',','lu',',','seg',',','var',',',
     .                   'years',',','loads'

      do np = 1,numplts
       dfnam = outdir//'pltgen/land/'//lscen(:lenlscen)//'/'//clu//
     .         '_'//lseg(:lenlseg)//'.'//pltname(np)
        open(dfile+1,file=dfnam,status='old',iostat=err)
        if (err.ne.0) go to 991
         
        read(dfile+1,'(A26)') line
        if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! original pltgen file
          call plt2cal(dfile+1, dfnam)                      ! modify pltgen file for readiblity
          read(dfile+1,'(A26)') line                       ! read the first line of modified pltgen file
        end if
      
        read(dfile+1,'(A26)') line
        read(line,*) oldyear
        year1 = oldyear
        backspace (dfile+1)

        allvalues = 0.0
        do
          read(dfile+1,*,err=994,end=222) year,month,day,hour,zero,value
          if (year.ne.oldyear) then
            oldyear = year
          end if
         allvalues = allvalues + value
        end do                          ! end reading the pltgen file
222     close(dfile+1)

        write(dfile,200,err=951) lscen(:lenlscen),',',clu,',',
     .                     lseg(:lenlseg),',',pltname(np),',',
     .                     year1,'-',oldyear,',',
     .                     allvalues/real(oldyear-year1+1)

      end do                                  ! loop over all variable types

      close(dfile)

100   format(a8,a1,1x,a3,a1,1x,a6,a1,1x,a6,a1,1x,a9,a1,1x,a12)
200   format(a8,a1,1x,a3,a1,1x,a6,a1,1x,a6,a1,1x,i4,a1,i4,a1,1x,f12.5)

      stop

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = dfnam
      report(3) = ' '
      go to 999

999   call stopreport(report)
      end

