      subroutine onefile(fnam,version,scenario) 
      implicit none
      include 'sumsumstats.inc'

      character*300 longline,titleline

      real tempvalues(nparms)

      integer firsttime
      save firsttime

************ END DECLARATIONS ******************

*********** OPEN AND READ FILE ************************
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a300)',err=992) titleline
      
      if (firsttime.eq.0) then
        titleline(21:) = titleline(14:)
        titleline(:20) = 'scenario,vers,avetyp'
        write(*,'(a300)'),titleline
        firsttime = 1
      end if

      do i = 1,nparms
        nallvalues(i) = 0
      end do

      do 
        read(dfile,'(a300)',err=993,end=100) longline
        call shift(longline)
        read(longline,*,err=995) (tempvalues(np),np=1,nparms)
        do np = 1,nparms
          if (abs(tempvalues(np)+99.0).gt..01) then
            nallvalues(np) = nallvalues(np) + 1
            allvalues(np,nallvalues(np)) = tempvalues(np)
          end if
        end do
      end do

100   close(dfile)
************* DONE WITH READING FILE


*********** CALCULATE AND PRINT OUT AVES AND MEDIANS
      title(:8) = scenario(:8)
      title(9:9) = ','
      title(10:13) = version
      title(14:14) = ','
      call avesums(allvalues,nallvalues,title)

      return

991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*)' Error = ',err
      go to 999

992   report(1) = 'Problem reading first line of file:'
      report(2) = fnam
      report(3) = titleline
      go to 999

993   report(1) = 'Problem reading file: near line:'
      report(2) = fnam
      report(3) = longline
      go to 999

994   report(1) = 'Problem reading size of river from file: line:'
      report(2) = fnam
      report(3) = longline
      go to 999

995   write(report(1),*) 'Problem reading ',nparms,
     .                   ' values from line in file'
      report(2) = fnam
      report(3) = longline
      go to 999

999   call stopreport(report)

      end

