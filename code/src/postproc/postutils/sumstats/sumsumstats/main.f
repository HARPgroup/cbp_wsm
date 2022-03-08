 
      implicit none
      include 'sumsumstats.inc'

      character*145 longline,titleline

      real allvalues(0:9,nparms,maxsites)
      integer nallvalues(0:9)

************ END DECLARATIONS ******************

*********** OPEN AND READ FILE ************************
      read*,fnam
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      read(dfile,'(a145)',err=992) titleline
      titleline(:13) = 'segsiz,avetyp'
      write(*,'(a145)'),titleline

      do i = 0,9
        nallvalues(i) = 0
      end do

      do 
        read(dfile,'(a145)',err=993,end=100) longline
        read(longline(3:3),'(i1)',err=994) i
        nallvalues(i) = nallvalues(i) + 1
        call shift(longline)
        read(longline,*,err=995)
     .           (allvalues(i,np,nallvalues(i)),np = 1,nparms)
      end do

100   close(dfile)
************* DONE WITH READING FILE


*********** CALCULATE AND PRINT OUT AVES AND MEDIANS
      title(7:7) = ','

      title(:6) = 'All   '
      nvalues = 0
      do i = 0,9
        do nv = 1,nallvalues(i)
          do np = 1,nparms
            values(np,nvalues+nv) = allvalues(i,np,nv)
          end do
        end do
        nvalues = nvalues + nallvalues(i)
      end do
      call avesums(values,nvalues,title)

      title(:6) = 'Size  '
      do i = 0,9
        write(title(5:5),'(i1)') i
        do nv = 1,nallvalues(i)
          do np = 1,nparms
            values(np,nv) = allvalues(i,np,nv)
          end do
        end do
        call avesums(values,nallvalues(i),title)
      end do
      
      stop

112   format(a13,15(', ',f7.3))

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

