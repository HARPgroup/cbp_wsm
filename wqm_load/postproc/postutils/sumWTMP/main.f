C
      implicit none
      include 'sumstats.inc'

      integer ntypes,nt
      parameter (ntypes = 1)
      character*4 type(ntypes)
      data type /'WTMP'/

      integer numsites(ntypes)

      real allvalues(ntypes,nparms,maxsites,12)
      character*13 allsegs(ntypes,maxsites)

      real base(12),quik(12)

************ END DECLARATIONS ******************
      read*,rscen
      call lencl(rscen,lenrscen)

********* LOOP OVER DATA TYPES, MAKE FILES AND POPULATE ALL DATA
      do nt = 1,ntypes
        call make1type(rscen,type(nt),
     O                 segment,values,numsites(nt))
        do ns = 1,numsites(nt)
          allsegs(nt,ns) = segment(ns)
          do np = 1,nparms
            do nm = 1,12
              if (abs(values(np,ns,nm)).gt.999) values(np,ns,nm) = -999
              allvalues(nt,np,ns,nm) = values(np,ns,nm)
            end do
          end do
        end do
      end do

******** LOOP OVER ALL SEGMENTS LOOK FOR EXISTANCE OF ALL DATA
***********  AND CACULATE INFILTRATION STATISTICS
      fnam=outdir//'river/summary/'//rscen(:lenrscen)//
     .     '_wtmp_stats.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,'(a13,24(a1,a8))',err=951)'SEGMENT',
     .      ',','quick1',',','quick2',',','quick3',',','quick4',
     .      ',','quick5',',','quick6',',','quick7',',','quick8',
     .      ',','quick9',',','quick10',',','quick11',',','quick12',
     .      ',','base1',',','base2',',','base3',',','base4',
     .      ',','base5',',','base6',',','base7',',','base8',
     .      ',','base9',',','base10',',','base11',',','base12'
      do ns = 1,numsites(1)
        write(dfile,112,err=951) allsegs(1,ns),
     .                   ((allvalues(1,np,ns,nm),nm=1,12),np=1,2)
      end do
      close(dfile)

      stop

112   format(a13,24(', ',f7.3))

*********** ERROR SPACE **************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      write(report(3),*)' Error = ',err
      go to 999

999   call stopreport(report)

      end

