C
      implicit none
      include 'sumstats.inc'

      integer ntypes,nt
      parameter (ntypes = 3)
      character*4 type(ntypes)
      data type /'flow','qflw','bflw'/

      integer numsites(ntypes)

      real allvalues(ntypes,nparms,maxsites)
      character*13 allsegs(ntypes,maxsites)

      integer ns2,ns3
      real Tbias,Qbias,Bbias,Wbias,Sbias 
      real Qstat,Bstat,Wstat,Sstat

*********** special variables for peak file
      character*5 ptype
      data ptype /'peaks'/
      integer numpsites
      character*13 allpsegs(maxsites)

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
            if (abs(values(np,ns)).gt.999) values(np,ns) = -99
            allvalues(nt,np,ns) = values(np,ns)
          end do
        end do
      end do

      call makeptype(rscen,ptype,
     O               segment,peakvalues,numpsites)
      do ns = 1,numpsites
        allpsegs(ns) = segment(ns)
      end do

******** LOOP OVER ALL SEGMENTS LOOK FOR EXISTANCE OF ALL DATA
***********  AND CACULATE INFILTRATION STATISTICS
      fnam=outdir//'river/summary/'//rscen(:lenrscen)//
     .     '_sum_stats.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      if (err.ne.0) go to 991

      write(dfile,'(a13,16(a1,a8))',err=951)
     .      'SEGMENT',',','Tbias',',','Wstat',
     .      ',','Sstat',',','Qstat',',','Bstat',',',
     .      'Total_E',',','Total_LE',',','Mon_eff',',',
     .      'QaveRI',',','BaveRI',',','Pbias',',','VPbias',
     .      ',','WBaveRI',',','SBaveRI',',','lo10bias'
      do ns = 1,numsites(1)
        do ns2 = 1,numsites(2)
          if (allsegs(1,ns).eq.allsegs(2,ns2)) then  ! match 1 and 2
            do ns3 = 1,numsites(3)
              if (allsegs(1,ns).eq.allsegs(3,ns3)) then ! match 2 and 3
                do np = 1,numpsites
                  if (allsegs(1,ns).eq.allpsegs(np)) then ! match 1 & p
                    Tbias = allvalues(1,1,ns)
                    Qbias = allvalues(2,1,ns2)
                    Bbias = allvalues(3,1,ns3)
                    Wbias = allvalues(1,6,ns)
                    Sbias = allvalues(1,7,ns)
                    Qstat = (Qbias+1.0) / (Tbias+1.0)
                    Bstat = (Bbias+1.0) / (Tbias+1.0)
                    Wstat = (Wbias+1.0) / (Tbias+1.0)
                    Sstat = (Sbias+1.0) / (Tbias+1.0)
              write(dfile,112,err=951) allsegs(1,ns),Tbias,Wstat,Sstat,
     .        Qstat,Bstat,
     .        allvalues(1,2,ns),allvalues(1,3,ns),allvalues(1,5,ns),
     .        allvalues(2,4,ns2),allvalues(3,4,ns3),
     .        peakvalues(1,np),peakvalues(2,np),
     .        allvalues(3,8,ns3),allvalues(3,9,ns3),allvalues(1,10,ns)
                  end if
                end do
              end if
            end do
          end if
        end do
      end do
      close(dfile)

      stop

112   format(a13,16(', ',f7.3))

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

