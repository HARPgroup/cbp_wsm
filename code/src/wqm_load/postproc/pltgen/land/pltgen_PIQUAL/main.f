************************************************************************
** subroutine to summarize PLTGEN results                             **
************************************************************************
 
      implicit none
      include '../../../../lib/inc/standard.inc'
      include '../../../../lib/inc/locations.inc'
      include '../../../../lib/inc/lsegs.inc'

      integer:: file1, file2                             ! file number

      character(16):: date(ndaymax), pltvalue(ndaymax)   ! reading variables

      character(3):: clu                                 ! character land use

      integer:: maxtypes,ntypes,nt                       ! number of variable types
      parameter (maxtypes=40)

      character(4):: vartype(maxtypes)        ! variable type

      integer:: n,ns                                  ! indices

      double precision value, accvalue(maxtypes)
      integer year1,year2

      integer:: year,month,day,hour,zero

      character*100 basin
      integer lenbasin

      real denom

      logical found

********** END DECLARATIONS  *******************************************

      read(*,*) lscen,basin,clu,year1,year2
      print*,'summarizing pltgens for ',lscen,' ',basin,' ',clu

      call readLandSeglist(            ! get the list of segments
     I                     basin,
     O                     lsegs,nlsegs)

      call lencl(lscen,lenlscen)
 
      call readcontrol_vartypes(        ! get the list of pltgens
     I                          clu,lscen,maxtypes,
     O                          vartype,ntypes)

      denom = real(year2-year1+1)


************** open new file to write into summary results
      fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//
     .       '/aveann_summary_'//clu//'.csv'
      call findopen(file2)
      open(file2,file=fnam,status='unknown',iostat=err) 
      if (err.ne.0) go to 991

      write(file2,100,err=951) 'lseg',(',',vartype(n),n=1,ntypes)

      do ns = 1,nlsegs

        call lencl(lsegs(ns),lenlseg)
 
        do nt = 1,ntypes
      
          call findopen(file1)           ! open pltgen  
          fnam = outdir//'pltgen/land/'//lscen(:lenlscen)//'/'//clu
     .           //'_'//lsegs(ns)(:lenlseg)//'.'//vartype(nt)
          open(file1,file=fnam,status='old',iostat=err)
          if (err.ne.0) go to 991
         
          read(file1,'(A26)') line
        
          if (line(6:26).eq.'HSPF FILE FOR DRIVING') then  ! orig pltgen
            do n = 2,26                   ! get rid of headers
              read(file1,'(A26)') line
            end do
          end if
      
          found = .false.
          accvalue(nt) = 0.0
          do
            read(file1,*,err=994,end=222) year,month,day,hour,zero,value
            if (year.gt.year2) exit
            if (year.eq.year2) then
              if (month.eq.12) then
                if (day.eq.31) then
                  found = .true.
                end if
              end if
            end if
            if (year.ge.year1) accvalue(nt) = accvalue(nt) + value
          end do                          ! end reading the pltgen file

222       close(file1)
          if (.not.found) go to 993

        end do

        do n = 1,ntypes
          accvalue(n) = accvalue(n) / denom
        end do
        write(file2,200,err=951)lsegs(ns)(:lenlseg),
     .        (',',accvalue(n),n=1,ntypes)

      end do                                  ! loop over all variable types

      close(file2)


      return
100   format(a6,30(a1,a14))
200   format(a6,30(a1,e14.7))

************* ERROR SPACE **********************************************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file'
      report(2) = fnam
      report(3) = ' '
      go to 999

993   report(1) = 'simulation period not long enough'
      write(report(2),*) 'simulation stops in ',year
      write(report(3),*) 'script calls for ',year2
      go to 999

994   report(1) = 'Problem reading file: near line'
      report(2) = fnam
      write(report(3),*) year,month,day,hour,zero,value
      go to 999

999   call stopreport(report)
      end

