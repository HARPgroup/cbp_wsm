************************************************************************
** function to return rseg most appropriate for a wqm or ch3d cell    **
************************************************************************
      function cell2rseg(rsegs,nrsegs,ccell,psmethod)
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

*********** inputs
      character*(*) psmethod  ! wcell or hcell
      character*(*) ccell  ! wqm or ch3d cell number

********** output
      integer cell2rseg  ! nrsegs index for the cell

********* reading variables
      character*15 facility,state
      character*6 hydcell,w57cell,w13cell
      character*13 Trseg
      character*6 Tlseg

********* store matched segs if multiple rsegs go to the same cell
      integer maxmatch,nmatch,nm
      parameter (maxmatch = 20)
      integer matchsegs(maxmatch)   ! index of matching segs

******** utilities
      integer nr  ! index
      logical found

*********** open file and search for cell matches, store nrseg
      fnam = 
     . '/model/p5_big_dataset_storage/point_source/NPDES_CELL_SEG.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

      nmatch = 0
      do 
        read(dfile,*,err=992,end=111) facility,state,hydcell,
     .                                w57cell,w13cell,Trseg,Tlseg
        if ((psmethod.eq.'hcell'.and.hydcell.eq.ccell) .or.
     .      (psmethod.eq.'wcell'.and.w57cell.eq.ccell)) then
          nmatch = nmatch + 1
          if (nmatch.gt.maxmatch) go to 993
          found = .false.
          do nr = 1,nrsegs
            if (Trseg.eq.rsegs(nr)) then
              found = .true.
              matchsegs(nmatch) = nr
            end if
          end do
          if (.not.found) go to 995
        end if
      end do
111   close(dfile)

*********** if no matches probably no point sources in that cell
      if (nmatch.eq.0) cell2rseg = -999

********* if one match, use it
      cell2rseg = matchsegs(1)
      if (nmatch.eq.1) return

********** if multiple matches, apply rules
********** can't think of what these might be at the moment
********* accept first match
      return

***************** ERROR SPACE ******************************************
991   report(1) = '  Problem opening file'
      report(2) = fnam
      report(3) = '  error code = '
      write(report(3)(16:18),'(i3)') err
      go to 999

992   report(1) = 'error reading file:  near line:'
      report(2) = fnam
      report(3) = facility//' '//state//' '//hydcell
      go to 999

993   report(1) = 'too many matches between cell and rseg'
      report(2) = fnam
      report(3) = 'adjust maxmatch variable in point source program'
      go to 999

C994   report(1) = 'no matches between cell and rseg'
C      report(2) = fnam
C      report(3) = 'check logic and input files'
C      go to 999
C
995   report(1) = 'found rseg '//Trseg//' in file'
      report(2) = fnam
      report(3) = 'that did not match segment list'
      go to 999

999   call stopreport(report)

      end

