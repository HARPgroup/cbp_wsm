************************************************************************
**  subroutine to get the load for 1 land segment in terms of the     **
**     delivery variables                                             **
************************************************************************
      subroutine datmon(
     I                   rseg,lenrseg,lseg,rscen,lenrscen,
     I                   ndel,delname,sdate,edate,nloads,
     I                   dops,doatdep,dosep,dorib,dorpa,
     M                   inmon)
      implicit none
      include 'tfs.inc'

      integer nl,ndl,lu,ny,nm,nm1,nm2,Tny,Tnm

      integer columndel(ndelmax)      ! column of each delivered load

      character*4 Tloadname(nloadmax),Tdum1,Tdum2,Tdum3

      logical founddel(ndelmax)

      real Tload(nloadmax)

      character*11 datname(5)
      integer ndats,ndat,lendat

      logical dops,doatdep,dosep,dorib,dorpa

******************* END DECLARATION ****************************
      call lencl(lseg,lenlseg)
      
      ndats = 0
      if (dops) then               ! three pointsource files
        ndats = ndats + 1
        datname(ndats) = wwtp
      end if
      if (dops) then
        ndats = ndats + 1
        datname(ndats) = indus
      end if
      if (dops) then
        ndats = ndats + 1
        datname(ndats) = cso
      end if
      if (doatdep) then
        ndats = ndats + 1
        datname(ndats) = atdep
      end if
      if (dosep) then
        ndats = ndats + 1
        datname(ndats) = sep
      end if
      if (dorib) then
        ndats = ndats + 1
        datname(ndats) = rib
      end if
      if (dorpa) then
        ndats = ndats + 1
        datname(ndats) = rpa
      end if

************ read and add loads for pointsources, septic and atdep
      do ndat = 1,ndats

        do ndl = 1,ndel
          founddel(ndl) = .false.
        end do

        call lencl(datname(ndat),lendat) 

        fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .           '/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .           datname(ndat)(:lendat)//'.mon'
        open(11,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        read (11,'(a100)') line
        if (line(:8).eq.'NO LOADS') cycle   !   go to next landuse
        backspace 11
  
        read (11,*,err=992) Tdum1,Tdum2,(Tloadname(nl),nl=1,nloads)
        do ndl = 1,ndel
          do nl = 1,nloads
            if (Tloadname(nl).eq.delname(ndl)) then
              founddel(ndl) = .true.
              columndel(ndl) = nl
            end if
          end do
        end do

        do ndl = 1,ndel
          if (.not.founddel(ndl)) goto 994
        end do

        do ny = sdate(1),edate(1)
          nm1 = 1
          if (ny.eq.sdate(1)) nm1 = sdate(2)
          nm2 = 12
          if (ny.eq.edate(1)) nm2 = edate(2)

          do nm = nm1,nm2
            read(11,*,err=993)Tny,Tnm,(Tload(nl),nl=1,nloads)
            if (ny.ne.Tny.or.nm.ne.Tnm) go to 995
            do ndl = 1,ndel
              inmon(ndl,ny,nm) = inmon(ndl,ny,nm)+Tload(columndel(ndl))
            end do
          end do

        end do

        close(11)

      end do     ! end loop over ndats

      return

************************* ERROR SPACE
991   report(1) = '  Problem opening '
      report(2) = fnam
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

992   report(1) = '  Problem reading file'
      report(2) = fnam
      report(3) = ' in first line' 
      go to 999

993   report(1) = '  Problem reading file'
      report(2) = fnam
      write(report(3),*) ' for year and month: ',ny,' ',nm
      go to 999

994   report(1) = ' Did not find load in file'
      report(2) = fnam
      report(3) = delname(ndl)
      go to 999

995   report(1) = ' Dates in file:'
      report(2) = fnam
      report(3) = ' did not match river scenario control file'
      go to 999

999   call stopreport(report)

      end

