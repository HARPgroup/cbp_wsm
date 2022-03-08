************************************************************************
**  subroutine to deliver the load for 1 land segment                 **
************************************************************************
      subroutine delatdepmon
     I                (rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 DFscen,lenDFscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 sdate,edate,segDFmon,basinDFmon,resDFmon)
      implicit none
      include 'delload.inc'

      integer nl,nd,tnl,ny,year,lu,month,nm
      integer columnload(nloadmax),colacre  ! column of delivered load

      character*150 fnam2
      character*200 lin2

      character*4 Tloadname(nloadmax),Tland,Tdum1
      character*5 Tdum2

      logical foundload(nloads),foundacre

********* load reading variable
      real Tload(nloadmax)

********* deliverd loads, delivered by different methods
      real segload(nloadmax),basinload(nloadmax),resload(nloadmax)

      integer nm1,nm2,Tny,Tnm

      character*11 datname(3)
      integer ndats,ndat,lendat

      logical dops,doatdep,dosep

*************** END DECLARATION *********************************

      ndats = 0
      if (doatdep) then
        ndats = ndats + 1
        datname(ndats) = atdep
      end if

      do ndat = 1,ndats

        call lencl(datname(ndat),lendat)

        do nl = 1,nloads
          foundload(nl) = .false.
        end do

        fnam = outdir//'eos/monthly/'//rscen(:lenrscen)//
     .         '/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .         datname(ndat)(:lendat)//'.mon'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        fnam2= outdir//'del/lseg/monthly/'//rscen(:lenrscen)
     .         //'_DF_'//DFscen(:lenDFscen)
     .         //'/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .         datname(ndat)(:lendat)//'.mon'
        open(dfile+1,file=fnam2,status='unknown',iostat=err)
        if (err.ne.0) go to 995

        read (dfile,'(a200)') lin2

        if (lin2(:8).eq.'NO LOADS') then
          write(dfile+1,'(a8)',err=951) lin2(:8)
        else

          read (lin2,*,err=992) Tdum1,Tdum2,
     .                           (Tloadname(tnl),tnl=1,nloads+1)

          do nl = 1,nloads
            do tnl = 1,nloads+1
              if (Tloadname(tnl).eq.loadname(nl)) then
                foundload(nl) = .true.
                columnload(nl) = tnl
              end if
            end do
          end do

          foundacre = .false.
          do tnl = 1,nloads+1
            if (Tloadname(tnl).eq.'ACRE') then
              foundacre = .true.
              colacre = tnl
            end if
          end do

          do nl = 1,nloads
            if (.not.foundload(nl)) goto 994
          end do
          if (.not.foundacre) go to 995

          write(dfile+1,123,err=951) Tdum1,Tdum2,
     .                  (segLoadName(nl),nl=1,nloads),
     .                  (basinLoadName(nl),nl=1,nloads),
     .                  (resLoadName(nl),nl=1,nloads),'ACRE'

          do ny = sdate(1),edate(1)
            nm1 = 1
            if (ny.eq.sdate(1)) nm1 = sdate(2)
            nm2 = 12
            if (ny.eq.edate(1)) nm2 = edate(2)
            do nm = nm1,nm2
              read(dfile,*,err=993) Tny,Tnm,(Tload(tnl),tnl=1,nloads+1)
              if (ny.ne.Tny.or.nm.ne.Tnm) go to 996
              do nl = 1,nloads
                segload(nl) = Tload(columnload(nl)) *
     .                        segDFmon(load2del(nl),ny,nm)
                basinload(nl) = Tload(columnload(nl)) *
     .                        basinDFmon(load2del(nl),ny,nm)
                resload(nl) = Tload(columnload(nl)) *
     .                        resDFmon(load2del(nl),ny,nm)
              end do
              write(dfile+1,1234,err=951) ny,nm,
     .              (segload(nl),nl=1,nloads),
     .              (basinload(nl),nl=1,nloads),
     .              (resload(nl),nl=1,nloads),Tload(colacre)
            end do ! loop over months
          end do

        end if

        close(dfile)
        close(dfile+1)

      end do        ! end loop over land uses

      return

123   format(a4,',',a5,60(',',a14))
1234  format(i4,',',i5,60(',',e14.7))

************************* ERROR SPACE
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

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
      report(3) = ' Should be in format (a3,1x,60(1x,e14.7)) '//
     .             'for lu and value'
      go to 999

994   report(1) = ' Did not find load in file'
      report(2) = fnam
      report(3) = loadname(nl)
      go to 999

995   report(1) = '  Problem opening '
      report(2) = fnam2
      report(3) = 'error code = '
      write(report(3)(14:16),'(i3)')err
      go to 999

996   report(1) = '  Problem reading file'
      report(2) = fnam
      write(report(3),*) ' for year and month: ',ny,' ',nm
      go to 999

999   call stopreport(report)

      end

