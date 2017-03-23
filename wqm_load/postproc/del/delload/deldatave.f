************************************************************************
**  subroutine to deliver the load for 1 land segment                 **
************************************************************************
      subroutine deldatave
     I                (rseg,lenrseg,lseg,lenlseg,rscen,lenrscen,
     I                 ndel,delname,nloads,loadname,load2del,
     I                 segLoadName,basinLoadName,resLoadName,
     I                 dops,doatdep,dosep,
     I                 year1,year2,segDFave,basinDFave,resDFave)
      implicit none
      include 'delload.inc'
      integer nl,nd,tnl,ny,year,lu
      integer columnload(nloadmax),colacre  ! column of delivered load

      character*100 fnam2
      character*200 lin2

      character*4 Tloadname(nloadmax),Tland,cy1,cy2

      logical foundload(nloads),foundacre

********* load reading variable
      real Tload(nloadmax)

********* deliverd loads, delivered by different methods
      real segload(nloadmax),basinload(nloadmax),resload(nloadmax)

      character*11 datname(4)
      integer ndats,ndat,lendat

      logical dops,doatdep,dosep

******************* END DECLRATION *************************************
      write(cy1,'(i4)') year1
      write(cy2,'(i4)') year2

      ndats = 0
      if (dops) then              ! three pointsource files
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
      if (dosep) then
        ndats = ndats + 1
        datname(ndats) = sep
      end if

*********** read and add loads together from each data file
      do ndat = 1,ndats

        call lencl(datname(ndat),lendat)

        do nl = 1,nloads
          foundload(nl) = .false.
        end do

        fnam = outdir//'eos/aveann/'//rscen(:lenrscen)//
     .         '/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .         datname(ndat)(:lendat)//'_'//cy1//'-'//cy2//'.ave'
        open(dfile,file=fnam,status='old',iostat=err)
        if (err.ne.0) go to 991

        fnam2= outdir//'del/lseg/aveann/'//rscen(:lenrscen)
     .         //'/'//lseg(:lenlseg)//'_to_'//rseg(:lenrseg)//'_'//
     .         datname(ndat)(:lendat)//'_'//cy1//'-'//cy2//'.ave'
        open(dfile+1,file=fnam2,status='unknown',iostat=err)
        if (err.ne.0) go to 995

        read (dfile,'(a200)') lin2
        
        if (lin2(:8).eq.'NO LOADS') then
          write(dfile+1,'(a8)',err=951) lin2(:8)
        else

          read (lin2,*,err=992) Tland,(Tloadname(tnl),tnl=1,nloads)
          
          do nl = 1,nloads
            do tnl = 1,nloads
              if (Tloadname(tnl).eq.loadname(nl)) then
                foundload(nl) = .true.
                columnload(nl) = tnl
              end if
            end do
          end do

          do nl = 1,nloads
            if (.not.foundload(nl)) goto 994
          end do

          write(dfile+1,123,err=951) Tland,
     .                  (segLoadName(nl),nl=1,nloads),
     .                  (basinLoadName(nl),nl=1,nloads),
     .                  (resLoadName(nl),nl=1,nloads)

          read(dfile,*,err=993) line,(Tload(tnl),tnl=1,nloads)

          do nl = 1,nloads
             
            segload(nl) = Tload(columnload(nl)) *
     .                    segDFave(load2del(nl))
            basinload(nl) = Tload(columnload(nl)) *
     .                    basinDFave(load2del(nl))
            resload(nl) = Tload(columnload(nl)) *
     .                    resDFave(load2del(nl))
          end do
          write(dfile+1,1234,err=951) line(:9),
     .          (segload(nl),nl=1,nloads),
     .          (basinload(nl),nl=1,nloads),
     .          (resload(nl),nl=1,nloads)

        end if

        close(dfile)
        close(dfile+1)

      end do        ! end loop over data files

      return

123   format(a4,60(',',a14))
1234  format(1x,a9,60(',',e14.7))

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

999   call stopreport(report)

      end

