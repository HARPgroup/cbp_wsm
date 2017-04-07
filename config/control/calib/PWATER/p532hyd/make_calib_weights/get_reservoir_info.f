************************************************************************
** river info program.  reads in a uniqid and gives the upstream      **
**  rivers, the size of the counties in the entire upstream watershed **
**   and the other calibration sites that are affected by these       **
**   counties                                                         **
************************************************************************
      subroutine get_reservoir_info(
     I                        rscen,lenrscen,
     I                        nrsegs,rsegs,nlsegs,lsegs,
     I                        nallland,allland,acres,
     I                        uniqindex,uniqid,dsid,
     O                        nreservoir,revname,revsize,nallup,allup)
      include 'flow_weight.inc'
      integer ns,nl,nr,n2,nc           ! indices
      integer nsup,nu                  ! index to upstream segment

      character*13 revname(maxrservoir)  ! name of reservoir
      real         revsize(maxrservoir)
      integer nl2,nl3,ic
      integer nreservoir,lakeflag
      
************** END DECLARATION *****************************************

      do ns = 1,maxrsegs
       revsize(ns) = 0.0
       do nl =1, maxlsegs
        clseglist(nl) = 0
        clarea(nl) = 0.0
       end do
      end do

      nreservoir = 0
*************** find all reservoir stations ****************************
      call readcontrol_Rparamscen(
     I                            rscen,lenrscen,
     O                            paramscen)
      call lencl(paramscen,lenparamscen)
      fnam = pardir//'river/'//paramscen(:lenparamscen)//
     .       '/gen_info_rseg.csv'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991
     
      read(dfile,'(a100)')line            ! skip the header
      call d2x(line,last)

      read(dfile,'(a100)')line            ! read the first real line
      call d2x(line,last)
      do while (line(:3).ne.'end')
        if (.not.comment(line)) then
          call findcomma(line,ic)
          Tseg = line(:ic-1)
          call trims(Tseg,last)
          if (last.ne.13) then
            err = 992
            return
          end if

          call shift(line)
          call findcomma(line,ic)
          call shift(line)
          read(line,'(i1)') lakeflag        ! read in lake flag
                  
          if (lakeflag .eq. 1) then         ! find a reservoir
           do ns = 1, nrsegs
            if(Tseg(:last) .eq. rsegs(ns)) then   ! the reservoir found in river segment
             nreservoir = nreservoir + 1
             revname(nreservoir)= Tseg(:last)
             exit
            end if
           end do       ! loop over all river sites
         end if

       end if
       read(dfile,'(a100)')line
       call d2x(line,last)
      end do
      close (dfile)

************ get upstream stations for each reservoir
      do nr = 1,nreservoir

        call findupstream(
     I                    revname(nr),
     I                    nrsegs,uniqindex,uniqid,dsid,     ! use nrsegs here INSTEAD OF ncsegs
     O                    upstream,nup)
        nallup(nr) = nup
        do nu = 1,nup
          allup(nr,nu) = upstream(nu)
        end do
        if (revname(nr)(10:13).eq.'0003') then
          nallup(nr) = nup-1
        end if

      end do

************ get basin size of each reservoir
       do nr = 1,nreservoir
        call getlseglist(
     I                   nr,nallup,allup,nallland,allland,acres,
     O                   clseglist,clarea,nclseglist)

        do nl = 1, nclseglist
          revsize(nr) = revsize(nr) + clarea(nl)
        end do

       end do          ! loop over all calibration stations


      return

************* ERROR SPACE ****************
991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

999   call stopreport(report)

      end

