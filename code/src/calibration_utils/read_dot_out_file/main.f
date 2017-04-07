************************************************************************
************************************************************************
      implicit none
      include '../../lib/inc/standard.inc'
      include '../../lib/inc/locations.inc'
      include '../../lib/inc/rsegs.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg

      integer years,startyear,ny,year1,year2

      integer nr,nr2

      character*24 basin
      integer lenbasin

      logical found

*************** OUTPUT VARIABLES
********* sediment balances
      real sandin(maxrsegs),siltin(maxrsegs),clayin(maxrsegs)
      real sandout(maxrsegs),siltout(maxrsegs),clayout(maxrsegs)
      real sanddiv(maxrsegs),siltdiv(maxrsegs),claydiv(maxrsegs)
      real sandtf(maxrsegs),silttf(maxrsegs),claytf(maxrsegs)

************ TN Balance
      real tnin(maxrsegs),tnout(maxrsegs),tntf(maxrsegs)
      real tnbrbod(maxrsegs),tnbods(maxrsegs),tnbenal(maxrsegs)
      real tntamscr(maxrsegs),tnbrtam(maxrsegs)
      real tntamvol(maxrsegs),tnnitden(maxrsegs),tnrefset(maxrsegs)
      real tnphyset(maxrsegs),tndiv(maxrsegs)

************ TP Balance
      real tpin(maxrsegs),tpout(maxrsegs),tptf(maxrsegs)
      real tpbrbod(maxrsegs),tpbods(maxrsegs),tpbenal(maxrsegs)
      real tppo4scr(maxrsegs),tpbrpo4(maxrsegs)
      real tprefset(maxrsegs),tpphyset(maxrsegs),tpdiv(maxrsegs)


************ END DECLARATIONS ******************************************
      print*,' reading the river .out files'
      print*,' enter river scen, basin, year1, year2'

      read*,rscen,basin,year1,year2
      call lencl(rscen,lenrscen)
      call lencl(basin,lenbasin)
      if (lenbasin.ge.25) go to 998

*********** read in all possible calibration segments
********** and store list of simulated rivers
******* csegs and rsegs will differ in that csegs will contain doubles
      call readRiverSeglist(
     I                      basin,
     O                      rsegs,nrsegs)
      rsegs(nrsegs+1) = 'total'

**************** initialize accumulation variables
      sandin(nrsegs+1) = 0.0
      siltin(nrsegs+1) = 0.0
      clayin(nrsegs+1) = 0.0
      sandout(nrsegs+1) = 0.0
      siltout(nrsegs+1) = 0.0
      clayout(nrsegs+1) = 0.0
      sanddiv(nrsegs+1) = 0.0
      siltdiv(nrsegs+1) = 0.0
      claydiv(nrsegs+1) = 0.0
      sandtf(nrsegs+1) = 0.0
      silttf(nrsegs+1) = 0.0
      claytf(nrsegs+1) = 0.0
      tnin(nrsegs+1) = 0.0
      tnout(nrsegs+1) = 0.0
      tntf(nrsegs+1) = 0.0
      tnbrbod(nrsegs+1) = 0.0
      tnbods(nrsegs+1) = 0.0
      tnbenal(nrsegs+1) = 0.0
      tntamscr(nrsegs+1) = 0.0
      tnbrtam(nrsegs+1) = 0.0
      tntamvol(nrsegs+1) = 0.0
      tnnitden(nrsegs+1) = 0.0
      tnrefset(nrsegs+1) = 0.0
      tnphyset(nrsegs+1) = 0.0
      tndiv(nrsegs+1) = 0.0
      tpin(nrsegs+1) = 0.0
      tpout(nrsegs+1) = 0.0
      tptf(nrsegs+1) = 0.0
      tpbrbod(nrsegs+1) = 0.0
      tpbods(nrsegs+1) = 0.0
      tpbenal(nrsegs+1) = 0.0
      tppo4scr(nrsegs+1) = 0.0
      tpbrpo4(nrsegs+1) = 0.0
      tprefset(nrsegs+1) = 0.0
      tpphyset(nrsegs+1) = 0.0
      tpdiv(nrsegs+1) = 0.0

***************** loop over segments and get values
******** store in variables and add to accumulation variables
      do nr = 1,nrsegs
        print*,' reading ',rsegs(nr),' ',rscen(:lenrscen)
        call read_outfile(
     I                    rsegs(nr),rscen,year1,year2,
     O                    sandin(nr),siltin(nr),clayin(nr),
     O                    sandout(nr),siltout(nr),clayout(nr),
     O                    sanddiv(nr),siltdiv(nr),claydiv(nr),
     O                    sandtf(nr),silttf(nr),claytf(nr),
     O                    tnin(nr),tnout(nr),tntf(nr),
     O      tnbrbod(nr),tnbods(nr),tnbenal(nr),tntamscr(nr),tnbrtam(nr),
     O    tntamvol(nr),tnnitden(nr),tnrefset(nr),tnphyset(nr),tndiv(nr),
     O                    tpin(nr),tpout(nr),tptf(nr),
     O      tpbrbod(nr),tpbods(nr),tpbenal(nr),tppo4scr(nr),tpbrpo4(nr),
     O                    tprefset(nr),tpphyset(nr),tpdiv(nr))

************* add to totals
********** only add outputs if pour point of the watershed
************* no downstream segments in rsegs
        found = .false.
        do nr2 = 1,nrsegs
          if (rsegs(nr)(10:13).eq.rsegs(nr2)(5:8)) then
            found = .true.
            exit
          end if
        end do
        if (.not.found) then  ! no downstream segs, add output
          sandout(nrsegs+1) = sandout(nrsegs+1) + sandout(nr) 
          siltout(nrsegs+1) = siltout(nrsegs+1) + siltout(nr) 
          clayout(nrsegs+1) = clayout(nrsegs+1) + clayout(nr) 
          tnout(nrsegs+1) = tnout(nrsegs+1) + tnout(nr) 
          tpout(nrsegs+1) = tpout(nrsegs+1) + tpout(nr) 
        end if

*********** get EOS inputs only by getting rid of upstream inputs
        sandin(nrsegs+1) = sandin(nrsegs+1) + sandin(nr) 
        siltin(nrsegs+1) = siltin(nrsegs+1) + siltin(nr) 
        clayin(nrsegs+1) = clayin(nrsegs+1) + clayin(nr) 
        tnin(nrsegs+1) = tnin(nrsegs+1) + tnin(nr) 
        tpin(nrsegs+1) = tpin(nrsegs+1) + tpin(nr) 
        do nr2 = 1,nr-1
          if (rsegs(nr2)(10:13).eq.rsegs(nr)(5:8)) then
            sandin(nrsegs+1) = sandin(nrsegs+1) - sandout(nr2) 
            siltin(nrsegs+1) = siltin(nrsegs+1) - siltout(nr2) 
            clayin(nrsegs+1) = clayin(nrsegs+1) - clayout(nr2) 
            tnin(nrsegs+1) = tnin(nrsegs+1) - tnout(nr2) 
            tpin(nrsegs+1) = tpin(nrsegs+1) - tpout(nr2) 
          end if
        end do

************ fluxes are purely cumulative
        sanddiv(nrsegs+1) = sanddiv(nrsegs+1) +  sanddiv(nr) 
        siltdiv(nrsegs+1) = siltdiv(nrsegs+1) +  siltdiv(nr) 
        claydiv(nrsegs+1) = claydiv(nrsegs+1) +  claydiv(nr) 
        tnbrbod(nrsegs+1) = tnbrbod(nrsegs+1) + tnbrbod(nr) 
        tnbods(nrsegs+1) = tnbods(nrsegs+1) + tnbods(nr) 
        tnbenal(nrsegs+1) = tnbenal(nrsegs+1) + tnbenal(nr) 
        tntamscr(nrsegs+1) = tntamscr(nrsegs+1) + tntamscr(nr) 
        tnbrtam(nrsegs+1) = tnbrtam(nrsegs+1) + tnbrtam(nr) 
        tntamvol(nrsegs+1) = tntamvol(nrsegs+1) + tntamvol(nr) 
        tnnitden(nrsegs+1) = tnnitden(nrsegs+1) + tnnitden(nr) 
        tnrefset(nrsegs+1) = tnrefset(nrsegs+1) + tnrefset(nr) 
        tnphyset(nrsegs+1) = tnphyset(nrsegs+1) +  tnphyset(nr) 
        tndiv(nrsegs+1) = tndiv(nrsegs+1) +  tndiv(nr) 
        tpbrbod(nrsegs+1) = tpbrbod(nrsegs+1) + tpbrbod(nr) 
        tpbods(nrsegs+1) = tpbods(nrsegs+1) + tpbods(nr) 
        tpbenal(nrsegs+1) = tpbenal(nrsegs+1) + tpbenal(nr) 
        tppo4scr(nrsegs+1) = tppo4scr(nrsegs+1) + tppo4scr(nr) 
        tpbrpo4(nrsegs+1) = tpbrpo4(nrsegs+1) + tpbrpo4(nr) 
        tprefset(nrsegs+1) = tprefset(nrsegs+1) + tprefset(nr) 
        tpphyset(nrsegs+1) = tpphyset(nrsegs+1) + tpphyset(nr) 
        tpdiv(nrsegs+1) = tpdiv(nrsegs+1) + tpdiv(nr) 
      end do

************** recalculate transport factors, since they do not add
      sandtf(nrsegs+1) = sandout(nrsegs+1)/sandin(nrsegs+1)
      silttf(nrsegs+1) = siltout(nrsegs+1)/siltin(nrsegs+1)
      claytf(nrsegs+1) = clayout(nrsegs+1)/clayin(nrsegs+1)
      tntf(nrsegs+1) = tnout(nrsegs+1)/tnin(nrsegs+1)
      tptf(nrsegs+1) = tpout(nrsegs+1)/tpin(nrsegs+1)

**************** recalculate fluxes as fractions of inputs
      do nr = 1,nrsegs+1
        sanddiv(nr) = sanddiv(nr) / sandin(nr)
        siltdiv(nr) = siltdiv(nr) / siltin(nr)
        claydiv(nr) = claydiv(nr) / clayin(nr)
        tnbrbod(nr) = tnbrbod(nr) / tnin(nr)
        tnbods(nr) = tnbods(nr) / tnin(nr)
        tnbenal(nr) = tnbenal(nr) / tnin(nr)
        tntamscr(nr) = tntamscr(nr) / tnin(nr)
        tnbrtam(nr) = tnbrtam(nr) / tnin(nr)
        tntamvol(nr) = tntamvol(nr) / tnin(nr)
        tnnitden(nr) = tnnitden(nr) / tnin(nr)
        tnrefset(nr) = tnrefset(nr) / tnin(nr)
        tnphyset(nr) = tnphyset(nr) / tnin(nr)
        tndiv(nr) = tndiv(nr) / tnin(nr)
        tpbrbod(nr) = tpbrbod(nr) / tpin(nr)
        tpbods(nr) = tpbods(nr) / tpin(nr)
        tpbenal(nr) = tpbenal(nr) / tpin(nr)
        tppo4scr(nr) = tppo4scr(nr) / tpin(nr)
        tpbrpo4(nr) = tpbrpo4(nr) / tpin(nr)
        tprefset(nr) = tprefset(nr) / tpin(nr)
        tpphyset(nr) = tpphyset(nr) / tpin(nr)
        tpdiv(nr) = tpdiv(nr) / tpin(nr)
      end do


      fnam = outdir//'river/summary/'//rscen(:lenrscen)//'/sumout_'
     .       //basin(:lenbasin)//'.csv'
      open(dfile,file=fnam,status='unknown',iostat=err)
      print*,' '
      print*,' CREATING OUTPUT FILE:'
      print*,fnam
      print*,' '
      if (err.ne.0) go to 991
      write(dfile,'(7a)',err=951)
     .                    'rseg,sandin,sandout,sanddiv,sandtf,siltin,',
     .                    'siltout,siltdiv,silttf,clayin,clayout,',
     .                    'claydiv,claytf,TNin,TNout,TNtf,TNbrbod,',
     .                    'TNbodsink,TNbenal,TNtamscr,TNbrtam,',
     .                    'TNtamvol,TNnitden,TNrefset,TNphyset,TNdiv,',
     .                    'TPin,TPout,TPtf,TPbrbod,TPbodsink,TPbenal,',
     .                    'TPpo4scr,TPbrpo4,TPrefset,TPphyset,TPdiv'
      do nr = 1,nrsegs+1
        write(dfile,1234,err=951) 
     .                    rsegs(nr),sandin(nr),sandout(nr),sanddiv(nr),
     .                    sandtf(nr),siltin(nr),siltout(nr),siltdiv(nr),
     .                    silttf(nr),clayin(nr),clayout(nr),claydiv(nr),
     .                    claytf(nr),tnin(nr),tnout(nr),tntf(nr),
     .                    tnbrbod(nr),tnbods(nr),tnbenal(nr),
     .                    tntamscr(nr),tnbrtam(nr),tntamvol(nr),
     .                    tnnitden(nr),tnrefset(nr),tnphyset(nr),
     .                    tndiv(nr),tpin(nr),tpout(nr),tptf(nr),
     .                    tpbrbod(nr),tpbods(nr),tpbenal(nr),
     .                    tppo4scr(nr),tpbrpo4(nr),tprefset(nr),
     .                    tpphyset(nr),tpdiv(nr)
      end do
      close(dfile)

      stop

1234  format(a13,36(',',e10.3))

************* ERROR SPACE ****************
951   report(1) = 'error writing to file'
      report(2) = fnam
      report(3) = 'possible permission problem'
      go to 999

991   report(1) = 'Problem opening file:'
      report(2) = fnam
      report(3) = 'error = '
      write(report(3)(9:11),'(i3)')err
      go to 999

998   report(1) = 'Problem with basin '//basin
      report(2) = ' rename with fewer characters, or change code:'
      report(3) = ' ./pp/src/calibration_utils/make_WQ_calib_site_list/'
      go to 999

999   call stopreport(report)

      end

