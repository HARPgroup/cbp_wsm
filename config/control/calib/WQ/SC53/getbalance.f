************************************************************************
** calls the sumoutfile subroutine for each calibration segment (cseg)**
** The 
************************************************************************
      subroutine getbalance(
     I                      rscen,csegs,ncsegs,rsegs,nrsegs,
     I                      year1,year2,
     O                      sandin,sandout,sandtf,
     O                      siltin,siltout,silttf,
     O                      clayin,clayout,claytf,
     O                      sanddiv,siltdiv,claydiv,
     O                      tnin,tnout,tntf,tnbrbod,tnbods,tnbenal,
     O                      tntamscr,tnbrtam,tntamvol,tnnitden,
     O                      tnrefset,tnphyset,tndiv,
     O                      tpin,tpout,tptf,tpbrbod,tpbods,tpbenal,
     O                      tppo4scr,tpbrpo4,tprefset,tpphyset,tpdiv,
     O                      Csandin,Csandout,Csandtf,
     O                      Csiltin,Csiltout,Csilttf,
     O                      Cclayin,Cclayout,Cclaytf,
     O                      Csanddiv,Csiltdiv,Cclaydiv,
     O                      Ctnin,Ctnout,Ctntf,Ctnbrbod,Ctnbods,
     O                      Ctnbenal,Ctntamscr,Ctnbrtam,Ctntamvol,
     O                      Ctnnitden,Ctnrefset,Ctnphyset,Ctndiv,
     O                      Ctpin,Ctpout,Ctptf,Ctpbrbod,Ctpbods,
     O                      Ctpbenal,Ctppo4scr,Ctpbrpo4,Ctprefset,
     O                      Ctpphyset,Ctpdiv,
     O                      overscour)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'
      include 'weights.inc'
      include 'outfile.inc'

      integer year1,year2,nr

*************** END DECLARATIONS ***************************************


      do nr = 1,nrsegs
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
     O                    tprefset(nr),tpphyset(nr),tpdiv(nr),
     O                    overscour(nr))
      end do

      do ns = 1,ncsegs
        call sumoutfile(
     I                  rscen,csegs(ns),year1,year2,
     I                  rsegs,nrsegs,
     I                  sandin,siltin,clayin,
     I                  sandout,siltout,clayout,
     I                  sanddiv,siltdiv,claydiv,
     I                  sandtf,silttf,claytf,
     I                  tnin,tnout,tntf,
     I                  tnbrbod,tnbods,tnbenal,tntamscr,tnbrtam,
     I                  tntamvol,tnnitden,tnrefset,tnphyset,tndiv,
     I                  tpin,tpout,tptf,
     I                  tpbrbod,tpbods,tpbenal,tppo4scr,tpbrpo4,
     I                  tprefset,tpphyset,tpdiv,
     O                  Csandin(ns),Csandout(ns),Csandtf(ns),
     O                  Csiltin(ns),Csiltout(ns),Csilttf(ns),
     O                  Cclayin(ns),Cclayout(ns),Cclaytf(ns),
     O                  Csanddiv(ns),Csiltdiv(ns),Cclaydiv(ns),
     O                  Ctnin(ns),Ctnout(ns),Ctntf(ns),Ctnbrbod(ns),
     O                  Ctnbods(ns),Ctnbenal(ns),Ctntamscr(ns),
     O                  Ctnbrtam(ns),Ctntamvol(ns),Ctnnitden(ns),
     O                  Ctnrefset(ns),Ctnphyset(ns),Ctndiv(ns),
     O                  Ctpin(ns),Ctpout(ns),Ctptf(ns),Ctpbrbod(ns),
     O                  Ctpbods(ns),Ctpbenal(ns),Ctppo4scr(ns),
     O                  Ctpbrpo4(ns),Ctprefset(ns),Ctpphyset(ns),
     O                  Ctpdiv(ns))
      end do

**************** recalculate fluxes as fractions of inputs
      do nr = 1,nrsegs
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

      return

      end
