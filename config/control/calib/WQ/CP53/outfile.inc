************************************************************************
** information for individual segments and for everything upstream    **
**  of a calibration point is needed, two types of variables, one     **
**  dimensioned as maxrsegs for segments and one dimensioned as       **
**  maxcsegs for the calibration stations                             **
** the 'out' and 'in' variables are in total mass                     **
** the 'tf' variables are in fraction                                 **
** the rest of the variables are expressed as fraction of input       **
************************************************************************
********* sediment balances
      real sandin(maxrsegs),siltin(maxrsegs),clayin(maxrsegs)
      real sandout(maxrsegs),siltout(maxrsegs),clayout(maxrsegs)
      real sanddiv(maxrsegs),siltdiv(maxrsegs),claydiv(maxrsegs)
      real sandtf(maxrsegs),silttf(maxrsegs),claytf(maxrsegs)
      real Csandin(maxcsegs),Csiltin(maxcsegs),Cclayin(maxcsegs)
      real Csandout(maxcsegs),Csiltout(maxcsegs),Cclayout(maxcsegs)
      real Csanddiv(maxcsegs),Csiltdiv(maxcsegs),Cclaydiv(maxcsegs)
      real Csandtf(maxcsegs),Csilttf(maxcsegs),Cclaytf(maxcsegs)

************ TN Balance
      real tnin(maxrsegs),tnout(maxrsegs),tntf(maxrsegs)
      real tnbrbod(maxrsegs),tnbods(maxrsegs),tnbenal(maxrsegs)
      real tntamscr(maxrsegs),tnbrtam(maxrsegs)
      real tntamvol(maxrsegs),tnnitden(maxrsegs),tnrefset(maxrsegs)
      real tnphyset(maxrsegs),tndiv(maxrsegs)
      real Ctnin(maxcsegs),Ctnout(maxcsegs),Ctntf(maxcsegs)
      real Ctnbrbod(maxcsegs),Ctnbods(maxcsegs),Ctnbenal(maxcsegs)
      real Ctntamscr(maxcsegs),Ctnbrtam(maxcsegs)
      real Ctntamvol(maxcsegs),Ctnnitden(maxcsegs),Ctnrefset(maxcsegs)
      real Ctnphyset(maxcsegs),Ctndiv(maxcsegs)

************ TP Balance
      real tpin(maxrsegs),tpout(maxrsegs),tptf(maxrsegs)
      real tpbrbod(maxrsegs),tpbods(maxrsegs),tpbenal(maxrsegs)
      real tppo4scr(maxrsegs),tpbrpo4(maxrsegs)
      real tprefset(maxrsegs),tpphyset(maxrsegs),tpdiv(maxrsegs)
      real Ctpin(maxcsegs),Ctpout(maxcsegs),Ctptf(maxcsegs)
      real Ctpbrbod(maxcsegs),Ctpbods(maxcsegs),Ctpbenal(maxcsegs)
      real Ctppo4scr(maxcsegs),Ctpbrpo4(maxcsegs)
      real Ctprefset(maxcsegs),Ctpphyset(maxcsegs),Ctpdiv(maxcsegs)

*********** logical variable for overscouring
      logical overscour(maxrsegs)
      
