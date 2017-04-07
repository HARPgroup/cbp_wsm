************************************************************************
** loops over segments above the given calibration segment and sums   **
**  the upstream balances to produce a mass balance around the shed   **
************************************************************************
      subroutine sumoutfile(
     I                  rscen,cseg,year1,year2,
     I                  rsegs,nrsegs,
     I                    sandin,siltin,clayin,
     I                    sandout,siltout,clayout,
     I                    sanddiv,siltdiv,claydiv,
     I                    sandtf,silttf,claytf,
     I                    tnin,tnout,tntf,
     I      tnbrbod,tnbods,tnbenal,tntamscr,tnbrtam,
     I      tntamvol,tnnitden,tnrefset,tnphyset,tndiv,
     I                    tpin,tpout,tptf,
     I      tpbrbod,tpbods,tpbenal,tppo4scr,tpbrpo4,
     I                    tprefset,tpphyset,tpdiv,
     O                  Csandin,Csandout,Csandtf,
     O                  Csiltin,Csiltout,Csilttf,
     O                  Cclayin,Cclayout,Cclaytf,
     O                  Csanddiv,Csiltdiv,Cclaydiv,
     O                  Ctnin,Ctnout,Ctntf,Ctnbrbod,
     O                  Ctnbods,Ctnbenal,Ctntamscr,
     O                  Ctnbrtam,Ctntamvol,Ctnnitden,
     O                  Ctnrefset,Ctnphyset,Ctndiv,
     O                  Ctpin,Ctpout,Ctptf,Ctpbrbod,
     O                  Ctpbods,Ctpbenal,Ctppo4scr,
     O                  Ctpbrpo4,Ctprefset,Ctpphyset,
     O                  Ctpdiv)

      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'
      include '../../../../../code/src/lib/inc/rsegs.inc'

      character*140 outline
      character*3 clu
      character*6 thislseg

      integer years,startyear,ny,year1,year2

      integer nr,nr1,nr2

      character*24 basin
      integer lenbasin

      logical found

      character*13 cseg

      logical upstream(maxrsegs)

      logical foundnew

*************** OUTPUT VARIABLES
********* sediment balances
      real sandin(maxrsegs),siltin(maxrsegs),clayin(maxrsegs)
      real sandout(maxrsegs),siltout(maxrsegs),clayout(maxrsegs)
      real sanddiv(maxrsegs),siltdiv(maxrsegs),claydiv(maxrsegs)
      real sandtf(maxrsegs),silttf(maxrsegs),claytf(maxrsegs)
      real Csandin,Csiltin,Cclayin
      real Csandout,Csiltout,Cclayout
      real Csanddiv,Csiltdiv,Cclaydiv
      real Csandtf,Csilttf,Cclaytf

************ TN Balance
      real tnin(maxrsegs),tnout(maxrsegs),tntf(maxrsegs)
      real tnbrbod(maxrsegs),tnbods(maxrsegs),tnbenal(maxrsegs)
      real tntamscr(maxrsegs),tnbrtam(maxrsegs)
      real tntamvol(maxrsegs),tnnitden(maxrsegs),tnrefset(maxrsegs)
      real tnphyset(maxrsegs),tndiv(maxrsegs)
      real Ctnin,Ctnout,Ctntf
      real Ctnbrbod,Ctnbods,Ctnbenal
      real Ctntamscr,Ctnbrtam
      real Ctntamvol,Ctnnitden,Ctnrefset
      real Ctnphyset,Ctndiv

************ TP Balance
      real tpin(maxrsegs),tpout(maxrsegs),tptf(maxrsegs)
      real tpbrbod(maxrsegs),tpbods(maxrsegs),tpbenal(maxrsegs)
      real tppo4scr(maxrsegs),tpbrpo4(maxrsegs)
      real tprefset(maxrsegs),tpphyset(maxrsegs),tpdiv(maxrsegs)
      real Ctpin,Ctpout,Ctptf
      real Ctpbrbod,Ctpbods,Ctpbenal
      real Ctppo4scr,Ctpbrpo4
      real Ctprefset,Ctpphyset,Ctpdiv


************ END DECLARATIONS ******************************************
      call lencl(rscen,lenrscen)


******** find which of all rsegs are upstream of the cseg
      do nr = 1,nrsegs
        upstream(nr) = .false.
        if (rsegs(nr).eq.cseg) upstream(nr) = .true.
      end do

      if (cseg(10:13).eq.'0003') then  ! special for confluence
        do nr = 1,nrsegs
          if (rsegs(nr)(10:13).eq.cseg(5:8)) then
            upstream(nr) = .true.
          end if
        end do
      end if

      foundnew = .true.
      do while (foundnew)
        foundnew = .false.
        do nr1 = 1,nrsegs
          if (upstream(nr1)) then
            do nr2 = 1,nrsegs
              if (rsegs(nr2)(10:13).eq.rsegs(nr1)(5:8)) then
                if (.not.upstream(nr2)) then
                  upstream(nr2) = .true.
                  foundnew = .true.
                end if
              end if
            end do
          end if
        end do
      end do
   
**************** initialize accumulation variables
      Csandin = 0.0
      Csiltin = 0.0
      Cclayin = 0.0
      Csandout = 0.0
      Csiltout = 0.0
      Cclayout = 0.0
      Csanddiv = 0.0
      Csiltdiv = 0.0
      Cclaydiv = 0.0
      Csandtf = 0.0
      Csilttf = 0.0
      Cclaytf = 0.0
      Ctnin = 0.0
      Ctnout = 0.0
      Ctndiv = 0.0
      Ctntf = 0.0
      Ctnbrbod = 0.0
      Ctnbods = 0.0
      Ctnbenal = 0.0
      Ctntamscr = 0.0
      Ctnbrtam = 0.0
      Ctntamvol = 0.0
      Ctnnitden = 0.0
      Ctnrefset = 0.0
      Ctnphyset = 0.0
      Ctpin = 0.0
      Ctpout = 0.0
      Ctpdiv = 0.0
      Ctptf = 0.0
      Ctpbrbod = 0.0
      Ctpbods = 0.0
      Ctpbenal = 0.0
      Ctppo4scr = 0.0
      Ctpbrpo4 = 0.0
      Ctprefset = 0.0
      Ctpphyset = 0.0

***************** loop over segments and get values
******** store in variables and add to accumulation variables
      do nr = 1,nrsegs
        if (.not.upstream(nr)) cycle

********** only add outputs if pour point(s) of the watershed
        if (rsegs(nr).eq.cseg) then  
          Csandout = Csandout + sandout(nr) 
          Csiltout = Csiltout + siltout(nr) 
          Cclayout = Cclayout + clayout(nr) 
          Ctnout = Ctnout + tnout(nr) 
          Ctpout = Ctpout + tpout(nr) 
        end if
        if (cseg(10:13).eq.'0003') then
          if (rsegs(nr)(10:13).eq.cseg(5:8)) then
            Csandout = Csandout + sandout(nr)
            Csiltout = Csiltout + siltout(nr)
            Cclayout = Cclayout + clayout(nr)
            Ctnout = Ctnout + tnout(nr)
            Ctpout = Ctpout + tpout(nr)
          end if
        end if

*********** get EOS inputs only by getting rid of upstream inputs
        Csandin = Csandin + sandin(nr) 
        Csiltin = Csiltin + siltin(nr) 
        Cclayin = Cclayin + clayin(nr) 
        Ctnin = Ctnin + tnin(nr) 
        Ctpin = Ctpin + tpin(nr) 
        do nr2 = 1,nrsegs
          if (rsegs(nr2)(10:13).eq.rsegs(nr)(5:8)) then
            Csandin = Csandin - sandout(nr2) 
            Csiltin = Csiltin - siltout(nr2) 
            Cclayin = Cclayin - clayout(nr2) 
            Ctnin = Ctnin - tnout(nr2) 
            Ctpin = Ctpin - tpout(nr2) 
          end if
        end do

************ fluxes are purely cumulative
        Csanddiv = Csanddiv +  sanddiv(nr) 
        Csiltdiv = Csiltdiv +  siltdiv(nr) 
        Cclaydiv = Cclaydiv +  claydiv(nr) 
        Ctnbrbod = Ctnbrbod + tnbrbod(nr) 
        Ctnbods = Ctnbods + tnbods(nr) 
        Ctnbenal = Ctnbenal + tnbenal(nr) 
        Ctntamscr = Ctntamscr + tntamscr(nr) 
        Ctnbrtam = Ctnbrtam + tnbrtam(nr) 
        Ctntamvol = Ctntamvol + tntamvol(nr) 
        Ctnnitden = Ctnnitden + tnnitden(nr) 
        Ctnrefset = Ctnrefset + tnrefset(nr) 
        Ctnphyset = Ctnphyset +  tnphyset(nr) 
        Ctndiv = Ctndiv +  tndiv(nr) 
        Ctpbrbod = Ctpbrbod + tpbrbod(nr) 
        Ctpbods = Ctpbods + tpbods(nr) 
        Ctpbenal = Ctpbenal + tpbenal(nr) 
        Ctppo4scr = Ctppo4scr + tppo4scr(nr) 
        Ctpbrpo4 = Ctpbrpo4 + tpbrpo4(nr) 
        Ctprefset = Ctprefset + tprefset(nr) 
        Ctpphyset = Ctpphyset + tpphyset(nr) 
        Ctpdiv = Ctpdiv + tpdiv(nr) 
      end do

************** recalculate transport factors, since they do not add
      Csandtf = Csandout/Csandin
      Csilttf = Csiltout/Csiltin
      Cclaytf = Cclayout/Cclayin
      Ctntf = Ctnout/Ctnin
      Ctptf = Ctpout/Ctpin

**************** recalculate fluxes as fractions of inputs
      Csanddiv = Csanddiv / Csandin
      Csiltdiv = Csiltdiv / Csiltin
      Cclaydiv = Cclaydiv / Cclayin
      Ctnbrbod = Ctnbrbod / Ctnin
      Ctnbods = Ctnbods / Ctnin
      Ctnbenal = Ctnbenal / Ctnin
      Ctntamscr = Ctntamscr / Ctnin
      Ctnbrtam = Ctnbrtam / Ctnin
      Ctntamvol = Ctntamvol / Ctnin
      Ctnnitden = Ctnnitden / Ctnin
      Ctnrefset = Ctnrefset / Ctnin
      Ctnphyset = Ctnphyset / Ctnin
      Ctndiv = Ctndiv / Ctnin
      Ctpbrbod = Ctpbrbod / Ctpin
      Ctpbods = Ctpbods / Ctpin
      Ctpbenal = Ctpbenal / Ctpin
      Ctppo4scr = Ctppo4scr / Ctpin
      Ctpbrpo4 = Ctpbrpo4 / Ctpin
      Ctprefset = Ctprefset / Ctpin
      Ctpphyset = Ctpphyset / Ctpin
      Ctpdiv = Ctpdiv / Ctpin

      return

************* ERROR SPACE ****************
999   call stopreport(report)

      end

