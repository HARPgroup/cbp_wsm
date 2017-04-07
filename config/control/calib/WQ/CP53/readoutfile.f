************************************************************************
** program to read the .out file, which is sometimes easier to deal   **
**   with than pltgens                                                **
************************************************************************
      subroutine read_outfile(
     I                        rseg,rscen,year1,year2,
     O                        sandinA,siltinA,clayinA,
     O                        sandoutA,siltoutA,clayoutA,
     O                        sanddivA,siltdivA,claydivA,
     O                        sandtf,silttf,claytf,
     O                        tnin,tnout,tntf,
     O                        tnbrbod,tnbods,tnbenal,tntamscr,tnbrtam,
     O                        tntamvol,tnnitden,tnrefset,tnphyset,tndiv,
     O                        tpin,tpout,tptf,
     O                        tpbrbod,tpbods,tpbenal,tppo4scr,tpbrpo4,
     O                        tprefset,tpphyset,tpdiv,
     O                        overscour)
      implicit none
      include '../../../../../code/src/lib/inc/standard.inc'
      include '../../../../../code/src/lib/inc/locations.inc'

      character*140 outline

      integer years,ny,year1,year2,i

      real bod2orgn,bod2orgp,bod2orgc
      parameter (bod2orgn=0.0436,bod2orgp=0.00603,bod2orgc=0.2475)
      real phy2orgn,phy2orgp,phy2orgc
      parameter (phy2orgn=0.0863,phy2orgp=0.0119,phy2orgc=0.49)

      logical found(year1:year2)

      real average
      external average

      logical fourexits

****************** READING VARIABLES ***********************************
      real xdum,d1,d2,d3,d4,d5,d6

************* SEDIMENT
      real sandin(year1:year2),siltin(year1:year2),clayin(year1:year2)
      real sandout(year1:year2),siltout(year1:year2)
      real clayout(year1:year2)
      real sanddiv(year1:year2),siltdiv(year1:year2)
      real claydiv(year1:year2)

      logical overscour
      real siltbed(year1:year2),claybed(year1:year2)

************* OXYGEN
      real oxin(year1:year2),oxout(year1:year2)
      real oxrear(year1:year2),oxbod(year1:year2),oxben(year1:year2),
     .     oxni(year1:year2),oxphyt(year1:year2),oxbenal(year1:year2),
     .     oxdiv(year1:year2)

************* BOD
      real bodin(year1:year2),bodout(year1:year2)
      real bodd(year1:year2),bodr(year1:year2),bods(year1:year2),
     .     bodphyt(year1:year2),bodbenal(year1:year2),
     .     boddiv(year1:year2)

************* AMMONIA
      real tamin(year1:year2),tamout(year1:year2)
      real tamdep(year1:year2)
      real tamni(year1:year2),tamvol(year1:year2),tamben(year1:year2),
     .     tambod(year1:year2),tamphy(year1:year2),
     .     tambenal(year1:year2),tamdiv(year1:year2)

************* NITRATE
      real nitin(year1:year2),nitout(year1:year2)
      real nitni(year1:year2),nitden(year1:year2),
     .     nitphy(year1:year2),nitbenal(year1:year2),nitdiv(year1:year2)

************* PHOSPHATE
      real po4in(year1:year2),po4out(year1:year2)
      real po4dep(year1:year2)
      real po4ben(year1:year2),po4bod(year1:year2),po4phy(year1:year2),
     .     po4benal(year1:year2),po4div(year1:year2)

************** PLANK
      real phyin(year1:year2),phyout(year1:year2),phydiv(year1:year2)
      real rornin(year1:year2),rornout(year1:year2),rorndiv(year1:year2)
      real rorpin(year1:year2),rorpout(year1:year2),rorpdiv(year1:year2)
      real rorcin(year1:year2),rorcout(year1:year2),rorcdiv(year1:year2)

*************** OUTPUT VARIABLES
********* sediment delivery factors
      real sandinA,siltinA,clayinA
      real sandoutA,siltoutA,clayoutA
      real sanddivA,siltdivA,claydivA
      real sandtf,silttf,claytf

************ TN Balance
      real tnin,tnout,tntf
      real tnbrbod,tnbods,tnbenal,tntamscr,tnbrtam
      real tntamvol,tnnitden,tnrefset,tnphyset,tndiv

************ TP Balance
      real tpin,tpout,tptf
      real tpbrbod,tpbods,tpbenal,tppo4scr,tpbrpo4
      real tprefset,tpphyset,tpdiv

*************** END DECLARATIONS ***************************************

      call lencl(rscen,lenrscen)
      do ny = year1,year2
        found(ny) = .false.
      end do

      fourexits = .false.
       
**********            OPEN FILE
      fnam = tree//'output/hspf/river/out/'//rscen(:lenrscen)//
     .       '/'//rseg//'.out'
      open(dfile,file=fnam,status='old',iostat=err)
      if (err.ne.0) go to 991

**********     loop over lines look for meaningful lines
      do  
        read(dfile,'(a140)',err=994,end=111) outline 

        if (outline(113:116).ne.'YEAR') cycle
        read(outline(125:128),'(i4)') ny

        if (ny.lt.year1.or.ny.gt.year2) cycle

        found(ny) = .true.

        do while (outline(1:1).ne.'1')
          read(dfile,'(a140)',err=994,end=111) outline

************* SEDIMENT

          if (outline(15:17).eq.'BED') then
            read(outline(52:71),*) siltbed(ny),claybed(ny)
          end if

          if (outline(6:18).eq.'TOTAL INFLOWS') then
            read(outline(42:71),*) sandin(ny),siltin(ny),clayin(ny)
            do i = 1,5
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:71),*) sanddiv(ny),siltdiv(ny),claydiv(ny)
            read(dfile,'(a140)',err=994,end=111) outline  ! exit 2
            read(outline(42:71),*) d1,d2,d3 
            sanddiv(ny) = sanddiv(ny) + d1
            siltdiv(ny) = siltdiv(ny) + d2
            claydiv(ny) = claydiv(ny) + d3
            read(dfile,'(a140)',err=994,end=111) outline  ! exit 3
            read(outline(42:71),*) sandout(ny),siltout(ny),clayout(ny)
            read(dfile,'(a140)',err=994,end=111) outline  ! exit 4?
            if (outline(8:13).eq.'EXIT 4') then
              fourexits = .true.
              read(outline(42:71),*) d1,d2,d3 
              sandout(ny) = sandout(ny) + d1
              siltout(ny) = siltout(ny) + d2
              clayout(ny) = clayout(ny) + d3
            end if
          end if

************* OXYGEN
          if (outline(4:19).eq.'DISSOLVED OXYGEN') then
            do i = 1,8
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(30:39),*) oxin(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(40:123),*) oxrear(ny),oxbod(ny),oxben(ny),
     .                              oxni(ny),oxphyt(ny),xdum,oxbenal(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            if (fourexits) then
              read(outline(30:87),*) d1,oxdiv(ny),d2,oxout(ny),d3
              oxout(ny) = oxout(ny) + d3
            else
              read(outline(30:75),*) d1,oxdiv(ny),d2,oxout(ny)
            end if
            oxdiv(ny) = oxdiv(ny) + d2
          end if

************* BOD
          if (outline(4:14).eq.'BIOCHEMICAL') then
            do i = 1,8
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(30:39),*) bodin(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(40:123),*) bodd(ny),bodr(ny),bods(ny),xdum,
     .                              bodphyt(ny),xdum,bodbenal(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            if (fourexits) then
              read(outline(30:87),*) d1,boddiv(ny),d2,bodout(ny),d3
              bodout(ny) = bodout(ny) + d3
            else
              read(outline(30:75),*) d1,boddiv(ny),d2,bodout(ny)
            end if
            boddiv(ny) = boddiv(ny) + d2
          end if

************* AMMONIA
          if (outline(6:24).eq.'TOTAL INFLOW OF TAM') then
            read(outline(42:51),*) tamin(ny)
            do i = 1,6
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(76:87),*) tamdep(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(40:123),*) tamni(ny),tamvol(ny),tamben(ny),
     .                     tambod(ny),tamphy(ny),xdum,tambenal(ny)
            do i = 1,10
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:51),*) tamdiv(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:51),*) d2
            tamdiv(ny) = tamdiv(ny) + d2
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:51),*) tamout(ny)
            if (fourexits) then
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(42:51),*) d4
              tamout(ny) = tamout(ny) + d4
            end if
            do i = 1,3
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:75),*) d1,d2,d3
            tamdiv(ny) = tamdiv(ny) + d1 + d2 + d3
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:75),*) d1,d2,d3
            tamdiv(ny) = tamdiv(ny) + d1 + d2 + d3
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:75),*) d1,d2,d3
            tamout(ny) = tamout(ny) + d1 + d2 + d3
            if (fourexits) then
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(42:75),*) d1,d2,d3
              tamout(ny) = tamout(ny) + d1 + d2 + d3
            end if
          end if

************* NITRATE
          if (outline(4:10).eq.'NITRATE') then
            do i = 1,8
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(30:39),*) nitin(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(40:111),*) nitni(ny),nitden(ny),xdum,
     .                              nitphy(ny),xdum,nitbenal(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            if (fourexits) then
              read(outline(30:87),*) d1,nitdiv(ny),d2,nitout(ny),d3
              nitout(ny) = nitout(ny) + d3
            else
              read(outline(30:75),*) d1,nitdiv(ny),d2,nitout(ny)
            end if
            nitdiv(ny) = nitdiv(ny) + d2
            read(outline(30:39),*) nitout(ny)
          end if

************* PHOSPHATE
          if (outline(4:18).eq.'ORTHO-PHOSPHATE') then
            do i = 1,21
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:51),*) po4in(ny)
            do i = 1,6
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(76:87),*) po4dep(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(40:99),*) po4ben(ny),po4bod(ny),po4phy(ny),
     .                                           xdum,po4benal(ny)
            do i = 1,10
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:51),*) po4div(ny)
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:51),*) d2
            po4div(ny) = po4div(ny) + d2
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:51),*) po4out(ny)
            if (fourexits) then
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(42:51),*) d4
              po4out(ny) = po4out(ny) + d4
            end if
            do i = 1,3
              read(dfile,'(a140)',err=994,end=111) outline
            end do
            read(outline(42:75),*) d1,d2,d3
            po4div(ny) = po4div(ny) + d1 + d2 + d3
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:75),*) d1,d2,d3
            po4div(ny) = po4div(ny) + d1 + d2 + d3
            read(dfile,'(a140)',err=994,end=111) outline
            read(outline(42:75),*) d1,d2,d3
            po4out(ny) = po4out(ny) + d1 + d2 + d3
            if (fourexits) then
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(42:75),*) d1,d2,d3
              po4out(ny) = po4out(ny) + d1 + d2 + d3
            end if
          end if

************** PLANK
          if (outline(6:24).eq.'PHYTOPLANKTON (LBS)') then
            if (fourexits) then
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:91),*) phyin(ny),d1,d2,d3,d4,d5
              phyout(ny) = d4 + d5
              phydiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:91),*) rornin(ny),d1,d2,d3,d4,d5
              rornout(ny) = d4 + d5
              rorndiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:91),*) rorpin(ny),d1,d2,d3,d4,d5
              rorpout(ny) = d4 + d5
              rorpdiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:91),*) rorcin(ny),d1,d2,d3,d4,d5
              rorcout(ny) = d4 + d5
              rorcdiv(ny) = d2 + d3
            else
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:81),*) phyin(ny),d1,d2,d3,phyout(ny)
              phydiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:81),*) rornin(ny),d1,d2,d3,rornout(ny)
              rorndiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:81),*) rorpin(ny),d1,d2,d3,rorpout(ny)
              rorpdiv(ny) = d2 + d3
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(dfile,'(a140)',err=994,end=111) outline
              read(outline(32:81),*) rorcin(ny),d1,d2,d3,rorcout(ny)
              rorcdiv(ny) = d2 + d3
            end if
          end if

        end do
      end do
111   close(dfile)
*********** got all preliminary variables

********** check for completeness
      do ny = year1,year2
        if (.not.found(ny)) go to 992
      end do
      years = year2-year1+1

************** calculate summary variables

********* sediment delivery factors
      sandinA  = average(sandin,years,years,err)
      sandoutA = average(sandout,years,years,err) 
      sanddivA = -average(sanddiv,years,years,err) 
      siltinA  = average(siltin,years,years,err)
      siltoutA = average(siltout,years,years,err) 
      siltdivA = -average(siltdiv,years,years,err) 
      clayinA  = average(clayin,years,years,err)
      clayoutA = average(clayout,years,years,err) 
      claydivA = -average(claydiv,years,years,err) 
      sandtf = average(sandout,years,years,err)/
     .         average(sandin,years,years,err)
      silttf = average(siltout,years,years,err)/
     .         average(siltin,years,years,err)
      claytf = average(clayout,years,years,err)/
     .         average(clayin,years,years,err)

********* major oxygen players
C      o2rear = average(rear,years,years,err)/
C     .         average(o2out,years,years,err)
C      o2bodo = average(bodo,years,years,err)/
C     .         average(o2out,years,years,err)
C        not really needed at the moment
      
************ TN Balance
      tnin = average(tamin,years,years,err) +
     .       average(nitin,years,years,err) + 
     .       average(rornin,years,years,err) + 
     .       average(bodin,years,years,err)*bod2orgn + 
     .       average(phyin,years,years,err)*phy2orgn
      tnout = average(tamout,years,years,err) +
     .       average(nitout,years,years,err) + 
     .       average(rornout,years,years,err) + 
     .       average(bodout,years,years,err)*bod2orgn + 
     .       average(phyout,years,years,err)*phy2orgn
      tntf = tnout / tnin

      tnbrbod = average(bodr,years,years,err)*bod2orgn
      tnbods = average(bods,years,years,err)*bod2orgn
      tnbenal = average(bodbenal,years,years,err)*bod2orgn
     .         + average(tambenal,years,years,err)
     .         + average(nitbenal,years,years,err)
      tntamscr = - average(tamdep,years,years,err)
      tnbrtam = average(tamben,years,years,err)
      tntamvol = average(tamvol,years,years,err)
      tnnitden = average(nitden,years,years,err)
      tnrefset = average(rornout,years,years,err)
     .         - average(rornin,years,years,err)
      tnphyset = average(phyout,years,years,err)*phy2orgn
     .         - average(phyin,years,years,err)*phy2orgn
     .         + average(tamphy,years,years,err)
     .         + average(nitphy,years,years,err)
      tndiv = -average(boddiv,years,years,err)*bod2orgn
     .      - average(tamdiv,years,years,err)
     .      - average(nitdiv,years,years,err)
     .      - average(phydiv,years,years,err)*phy2orgn
     .      - average(rorndiv,years,years,err)

************ TP Balance
      tpin = average(po4in,years,years,err) +
     .       average(rorpin,years,years,err) + 
     .       average(bodin,years,years,err)*bod2orgp + 
     .       average(phyin,years,years,err)*phy2orgp
      tpout = average(po4out,years,years,err) +
     .       average(rorpout,years,years,err) + 
     .       average(bodout,years,years,err)*bod2orgp + 
     .       average(phyout,years,years,err)*phy2orgp
      tptf = tpout / tpin

      tpbrbod = average(bodr,years,years,err)*bod2orgp
      tpbods = average(bods,years,years,err)*bod2orgp
      tpbenal = average(bodbenal,years,years,err)*bod2orgp
     .         + average(po4benal,years,years,err)
      tppo4scr = - average(po4dep,years,years,err)
      tpbrpo4 = average(po4ben,years,years,err)
      tprefset = average(rorpout,years,years,err)
     .         - average(rorpin,years,years,err)
      tpphyset = average(phyout,years,years,err)*phy2orgp
     .         - average(phyin,years,years,err)*phy2orgp
     .         + average(po4phy,years,years,err)
      tpdiv = -average(boddiv,years,years,err)*bod2orgp
     .      - average(po4div,years,years,err)
     .      - average(phydiv,years,years,err)*phy2orgp
     .      - average(rorpdiv,years,years,err)

********* DETERMINE IF OVERSCOURED
      overscour = .false.
      do ny = year1,year2
        if (siltbed(ny).lt.10) overscour = .true.
        if (claybed(ny).lt.10) overscour = .true.
        if (siltbed(year1)/siltbed(ny).gt.1000.0) overscour = .true.
        if (claybed(year1)/claybed(ny).gt.1000.0) overscour = .true.
      end do
      

      return

********************* ERROR SPACE **************************************
991   report(1) = 'could not open file'
      report(2) = fnam
      write(report(3),*) 'error = ',err
      go to 999

992   write(report(1),*) 'did not find year ',ny
      report(2) = 'in file'
      report(3) = fnam
      go to 999

994   report(1) = 'problem reading file'
      report(2) = fnam
      report(3) = 'near line:'//outline
      go to 999

999   call stopreport(report)

      end

