# Makefile for libraries and library utilities programs
#
# History:  95/12/05 kmflynn

# Usage:   make [LIBDIR=install_libs_in] [BINDIR=install_binary_in] [install] [| tee makeout]
#
# Example:  make
#           make BINDIR=/usr/opt/bin LIBDIR=/usr/lib
#           make BINDIR=$HOME/bin

# this description file uses the Bourne shell
SHELL = /bin/sh
shCmd = sh

# Sequential file, installation specific
# where parm[dg ux pc].seq sets computer type, graphics parameters
SeqDG   = dg
SeqSun  = ux
SeqSGI  = ux
SeqRISC = ux
SeqDec  = ux
SeqHP   = ux
# SeqVrsn = $(SeqDG)
 SeqVrsn = $(SeqSun)
# by pw

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

BINDIR = ../bin
LIBDIR = ../lib
binary = ../bin
library= ../lib

# libraries and programs
Libs = utillib.a      adwdmlib.a  wdmlib.a    \
       aidelib.a      graphlib.a  statslib.a  \
       waidelib.a     awstatlib.a annlib.a    \
       hspflib.a      newaqtlib.a             \
       hspnodsslib.a
LibH = hspdsslib.a    heclib.a
Pgms = wdimex     wdmrx
Msgg = ../lib_data/message.wdm
Msgh = ../lib_data/hspfmsg.wdm

# sequential files:
SeqAtr = ../msg/adwdm/attr001.seq  ../msg/adwdm/attr051.seq  ../msg/adwdm/attr101.seq \
         ../msg/adwdm/attr151.seq  ../msg/adwdm/attr201.seq  ../msg/adwdm/attr251.seq \
         ../msg/adwdm/attr301.seq  ../msg/adwdm/attr351.seq  ../msg/adwdm/attr401.seq \
         ../msg/aide/message.seq   ../msg/aide/parm$(SeqVrsn).seq
SeqMsg = ../msg/waide/awfeb.seq    ../msg/waide/prwfil.seq   ../msg/waide/atinfo.seq
SeqAwa = ../msg/waide/awfeb.seq    ../msg/waide/tslist.seq   ../msg/waide/agplot.seq  \
         ../msg/awstat/tscmpr.seq  ../msg/awstat/a193.seq    ../msg/awstat/profdr.seq \
         ../msg/ann/pgener.seq     ../msg/ann/qtprnt.seq
SeqHsp = ../msg/hspf/copy.seq      ../msg/hspf/copyts.seq    ../msg/hspf/disply.seq   \
         ../msg/hspf/displyts.seq  ../msg/hspf/duranl.seq    ../msg/hspf/duranlts.seq \
         ../msg/hspf/gener.seq     ../msg/hspf/generts.seq   ../msg/hspf/hdatut.seq   \
         ../msg/hspf/hdssx.seq     ../msg/hspf/himp.seq                               \
         ../msg/hspf/himpqua.seq   ../msg/hspf/himpwat.seq   ../msg/hspf/hper.seq     \
         ../msg/hspf/hperagut.seq  ../msg/hspf/hpernit.seq   ../msg/hspf/hperpes.seq  \
         ../msg/hspf/hperpho.seq   ../msg/hspf/hperqua.seq   ../msg/hspf/hpersno.seq  \
         ../msg/hspf/hperwat.seq   ../msg/hspf/hprbut.seq    ../msg/hspf/hrch.seq     \
         ../msg/hspf/hrchaci.seq   ../msg/hspf/hrchgqu.seq   ../msg/hspf/hrchhtr.seq  \
         ../msg/hspf/hrchhyd.seq   ../msg/hspf/hrchnut.seq   ../msg/hspf/hrchphc.seq  \
         ../msg/hspf/hrchplk.seq   ../msg/hspf/hrchsed.seq                            \
         ../msg/hspf/hringen.seq   ../msg/hspf/hringeut.seq  ../msg/hspf/hrinoput.seq \
         ../msg/hspf/hrinseq.seq   ../msg/hspf/hrints.seq    ../msg/hspf/hrintss.seq  \
         ../msg/hspf/hrinwdm.seq   ../msg/hspf/hruntsgp.seq                           \
         ../msg/hspf/hruntsgq.seq  ../msg/hspf/hruntsgt.seq  ../msg/hspf/hruntsgw.seq \
         ../msg/hspf/hruntspt.seq  ../msg/hspf/hruntspw.seq                           \
         ../msg/hspf/hruntsut.seq  ../msg/hspf/hrunut.seq    ../msg/hspf/hspf.seq     \
         ../msg/hspf/hspfec.seq    ../msg/hspf/hspfitab.seq  ../msg/hspf/htsinsi.seq  \
         ../msg/hspf/htssut.seq    ../msg/hspf/hutdura.seq   ../msg/hspf/hutop.seq    \
         ../msg/hspf/hutopinp.seq  ../msg/hspf/hwdmut.seq    ../msg/hspf/implnd.seq   \
         ../msg/hspf/implndts.seq  ../msg/hspf/mutsin.seq    ../msg/hspf/mutsints.seq \
         ../msg/hspf/perlnd.seq    ../msg/hspf/perlndts.seq  ../msg/hspf/pltgen.seq   \
         ../msg/hspf/pltgents.seq  ../msg/hspf/rchres.seq    ../msg/hspf/rchrests.seq \
         ../msg/hspf/specact.seq   ../msg/hspf/hiouci.seq    ../msg/hspf/hdssut.seq   \
         ../msg/hspf/hrindss.seq   ../msg/hspf/hruntsgd.seq  ../msg/hspf/hruntspd.seq
SeqNaq = ../msg/newaqt/agmap.seq   ../msg/newaqt/dspeci.seq  ../msg/newaqt/durani.seq \
         ../msg/newaqt/prwplt.seq  ../msg/newaqt/sgtabl.seq  ../msg/newaqt/tsplot.seq \
         ../msg/newaqt/ucimod.seq  ../msg/newaqt/ucirea.seq  ../msg/newaqt/wsgsim.seq \
         ../msg/newaqt/wsgsys.seq  ../msg/newaqt/wsgutl.seq  ../msg/newaqt/tsfreq.seq \
         ../msg/newaqt/sturbn.seq

# rules
all:  first $(Libs) utility_pgms $(Msgg) $(Msgh) install
more: first $(Libs) $(LibH) utility_pgms $(Msgg) $(Msgh) install

# make sure all of the libraries and utility programs are current
utillib.a:     ; cd util     ; make libdir=../$(LIBDIR)
adwdmlib.a:    ; cd adwdm    ; make libdir=../$(LIBDIR)
wdmlib.a:      ; cd wdm      ; make libdir=../$(LIBDIR)
aidelib.a:     ; cd aide     ; make libdir=../$(LIBDIR)
graphlib.a:    ; cd graph    ; make libdir=../$(LIBDIR)
statslib.a:    ; cd stats    ; make libdir=../$(LIBDIR)
waidelib.a:    ; cd waide    ; make libdir=../$(LIBDIR)
awstatlib.a:   ; cd awstat   ; make libdir=../$(LIBDIR)
annlib.a:      ; cd ann      ; make libdir=../$(LIBDIR)
hspflib.a:     ; cd hspf     ; make libdir=../$(LIBDIR)
newaqtlib.a:   ; cd newaqt   ; make libdir=../$(LIBDIR)
hspnodsslib.a: ; cd hspnodss ; make libdir=../$(LIBDIR)
hspdsslib.a:   ; cd hspdss   ; make libdir=../$(LIBDIR)
heclib.a:      ; cd hec      ; make libdir=../$(LIBDIR)
utility_pgms:
	cd wdimex ; make bindir=../$(BINDIR)
	cd wdmrx  ; make bindir=../$(BINDIR)

# make sure message files are current
$(Msgg): $(SeqAtr) $(SeqMsg) ../msg/wdimex.sh
	rm -f $(Msgg)
	cd ../msg ; rm -f message.out  ; $(shCmd) ./wdimex.sh message $(SeqVrsn) > message.out
$(Msgh): $(SeqAtr) $(SeqAwa) $(SeqHsp) $(SeqNaq) ../msg/wdimex.sh
	rm -f $(Msgh)
	cd ../msg ; rm -f hspf.out ; $(shCmd) ./wdimex.sh hspf $(SeqVrsn) > hspf.out

# Create directories for libraries, executables, and message files, if needed
first:  
	@if [ ! -d ../lib ]   ; then             \
	   mkdir ../lib  ;                       \
	   echo  Created directory ../lib ;      \
	fi
	@if [ ! -d ../bin ]   ; then             \
	   mkdir ../bin  ;                       \
	   echo  Created directory ../bin ;      \
	fi
	@if [ ! -d ../lib_data ]   ; then        \
	   mkdir ../lib_data  ;                  \
	   echo  Created directory ../lib_data ; \
        fi
install:
	@if [ ! -d $(LIBDIR) ]  ; then           \
	   mkdir $(LIBDIR) ;                     \
	   echo Created directory $(LIBDIR) ;    \
	fi
	@for lib in $(Libs) ; do                                \
	   if [ ! -s ../lib/$$lib ] ; then                      \
	      echo "--NOTICE:  library $$lib does not exist"  ; \
	      echo "use one of the following to generate it:" ; \
	      echo "     make"     ;                            \
	      echo "     make all" ;                            \
	   else                                                 \
	      if [ $(LIBDIR) != $(library) ] ; then             \
	         rm -f $(LIBDIR)/$$lib ;                        \
	         cd ..; ln -s `pwd`/lib/$$lib $(LIBDIR)/$$lib ; \
	         chmod 755 $(LIBDIR)/$$lib ;                    \
	         echo "\n"Library $$lib has been updated. ;     \
	      fi ;                                              \
	   fi                                                   \
	done
	@if [ ! -d $(BINDIR) ]  ; then           \
	   mkdir $(BINDIR) ;                     \
	   echo Created directory $(BINDIR) ;    \
	fi
	@for pgm in $(Pgms) ; do                                \
	   if [ ! -s ../bin/$$pgm ] ; then                      \
	      echo "--NOTICE:  program $$pgm does not exist"  ; \
	      echo "use one of the following to generate it:" ; \
	      echo "     make"     ;                            \
	      echo "     make all" ;                            \
	   else                                                 \
	      if [ $(BINDIR) != $(binary) ] ; then              \
	         rm -f $(BINDIR)/$$pgm ;                        \
	         cd ..; ln -s `pwd`/bin/$$pgm $(BINDIR)/$$pgm ; \
	         chmod 755 $(BINDIR)/$$pgm ;                    \
	         echo "\n"Program $$pgm has been updated. ;     \
	      fi ;                                              \
	   fi                                                   \
	done

# end of make
