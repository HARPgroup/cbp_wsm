###########################  DEFINITIONS ###################

 wlibdir = /usr/opt/wrdapp/lib3.2/lib/
 wlibs = $(wlibdir)/wdmlib.a $(wlibdir)/adwdmlib.a $(wlibdir)/utillib.a

 ulibdir = ../../../lib/
 dlib = $(ulibdir)/dsn_utils/dsn_utils.o 
 ulib = $(ulibdir)/utils/util_lib.a 
 glib = $(ulibdir)/utils/get_lib.a 
 xlib = $(ulibdir)/utils/ttyux.o
 
 OBJS =  main.o Rmasslink.o readcontrol_swdm.o getupstream.o checkflow.o checktime.o addRwdm.o conowout.o conow_outflow/conreg.o conow_outflow/getrules.o conow_outflow/iwkday.o conow_outflow/table.o

 localinc = stream_wdm.inc

 globalinc = $(ulibdir)/inc/standard.inc $(ulibdir)/inc/locations.inc $(ulibdir)/inc/wdm.inc

 EXE = ../../bin/stream_wdm.exe

############## DEPENDENCIES #####################

 $(EXE) : $(OBJS) $(ulibs) $(wlibs)
	f77 -o $@ $(OBJS) $(ulibs) $(wlibs)

 $(OBJS) : $(localinc) $(globalinc)

########## STANDARD DEPENDENCIES - NOT ALL USED  ##################

 $dlib : $(ulibdir)/dsn_utils/dsn_utils.f $(ulibdir)/inc/standard.inc $ulibdir)/inc/wdm.inc
	f77 -o $@ $(ulibdir)/dsn_utils/dsn_utils.f


