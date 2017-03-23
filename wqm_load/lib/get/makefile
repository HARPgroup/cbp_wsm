
OBJS = checkriver.o getdsn.o getl2r.o findIFhave.o getrflags.o getevap.o getwdm.o



../get_lib.a : ${OBJS}
	ar rv ${OBJS}

checkriver.o : checkriver.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c checkriver.f

getdsn.o : getdsn.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c getdsn.f

getl2r.o : getl2r.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c getl2r.f

findIFhave.o : findIFhave.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c findIFhave.f

getrflags.o : getrflags.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c getrflags.f

getevap.o : getevap.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c getevap.f

getwdm.o : getwdm.f ../../inc/standard.inc ../../inc/locations.inc
	f77 -c getwdm.f



#!/bin/csh

f77 -c checkriver.f
f77 -c getdsn.f
f77 -c getl2r.f
f77 -c findIFhave.f
f77 -c getrflags.f
f77 -c getevap.f
f77 -c getwdm.f

ar rv ../get_lib.a checkriver.o getdsn.o getl2r.o findIFhave.o getrflags.o getevap.o getwdm.o
