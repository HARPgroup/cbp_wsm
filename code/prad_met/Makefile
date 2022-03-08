PROG1 =  wdm_insert_ALL

SRCS =	wdm_insert_ALL.f 

OBJS =	wdm_insert_ALL.o

LIBS =  /opt/model/p53/cbp_wsm/code/src/hspf/lib3.2/lib/utillib.a \
        /opt/model/p53/cbp_wsm/code/src/hspf/lib3.2/lib/wdmlib.a \
        /opt/model/p53/cbp_wsm/code/src/hspf/lib3.2/lib/adwdmlib.a \
        /opt/model/p53/cbp_wsm/code/src/lib/util_lib.a

FC = f77
FFLAGS = -W -Wall -pedantic -Wsurprising -fbounds-check 
#FFLAGS = -W -Wall -Wsurprising -pedantic    
LDFLAGS =   


all: $(PROG1)
	cp $(PROG1) ../../../bin/

$(PROG1): $(OBJS)
	$(FC) $(LDFLAGS) -o wdm_insert_ALL $(OBJS) $(LIBS)

clean:
	rm -f $(PROG1) $(OBJS)
