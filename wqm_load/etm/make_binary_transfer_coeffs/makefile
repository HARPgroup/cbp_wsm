include ../../../workspace

INCDIR = $(workspace)/pp/lib/inc/
UTDIR = $(workspace)/pp/lib/utils/
PWD = $(workspace)/pp/etm/src/make_binary_transfer_coeffs/
INC = $(INCDIR)/locations.inc $(INCDIR)/standard.inc $(INCDIR)/land_use.inc $(INCDIR)/bmp_constituents.inc mbtc.inc

EXE = $(workspace)/pp/etm/bin/make_binary_transfer_coeffs.exe
OBJ = getbmpbreaks.o getlandbreaks.o getlsegs.o main.o maketransfile.o readcontrol.o sortB.o
OBJf = getbmpbreaks.f getlandbreaks.f getlsegs.f main.f maketransfile.f readcontrol.f sortB.f
UT  = $(UTDIR)/date.o $(UTDIR)/comment.o $(UTDIR)/trim.o $(UTDIR)/read_csv_line_300.o
UTf  = $(UTDIR)/date.f $(UTDIR)/comment.f $(UTDIR)/trim.f $(UTDIR)/read_csv_line_300.f

$(EXE) : $(OBJ) $(INC) $(UT) $(INC)
	f77 -o $(EXE) $(OBJ) $(UT)

$(OBJ):  $(INC) $(OBJf)
	f77 -c $(OBJf)

$(UT):  $(UTf)
	f77 -c $(UTf)

