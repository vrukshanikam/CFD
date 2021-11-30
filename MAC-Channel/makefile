################################################################
#
#      Erweiterter Compile-File fuer das Uebersetzen und Laden
#      eines Hauptprogrammes und mehrerer Unterprogramme unter
#      Ausnutzung bestimmter Voreinstellungen, die im System
#      bereits vorhanden sind.
# 
#####################################################################


#FFLAGS= -qsource -C
FFLAGS= -O -Pk
#FFLAGS= -g
LDFLAGS=
LIBS=

OBJS= bcc.o bccst.o bcns.o bcnsst.o bco.o bcot.o bcov.o bct.o \
      bctst.o boundt.o ceqcp.o ceqvp.o conti.o delta.o energy.o \
      init.o mesh.o nseqso.o nseqvp.o open.o output.o restar.o \
      start.o state.o temalt.o teqso.o teqvp.o ticorr.o tigrad.o \
      velalt.o zone.o nusselt.o boundk.o

comp:	$(OBJS) 
	f77  $(FFLAGS) $(OBJS) $(LIBS) -o winglet
#####################################################################
