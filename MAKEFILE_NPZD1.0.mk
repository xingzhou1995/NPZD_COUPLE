# Makefile for NPZD
#
#
#

# Compiler, linker, compilation and optimization options

COMPILER = gfortran
LINKER   = gfortran
CFLAGS   = -c
DFLAGS   = -o
# PROGRAM NAME
MAINFILES  = MAIN.f90
SUBFILES   = NPZD_READ.f90  
MODFILES   = NPZD_CONVEY.f90 \
             NPZD_INPUT.f90 \
						 bio_parameter.f90 \
             bio_process.f90 \
             phy_parameter.f90 \
             phy_process.f90 
BIOFILES   = NPZD_BIOLOGY.f90
PHYFILES   = NPZD_PHYSICS.f90
PREFILES   = generate_NPZD_namelist.f90


# Varibles
#MAINPROGRAM = $(echo "${MAINFILES}" | awk -F '.' '{ print $1 }')
#SUBPROGRAM  = $(echo "${SUBFILES}" | awk -F '.' '{ print $1 }')
#MODPROGRAM  = $(echo "${MODFILES}" | awk -F '.' '{ print $1 }')
#PREPROGRAM  = $(echo "$(PREFILES)" | awk -F '.' '{ print $1 }')
MAINPROGRAM  = MAIN
PREPROGRAM   = generate_NPZD_namelist
MODPROGRAM   = bio_parameter
OBJS        = $(MAINPROGRAM).o $(MODPROGRAM).o
EXENAME     = NPZD


all:
	make clean
	make NML
	make MAIN

#all:
#	make clean
#	make NML
#	make MOD
#	make MAIN
#	make LINK

#LINK:	$(LINKER) $(OBJS) $(DFLAGS) $(EXENAME).x

#MAIN:
		
#	$(COMPILER) $(MODFILES) $(DFLAGS) $(MODPROGRAM).o
#	$(COMPILER) $(MAINFILES) $(SUBFILES) $(DFLAGS) $(MAINPROGRAM).o
#	$(COMPILER) $(OBJS) -lm -o $(EXENAME).x		
#	$(COMPILER) $(MAINFILES) $(MODFILES) $(SUBFILES) $(DFLAGS) $(EXENAME).x

#	MAKEFILE 1.0 DEBUG 1

#$(EXENAME).x: $(MAINPROGRAM).o $(MODPROGRAM).mod
#LINK:
#		$(COMPILER) $(OBJS) -lm -o $(EXENAME).x

#$(MAINPROGRAM).o: $(MAINFILES) $(SUBFILES)
#MAIN:
#		$(COMPILER) $(MAINFILES) $(SUBFILES) -c $(DFLAGS) $(MAINPROGRAM).o

#$(MODPROGRAM).mod: $(MODFILES)
#MOD:
#		$(COMPILER) $(MODFILES) -c $(DFLAGS) $(MODPROGRAM).o
##############################################################################

MAIN:
		$(COMPILER) $(MODFILES) $(MAINFILES) $(SUBFILES) $(BIOFILES) $(PHYFILES) $(DFLAGS) $(EXENAME).x

NML:
		$(COMPILER) $(PREFILES) $(DFLAGS) $(PREPROGRAM).x  
		./$(PREPROGRAM).x	

clean:
	rm -f *.mod	
	rm -f *.o	
	rm -f *.x
	rm -f *.nml
       
