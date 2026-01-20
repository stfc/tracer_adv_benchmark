# Makefile for the tracer-advection benchmark.
#

F90 ?= gfortran

FORT_FLAGS ?= -fopenmp

tra_adv.exe: tra_adv.o
		${F90} ${FORT_FLAGS} -o $@ tra_adv.o ${LDFLAGS}

tra_adv_1d.exe: tra_adv_1d.o
		${F90} ${FORT_FLAGS} -o $@ tra_adv_1d.o ${LDFLAGS}

%.o: %.f90
		${F90} ${FORT_FLAGS} -c $<
%.o: %.F90
		${F90} ${FORT_FLAGS} -c $<

clean:
		rm -f *.o *.mod
		rm -f *~

allclean: clean
		rm -f output.dat tra_adv.exe tra_adv_1d.exe
