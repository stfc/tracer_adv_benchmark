# Makefile for the tracer-advection benchmark.
#

tra_adv.exe: tra_adv.o
		${F90} ${F90FLAGS} -o $@ tra_adv.o -lomp -lomptarget -fopenmp=libomp -fno-lto --offload-arch=gfx942 -mp -fopenmp-offload-mandatory -fopenmp-force-usm -lflang_rt.hostdevice -fopenmp=libomp

tra_adv_1d.exe: tra_adv_1d.o
		${F90} ${F90FLAGS} -o $@ tra_adv_1d.o -lomp -lomptarget -fopenmp=libomp -fno-lto --offload-arch=gfx942 -mp -fopenmp-offload-mandatory -fopenmp-force-usm -lflang_rt.hostdevice -fopenmp=libomp

%.o: %.f90
		${F90} -O3 -g -fsave-optimization-record -ffast-math -fopenmp=libomp --offload-arch=gfx942 -mp -fopenmp-offload-mandatory -fopenmp-force-usm -c $<
%.o: %.F90
		${F90} -O3 -g -fsave-optimization-record -ffast-math -fopenmp=libomp --offload-arch=gfx942 -mp -fopenmp-offload-mandatory -fopenmp-force-usm -c $<

clean:
		rm -f *.o *.mod
		rm -f *~

allclean: clean
		rm -f output.dat tra_adv.exe tra_adv_1d.exe
