F90=gfortran
#FFLAGS=-std=f2008 -ffree-form -ffree-line-length-280 -O3 -fopenmp -fopenmp-simd -march=native   #-pg #-fopt-info #-g
FFLAGS=-std=f2008 -ffree-form -ffree-line-length-280 -O3 -fopenmp -fopenmp-simd -march=native    #-pg #-fopt-info #-g

NV90=nvfortran
NVFFLAGS=-O3 -fast -tp=skylake -mp=multicore -Minfo
IFX=ifx
IFXFFLAGS=-Ofast -xcore-avx2 -qopenmp-simd -qopenmp -warn -qopt-report
IF=ifort
IFFLAGS=-Ofast -qopen-simd -qopenmp -xcore-avx2 -warn
LIBS=-I/usr/include -lnsl -L/lib/x86_64-linux-gnu
#-lmpi -lfftw3 -lm  -lcfitsio 

all: 1brc_sshestov
#	$(F90) $(FFLAGS) test_mmap_file.f90 -o test_mmap $(LIBS)

1brc_sshestov: 1brc_shestov.f90 get_array
	$(F90) $(FFLAGS) 1brc_shestov.f90 -o 1brc_shestov get_array.o $(LIBS)

nv: 1brc_shestov.f90
	$(NV90) $(NVFFLAGS) 1brc_shestov.f90 -o 1brc_nv

ifx: 1brc_shestov.f90 get_array
	$(IFX) $(IFXFFLAGS) 1brc_shestov.f90 -o 1brc_ifx get_array.o

ifort: 1brc_shestov.f90 get_array
	$(IF) $(IFFLAGS) 1brc_shestov.f90 -o 1brc_ifort get_array.o

2brc: 2brc_shestov.f90 get_array
	$(F90) $(FFLAGS) 2brc_shestov.f90 -o 2brc_shestov get_array.o $(LIBS)
	#$(IFX) $(IFXFFLAGS) 2brc_shestov.f90 -o 2brc_shestov get_array.o $(LIBS)

test_simd: test_simd.f90
	$(F90) $(FFLAGS) test_simd.f90 -o test_simd $(LIBS)

test_mmap: test_mmap_file.f90 get_array.o 
	$(F90) $(FFLAGS) test_mmap_file.f90 -o test_mmap get_array.o $(LIBS)

test_c: test_mmap_file.c
	gcc -g test_mmap_file.c -o test_c $(LIBS)

get_array: get_array.c
	gcc -O3 -c get_array.c -o get_array.o $(LIBS)
