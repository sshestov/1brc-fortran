My trial of the 1brc challenge in Fortran. 

I have been using Fortran occasionally for HPC calculations and thought "why not".

The ideas are: 
 - mmap the file;
 - split the file in chunks and do OpenMP over chunks;
 - find position of next ';'  and use a hash to get id;
 - do branchless scan of temperature

My typical time is around 3 s on 16 threads. Under -O3 the compiler does a lot of optimization and vectorization. While AVX instructions are not used, though. I've tried several tricks to speed-up individual places, in particular tried to convince the compiler to use SIMD for FINDLOC. Did not help at all.

Make sure to rename or make a symlink the input data file (measurements_100k.txt from this repo) as `measurements.txt` and have it in the `cwd`.

The major time is taken by `findloc(huge_array, semicolon)` function (~1.6 s out of 3 s).

There was quite intense [discussions](https://fortran-lang.discourse.group/t/1-billion-row-challenge-in-fortran-mmap-openmp-branchless-scan/7254) of how to improve `findloc`, but nothing helped.

Tried `gfortran`, `nvfortran`, `ifx` and `ifort`; `gfortran` gives much faster (~3 s vs ~12 s) than the others.
