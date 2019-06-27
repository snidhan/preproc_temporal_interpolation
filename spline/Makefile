#Name - Sheel Nidhan
#Date - 20th June, 2019

programs = main.f90 io_data_files.f90 spline.f90
files    = main.o io_data_files.o spline.o

OPT = -O3 
#DEBUG = -g -traceback
EXTRA = -heap-arrays 100000
fcomps= ifort $(OPT) $(EXTRA) $(DEBUG)

# Making the Makefile

output: $(files) 
	$(fcomps) $(INCL) -o output $(files)

io_data_files.o: io_data_files.f90 
	$(fcomps) -c io_data_files.f90

spline.o: spline.f90
	$(fcomps) -c spline.f90

main.o: main.f90 
	$(fcomps) -c main.f90

clean:
	rm -f *.o *.mod
