FC = gfortran
FFLAGS ?= -O3 -march=native -ffast-math -funroll-loops -std=f2018

.PHONY: all clean run
all: shapes

shapes: shapes.f90
	$(FC) $(FFLAGS) $< -o $@

run: shapes
	./shapes

clean:
	rm -f shapes *.o *.mod
