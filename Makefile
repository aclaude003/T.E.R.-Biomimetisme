F90=gfortran
Flags= -O0 -Wall -fcheck=all -ffpe-trap=invalid -pedantic
all:exec
exec:constantes_mod.o RK4mod.o resolution_mod.o main.o
	$(F90) $(Flags)  -o exec $^
    %.o : %.f90
	$(F90) $(Flags)  -c $^
clean:
	rm -f *.o *.mod $(EXE)
