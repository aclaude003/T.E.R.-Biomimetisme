F90=gfortran

all:exec
exec:constantes_mod.o RK4mod.o main.o
	$(F90)  -o exec $^
    %.o : %.f90
	$(F90)  -c $^
clean:
	rm -f *.o *.mod $(EXE)
