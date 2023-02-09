F90=gfortran

all:exec
exec:constantes_mod.o particule_mod.o newton_mod.o main.o
	$(F90)  -o exec $^
    %.o : %.f90
	$(F90)  -c $^
clean:
	rm -f *.o *.mod $(EXE)
