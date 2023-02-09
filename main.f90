program lignescourant
  use constantes_mod
  use RK4mod
  implicit none
  real(PR), parameter :: h = 0.1_PR ! step size
  real(PR) :: x0, x_final, x, y0, y
  integer :: i, k,n




  open(unit=2,file='result.dat',status='Unknown')
  do k=1,m
    x0 = -100._PR
    y0 = k*1_Pr
    x_final = 100._Pr
    x = x0
    y = y0
    n = int((x_final - x0)/h + 0.5)
    write(2,*) x ,Y
      do i = 1, n
        call rk4_step(x, y, h)
        x = x + h
        write(2,*) x ,y
      end do
  end do
  do k=-m,-1
    x0 = -100._PR
    y0 = k*1_Pr
    x_final = 100._Pr
    x = x0
    y = y0
    n = int((x_final - x0)/h + 0.5)
    write(2,*) x ,Y
      do i = 1, n
        call rk4_step(x, y, h)
        x = x + h
        write(2,*) x ,y
      end do
  end do
  close (2)



















end program
