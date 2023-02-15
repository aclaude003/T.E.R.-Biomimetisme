program lignescourant
  use constantes_mod
  use RK4mod
  implicit none
  real(PR), parameter :: h = 0.1_PR ! Le pas de discrétisation de l'axe des abscisses
  real(PR) :: x0, x_final, x, y0, y
  integer :: i, k,n
!x0 est l'abscisse initial
!x_final est l'abscisse final
! (x,y) sont les points dans le repère cartésien
!y0 est la condition initial.Elle correspond à la position de la particule fluide quand on est loin des éoliennes. chaque particule occupera donc k*y0 comme ordonnée.




  open(unit=2,file='result.dat',status='Unknown')
  do k=1,m
    x0 = -100._PR
    y0 = k*d
    x_final = 150._Pr
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
    y0 = k*d
    x_final = 150._Pr
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
