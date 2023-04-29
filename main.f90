program lignescourant
  use constantes_mod
  use RK4mod
  use resolution_mod
  implicit none
  integer::k,N
  real(PR)::a,xk,yk
! Le pas de discrétisation de l'axe des abscisses

!x0 est l'abscisse initial
!x_final est l'abscisse final
! (x,y) sont les points dans le repère cartésien
!y0 est la condition initial.Elle correspond à la position de la particule fluide quand on est loin des éoliennes. chaque particule occupera donc k*y0 comme ordonnée.


!!!!!premier programme

call resolve
call erreuranalyse

!affichage des cylindres
  open(unit=1,file='commandes_gnuplot.txt',status='Unknown')
  write(1,*) "plot 'result.dat' w l"
    write(1,*) "unset key"
  open(unit=2,file='eoliennes.dat',status='Unknown')
  read(2,*) N
  do k=1,N
    read(2,*) xk,yk,a
    ! write(1,*) 'set object circle at',xk,',',yk,'size',a,'fillstyle empty'
    ! write(1,*) 'replot '

    write(1,*) "replot",yk,'+sqrt(',a**2,'-(x-',xk,')**2)'
    write(1,*) "unset key"
    write(1,*) "replot",yk,'-sqrt(',a**2,'-(x-',xk,')**2)'
    write(1,*) "unset key"
  end do
  close(2)
  close(1)


























  !!!!!validation
! open(unit=2,file='erreur.dat',status='Unknown')
! do j=0,4
!   do k=1,m
!     x0 = -100._PR
!     y0 = k*d
!     x_final = 150._Pr
!     x = x0
!     y = y0
!     n = int((x_final - x0)/h + 0.5)*2**j
!       do i = 1, n
!         call rk4_step(x, y, h)
!         x = x + h
!         write(2,*) x ,y
!       end do
!   end do
!   write(2,*) 1._PR/(n+1),exp()














end program
