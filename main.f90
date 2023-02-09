program solution_equa_diff
  use particule_mod
  use constantes_mod
  use newton_mod
  implicit none
  integer::i,k1,k2,k3
  type(particule)::p
  k2=2
  k3=1

  open(unit=2,file='shemaup2.dat',status='Unknown')
  do i=2,99
    p%teta(i)=(i-1)*pi/(99)
    p%r(i)=(k2*u_infini*y0+sqrt((k2*u_infini*y0)**2+4*(a*u_infini*sin(p%teta(i)))**2))/(2*u_infini*sin(p%teta(i)))
    write(2,*) p%r(i)*cos(p%teta(i)),p%r(i)*sin(p%teta(i))
  end do
  close(2)

  open(unit=3,file='shemaup3.dat',status='Unknown')
  do i=2,99
    p%teta(i)=(i-1)*pi/(99)
    p%r(i)=(k3*u_infini*y0+sqrt((k3*u_infini*y0)**2+4*(a*u_infini*sin(p%teta(i)))**2))/(2*u_infini*sin(p%teta(i)))
    write(3,*) p%r(i)*cos(p%teta(i)),p%r(i)*sin(p%teta(i))
  end do
  close(3)

  open(unit=4,file='shemadown2.dat',status='Unknown')
  do i=2,99
    p%teta(i)=(i-1)*pi/(99)
    p%r(i)=-(k2*u_infini*y0+sqrt((k2*u_infini*y0)**2+4*(a*u_infini*sin(p%teta(i)))**2))/(2*u_infini*sin(p%teta(i)))
    write(4,*) p%r(i)*cos(p%teta(i)),p%r(i)*sin(p%teta(i))
  end do
  close(4)

  open(unit=6,file='shemadown3.dat',status='Unknown')
  do i=2,99
    p%teta(i)=(i-1)*pi/(99)
    p%r(i)=-(k3*u_infini*y0+sqrt((k3*u_infini*y0)**2+4*(a*u_infini*sin(p%teta(i)))**2))/(2*u_infini*sin(p%teta(i)))
    write(6,*) p%r(i)*cos(p%teta(i)),p%r(i)*sin(p%teta(i))
  end do
  close(6)
   open (unit=8,file='shemaeolienne.dat',status='unknown')





end program
