module RK4mod
  use constantes_mod
  implicit none







contains
  subroutine rk4_step(x,y,h)
    real(PR),intent(inout)::x,y
    real(PR),intent(in)   ::h
    real(PR)              ::k1,k2,k3,k4
    k1=h*f(x,y)
    k2=h*f(x+h/2,y+k1/2)
    k3=h*f(x+h/2,y+k2/2)
    k4=h*f(x+h,y+k3)
    y=y+(k1+2*k2+2*k3+k4)/6
  end subroutine
  function f(x,y) result(res)
    real(PR),intent(in)::x,y
    real(PR)           ::res,gamma,u_infini,a,c,s,t
    integer            ::N,k
    open(unit=1,file='constantes.dat',status='Unknown')
    read (1,*) a
    read(1,*) N
    read(1,*) c
    read(1,*) u_infini
    read(1,*) gamma
    close(1)
    s=0._Pr
    t=0._Pr
    do k=0,N
      s=s+(gamma/2*Pi)*(x-k*c)*(1/((x-k*c)**2+(y)**2))-2*u_infini*(a**2)*((1/((x-k*c)**2+(y)**2))**2)*(x-k*c)*y
      t=t+(gamma/2*Pi)*(y)*(1/((x-k*c)**2+(y)**2))+(u_infini*a**2)*((1/((x-k*c)**2+y**2))**2)*((x-k*c)**2-(y)**2)
    end do
    res=(s/(u_infini-t))
  end function






end module
