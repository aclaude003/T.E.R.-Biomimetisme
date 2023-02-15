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
    real(PR)           ::res,gamma,u_infini,a,cx,cy,s,t,var1
    integer            ::N,k,l
    open(unit=1,file='constantes.dat',status='Unknown')
    read (1,*) a !le diamètre de chaque éolienne
    read(1,*) N !(2N+1)^2 est le nombre d'éolienne
    read(1,*) cx ! la disatance entre deux centres d'éoliennes alignées paralèlement à l'axe des abscisses
    read(1,*) cy !la distance entre deux centres d'éoliennes alignées paralèlement à l'axe des ordonnées
    read(1,*) u_infini !vitesse d'écoulement de l'air loin des éoliennes
    read(1,*) gamma !force du tourbillon
    close(1)
    s=0._Pr
    t=0._Pr
    do l=0,2*N
      do k=0,2*N !on implémente f(x,y)
        var1=(1/(((x-k*cx)**2+(y-(l-N)*cy)**2)**2))
        s=s+(gamma/(2*Pi))*(x-k*cx)*(1/((x-k*cx)**2+(y-(l-N)*cy)**2))-2*u_infini*(a**2)*var1*(x-k*cx)*(y-(l-N)*cy)
        t=t+(gamma/(2*Pi))*(y-(l-N)*cy)*(1/((x-k*cx)**2+(y-(l-N)*cy)**2))+u_infini*(a**2)*var1*((x-k*cx)**2-(y-(l-N)*cy)**2)
      end do
    end do
    res=(s/(u_infini-t))
  end function






end module
