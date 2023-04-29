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
    real(PR)           ::res,gamma,u_infini,a,cx,cy,uppersum,lowersum,xk,yk,var1,varx,vary
    integer            ::N,k,l,case
    open(unit=1,file='constantes.dat',status='Unknown')
    read (1,*) case
    read(1,*) u_infini !vitesse d'écoulement de l'air loin des éoliennes
    close(1)
    uppersum=0._Pr
    lowersum=0._Pr
    select case(case)
    case(1)
      open(unit=1,file='eoliennes.dat',status='Unknown')
      read(1,*) N
      do l=1,N
        read(1,*) xk,yk,a,gamma
        var1=(1/((x-xk)**2+(y-yk)**2))
        uppersum=uppersum+(gamma/(2*Pi))*(x-xk)*var1-2*u_infini*(a**2)*(var1**2)*(x-xk)*(y-yk)
        lowersum=lowersum+(gamma/(2*Pi))*(y-yk)*var1+u_infini*(a**2)*(var1**2)*((x-xk)**2-(y-yk)**2)
      end do
      res=(uppersum/(u_infini-lowersum))
      close(1)



    case(2)
      open(unit=1,file='carre_eoliennes.dat',status='Unknown')
      read(1,*) a
      read(1,*) N
      read(1,*) cx
      read(1,*) cy
      read(1,*) gamma
      close(1)
      do l=0,2*N
        do k=0,2*N
          var1=(1/(((x-k*cx)**2+(y-(l-N)*cy)**2)**2))
          varx=(x-k*cx)
          vary=(y-(l-N)*cy)
          uppersum=uppersum+(gamma/(2*Pi))*varx*(1/(varx**2+vary**2))-2*u_infini*(a**2)*var1*varx*vary
          lowersum=lowersum+(gamma/(2*Pi))*vary*(1/(varx**2+vary**2))+u_infini*(a**2)*var1*(varx**2-vary**2)
        end do
      end do
    res=(uppersum/(u_infini-lowersum))

    end select
  end function f
  ! 
  ! function valid(x,y) result(res)
  !   real(PR),intent(in)::x,y
  !   real(PR)         ::res
  !
  !
  ! end function valid





end module
