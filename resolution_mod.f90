module resolution_mod
  use constantes_mod
  use RK4mod
  implicit none


contains
  subroutine resolve()
    integer::k,n,i
    real(PR)::x0,y0,x_final,x,y
    open(unit=2,file='result.dat',status='Unknown')
    do k=1,m
      x0 = -100._PR
      y0 = k*d
      x_final = 200._Pr
      x = x0
      y = y0
      n = int((x_final - x0)/h + 0.5)
      write(2,*) x ,'NaN'
        do i = 1, n
          call rk4_step(x, y, h)
          x = x + h
          write(2,*) x ,y
        end do
    end do


    do k=-m,-1
      x0 = -100._PR !condition initiale
      y0 = k*d
      x_final = 200._Pr
      x = x0
      y = y0
      n = int((x_final - x0)/h + 0.5)
      write(2,*) x ,'NaN'
        do i = 1, n
          call rk4_step(x, y, h)
          x = x + h
          write(2,*) x ,y
        end do
    end do
    close (2)







  end subroutine
  subroutine erreuranalyse()
    integer::k,ng,nf,i
    real(PR)::x0,y0,x_final,x,y,E
    real(PR),dimension(:),allocatable::tabg,tabf

    open(unit=3,file='erreur.dat',status='Unknown')
    do k=1,m
      x0 = -100._PR
      y0 = k*d
      x_final = 200._Pr
      x = x0
      y = y0
      ng = int((x_final - x0)/h + 0.5)
      nf= int((x_final - x0)/(h/2._PR) + 0.5)
      allocate(tabg(ng),tabf(nf))

        do i = 1, ng
          call rk4_step(x, y, h)
          tabg(i)=y
          x = x + h
        end do

        do i = 1, nf
          call rk4_step(x, y, h/2._Pr)
          tabf(i)=y
          x = x + h/2._PR
        end do
        E=0._PR
        do i=1,ng
          E=E+((tabf(2*i)-tabg(i))**2)*h
        end do
        E=sqrt(E)
        write(3,*)E
        deallocate(tabf,tabg)
    end do
    close(3)

end subroutine









end module
