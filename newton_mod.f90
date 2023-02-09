module newton_mod
  use constantes_mod
  use particule_mod
  implicit none


contains
  subroutine f(r,p,res)
    real(PR),intent(in)::r
    type(particule),intent(in)::p
    real(PR),intent(out)::res
  end subroutine f



  function derivee(r) result (f_prime)
  real(PR),intent(in)::r
  real(PR)::f_prime

  end function derivee

  subroutine newton(r_zero,zero)
    real(PR),intent(in)::r_zero
    real(PR),intent(out)::zero
    real(PR)            ::r
    r=r_zero

  end subroutine newton







end module
