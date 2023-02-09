module particule_mod
  use constantes_mod
  implicit none
  Type particule
    real(PR),dimension(100)::r,teta
  end type

end module
