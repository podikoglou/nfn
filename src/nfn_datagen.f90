program nfn_datagen
  implicit none

  ! ---------------------------------- !
  ! OPTIONS
  ! ---------------------------------- !

  ! Dataset Size, in entries
  integer, parameter :: size = 64

  ! a and c in ax + c
  real, parameter :: a = 31.3829
  real, parameter :: c = 7.12

  ! ---------------------------------- !
  ! ACTUAL VARIABLES
  ! ---------------------------------- !

  ! Counters
  integer :: i

  ! Data
  real :: x, y

  ! initialize rng
  call RANDOM_INIT(.false., .true.)

  do i=1,size
    ! generate random x value
    call RANDOM_NUMBER(x)
    x = x * 10

    ! calculate y value
    y = f(x)

    print*, x, y
  enddo

contains
  real function f(x_in)
    real, intent(in) :: x_in

    f = a * x + c
  end function f
end program nfn_datagen
