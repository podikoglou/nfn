program main
  implicit none

  ! matrix of training data, containing x values and expected y values
  real :: data(2, 32)

  ! counters
  integer :: i, j
 
  do i=1,32
      read (*,*) data(1, i), data(2, i)
  enddo

  call data_display(data)
contains
  subroutine data_display(in_data)
    real, intent(in) :: in_data(2, 32)

    do i = 1,32
      print*, in_data(1, i), in_data(2, i)
    end do
  end subroutine data_display
end program main
