program main
  implicit none

  ! matrix of training data, containing x values and expected y values
  real :: data(2, 32)

  ! model parameters
  real :: weight, bias

  ! counters
  integer :: i, j
 
  weight = 0.38
  bias = 0

  do i=1,32
      read (*,*) data(1, i), data(2, i)
  enddo

  call data_display(data)
  call train(data, 0.001, weight, bias)
contains
  subroutine data_display(in_data)
    real, intent(in) :: in_data(2, 32)

    do i = 1,32
      print*, in_data(1, i), in_data(2, i)
    end do
  end subroutine data_display

  real function forward(x, in_weight, in_bias)
    real, intent(in) :: x, in_weight, in_bias
    
    forward = x * in_weight + in_bias
  end function forward

  subroutine train(in_data, learning_rate, in_weight, in_bias)
    real, intent(in) :: in_data(2, 32)
    real, intent(in) :: learning_rate

    real, intent(inout) :: in_weight, in_bias

    real :: x, y_pred

    do x = 1,32
      y_pred = forward(x, in_weight, in_bias)

      ! TODO: calculate gradients & adjust params
    end do
  end
end program main
