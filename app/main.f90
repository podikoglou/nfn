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

  !call data_display(data)

do j=1, 32
  call train(data, 0.001, weight, bias)
  enddo
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

    real :: x, x_real, y_real, y_pred, new_weight, new_bias, grad_weight, grad_bias, loss, loss_sum

    loss_sum = 0

    do x = 1,32
      x_real = in_data(1, x)
      y_real = in_data(2, x)

      y_pred = forward(x_real, in_weight, in_bias)

      grad_weight = 2 * (y_pred - y_real) * x_real
      grad_bias = 2 * (y_pred - y_real) * 2

      new_weight = in_weight - learning_rate * grad_weight
      new_bias= in_bias - learning_rate * grad_bias

      loss = (y_pred - y_real) ** 2

      loss_sum = loss_sum + loss
    end do

      print*, loss_sum/32
  end
end program main
