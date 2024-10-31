program nfn_reg_lin
  implicit none

  ! ---------------------------------- !
  ! OPTIONS
  ! ---------------------------------- !

  ! Amount of epochs to train for
  integer, parameter :: epochs = 512

  ! Amount of training data entries
  integer, parameter :: training_data_entries = 32

  ! Amount of test data entries
  integer, parameter :: test_data_entries = 32

  ! Learning rate
  real, parameter :: learning_rate = 0.001

  ! ---------------------------------- !
  ! ACTUAL VARIABLES
  ! ---------------------------------- !

  ! Matrix of training data. Contains x values (on the first dimension) 
  ! and expected y values (on the second dimension)
  real :: training_data(2, training_data_entries)

  ! Matrix of test data. Structured the same as training data.
  ! This data is not used for training but rather verifying the model's accuracy.
  real :: test_data(2, test_data_entries)

  ! Model Parameters
  real :: weight, bias

  ! Counters
  integer :: i, j

  ! ---------------------------------- !
 
  ! initialize weight (randomly) and bias (non randomly)
  call RANDOM_INIT(.false., .true.)
  call RANDOM_NUMBER(weight)

  bias = 0

  call data_read(training_data_entries, training_data)
  call data_read(test_data_entries, test_data)

  ! call data_display(data_entries, data)

  do i=1, epochs
    call train(training_data_entries, training_data, learning_rate, weight, bias)
    call test_accuracy(test_data_entries, test_data, weight, bias)
  enddo
contains
  ! Utility subroutine to read tab-separated X and Y values from stdin.
  subroutine data_read(data_size_in, data_in)
    integer, intent(in) :: data_size_in
    real, intent(inout) :: data_in(2, data_size_in)

    do i=1,data_size_in
        read (*,*) data_in(1, i), data_in(2, i)
    enddo
  end subroutine data_read

  ! Utility subroutine to display x and y values (tab-separated)
  ! from a given dataset.
  subroutine data_display(data_size_in, data_in)
    integer, intent(in) :: data_size_in
    real, intent(in) :: data_in(2, data_size_in)

    do i = 1,data_size_in
      print*, data_in(1, i), data_in(2, i)
    end do
  end subroutine data_display

  ! Forward pass function. Given an x value, it makes a prediction
  ! about what the y value would be in the function we're trying
  ! to predict, using the weight and bias.
  real function forward(x, weight_in, bias_in)
    real, intent(in) :: x, weight_in, bias_in
    
    forward = x * weight_in + bias_in
  end function forward

  ! Adjusts the weight and bias of the model by passing a dataset
  ! through it and calculating the gradients.
  subroutine train(data_size_in, data_in, learning_rate_in, weight_in, bias_in)
    integer, intent(in) :: data_size_in

    real, intent(in) :: data_in(2, data_size_in)
    real, intent(in) :: learning_rate_in

    real, intent(inout) :: weight_in, bias_in

    real :: x_real, y_real, y_pred, grad_weight, grad_bias

    do j = 1,data_size_in
      ! get real values from the dataset
      x_real = data_in(1, j)
      y_real = data_in(2, j)

      ! make prediction (forward pass)
      y_pred = forward(x_real, weight_in, bias_in)

      ! calculate gradients (backward pass)

      ! derivative of MSE with respect to weight
      grad_weight = 2 * (y_pred - y_real) * x_real

      ! derivative of MSE with respect to bias
      grad_bias = 2 * (y_pred - y_real)

      ! update weights
      weight_in = weight_in - learning_rate * grad_weight
      bias_in = bias_in - learning_rate * grad_bias
    end do
  end

  ! Passes test data through the model and calculates (and prints) the loss,
  ! without adjusting the model's parameters.
  subroutine test_accuracy(data_size_in, data_in, weight_in, bias_in)
    integer, intent(in) :: data_size_in

    real, intent(in) :: data_in(2, data_size_in)

    real, intent(inout) :: weight_in, bias_in

    real :: x_real, y_real, y_pred, loss, loss_sum

    loss_sum = 0

    do j = 1,data_size_in
      ! get real values from the dataset
      x_real = data_in(1, j)
      y_real = data_in(2, j)

      ! make prediction (forward pass)
      y_pred = forward(x_real, weight_in, bias_in)

      ! calculate loss (for calculating the average at the end of the epoch)
      loss = (y_pred - y_real) ** 2
      loss_sum = loss_sum + loss
    end do

    write (6, '(F24.18)') loss_sum / data_size_in
  end

end program nfn_reg_lin
