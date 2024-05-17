program interpolazione
    implicit none

    ! Dichiarazione delle variabili
    real:: x_input(10) = [1.0, 1.7, 2.8, 5.0, 5.65, 7.2, 11.3, 12.0, 13.7, 21.3] ! Dati x di input
    integer:: y_input(10) = [10, 15, 20, 25, 30, 35, 40, 45, 50, 55] ! Dati y di input
    ! real:: x_input(2) = [5.0, 10.0] ! Dati x di input
    ! integer:: y_input(2) = [1, 2] ! Dati y di input
    real, dimension(:), allocatable:: y_interpolati  ! Valori interpolati di y
    integer:: i, ii
    integer:: start, last, pace, length
    integer, dimension(:), allocatable:: arr
    
    ! Numero di punti su cui interpolare
    pace = 1
    start = minval(x_input)
    last = maxval(x_input)
    length = ceiling(real((last-start)/real(pace))+1.0)  
    allocate(arr(length))
    
    arr(1) = start
    do i = 2, length
      arr(i) = start+((i-1)*pace)
    end do

    allocate(y_interpolati(length))
    call interplin(x_input, y_input, size(x_input), arr, y_interpolati, length)

    print *, "Valori interpolati di y:"
    do ii = 1, length
      write(6, '(a, i5, a, f6.2)') "x = ", arr(ii), ", y = ", y_interpolati(ii)
    end do

contains

    subroutine interplin(x_input, y_input, n_input, x_interpolati, y_interpolati, m_interpolati)
        implicit none
        real, dimension(:), intent(in):: x_input
        integer, dimension(:), intent(in):: y_input 
        integer, intent(in):: n_input                         ! Numero di punti dati
        integer, dimension(:), intent(in):: x_interpolati     ! Punti x su cui interpolare
        real, dimension(:), intent(out):: y_interpolati       ! Valori interpolati di y
        integer, intent(in):: m_interpolati                   ! Numero di punti su cui interpolare
        real:: slope, intercept
        integer:: i, j

        do j = 1, m_interpolati
          if (x_interpolati(j) < x_input(1)) then
            y_interpolati(j) = -9999.00  
          elseif (x_interpolati(j) > x_input(n_input)) then
            y_interpolati(j) = y_input(n_input)
            cycle  
          end if
          do i = 1, n_input
            if (x_input(i) < x_interpolati(j) .and. x_interpolati(j) < x_input(i+1)) then
              slope = (y_input(i+1) - y_input(i)) / (x_input(i+1) - x_input(i))
              intercept = y_input(i) - slope*x_input(i)
              y_interpolati(j) = slope*x_interpolati(j) + intercept
              exit  
            elseif (x_interpolati(j) == x_input(i)) then
              y_interpolati(j) = y_input(i)
            end if
          end do
        end do
    end subroutine interplin

end program interpolazione

