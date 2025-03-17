## PROJEKT RÓWNANIA RÓŻNICZKOWE I RÓŻNICOWE ###
## dla n punktów,przy czym n dowolne
## calki obliczone metoda prostokatow
get_value_of_x_i <- function(i, n)
{
  return (i * 3 / n);
}
get_value_of_e_i_in_point_x <- function(i, x, n)
{
  x_i_minus_1 <- get_value_of_x_i(i-1,n)
  x_i <- get_value_of_x_i(i,n)
  x_i_plus_1 <- get_value_of_x_i(i+1,n)
  if ( x > x_i_minus_1 & x <= x_i)
  {
    return (n/3*(x-x_i_minus_1))
  }
  else if ( x > x_i & x < x_i_plus_1)
  {
    return (n/3*(x_i_plus_1-x))
  }
  else
  {
    return (0.0)
  }
}
get_vector_value_of_e_i <- function(i, x, n)
{
  x_i_minus_1 <- get_value_of_x_i(i - 1, n)
  x_i <- get_value_of_x_i(i, n)
  x_i_plus_1 <- get_value_of_x_i(i + 1, n)
  cond1 <- (x > x_i_minus_1) & (x <= x_i)
  cond2 <- (x > x_i) & (x < x_i_plus_1)
  result <- numeric(length(x))
  result[cond1] <- n / 3 * (x[cond1] - x_i_minus_1)
  result[cond2] <- n / 3 * (x_i_plus_1 - x[cond2])
  return(result)
}
get_integral_of_e_i <- function(i,integral_start,integral_end,n)
{
  x_start <- get_value_of_x_i(i-1,n)
  x_end <- get_value_of_x_i(i+1,n)
  start_to_integrate <- max(x_start,integral_start)
  end_to_integrate <- min(x_end, integral_end)
  if (start_to_integrate >= end_to_integrate)
  {
    return (0.0)
  }
  else
  {
    ### rozmiar prostokacika
    dist <- (end_to_integrate - start_to_integrate) / n*10
    sum <- 0.0
    for ( j in 0:(10*n-1) )
    {
      x <- start_to_integrate + j*dist
      sum <- sum + get_value_of_e_i_in_point_x(i,x,n)*dist
    }
    return (sum)
  }
}
## macierz B
b_martix_index_i_j <- function(i,j,n){
  if(abs(i-j) > 1)
  {
    return (0.0)
  }
  else if ( abs(i-j) == 1)
  {
    return (n/3)
  }
  else
  {
    return ((-2) * n/3)
  }
}
b_matrix <- function(n)
{
  main_matrix <- matrix(nrow=n-1,ncol=n-1)
  for ( i in 1:(n - 1) )
  {
    for ( j in 1:(n - 1) )
    {
      main_matrix[j, i]<-b_martix_index_i_j(i,j,n)
    }
  }
  return (main_matrix)
}
## macierz L
## dziedzina jest od 0 do 3
l_matrix_i <- function(i, n)
{
  return ( -40*pi*get_integral_of_e_i(i,0,1,n) + 4*pi*get_integral_of_e_i(i,1,2,n) - 40*pi*get_integral_of_e_i(i,2,3,n))
}
l_matrix <- function(n) {
  main_matrix <- numeric(n - 1)
  for (i in 1:(n - 1)) {
    main_matrix[i] <- l_matrix_i(i, n)
  }
  return(matrix(main_matrix, ncol = 1))
}
fi <- function(x, w, n) {
  suma <- numeric(length(x))
  for (i in 1:(n - 1))
  {
    suma <- suma + w[i] * get_vector_value_of_e_i(i, x, n)
  }
  # nalezy dodac przesuniecie do wektora
  return (5 - x + suma)
}
main <- function(n)
{
  b <- b_matrix(n)
  l <- l_matrix(n)
  w <- solve(b, l)
  x <- seq(0, 3, length.out = n)
  y <- fi(x, w, n)
  plot(x, y, type = "l", col = "green", lwd = 3, main = "fi(x)",
       xlab = "x", ylab = "fi(x)")
}
main(200)
