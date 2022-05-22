#Monte Carlo Całka  
x <- runif(1000,0,1)

f <- function(x) { return (sin(x)) }

f <- function(x) { return (x^2 + 1) }

MonteCarloIntegral <- function(f, x_start, x_stop, N) {
  
  if (!is.function(f)) {
    stop("\nf is not a function!")
  }

  losVec <- runif(N, 0, 1)
  theta <- x_start + losVec * (x_stop - x_start)
  
  f_out <- f(theta)
  
  out <- ((x_stop - x_start)/N) * sum(f_out)
  
  return (out)
}

MonteCarloIntegral(f, -1, 1, 1000)
