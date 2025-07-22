# Definimos la matriz y el vector b correctamente
A <- matrix(c(3, -1, -1,
              -1, 3, 1,
              2, 1, 4), 
            nrow = 3, byrow = TRUE)

b <- c(1, 3, 7)
x0 <- c(0, 0, 0)

# Función corregida Gauss-Seidel
gauss_seidel <- function(A, b, x0, tol, max_iter) {
  n <- length(b)
  x <- x0
  
  cat("Iter | x1       | x2       | x3       | Error\n")
  cat("------------------------------------------------\n")
  
  for (k in 1:max_iter) {
    x_new <- x
    
    for (i in 1:n) {
      sum1 <- 0
      sum2 <- 0
      if (i > 1) {
        sum1 <- sum(A[i, 1:(i-1)] * x_new[1:(i-1)])
      }
      if (i < n) {
        sum2 <- sum(A[i, (i+1):n] * x[(i+1):n])
      }
      x_new[i] <- (b[i] - sum1 - sum2) / A[i, i]
    }
    
    error <- max(abs(x_new - x))
    cat(sprintf("%4d | %.6f | %.6f | %.6f | %.6f\n",
                k, x_new[1], x_new[2], x_new[3], error))
    
    if (error < tol) {
      cat("------------------------------------------------\n")
      cat("Solución aproximada por Gauss-Seidel:\n")
      print(x_new)
      cat("Error final:", error, "\n")
      return(x_new)
    }
    x <- x_new
  }
  cat("------------------------------------------------\n")
  cat("Solución final tras", max_iter, "iteraciones:\n")
  print(x)
  return(x)
}

# Llamada a la función
sol_gauss <- gauss_seidel(A, b, x0, 1e-4, 100)


