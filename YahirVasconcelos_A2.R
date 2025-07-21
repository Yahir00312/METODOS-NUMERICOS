#        == Método de la Secante == 

# Valores iniciales
x0 = 1
xant = 0
error = 1  # Inicializamos error en 1 para entrar al ciclo

# Error permitido (delta)
delt = 0.00000001

# Número máximo de iteraciones 
n = 50

# Función para encontrar la raíz
f = function(x) sin(x) + cos(1 - x^2) - 1

# Cálculos usando el método
for (i in 1:n) {
  numera = f(x0) * (xant - x0)
  denomi = f(xant) - f(x0)
  
  if (abs(denomi) < 1e-10) {
    cat("Se detuvo porque el denominador es muy pequeño (posible división por cero).\n")
    break
  }
  
  x1 = x0 - (numera / denomi)
  print(c(i, x0, xant, x1))
  
  error = abs(x1 - x0)
  
  if (!is.na(error) && error < delt) {
    cat("\nLa solución converge en ", i, " iteraciones. raíz= ", x1, "\n")
    break
  }
  
  xant = x0
  x0 = x1
}

if (!is.na(error) && error >= delt) {
  print("Máximo número de iteraciones alcanzada !!!")
}

