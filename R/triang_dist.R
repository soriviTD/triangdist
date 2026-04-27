#min: a (valor donde empieza el triángulo)
#max: b (valor donde acaba el triángulo)
#mode: c (valor en x donde está la punta del triángulo, es el más común)

#Para calcular el área, es decir, la densidad, hay que dividir
#el triángulo en dos subtriángulos. Trazando una línea vertical
#desde la punta (c) hasta la base.
#La altura está en: 2/(b-a)

#Se calcula el área de cada triángulo por separado.

#(-b+-sqrt(b^2-4*a*c))/2*a

#Fórmula para calcular una ecuación de segundo grado.
res_g2 <- function(a, b, c){
  x = NA
  raiz = b^2-4*a*c
  if (raiz > 0){
    x1 <- -b+sqrt(raiz)/2*a
    x2 <- -b-sqrt(raiz)/2*a
    if (x1 >= 0){
      x <- x1
    } else if (x2 >= 0){
      x <- x2
    } else {
      stop('El valor introducido no es válido.')
    }
  } else if (raiz == 0) {
    x1 <- -b/2*a
    if (x1 >= 0){
      x <- x1
    }
  } else {
    stop('El valor introducido no es válido.')
  }
  return (x)
}

#Density function.
#La altura en un punto exacto.
dtriang <- function(x, min, max, mode){
  if (x == mode) {
    val = 2/(max-min)
  } else if (x<min | x>max){
    val = 0
  } else if (x < mode){
    val = 2*(x-min)/((max-min)*(mode-min))
  } else if (x>mode){
    val = 2*(max-x)/((max-min)*(max-mode))
  }
  return (val)
}

#Distribution function.
#Área bajo la curva a la izquierda de un punto q (P(X<q)).
ptriang <- function(q, min, max, mode){
  h = 0
  if (q == mode) {
    h = 2/(max-min)
  } else if (q<=min){
    val = 0
  } else if (q>=max){
    val = 1
  } else if (q < mode){
    h = 2*(q-min)/((max-min)*(mode-min))
    val = 0.5*h*q
  } else if (q>mode){
    h = 2*(max-q)/((max-min)*(max-mode))
    val = 1-(max-q)*h*0.5
  }
  return (val)
}

#Quantile function.
#Le das la probabilidad y te dice qué valor de x le corresponde (inversa de
#ptriang).
qtriang <- function(p, min, max, mode){
  h_mode = 2/(max-min)
  p_mode = ptriang(mode, min, max, mode)
  if (p>1 | p<0){
    stop('El valor introducido debe estar entre 0 y 1 incluidos.') #Error
  } else {
    if (p == p_mode){
      val = mode
    } else if (p < p_mode){
      val <- res_g2(2, -4*min, min^2-(max-min)*(mode-min)*2*p)
    } else if (p > p_mode) {
      aux <- res_g2(1, 2*max, max^2-(1-p)*(max-min)*(max-mode))
      x <- max-aux
    }
  }
}

#Random generation
rtriang <- function(n, min, max, mode){
  val <- runif(n, 0, 1)
  val <- qtriang(val, min, max, mode)
  return (val)
}




#Source/Reference (ver cuál)
#https://www.youtube.com/watch?v=kYmx_h5ril0
#Ayuda de R para runif function
