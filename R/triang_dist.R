'''
min: a (valor donde empieza el triángulo)
max: b (valor donde acaba el triángulo)
mode: c (valor en x donde está la punta del triángulo, es el más común)

Para calcular el área, es decir, la densidad, hay que dividir
el triángulo en dos subtriángulos. Trazando una línea vertical
desde la punta (c) hasta la base.
La altura está en: 2/(b-a)

Se calcula el área de cada triángulo por separado.

'''

#Density function.
dtriang <- function(x, min, max, mode){
  val = NA
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
ptriang <- function(q, min, max, mode){

}

#Quantile function.
qtriang <- function(p, min, max, mode){

}

#Random generation
rtriang <- function(n, min, max, mode){

}


"""
Source/Reference (ver cuál)
https://www.youtube.com/watch?v=kYmx_h5ril0
